package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/op/clip"
	"gioui.org/text"
)

type Events struct {
	Trace  *Trace
	Events []ptrace.EventID
	Filter struct {
		ShowGoCreate  widget.Bool
		ShowGoUnblock widget.Bool
		ShowGoSysCall widget.Bool
		ShowUserLog   widget.Bool
	}
	filteredEvents []ptrace.EventID
	list           widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (evs *Events) UpdateFilter() {
	// OPT(dh): if all filters are set, all events are shown. if no filters are set, no events are shown. neither case
	//   requires us to check each event.
	evs.filteredEvents = evs.filteredEvents[:0]
	for _, ev := range evs.Events {
		var b bool
		switch evs.Trace.Event(ev).Type {
		case trace.EvGoCreate:
			b = evs.Filter.ShowGoCreate.Value
		case trace.EvGoUnblock:
			b = evs.Filter.ShowGoUnblock.Value
		case trace.EvGoSysCall:
			b = evs.Filter.ShowGoSysCall.Value
		case trace.EvUserLog:
			b = evs.Filter.ShowUserLog.Value
		default:
			panic(fmt.Sprintf("unexpected type %v", evs.Trace.Event(ev).Type))
		}

		if b {
			evs.filteredEvents = append(evs.filteredEvents, ev)
		}
	}

	var max int
	for _, evID := range evs.filteredEvents {
		msgs := evs.eventMessage(evs.Trace.Event(evID))
		n := 0
		for _, msg := range msgs {
			n += len(msg)
		}
		if n > max {
			max = n
		}
	}
}

// Clicked returns all objects of text spans that have been clicked since the last call to Layout.
func (evs *Events) Clicked() []any {
	// This only allocates when links have been clicked, which is a very low frequency event.
	var out []any
	for i := 0; i < evs.texts.Len(); i++ {
		txt := evs.texts.Ptr(i)
		for j := range txt.Spans {
			if s := &txt.Spans[j]; s.Clickable != nil {
				for s.Clickable.Clicked() {
					out = append(out, s.Object)
				}
			}
		}
	}
	return out
}

func (evs *Events) eventMessage(ev *trace.Event) []string {
	switch ev.Type {
	case trace.EvGoCreate:
		return []string{"Created goroutine ", local.Sprintf("%d", ev.Args[trace.ArgGoCreateG])}
	case trace.EvGoUnblock:
		return []string{"Unblocked goroutine ", local.Sprintf("%d", ev.Args[trace.ArgGoUnblockG])}
	case trace.EvGoSysCall:
		if ev.StkID != 0 {
			frames := evs.Trace.Stacks[ev.StkID]
			fn := evs.Trace.PCs[frames[0]].Fn
			return []string{"Syscall ()", fn}
		} else {
			return []string{"Syscall"}
		}
	case trace.EvUserLog:
		cat := evs.Trace.Strings[ev.Args[trace.ArgUserLogKeyID]]
		msg := evs.Trace.Strings[ev.Args[trace.ArgUserLogMessage]]
		if cat != "" {
			return []string{"<> ", cat, msg}
		} else {
			return []string{msg}
		}
	default:
		panic(fmt.Sprintf("unhandled type %v", ev.Type))
	}
}

func (evs *Events) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Events.Layout").End()

	evs.list.Axis = layout.Vertical

	evs.timestampObjects.Reset()

	if evs.Filter.ShowGoCreate.Changed() ||
		evs.Filter.ShowGoUnblock.Changed() ||
		evs.Filter.ShowGoSysCall.Changed() ||
		evs.Filter.ShowUserLog.Changed() {
		evs.UpdateFilter()
	}

	columns := [...]string{
		"Time", "", "Message",
	}

	var txtCnt int
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		if col == 2 {
			gtx.Constraints.Min = image.Point{}
		}

		var txt *Text
		if txtCnt < evs.texts.Len() {
			txt = evs.texts.Ptr(txtCnt)
		} else {
			txt = evs.texts.Allocate(Text{})
		}
		txtCnt++
		txt.Reset(win.Theme)

		// OPT(dh): there are several allocations here, such as creating slices and using fmt.Sprintf

		if row == 0 {
			return widget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, win.Theme.TextSize, columns[col])
		} else {
			ev := evs.Trace.Event(evs.filteredEvents[row-1])
			// XXX styledtext wraps our spans if the window is too small

			addSpanG := func(gid uint64) {
				txt.Link(local.Sprintf("goroutine %d", gid), evs.Trace.G(gid))
			}

			addSpanTs := func(ts trace.Timestamp) {
				txt.Link(formatTimestamp(ts), evs.timestampObjects.Allocate(ts))
			}

			switch col {
			case 0:
				addSpanTs(ev.Ts)
			case 1:
			case 2:
				switch ev.Type {
				case trace.EvGoCreate:
					txt.Span("Created ")
					addSpanG(ev.Args[trace.ArgGoCreateG])
				case trace.EvGoUnblock:
					txt.Span("Unblocked ")
					addSpanG(ev.Args[trace.ArgGoUnblockG])
				case trace.EvGoSysCall:
					if ev.StkID != 0 {
						frames := evs.Trace.Stacks[ev.StkID]
						fn := evs.Trace.PCs[frames[0]].Fn
						txt.Span("Syscall (")
						txt.Span(fn)
						txt.Span(")")
					} else {
						txt.Span("Syscall")
					}
				case trace.EvUserLog:
					cat := evs.Trace.Strings[ev.Args[trace.ArgUserLogKeyID]]
					msg := evs.Trace.Strings[ev.Args[trace.ArgUserLogMessage]]
					if cat != "" {
						txt.Span("<")
						txt.Span(cat)
						txt.Span("> ")
						txt.Span(msg)
					} else {
						txt.Span(msg)
					}
				default:
					panic(fmt.Sprintf("unhandled type %v", ev.Type))
				}
			default:
				panic("unreachable")
			}
			// TODO(dh): hovering the entry should highlight the corresponding span marker
			if col == 0 && row != 0 {
				txt.Alignment = text.End
			}

			dims := txt.Layout(win, gtx)
			dims.Size = gtx.Constraints.Constrain(dims.Size)
			return dims
		}
	}

	ret := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
				layout.Rigid(theme.Dumb(win, theme.CheckBox(win.Theme, &evs.Filter.ShowGoCreate, "Goroutine creations").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(win.Theme, &evs.Filter.ShowGoUnblock, "Goroutine unblocks").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(win.Theme, &evs.Filter.ShowGoSysCall, "Syscalls").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(win.Theme, &evs.Filter.ShowUserLog, "User logs").Layout)),
			)
		}),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = gtx.Constraints.Max

			st := theme.List(win.Theme, &evs.list)
			st.EnableCrossScrolling = true
			return st.Layout(gtx, len(evs.filteredEvents)+1, func(gtx layout.Context, index int) layout.Dimensions {
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						gtx.Constraints.Min.X = 200
						gtx.Constraints.Max.X = 200
						dims := cellFn(gtx, index, 0)
						return dims
					}),
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return layout.Dimensions{Size: image.Pt(20, 0)}
					}),
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return cellFn(gtx, index, 2)
					}),
				)
			})
		}),
	)

	evs.texts.Truncate(txtCnt)

	return ret
}

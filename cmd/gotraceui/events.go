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
	"gioui.org/x/outlay"
)

type EventList struct {
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

func (evs *EventList) UpdateFilter() {
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
func (evs *EventList) Clicked() []TextEvent {
	// This only allocates when links have been clicked, which is a very low frequency event.
	var out []TextEvent
	for i := 0; i < evs.texts.Len(); i++ {
		txt := evs.texts.Ptr(i)
		out = append(out, txt.Events()...)
	}
	return out
}

func (evs *EventList) eventMessage(ev *trace.Event) []string {
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

var eventListColumns = []theme.TableListColumn{
	{
		Name: "Time",
		// FIXME(dh): the width depends on the font size and scaling
		MinWidth: 200,
		MaxWidth: 200,
	},

	{
		Name: "Message",
	},
}

func (evs *EventList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.EventList.Layout").End()

	evs.list.Axis = layout.Vertical

	evs.timestampObjects.Reset()

	if evs.Filter.ShowGoCreate.Changed() ||
		evs.Filter.ShowGoUnblock.Changed() ||
		evs.Filter.ShowGoSysCall.Changed() ||
		evs.Filter.ShowUserLog.Changed() {
		evs.UpdateFilter()
	}

	var txtCnt int
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		var txt *Text
		if txtCnt < evs.texts.Len() {
			txt = evs.texts.Ptr(txtCnt)
		} else {
			txt = evs.texts.Allocate(Text{})
		}
		txtCnt++
		txt.Reset(win.Theme)

		tb := TextBuilder{Theme: win.Theme}

		// OPT(dh): there are several allocations here, such as creating slices and using fmt.Sprintf

		ev := evs.Trace.Event(evs.filteredEvents[row])
		// XXX styledtext wraps our spans if the window is too small

		addSpanG := func(gid uint64) {
			tb.Link(local.Sprintf("goroutine %d", gid), evs.Trace.G(gid))
		}

		addSpanTs := func(ts trace.Timestamp) {
			tb.Link(formatTimestamp(ts), evs.timestampObjects.Allocate(ts))
		}

		switch col {
		case 0:
			addSpanTs(ev.Ts)
			txt.Alignment = text.End
		case 1:
			switch ev.Type {
			case trace.EvGoCreate:
				tb.Span("Created ")
				addSpanG(ev.Args[trace.ArgGoCreateG])
			case trace.EvGoUnblock:
				tb.Span("Unblocked ")
				addSpanG(ev.Args[trace.ArgGoUnblockG])
			case trace.EvGoSysCall:
				if ev.StkID != 0 {
					frames := evs.Trace.Stacks[ev.StkID]
					fn := evs.Trace.PCs[frames[0]].Fn
					tb.Span("Syscall (")
					tb.Span(fn)
					tb.Span(")")
				} else {
					tb.Span("Syscall")
				}
			case trace.EvUserLog:
				cat := evs.Trace.Strings[ev.Args[trace.ArgUserLogKeyID]]
				msg := evs.Trace.Strings[ev.Args[trace.ArgUserLogMessage]]
				if cat != "" {
					tb.Span("<")
					tb.Span(cat)
					tb.Span("> ")
					tb.Span(msg)
				} else {
					tb.Span(msg)
				}
			default:
				panic(fmt.Sprintf("unhandled type %v", ev.Type))
			}
		default:
			panic("unreachable")
		}
		// TODO(dh): hovering the entry should highlight the corresponding span marker

		dims := txt.Layout(win, gtx, tb.Spans)
		dims.Size = gtx.Constraints.Constrain(dims.Size)
		return dims
	}

	checkboxes := make([]Recording, 0, 4)
	var widestCheckbox Recording
	{
		gtx := gtx
		gtx.Constraints.Min = image.Point{}

		checkboxes = append(checkboxes,
			Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoCreate, "Goroutine creations").Layout),
			Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoUnblock, "Goroutine unblocks").Layout),
			Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoSysCall, "Syscalls").Layout),
			Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowUserLog, "User logs").Layout))

		for _, checkbox := range checkboxes {
			if checkbox.Dimensions.Size.X > widestCheckbox.Dimensions.Size.X {
				widestCheckbox = checkbox
			}
		}
	}
	// add padding
	widestCheckbox.Dimensions.Size.X += gtx.Dp(10)

	ret := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return outlay.FlowWrap{}.Layout(gtx, len(checkboxes), func(gtx layout.Context, i int) layout.Dimensions {
				checkboxes[i].Layout(win, gtx)
				return widestCheckbox.Dimensions
			})
		}),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = gtx.Constraints.Max

			tbl := theme.TableListStyle{
				Columns:       eventListColumns,
				List:          &evs.list,
				ColumnPadding: gtx.Dp(10),
			}

			return tbl.Layout(win, gtx, len(evs.filteredEvents), cellFn)
		}),
	)

	evs.texts.Truncate(txtCnt)

	return ret
}

package main

import (
	"context"
	"fmt"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	mywidget "honnef.co/go/gotraceui/widget"

	"gioui.org/layout"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/x/outlay"
	"golang.org/x/image/math/fixed"
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
	grid           outlay.Grid

	timestampLinks allocator[TimestampLink]
	goroutineLinks allocator[GoroutineLink]
	texts          allocator[Text]
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
}

// ClickedLinks returns all links that have been clicked since the last call to the method.
func (evs *Events) ClickedLinks() []Link {
	// This only allocates when links have been clicked, which is a very low frequency event.
	var out []Link
	for i := 0; i < evs.texts.Len(); i++ {
		txt := evs.texts.Ptr(i)
		for j := range txt.Spans {
			if s := &txt.Spans[j]; s.Clickable != nil {
				for s.Clickable.Clicked() {
					out = append(out, s.Link)
				}
			}
		}
	}
	return out
}

func (evs *Events) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Events.Layout").End()

	// XXX draw grid scrollbars

	evs.timestampLinks.Reset()
	evs.goroutineLinks.Reset()

	if evs.Filter.ShowGoCreate.Changed() ||
		evs.Filter.ShowGoUnblock.Changed() ||
		evs.Filter.ShowGoSysCall.Changed() ||
		evs.Filter.ShowUserLog.Changed() {
		evs.UpdateFilter()
	}

	evs.grid.LockedRows = 1

	dimmer := func(axis layout.Axis, index, constraint int) int {
		switch axis {
		case layout.Vertical:
			line := win.Theme.Shaper.LayoutString(text.Font{}, fixed.I(gtx.Sp(win.Theme.TextSize)), 0, gtx.Locale, "")[0]
			return line.Ascent.Ceil() + line.Descent.Ceil()
		case layout.Horizontal:
			// XXX don't guess the dimensions
			// XXX don't insist on a minimum if the window is too narrow or columns will overlap

			// XXX we do have to guess the dimensions. computing them accurately is too expensive if we have tens of
			// thousands of events because we can't shape them all. we can probably do some approximate math, pretending
			// the font is fixed width.
			switch index {
			case 0:
				return 200
			case 1:
				return 200
			case 2:
				w := constraint - 400
				if w < 0 {
					w = 0
				}
				return w
			default:
				panic("unreachable")
			}
		default:
			panic("unreachable")
		}
	}

	columns := [...]string{
		"Time", "Category", "Message",
	}

	var txtCnt int
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		var txt *Text
		if txtCnt < evs.texts.Len() {
			txt = evs.texts.Ptr(txtCnt)
			txt.Reset()
			txtCnt++
		} else {
			txt = evs.texts.Allocate(Text{theme: win.Theme})
			txtCnt++
		}

		// OPT(dh): there are several allocations here, such as creating slices and using fmt.Sprintf

		if row == 0 {
			return mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, win.Theme.TextSize, columns[col])
		} else {
			// XXX subtract padding from width

			ev := evs.Trace.Event(evs.filteredEvents[row-1])
			// XXX styledtext wraps our spans if the window is too small

			addSpanG := func(gid uint64) {
				txt.Link(local.Sprintf("goroutine %d", gid), evs.goroutineLinks.Allocate(GoroutineLink{evs.Trace.G(gid), GoroutineLinkKindOpenWindow}))
			}

			addSpanTs := func(ts trace.Timestamp) {
				txt.Link(formatTimestamp(ts), evs.timestampLinks.Allocate(TimestampLink{ts}))
			}

			switch col {
			case 0:
				addSpanTs(ev.Ts)
			case 1:
				if ev.Type == trace.EvUserLog {
					txt.Span(evs.Trace.Strings[ev.Args[trace.ArgUserLogKeyID]])
				}
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
						txt.Span(fmt.Sprintf("Syscall (%s)", fn))
					} else {
						txt.Span("Syscall")
					}
				case trace.EvUserLog:
					txt.Span(evs.Trace.Strings[ev.Args[trace.ArgUserLogMessage]])
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
			return txt.Layout(win, gtx)
		}
	}

	ret := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
				layout.Rigid(theme.Dumb(win, theme.CheckBox(&evs.Filter.ShowGoCreate, "Goroutine creations").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(&evs.Filter.ShowGoUnblock, "Goroutine unblocks").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(&evs.Filter.ShowGoSysCall, "Syscalls").Layout)),
				layout.Rigid(layout.Spacer{Width: 10}.Layout),

				layout.Rigid(theme.Dumb(win, theme.CheckBox(&evs.Filter.ShowUserLog, "User logs").Layout)),
			)
		}),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return evs.grid.Layout(gtx, len(evs.filteredEvents)+1, len(columns), dimmer, cellFn)
		}),
	)

	evs.texts.Truncate(txtCnt)

	return ret
}

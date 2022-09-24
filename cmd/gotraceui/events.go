package main

import (
	"fmt"
	"image"

	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"

	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/x/outlay"
	"golang.org/x/image/math/fixed"
)

type Events struct {
	theme     *theme.Theme
	trace     *Trace
	allEvents []EventID
	filter    struct {
		showGoCreate  widget.Bool
		showGoUnblock widget.Bool
		showGoSysCall widget.Bool
		showUserLog   widget.Bool
	}
	filteredEvents []EventID
	grid           outlay.Grid

	// slice used by ClickedLinks

	timestampLinks allocator[TimestampLink]
	goroutineLinks allocator[GoroutineLink]
	texts          allocator[Text]
}

func (evs *Events) updateFilter() {
	// OPT(dh): if all filters are set, all events are shown. if no filters are set, no events are shown. neither case
	//   requires us to check each event.
	evs.filteredEvents = evs.filteredEvents[:0]
	for _, ev := range evs.allEvents {
		var b bool
		switch evs.trace.Event(ev).Type {
		case trace.EvGoCreate:
			b = evs.filter.showGoCreate.Value
		case trace.EvGoUnblock:
			b = evs.filter.showGoUnblock.Value
		case trace.EvGoSysCall:
			b = evs.filter.showGoSysCall.Value
		case trace.EvUserLog:
			b = evs.filter.showUserLog.Value
		default:
			panic(fmt.Sprintf("unexpected type %v", evs.trace.Event(ev).Type))
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

func (evs *Events) Layout(gtx layout.Context) layout.Dimensions {
	// XXX draw grid scrollbars

	evs.timestampLinks.Reset()
	evs.goroutineLinks.Reset()

	if evs.filter.showGoCreate.Changed() ||
		evs.filter.showGoUnblock.Changed() ||
		evs.filter.showGoSysCall.Changed() ||
		evs.filter.showUserLog.Changed() {
		evs.updateFilter()
	}

	evs.grid.LockedRows = 1

	dimmer := func(axis layout.Axis, index, constraint int) int {
		switch axis {
		case layout.Vertical:
			line := evs.theme.Shaper.LayoutString(text.Font{}, fixed.I(gtx.Sp(evs.theme.TextSize)), 0, gtx.Locale, "")[0]
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
			txt = evs.texts.Allocate(Text{theme: evs.theme})
			txtCnt++
		}

		// OPT(dh): there are several allocations here, such as creating slices and using fmt.Sprintf

		if row == 0 {
			paint.ColorOp{Color: evs.theme.Palette.Foreground}.Add(gtx.Ops)
			return widget.Label{MaxLines: 1}.Layout(gtx, evs.theme.Shaper, text.Font{Weight: text.Bold}, evs.theme.TextSize, columns[col])
		} else {
			// XXX subtract padding from width

			ev := evs.trace.Event(evs.filteredEvents[row-1])
			// XXX styledtext wraps our spans if the window is too small

			addSpanG := func(gid uint64) {
				txt.Link(local.Sprintf("goroutine %d", gid), evs.goroutineLinks.Allocate(GoroutineLink{evs.trace.getG(gid), GoroutineLinkKindOpenWindow}))
			}

			addSpanTs := func(ts trace.Timestamp) {
				txt.Link(formatTimestamp(ts), evs.timestampLinks.Allocate(TimestampLink{ts}))
			}

			switch col {
			case 0:
				addSpanTs(ev.Ts)
			case 1:
				if ev.Type == trace.EvUserLog {
					txt.Span(evs.trace.Strings[ev.Args[trace.ArgUserLogKeyID]])
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
						frames := evs.trace.Stacks[ev.StkID]
						fn := evs.trace.PCs[frames[0]].Fn
						txt.Span(fmt.Sprintf("Syscall (%s)", fn))
					} else {
						txt.Span("Syscall")
					}
				case trace.EvUserLog:
					txt.Span(evs.trace.Strings[ev.Args[trace.ArgUserLogMessage]])
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
			return txt.Layout(gtx)
		}
	}

	dims := layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoCreate, "Goroutine creations").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoUnblock, "Goroutine unblocks").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoSysCall, "Syscalls").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showUserLog, "User logs").Layout),
	)

	defer op.Offset(image.Pt(0, dims.Size.Y)).Push(gtx.Ops).Pop()
	ret := evs.grid.Layout(gtx, len(evs.filteredEvents)+1, len(columns), dimmer, cellFn)
	evs.texts.Truncate(txtCnt)

	return ret
}

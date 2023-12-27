package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"sort"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
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
	Events Items[ptrace.EventID]
	Filter struct {
		ShowGoCreate  widget.Bool
		ShowGoUnblock widget.Bool
		ShowGoSysCall widget.Bool
		ShowUserLog   widget.Bool
	}
	filteredEvents SortedItems[ptrace.EventID]

	table       theme.Table
	scrollState theme.YScrollableListState

	timestampObjects mem.BucketSlice[trace.Timestamp]
	texts            mem.BucketSlice[Text]
	prevSpans        []TextSpan
}

func (evs *EventList) UpdateFilter() {
	if evs.Filter.ShowGoCreate.Value &&
		evs.Filter.ShowGoUnblock.Value &&
		evs.Filter.ShowGoSysCall.Value &&
		evs.Filter.ShowUserLog.Value {

		// Everything is shown
		evs.filteredEvents = NewSortedItems(evs.Events)
	} else if !evs.Filter.ShowGoCreate.Value &&
		!evs.Filter.ShowGoUnblock.Value &&
		!evs.Filter.ShowGoSysCall.Value &&
		!evs.Filter.ShowUserLog.Value {

		// Nothing is shown
		evs.filteredEvents = NewSortedItems(NoItems[ptrace.EventID]{})
	} else {
		// OPT(dh): multiple calls to FilterItems should be able to reuse memory
		evs.filteredEvents = NewSortedItems(FilterItems[ptrace.EventID](evs.Events, func(ev *ptrace.EventID) bool {
			switch evs.Trace.Event(*ev).Type {
			case trace.EvGoCreate:
				return evs.Filter.ShowGoCreate.Value
			case trace.EvGoUnblock:
				return evs.Filter.ShowGoUnblock.Value
			case trace.EvGoSysCall:
				return evs.Filter.ShowGoSysCall.Value
			case trace.EvUserLog:
				return evs.Filter.ShowUserLog.Value
			default:
				panic(fmt.Sprintf("unexpected type %v", evs.Trace.Event(*ev).Type))
			}
		}))
	}

	var max int
	for i := 0; i < evs.filteredEvents.Len(); i++ {
		evID := evs.filteredEvents.At(i)
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

// HoveredLink returns the link that has been hovered during the last call to Layout.
func (evs *EventList) HoveredLink() ObjectLink {
	for i := 0; i < evs.texts.Len(); i++ {
		txt := evs.texts.Ptr(i)
		if h := txt.HoveredLink(); h != nil {
			return h
		}
	}
	return nil
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

func (evs *EventList) sort() {
	evs.filteredEvents.Sort(func(ap, bp *ptrace.EventID) int {
		a, b := *ap, *bp
		// This function has to stay in sync with the cell function in Layout
		switch evs.table.SortedBy {
		case 0: // Time
			ea := evs.Trace.Event(a)
			eb := evs.Trace.Event(b)
			return cmp(ea.Ts, eb.Ts, evs.table.SortOrder == theme.SortDescending)
		case 1: // Message
			cellFn := func(evID ptrace.EventID) string {
				ev := evs.Trace.Event(evID)

				switch ev.Type {
				case trace.EvGoCreate:
					gid := ev.Args[trace.ArgGoCreateG]
					return local.Sprintf("Created goroutine %d", gid)
				case trace.EvGoUnblock:
					gid := ev.Args[trace.ArgGoUnblockG]
					return local.Sprintf("Unblocked goroutine %d", gid)
				case trace.EvGoSysCall:
					if ev.StkID != 0 {
						frames := evs.Trace.Stacks[ev.StkID]
						fn := evs.Trace.PCs[frames[0]].Fn
						return fmt.Sprintf("Syscall (%s)", fn)
					} else {
						return "Syscall"
					}
				case trace.EvUserLog:
					cat := evs.Trace.Strings[ev.Args[trace.ArgUserLogKeyID]]
					msg := evs.Trace.Strings[ev.Args[trace.ArgUserLogMessage]]
					if cat != "" {
						return fmt.Sprintf("<%s> %s", cat, msg)
					} else {
						return msg
					}
				default:
					panic(fmt.Sprintf("unhandled type %v", ev.Type))
				}
			}

			return cmp(cellFn(a), cellFn(b), evs.table.SortOrder == theme.SortDescending)
		default:
			panic(fmt.Sprintf("unreachable: %d", evs.table.SortedBy))
		}
	})
}

func (evs *EventList) Update(gtx layout.Context) []TextEvent {
	evs.table.Update(gtx)

	if _, ok := evs.table.SortByClickedColumn(); ok {
		evs.sort()
	}

	if evs.Filter.ShowGoCreate.Update(gtx) ||
		evs.Filter.ShowGoUnblock.Update(gtx) ||
		evs.Filter.ShowGoSysCall.Update(gtx) ||
		evs.Filter.ShowUserLog.Update(gtx) {
		evs.UpdateFilter()
	}

	var out []TextEvent
	for i := 0; i < evs.texts.Len(); i++ {
		out = append(out, evs.texts.Ptr(i).Update(gtx, evs.prevSpans)...)
	}

	return out
}

func (evs *EventList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.EventList.Layout").End()

	if evs.table.Columns == nil {
		cols := []theme.Column{
			{Name: "Time", Clickable: true, Alignment: text.End},
			{Name: "Message", Clickable: true, Alignment: text.Start},
		}
		evs.table.SetColumns(win, gtx, cols)
	}

	evs.Update(gtx)

	evs.timestampObjects.Reset()
	evs.prevSpans = evs.prevSpans[:0]

	var txtCnt int
	cellFn := func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		var txt *Text
		if txtCnt < evs.texts.Len() {
			txt = evs.texts.Ptr(txtCnt)
		} else {
			txt = evs.texts.Append(Text{})
		}
		txtCnt++
		txt.Reset(win.Theme)

		tb := TextBuilder{Window: win}

		// OPT(dh): there are several allocations here, such as creating slices and using fmt.Sprintf

		ev := evs.Trace.Event(evs.filteredEvents.At(row))
		// XXX styledtext wraps our spans if the window is too small

		addSpanG := func(gid uint64) {
			tb.DefaultLink(local.Sprintf("goroutine %d", gid), "", evs.Trace.G(gid))
		}

		addSpanTs := func(ts trace.Timestamp) {
			tb.DefaultLink(formatTimestamp(nil, ts), "", evs.timestampObjects.Append(ts))
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
		evs.prevSpans = append(evs.prevSpans, tb.Spans...)
		return dims
	}

	checkboxes := make([]theme.Recording, 0, 4)
	var widestCheckbox theme.Recording
	{
		gtx := gtx
		gtx.Constraints.Min = image.Point{}

		checkboxes = append(checkboxes,
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoCreate, "Goroutine creations").Layout),
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoUnblock, "Goroutine unblocks").Layout),
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowGoSysCall, "Syscalls").Layout),
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowUserLog, "User logs").Layout))

		for _, checkbox := range checkboxes {
			if checkbox.Dimensions.Size.X > widestCheckbox.Dimensions.Size.X {
				widestCheckbox = checkbox
			}
		}
	}
	// add padding
	widestCheckbox.Dimensions.Size.X += gtx.Dp(10)

	ret := layout.Rigids(gtx, layout.Vertical,
		func(gtx layout.Context) layout.Dimensions {
			return outlay.FlowWrap{}.Layout(gtx, len(checkboxes), func(gtx layout.Context, i int) layout.Dimensions {
				checkboxes[i].Layout(win, gtx)
				return widestCheckbox.Dimensions
			})
		},
		func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = gtx.Constraints.Max

			return theme.SimpleTable(
				win,
				gtx,
				&evs.table,
				&evs.scrollState,
				evs.filteredEvents.Len(),
				cellFn,
			)
		},
	)

	evs.texts.Truncate(txtCnt)

	return ret
}

func Events(spans Items[ptrace.Span], tr *Trace) Items[ptrace.EventID] {
	if spans.Len() == 0 {
		return NoItems[ptrace.EventID]{}
	}

	if spans.Subslice() || spans.Len() == 1 {
		c, ok := spans.Container()
		assert(ok, "didn't expect subslice with multiple containers")

		sStart := spans.AtPtr(0).Start
		sEnd := spans.AtPtr(spans.Len() - 1).End

		allEvents := c.Track.events
		if len(allEvents) == 0 {
			return NoItems[ptrace.EventID]{}
		}

		eEnd := sort.Search(len(allEvents), func(i int) bool {
			return tr.Event(allEvents[i]).Ts >= sEnd
		})

		eStart := sort.Search(eEnd, func(i int) bool {
			return tr.Event(allEvents[i]).Ts >= sStart
		})

		if eStart == eEnd {
			return NoItems[ptrace.EventID]{}
		}

		return SimpleItems[ptrace.EventID, any]{
			items:      allEvents[eStart:eEnd],
			container:  c,
			contiguous: true,
			subslice:   true,
		}
	} else {
		if spans, ok := spans.(MergedItems[ptrace.Span]); ok {
			// This is an optimization. While the overall MergedItems won't be a subslice of a span, each individual
			// base might be.
			events := make([]Items[ptrace.EventID], 0, len(spans.bases))
			for _, base := range spans.bases {
				events = append(events, Events(base, tr))
			}
			return MergeItems[ptrace.EventID](events, func(a, b *ptrace.EventID) bool {
				return tr.Event(*a).Ts < tr.Event(*b).Ts
			})
		}

		// OPT(dh): even if all spans aren't a subslice, individual runs of spans might be. Detecting that, however,
		// would be the responsibility of the Items implementation, and wouldn't always be possible.
		events := make([]Items[ptrace.EventID], 0, spans.Len())
		for i := 0; i < spans.Len(); i++ {
			events = append(events, Events(spans.Slice(i, i+1), tr))
		}
		return MergeItems[ptrace.EventID](events, func(a, b *ptrace.EventID) bool {
			return tr.Event(*a).Ts < tr.Event(*b).Ts
		})
	}
}

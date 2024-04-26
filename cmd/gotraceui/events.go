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
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/op/clip"
	"gioui.org/text"
	"gioui.org/x/outlay"
	exptrace "golang.org/x/exp/trace"
)

type EventList struct {
	Trace  *Trace
	Events Items[ptrace.EventID]
	Filter struct {
		ShowGoCreate  widget.Bool
		ShowGoUnblock widget.Bool
		ShowGoSysCall widget.Bool
		ShowUserLog   widget.Bool
		ShowTasks     widget.Bool
	}
	filteredEvents SortedItems[ptrace.EventID]

	table       theme.Table
	scrollState theme.YScrollableListState

	timestampObjects mem.BucketSlice[exptrace.Time]
	texts            mem.BucketSlice[Text]
	prevSpans        []TextSpan
}

func (evs *EventList) UpdateFilter() {
	if evs.Filter.ShowGoCreate.Value &&
		evs.Filter.ShowGoUnblock.Value &&
		evs.Filter.ShowGoSysCall.Value &&
		evs.Filter.ShowUserLog.Value &&
		evs.Filter.ShowTasks.Value {

		// Everything is shown
		evs.filteredEvents = NewSortedItems(evs.Events)
	} else if !evs.Filter.ShowGoCreate.Value &&
		!evs.Filter.ShowGoUnblock.Value &&
		!evs.Filter.ShowGoSysCall.Value &&
		!evs.Filter.ShowUserLog.Value &&
		!evs.Filter.ShowTasks.Value {

		// Nothing is shown
		evs.filteredEvents = NewSortedItems(NoItems[ptrace.EventID]{})
	} else {
		// OPT(dh): multiple calls to FilterItems should be able to reuse memory
		evs.filteredEvents = NewSortedItems(FilterItems[ptrace.EventID](evs.Events, func(evID *ptrace.EventID) bool {
			ev := evs.Trace.Event(*evID)
			switch ev.Kind() {
			case exptrace.EventStateTransition:
				trans := ev.StateTransition()
				from, to := ev.StateTransition().Goroutine()
				if ptrace.IsGoroutineCreation(&trans) {
					return evs.Filter.ShowGoCreate.Value
				} else if ptrace.IsGoroutineUnblock(&trans) {
					return evs.Filter.ShowGoUnblock.Value
				} else if to == exptrace.GoSyscall {
					return evs.Filter.ShowGoSysCall.Value
				} else {
					panic(fmt.Sprintf("unexpected state transition %s -> %s", from, to))
				}
			case exptrace.EventLog:
				return evs.Filter.ShowUserLog.Value
			case exptrace.EventTaskBegin, exptrace.EventTaskEnd:
				return evs.Filter.ShowTasks.Value
			default:
				panic(fmt.Sprintf("unexpected kind %s", ev.Kind()))
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

func (evs *EventList) eventMessage(ev *exptrace.Event) []string {
	switch ev.Kind() {
	case exptrace.EventStateTransition:
		trans := ev.StateTransition()
		from, to := trans.Goroutine()
		if from == exptrace.GoNotExist && to == exptrace.GoRunnable {
			return []string{"Created goroutine ", local.Sprintf("%d", trans.Resource.Goroutine())}
		} else if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
			return []string{"Unblocked goroutine ", local.Sprintf("%d", trans.Resource.Goroutine())}
		} else if to == exptrace.GoSyscall {
			stk := ev.Stack()
			if stk != exptrace.NoStack {
				frame := evs.Trace.PCs[evs.Trace.Stacks[stk][0]]
				return []string{"Syscall ()", frame.Func}
			} else {
				return []string{"Syscall"}
			}
		} else {
			panic(fmt.Sprintf("unexpected state transition %s -> %s", from, to))
		}
	case exptrace.EventLog:
		l := ev.Log()
		if l.Category != "" {
			return []string{"<> ", l.Category, l.Message}
		} else {
			return []string{l.Message}
		}
	case exptrace.EventTaskBegin:
		return []string{"Created task ", local.Sprintf("%d", ev.Task())}
	case exptrace.EventTaskEnd:
		return []string{"Subtask ended: ", local.Sprintf("%d", ev.Task())}
	default:
		panic(fmt.Sprintf("unhandled kind %v", ev.Kind()))
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
			return cmp(ea.Time(), eb.Time(), evs.table.SortOrder == theme.SortDescending)
		case 1: // Message
			// XXX this code looks an awful lot like EventList.eventMessage
			cellFn := func(evID ptrace.EventID) string {
				ev := evs.Trace.Event(evID)
				switch ev.Kind() {
				case exptrace.EventStateTransition:
					trans := ev.StateTransition()
					from, to := trans.Goroutine()
					if from == exptrace.GoNotExist && to == exptrace.GoRunnable {
						return local.Sprintf("Created goroutine %d", trans.Resource.Goroutine())
					} else if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
						return local.Sprintf("Unblocked goroutine %d", trans.Resource.Goroutine())
					} else if to == exptrace.GoSyscall {
						stk := trans.Stack
						if stk != exptrace.NoStack {
							frame := evs.Trace.PCs[evs.Trace.Stacks[stk][0]]
							return fmt.Sprintf("Syscall (%s)", frame.Func)
						} else {
							return "Syscall"
						}
					} else {
						panic(fmt.Sprintf("unexpected state transition %s -> %s", from, to))
					}
				case exptrace.EventLog:
					l := ev.Log()
					if l.Category != "" {
						return fmt.Sprintf("<%s> %s", l.Category, l.Message)
					} else {
						return l.Message
					}
				default:
					panic(fmt.Sprintf("unhandled kind %v", ev.Kind()))
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
		evs.Filter.ShowUserLog.Update(gtx) ||
		evs.Filter.ShowTasks.Update(gtx) {
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

		addSpanG := func(gid exptrace.GoID) {
			tb.DefaultLink(local.Sprintf("goroutine %d", gid), "", evs.Trace.G(gid))
		}

		addSpanTask := func(tid exptrace.TaskID, typ string) {
			tb.DefaultLink(local.Sprintf("task %d (%s)", tid, typ), "", evs.Trace.Task(tid))
		}

		addSpanTs := func(ts exptrace.Time) {
			tb.DefaultLink(formatTimestamp(nil, evs.Trace.AdjustedTime(ts)), "", evs.timestampObjects.Append(ts))
		}

		switch col {
		case 0:
			addSpanTs(ev.Time())
			txt.Alignment = text.End
		case 1:
			switch ev.Kind() {
			case exptrace.EventStateTransition:
				trans := ev.StateTransition()
				from, to := trans.Goroutine()
				if from == exptrace.GoNotExist && to == exptrace.GoRunnable {
					tb.Span("Created ")
					addSpanG(trans.Resource.Goroutine())
				} else if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
					tb.Span("Unblocked ")
					addSpanG(trans.Resource.Goroutine())
				} else if to == exptrace.GoSyscall {
					stk := ev.Stack()
					if stk != exptrace.NoStack {
						frame := evs.Trace.PCs[evs.Trace.Stacks[stk][0]]
						tb.Span("Syscall (")
						tb.Span(frame.Func)
						tb.Span(")")
					} else {
						tb.Span("Syscall")
					}
				} else {
					panic(fmt.Sprintf("unexpected state transition %s -> %s", from, to))
				}
			case exptrace.EventLog:
				l := ev.Log()
				if l.Message != "" {
					tb.Span("<")
					tb.Span(l.Category)
					tb.Span("> ")
					tb.Span(l.Message)
				} else {
					tb.Span(l.Message)
				}
			case exptrace.EventTaskBegin:
				tb.Span("Created ")
				addSpanTask(ev.Task().ID, ev.Task().Type)
			case exptrace.EventTaskEnd:
				tb.Span("Subtask ended: ")
				addSpanTask(ev.Task().ID, ev.Task().Type)
			default:
				panic(fmt.Sprintf("unhandled kind %v", ev.Kind()))
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
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowUserLog, "User logs").Layout),
			theme.Record(win, gtx, theme.CheckBox(win.Theme, &evs.Filter.ShowTasks, "Task start/end").Layout),
		)

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
			return tr.Event(allEvents[i]).Time() >= sEnd
		})

		eStart := sort.Search(eEnd, func(i int) bool {
			return tr.Event(allEvents[i]).Time() >= sStart
		})

		if eStart == eEnd {
			return NoItems[ptrace.EventID]{}
		}

		return SimpleItems[ptrace.EventID, any]{
			items:      allEvents[eStart:eEnd],
			container:  c,
			contiguous: false,
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
				return tr.Event(*a).Time() < tr.Event(*b).Time()
			})
		}

		// OPT(dh): even if all spans aren't a subslice, individual runs of spans might be. Detecting that, however,
		// would be the responsibility of the Items implementation, and wouldn't always be possible.
		events := make([]Items[ptrace.EventID], 0, spans.Len())
		for i := 0; i < spans.Len(); i++ {
			events = append(events, Events(spans.Slice(i, i+1), tr))
		}
		return MergeItems[ptrace.EventID](events, func(a, b *ptrace.EventID) bool {
			return tr.Event(*a).Time() < tr.Event(*b).Time()
		})
	}
}

func EventsRange(events Items[exptrace.Event]) TimeSpan {
	return TimeSpan{
		// Offset by 1 so that the events don't fall on screen borders. This also gives a 3ns range for single
		// events.
		Start: events.AtPtr(0).Time() - 1,
		End:   LastItemPtr(events).Time() + 1,
	}
}

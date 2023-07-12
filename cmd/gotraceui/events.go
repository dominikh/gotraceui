package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"sort"

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
	Events Items[ptrace.EventID]
	Filter struct {
		ShowGoCreate  widget.Bool
		ShowGoUnblock widget.Bool
		ShowGoSysCall widget.Bool
		ShowUserLog   widget.Bool
	}
	filteredEvents Items[ptrace.EventID]
	list           widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (evs *EventList) UpdateFilter() {
	// OPT(dh): if all filters are set, all events are shown. if no filters are set, no events are shown. neither case
	//   requires us to check each event.
	// OPT(dh): multiple calls to FilterItems should be able to reuse memory
	evs.filteredEvents = FilterItems[ptrace.EventID](evs.Events, func(ev *ptrace.EventID) bool {
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
	})

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

		ev := evs.Trace.Event(evs.filteredEvents.At(row))
		// XXX styledtext wraps our spans if the window is too small

		addSpanG := func(gid uint64) {
			tb.DefaultLink(local.Sprintf("goroutine %d", gid), evs.Trace.G(gid))
		}

		addSpanTs := func(ts trace.Timestamp) {
			tb.DefaultLink(formatTimestamp(ts), evs.timestampObjects.Allocate(ts))
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

			return tbl.Layout(win, gtx, evs.filteredEvents.Len(), cellFn)
		}),
	)

	evs.texts.Truncate(txtCnt)

	return ret
}

func Events(spans Items[ptrace.Span], tr *Trace) Items[ptrace.EventID] {
	if spans.Len() == 0 {
		return NoItems[ptrace.EventID]{}
	}

	if spans.Subslice() {
		c, ok := spans.Container()
		assert(ok, "didn't expect contiguous spans with multiple containers")

		sStart := spans.At(0).Start
		sEnd := spans.At(spans.Len() - 1).End

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

		return SimpleItems[ptrace.EventID]{
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
			fmt.Printf("%T\n", spans)
			events = append(events, Events(spans.Slice(i, i+1), tr))
		}
		return MergeItems[ptrace.EventID](events, func(a, b *ptrace.EventID) bool {
			return tr.Event(*a).Ts < tr.Event(*b).Ts
		})
	}
}

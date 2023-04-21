package main

import (
	"context"
	rtrace "runtime/trace"
	"sort"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

type FilterMode uint8

const (
	FilterModeOr FilterMode = iota
	FilterModeAnd
)

type Filter struct {
	Mode FilterMode

	// Bitmap of ptrace.SchedulingState
	States uint64

	// Filters specific to processor timelines
	Processor struct {
		// Highlight processor spans for this goroutine
		Goroutine uint64
		// StartAfter and EndBefore are ANDed together, not ORed
		StartAfter trace.Timestamp
		EndBefore  trace.Timestamp
	}

	// Filters specific to machine timelines
	Machine struct {
		Processor int32
	}
}

func (f Filter) HasState(state ptrace.SchedulingState) bool {
	return f.States&(1<<state) != 0
}

func (f Filter) Match(spans ptrace.Spans, container SpanContainer) (out bool) {
	if !f.couldMatch(spans, container) {
		return false
	}

	steps := []func() (match, skip bool){
		func() (bool, bool) {
			if f.States == 0 {
				return false, true
			}

			for i := 0; i < spans.Len(); i++ {
				s := spans.At(i)
				if f.HasState(s.State) {
					return true, false
				}
			}
			return false, false
		},

		func() (bool, bool) {
			if f.Processor.StartAfter == 0 && f.Processor.EndBefore == 0 {
				return false, true
			}

			if _, ok := container.Timeline.item.(*ptrace.Processor); !ok {
				return false, false
			}

			var off int
			spans := spans
			if f.Processor.StartAfter != 0 {
				off = sort.Search(spans.Len(), func(i int) bool {
					return spans.At(i).Start >= f.Processor.StartAfter
				})
			}

			if f.Processor.EndBefore == 0 {
				return off < spans.Len(), false
			}

			for i := off; i < spans.Len(); i++ {
				span := spans.At(i)
				// OPT(dh): don't be O(n)

				if span.Start > f.Processor.EndBefore {
					return false, false
				}

				if f.Processor.Goroutine != 0 {
					// We are interested in the intersection of spans that are for the correct goroutine and spans that
					// fit into the time range. However, the individual filter steps only answer questions for the
					// merged span as a whole, which means that finding some spans with the right goroutine and some
					// spans with the time range would allow the merged span to match, even if the two sets of spans
					// didn't intersect.
					if container.Timeline.cv.trace.Event(span.Event).G != f.Processor.Goroutine {
						continue
					}
				}

				if span.End <= f.Processor.EndBefore {
					return true, false
				}
			}

			return false, false
		},

		func() (bool, bool) {
			if f.Processor.Goroutine != 0 {
				if _, ok := container.Timeline.item.(*ptrace.Processor); ok {
					tr := container.Timeline.cv.trace
					for i := 0; i < spans.Len(); i++ {
						s := spans.At(i)
						g := tr.G(tr.Event(s.Event).G)
						if g.ID == f.Processor.Goroutine {
							return true, false
						}
					}
				}

				return false, false
			} else {
				return false, true
			}
		},

		func() (bool, bool) {
			if f.Machine.Processor != 0 {
				if _, ok := container.Timeline.item.(*ptrace.Machine); ok {
					tr := container.Timeline.cv.trace
					for i := 0; i < spans.Len(); i++ {
						s := spans.At(i)
						p := tr.P(tr.Event(s.Event).P)
						if p.ID == f.Machine.Processor {
							return true, false
						}
					}
				}

				return false, false
			} else {
				return false, true
			}
		},
	}

	switch f.Mode {
	case FilterModeOr:
		for _, step := range steps {
			match, skip := step()
			if skip {
				continue
			}
			if match {
				return true
			}
		}
		return false

	case FilterModeAnd:
		for _, step := range steps {
			match, skip := step()
			if skip {
				continue
			}
			if !match {
				return false
			}
		}
		return true
	default:
		panic("unreachable")
	}
}

// couldMatch checks if the filter could possibly match the spans. It's an optimization to avoid checking impossible
// combinations.
func (f Filter) couldMatch(spans ptrace.Spans, container SpanContainer) bool {
	{
		// Unset Mode so we can compare with the empty literal
		f := f
		f.Mode = 0
		if f == (Filter{}) {
			return false
		}
	}

	b := f.couldMatchState(spans, container)
	b = b || f.couldMatchProcessor(spans, container)
	return b
}

func (f Filter) couldMatchProcessor(spans ptrace.Spans, container SpanContainer) bool {
	switch container.Timeline.item.(type) {
	case *ptrace.Processor:
		return true
	default:
		return false
	}
}

func (f Filter) couldMatchState(spans ptrace.Spans, container SpanContainer) bool {
	switch item := container.Timeline.item.(type) {
	case *ptrace.Processor:
		return f.HasState(ptrace.StateRunningG)
	case *ptrace.Goroutine:
		switch container.Track.kind {
		case TrackKindUnspecified:
			if item.Function.Fn == "runtime.bgsweep" {
				// bgsweep, especially in Go <1.21, can be responsible for millions of spans, but they can only ever be of
				// two states.

				return f.HasState(ptrace.StateActive) || f.HasState(ptrace.StateInactive)
			}
		case TrackKindUserRegions:
			return f.HasState(ptrace.StateUserRegion)
		case TrackKindStack:
			return f.HasState(ptrace.StateStack)
		}

	case *STW, *GC:
		return f.HasState(ptrace.StateActive)
	}

	return true
}

type HighlightDialogStyle struct {
	Filter *Filter

	bits [ptrace.StateLast]widget.BackedBit[uint64]

	list      widget.List
	foldables struct {
		states widget.Bool
	}
	stateClickables []widget.Clickable
}

func HighlightDialog(win *theme.Window, f *Filter) HighlightDialogStyle {
	hd := HighlightDialogStyle{
		Filter: f,
	}
	hd.list.Axis = layout.Vertical

	for i := range hd.bits {
		hd.bits[i].Bits = &f.States
		hd.bits[i].Bit = i
	}

	hd.stateClickables = make([]widget.Clickable, 3)

	return hd
}

func (hd *HighlightDialogStyle) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.HighlightDialogStyle.Layout").End()

	return theme.List(win.Theme, &hd.list).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
		return theme.Foldable(win.Theme, &hd.foldables.states, "States").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return theme.CheckBoxGroup(win.Theme, &hd.stateClickables[0], "General").Layout(win, gtx,
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateInactive], stateNamesCapitalized[ptrace.StateInactive]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateActive], stateNamesCapitalized[ptrace.StateActive]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateStuck], stateNamesCapitalized[ptrace.StateStuck]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateReady], stateNamesCapitalized[ptrace.StateReady]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateCreated], stateNamesCapitalized[ptrace.StateCreated]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateDone], stateNamesCapitalized[ptrace.StateDone]),
					)
				}),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return theme.CheckBoxGroup(win.Theme, &hd.stateClickables[1], "GC").Layout(win, gtx,
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCIdle], stateNamesCapitalized[ptrace.StateGCIdle]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCDedicated], stateNamesCapitalized[ptrace.StateGCDedicated]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCFractional], stateNamesCapitalized[ptrace.StateGCFractional]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCMarkAssist], stateNamesCapitalized[ptrace.StateGCMarkAssist]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCSweep], stateNamesCapitalized[ptrace.StateGCSweep]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyncTriggeringGC], stateNamesCapitalized[ptrace.StateBlockedSyncTriggeringGC]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedGC], stateNamesCapitalized[ptrace.StateBlockedGC]),
					)
				}),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return theme.CheckBoxGroup(win.Theme, &hd.stateClickables[2], "Blocked").Layout(win, gtx,
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlocked], stateNamesCapitalized[ptrace.StateBlocked]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSend], stateNamesCapitalized[ptrace.StateBlockedSend]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedRecv], stateNamesCapitalized[ptrace.StateBlockedRecv]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSelect], stateNamesCapitalized[ptrace.StateBlockedSelect]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSync], stateNamesCapitalized[ptrace.StateBlockedSync]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyncOnce], stateNamesCapitalized[ptrace.StateBlockedSyncOnce]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedCond], stateNamesCapitalized[ptrace.StateBlockedCond]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedNet], stateNamesCapitalized[ptrace.StateBlockedNet]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyscall], stateNamesCapitalized[ptrace.StateBlockedSyscall]),
					)
				}),
			)
		})
	})
}

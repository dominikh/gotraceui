package main

import (
	"context"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
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
}

func (f Filter) HasState(state ptrace.SchedulingState) bool {
	return f.States&(1<<state) != 0
}

func (f Filter) Match(spans ptrace.Spans, container ItemContainer) bool {
	if !f.couldMatch(spans, container) {
		return false
	}

	steps := []func() (match, skip bool){
		func() (bool, bool) {
			if f.States == 0 {
				return false, true
			}

			for i := 0; i < spans.Len(); i++ {
				if f.HasState(spans.AtPtr(i).State) {
					return true, false
				}
			}
			return false, false
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
func (f Filter) couldMatch(spans ptrace.Spans, container ItemContainer) bool {
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

func (f Filter) couldMatchProcessor(spans ptrace.Spans, container ItemContainer) bool {
	switch container.Timeline.item.(type) {
	case *ptrace.Processor:
		return true
	default:
		return false
	}
}

func (f Filter) couldMatchState(spans ptrace.Spans, container ItemContainer) bool {
	switch item := container.Timeline.item.(type) {
	case *ptrace.Processor:
		return f.HasState(ptrace.StateProcRunningG)
	case *ptrace.Goroutine:
		switch container.Track.kind {
		case TrackKindUnspecified:
			if item.Function.Func == "runtime.bgsweep" {
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

	return theme.List(win.Theme, &hd.list).Layout(win, gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
		return theme.Foldable(win.Theme, &hd.foldables.states, "States").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			return layout.Rigids(gtx, layout.Vertical,
				func(gtx layout.Context) layout.Dimensions {
					return theme.CheckBoxGroup(win.Theme, &hd.stateClickables[0], "General").Layout(win, gtx,
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateInactive], stateNamesCapitalized[ptrace.StateInactive]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateActive], stateNamesCapitalized[ptrace.StateActive]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateStuck], stateNamesCapitalized[ptrace.StateStuck]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateReady], stateNamesCapitalized[ptrace.StateReady]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateCreated], stateNamesCapitalized[ptrace.StateCreated]),
					)
				},
				func(gtx layout.Context) layout.Dimensions {
					return theme.CheckBoxGroup(win.Theme, &hd.stateClickables[1], "GC").Layout(win, gtx,
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCIdle], stateNamesCapitalized[ptrace.StateGCIdle]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCDedicated], stateNamesCapitalized[ptrace.StateGCDedicated]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCFractional], stateNamesCapitalized[ptrace.StateGCFractional]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCMarkAssist], stateNamesCapitalized[ptrace.StateGCMarkAssist]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCSweep], stateNamesCapitalized[ptrace.StateGCSweep]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyncTriggeringGC], stateNamesCapitalized[ptrace.StateBlockedSyncTriggeringGC]),
						theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedGC], stateNamesCapitalized[ptrace.StateBlockedGC]),
					)
				},
				func(gtx layout.Context) layout.Dimensions {
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
				},
			)
		})
	})
}

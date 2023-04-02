package main

import (
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

type Filter struct {
	// Bitmap of ptrace.SchedulingState
	States uint64

	And *Filter
	Or  *Filter
}

func (f Filter) HasState(state ptrace.SchedulingState) bool {
	return f.States&(1<<state) != 0
}

func (f Filter) Match(spanSel SpanSelector, container SpanContainer) bool {
	if !f.couldMatch(spanSel, container) {
		return false
	}

	var b bool
	for _, s := range spanSel.Spans() {
		if f.HasState(s.State) {
			b = true
			break
		}
	}

	if f.Or != nil {
		b = b || f.Or.Match(spanSel, container)
	}
	if f.And != nil {
		b = b && f.And.Match(spanSel, container)
	}

	return b
}

func (f Filter) Copy() Filter {
	var out Filter
	out.States = f.States
	if f.And != nil {
		and := f.And.Copy()
		out.And = &and
	}
	if f.Or != nil {
		or := f.Or.Copy()
		out.Or = &or
	}

	return out
}

func (f Filter) Equal(of Filter) bool {
	return (f.States == of.States) &&
		((f.And == nil && of.And == nil) || (f.And != nil && of.And != nil && f.And.Equal(*of.And))) &&
		((f.Or == nil && of.Or == nil) || (f.Or != nil && of.Or != nil && f.Or.Equal(*of.Or)))
}

// couldMatch checks if the filter could possibly match the spans. It's an optimization to avoid checking impossible
// combinations.
func (f Filter) couldMatch(spanSel SpanSelector, container SpanContainer) bool {
	if f == (Filter{}) {
		return false
	}

	b := f.couldMatchState(spanSel, container)
	if f.Or != nil {
		b = b || f.Or.couldMatch(spanSel, container)
	}
	if f.And != nil {
		b = b && f.And.couldMatch(spanSel, container)
	}
	return b
}

func (f Filter) couldMatchState(spanSel SpanSelector, container SpanContainer) bool {
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
	stateGroups     []layout.FlexChild
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

	groupGeneral := []theme.CheckBoxStyle{
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateInactive], stateNamesCapitalized[ptrace.StateInactive]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateActive], stateNamesCapitalized[ptrace.StateActive]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateStuck], stateNamesCapitalized[ptrace.StateStuck]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateReady], stateNamesCapitalized[ptrace.StateReady]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateCreated], stateNamesCapitalized[ptrace.StateCreated]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateDone], stateNamesCapitalized[ptrace.StateDone]),
	}

	groupGC := []theme.CheckBoxStyle{
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCIdle], stateNamesCapitalized[ptrace.StateGCIdle]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCDedicated], stateNamesCapitalized[ptrace.StateGCDedicated]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCFractional], stateNamesCapitalized[ptrace.StateGCFractional]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCMarkAssist], stateNamesCapitalized[ptrace.StateGCMarkAssist]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateGCSweep], stateNamesCapitalized[ptrace.StateGCSweep]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyncTriggeringGC], stateNamesCapitalized[ptrace.StateBlockedSyncTriggeringGC]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedGC], stateNamesCapitalized[ptrace.StateBlockedGC]),
	}

	groupBlocked := []theme.CheckBoxStyle{
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlocked], stateNamesCapitalized[ptrace.StateBlocked]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSend], stateNamesCapitalized[ptrace.StateBlockedSend]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedRecv], stateNamesCapitalized[ptrace.StateBlockedRecv]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSelect], stateNamesCapitalized[ptrace.StateBlockedSelect]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSync], stateNamesCapitalized[ptrace.StateBlockedSync]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyncOnce], stateNamesCapitalized[ptrace.StateBlockedSyncOnce]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedCond], stateNamesCapitalized[ptrace.StateBlockedCond]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedNet], stateNamesCapitalized[ptrace.StateBlockedNet]),
		theme.CheckBox(win.Theme, &hd.bits[ptrace.StateBlockedSyscall], stateNamesCapitalized[ptrace.StateBlockedSyscall]),
	}

	hd.stateClickables = make([]widget.Clickable, 3)

	hd.stateGroups = []layout.FlexChild{
		layout.Rigid(theme.Dumb(win, theme.CheckBoxGroup(win.Theme, &hd.stateClickables[0], "General", groupGeneral...).Layout)),
		layout.Rigid(theme.Dumb(win, theme.CheckBoxGroup(win.Theme, &hd.stateClickables[1], "GC", groupGC...).Layout)),
		layout.Rigid(theme.Dumb(win, theme.CheckBoxGroup(win.Theme, &hd.stateClickables[2], "Blocked", groupBlocked...).Layout)),
	}

	return hd
}

func (hd *HighlightDialogStyle) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	return theme.List(win.Theme, &hd.list).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
		return theme.Foldable(win.Theme, &hd.foldables.states, "States").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Vertical}.Layout(gtx, hd.stateGroups...)
		})
	})
}

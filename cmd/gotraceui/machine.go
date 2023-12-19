package main

import (
	"context"
	"fmt"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
)

func machineTrack0SpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	span := spans.AtPtr(0)
	switch span.State {
	case ptrace.StateRunningP:
		p := tr.P(tr.Event(span.Event).P)
		labels := tr.processorSpanLabels(p)
		return append(out, labels...)
	case ptrace.StateBlockedSyscall:
		return append(out, "syscall")
	default:
		panic(fmt.Sprintf("unexpected state %d", span.State))
	}
}

func machineTrack0SpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spans.Len() == 1 {
		s := state.spans.AtPtr(0)
		ev := tr.Event(s.Event)
		switch s.State {
		case ptrace.StateRunningP:
			label = local.Sprintf("Processor %d\n", ev.P)
		case ptrace.StateBlockedSyscall:
			label = "In blocking syscall\n"
		default:
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
	} else {
		label = local.Sprintf("mixed (%d spans)\n", state.spans.Len())
	}
	label += fmt.Sprintf("Duration: %s", roundDuration(SpansDuration(state.spans)))
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func machineTrack0SpanContextMenu(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem {
	items := []*theme.MenuItem{
		newZoomMenuItem(cv, spans),
		newOpenSpansMenuItem(spans),
	}

	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		switch s.State {
		case ptrace.StateRunningP:
			pid := cv.trace.Event(s.Event).P
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Action: func() theme.Action {
					return &ScrollToObjectAction{Object: cv.trace.P(pid)}
				},
			})
		case ptrace.StateBlockedSyscall:
		default:
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
	}

	return items
}

func machineTrack1SpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	g := tr.G(tr.Event(spans.AtPtr(0).Event).G)
	labels := tr.goroutineSpanLabels(g)
	return append(out, labels...)
}

func machineTrack1SpanColor(span *ptrace.Span, tr *Trace) colorIndex {
	gid := tr.Events[span.Event].G
	g := tr.G(gid)
	switch fn := g.Function.Fn; fn {
	case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
		return colorStateGC
	default:
		return stateColors[span.State]
	}
}

func machineTrack1SpanContextMenu(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spans))

	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		switch s.State {
		case ptrace.StateRunningG:
			gid := cv.trace.Event(s.Event).G
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
				Action: func() theme.Action {
					return &ScrollToObjectAction{
						Object: cv.trace.G(gid),
					}
				},
			})
		default:
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
	}

	return items
}

func machineInvalidateCache(tl *Timeline, cv *Canvas) bool {
	if cv.prevFrame.hoveredTimeline != cv.timeline.hoveredTimeline {
		return true
	}

	if cv.prevFrame.hoveredSpans.Len() == 0 && cv.timeline.hoveredSpans.Len() == 0 {
		// Nothing hovered in either frame.
		return false
	}

	if cv.prevFrame.hoveredSpans.Len() > 1 && cv.timeline.hoveredSpans.Len() > 1 {
		// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
		// frames, then nothing changes for rendering.
		return false
	}

	if cv.prevFrame.hoveredSpans.Len() != cv.timeline.hoveredSpans.Len() {
		// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
		// previously highlighted.
		//
		// The number of hovered spans changed, and at least in one frame the number was 1.
		return true
	}

	// If we got to this point, then both slices have exactly one element.
	if cv.trace.Event(cv.prevFrame.hoveredSpans.AtPtr(0).Event).P != cv.trace.Event(cv.timeline.hoveredSpans.AtPtr(0).Event).P {
		return true
	}

	return false
}

type MachineTooltip struct {
	m     *ptrace.Machine
	trace *Trace
}

func (tt MachineTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.MachineTooltip.Layout").End()

	// OPT(dh): compute statistics once, not on every frame

	tr := tt.trace
	// FIXME(dh): this doesn't seem right for machines that didn't start at 0
	d := time.Duration(tr.End())

	var procD, syscallD time.Duration
	for i := 0; i < len(tt.m.Spans); i++ {
		s := &tt.m.Spans[i]
		d := s.Duration()

		ev := tr.Events[s.Event]
		switch ev.Type {
		case trace.EvProcStart:
			procD += d
		case trace.EvGoSysBlock:
			syscallD += d
		default:
			panic(fmt.Sprintf("unexepcted event type %d", ev.Type))
		}
	}

	procPct := float32(procD) / float32(d) * 100
	syscallPct := float32(syscallD) / float32(d) * 100
	inactiveD := d - procD - syscallD
	inactivePct := float32(inactiveD) / float32(d) * 100

	l := local.Sprintf(
		"Machine %[1]d\n"+
			"Spans: %[2]d\n"+
			"Time running processors: %[3]s (%.2[4]f%%)\n"+
			"Time blocked in syscalls: %[5]s (%.2[6]f%%)\n"+
			"Time inactive: %[7]s (%.2[8]f%%)",
		tt.m.ID,
		len(tt.m.Spans),
		roundDuration(procD), procPct,
		roundDuration(syscallD), syscallPct,
		roundDuration(inactiveD), inactivePct,
	)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
}

func NewMachineTimeline(tr *Trace, cv *Canvas, m *ptrace.Machine) *Timeline {
	if !supportMachineTimelines {
		panic("NewMachineWidget was called despite supportmachineActivities == false")
	}
	l := local.Sprintf("Machine %d", m.ID)
	tl := &Timeline{
		cv:        cv,
		item:      m,
		label:     l,
		shortName: l,

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return MachineTooltip{m, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: machineInvalidateCache,
	}

	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
		NewTrack(tl, TrackKindUnspecified),
	}

	tl.tracks[0].Start = m.Spans[0].Start
	tl.tracks[0].End = m.Spans[len(m.Spans)-1].End
	tl.tracks[0].Len = len(m.Spans)
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
		items: m.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	})
	tl.tracks[0].spanLabel = machineTrack0SpanLabel
	tl.tracks[0].spanTooltip = machineTrack0SpanTooltip
	tl.tracks[0].spanContextMenu = machineTrack0SpanContextMenu

	tl.tracks[1].Start = m.Goroutines[0].Start
	tl.tracks[1].End = m.Goroutines[len(m.Goroutines)-1].End
	tl.tracks[1].Len = len(m.Goroutines)
	tl.tracks[1].spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
		items: m.Goroutines,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[1],
		},
		subslice: true,
	})
	tl.tracks[1].spanLabel = machineTrack1SpanLabel
	tl.tracks[1].spanColor = machineTrack1SpanColor
	tl.tracks[1].spanTooltip = processorTrackSpanTooltip
	tl.tracks[1].spanContextMenu = machineTrack1SpanContextMenu

	return tl
}

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

func machineTrack0HighlightSpan(spanSel SpanSelector, cv *Canvas) bool {
	if htl := cv.timeline.hoveredTimeline; htl != nil {
		var target int32
		switch hitem := htl.item.(type) {
		case *ptrace.Processor:
			target = hitem.ID
		case *ptrace.Machine:
			if cv.timeline.hoveredSpans.Size() != 1 {
				return false
			}
			o := cv.timeline.hoveredSpans.At(0)
			if o.State != ptrace.StateRunningP {
				return false
			}
			target = cv.trace.Event(o.Event).P
		default:
			return false
		}
		for _, span := range spanSel.Spans() {
			// OPT(dh): don't be O(n)
			if span.State == ptrace.StateRunningP && cv.trace.Event(span.Event).P == target {
				return true
			}
		}
	}
	return false
}

func machineTrack0SpanLabel(spanSel SpanSelector, tr *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	s := spanSel.At(0)
	switch s.State {
	case ptrace.StateRunningP:
		p := tr.P(tr.Event(s.Event).P)
		labels := tr.processorSpanLabels(p)
		return append(out, labels...)
	case ptrace.StateBlockedSyscall:
		return append(out, "syscall")
	default:
		panic(fmt.Sprintf("unexpected state %d", s.State))
	}
}

func machineTrack0SpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
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
		label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
	}
	label += fmt.Sprintf("Duration: %s", roundDuration(Duration(state.spanSel)))
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func machineTrack0SpanContextMenu(spanSel SpanSelector, cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spanSel))

	if spanSel.Size() == 1 {
		s := spanSel.At(0)
		switch s.State {
		case ptrace.StateRunningP:
			pid := cv.trace.Event(s.Event).P
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Do: func(gtx layout.Context) {
					cv.scrollToTimeline(gtx, cv.trace.P(pid))
				},
			})
		case ptrace.StateBlockedSyscall:
		default:
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
	}

	return items
}

func machineTrack1HighlightSpan(spanSel SpanSelector, cv *Canvas) bool {
	if htl := cv.timeline.hoveredTimeline; htl != nil {
		var target uint64
		switch hitem := htl.item.(type) {
		case *ptrace.Goroutine:
			target = hitem.ID
		case *ptrace.Processor:
			if cv.timeline.hoveredSpans.Size() != 1 {
				return false
			}
			o := cv.timeline.hoveredSpans.At(0)
			if o.State != ptrace.StateRunningG {
				return false
			}
			target = cv.trace.Event(o.Event).G
		case *ptrace.Machine:
			if cv.timeline.hoveredSpans.Size() != 1 {
				return false
			}
			o := cv.timeline.hoveredSpans.At(0)
			if o.State != ptrace.StateRunningG {
				return false
			}
			target = cv.trace.Event(o.Event).G
		default:
			return false
		}
		for _, span := range spanSel.Spans() {
			// OPT(dh): don't be O(n)
			if cv.trace.Event(span.Event).G == target {
				return true
			}
		}
	}
	return false
}

func machineTrack1SpanLabel(spanSel SpanSelector, tr *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	g := tr.G(tr.Event(spanSel.At(0).Event).G)
	labels := tr.goroutineSpanLabels(g)
	return append(out, labels...)
}

func machineTrack1SpanColor(spanSel SpanSelector, tr *Trace) [2]colorIndex {
	do := func(s ptrace.Span, tr *Trace) colorIndex {
		gid := tr.Events[s.Event].G
		g := tr.G(gid)
		switch fn := g.Function.Fn; fn {
		case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
			return colorStateGC
		default:
			return stateColors[s.State]
		}
	}

	if spanSel.Size() == 1 {
		return [2]colorIndex{do(spanSel.At(0), tr), 0}
	} else {
		spans := spanSel.Spans()
		c := do(spans[0], tr)
		for _, s := range spans[1:] {
			// OPT(dh): this can get very expensive; imagine a merged span with millions of spans, all
			// with the same color.
			cc := do(s, tr)
			if cc != c {
				return [2]colorIndex{colorStateMerged, 0}
			}
		}
		return [2]colorIndex{c, colorStateMerged}
	}
}

func machineTrack1SpanContextMenu(spanSel SpanSelector, cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spanSel))

	if spanSel.Size() == 1 {
		s := spanSel.At(0)
		switch s.State {
		case ptrace.StateRunningG:
			gid := cv.trace.Event(s.Event).G
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
				Do: func(gtx layout.Context) {
					cv.scrollToTimeline(gtx, cv.trace.G(gid))
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

	if cv.prevFrame.hoveredSpans.Size() == 0 && cv.timeline.hoveredSpans.Size() == 0 {
		// Nothing hovered in either frame.
		return false
	}

	if cv.prevFrame.hoveredSpans.Size() > 1 && cv.timeline.hoveredSpans.Size() > 1 {
		// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
		// frames, then nothing changes for rendering.
		return false
	}

	if cv.prevFrame.hoveredSpans.Size() != cv.timeline.hoveredSpans.Size() {
		// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
		// previously highlighted.
		//
		// The number of hovered spans changed, and at least in one frame the number was 1.
		return true
	}

	// If we got to this point, then both slices have exactly one element.
	if cv.trace.Event(cv.prevFrame.hoveredSpans.At(0).Event).P != cv.trace.Event(cv.timeline.hoveredSpans.At(0).Event).P {
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
	d := time.Duration(tr.Events[len(tr.Events)-1].Ts)

	var procD, syscallD time.Duration
	for i := range tt.m.Spans {
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
	return &Timeline{
		tracks: []Track{
			{spans: SliceToSpanSelector(m.Spans)},
			{spans: SliceToSpanSelector(m.Goroutines)},
		},
		item:  m,
		label: local.Sprintf("Machine %d", m.ID),

		buildTrackWidgets: func(tracks []Track) {
			for i := range tracks {
				track := &tracks[i]
				switch i {
				case 0:
					*track.TrackWidget = TrackWidget{
						highlightSpan:   machineTrack0HighlightSpan,
						spanLabel:       machineTrack0SpanLabel,
						spanTooltip:     machineTrack0SpanTooltip,
						spanContextMenu: machineTrack0SpanContextMenu,
					}
				case 1:
					*track.TrackWidget = TrackWidget{
						highlightSpan:   machineTrack1HighlightSpan,
						spanLabel:       machineTrack1SpanLabel,
						spanColor:       machineTrack1SpanColor,
						spanTooltip:     processorTrackSpanTooltip,
						spanContextMenu: machineTrack1SpanContextMenu,
					}
				}
			}
		},

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return MachineTooltip{m, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: machineInvalidateCache,
	}
}

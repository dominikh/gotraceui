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

type ProcessorTooltip struct {
	p     *ptrace.Processor
	trace *Trace
}

func (tt ProcessorTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.ProcessorTooltip.Layout").End()

	// OPT(dh): compute statistics once, not on every frame

	tr := tt.trace
	d := time.Duration(tr.Events[len(tr.Events)-1].Ts)

	var userD, gcD time.Duration
	for i := 0; i < tt.p.Spans.Len(); i++ {
		s := tt.p.Spans.AtPtr(i)
		d := s.Duration()

		ev := tr.Events[s.Event]
		switch ev.Type {
		case trace.EvGoStart:
			userD += d
		case trace.EvGoStartLabel:
			gcD += d
		default:
			panic(fmt.Sprintf("unexepcted event type %d", ev.Type))
		}
	}

	userPct := float32(userD) / float32(d) * 100
	gcPct := float32(gcD) / float32(d) * 100
	inactiveD := d - userD - gcD
	inactivePct := float32(inactiveD) / float32(d) * 100

	l := local.Sprintf(
		"Processor %[1]d\n"+
			"Spans: %[2]d\n"+
			"Time running user code: %[3]s (%.2[4]f%%)\n"+
			"Time running GC workers: %[5]s (%.2[6]f%%)\n"+
			"Time inactive: %[7]s (%.2[8]f%%)",
		tt.p.ID,
		(tt.p.Spans.Len()),
		roundDuration(userD), userPct,
		roundDuration(gcD), gcPct,
		roundDuration(inactiveD), inactivePct,
	)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
}

func processorTrackSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spans.Len() == 1 {
		s := state.spans.At(0)
		ev := tr.Event(s.Event)
		if s.State != ptrace.StateRunningG {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		g := tr.G(ev.G)
		label = local.Sprintf("Goroutine %d: %s\n", ev.G, g.Function)
	} else {
		label = local.Sprintf("mixed (%d spans)\n", state.spans.Len())
	}
	// OPT(dh): don't materialize all spans just to compute the duration
	label += fmt.Sprintf("Duration: %s", roundDuration(SpansDuration(state.spans)))
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func processorInvalidateCache(tl *Timeline, cv *Canvas) bool {
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
	if cv.trace.Event(cv.prevFrame.hoveredSpans.At(0).Event).G != cv.trace.Event(cv.timeline.hoveredSpans.At(0).Event).G {
		return true
	}

	return false
}

func processorTrackSpanLabel(spans ptrace.Spans, tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	g := tr.G(tr.Event(spans.At(0).Event).G)
	labels := tr.goroutineSpanLabels(g)
	return append(out, labels...)
}

func processorTrackSpanColor(spans ptrace.Spans, tr *Trace) [2]colorIndex {
	do := func(s ptrace.Span, tr *Trace) colorIndex {
		if s.Tags&ptrace.SpanTagGC != 0 {
			return colorStateGC
		} else {
			// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
			return stateColors[s.State]
		}
	}

	if spans.Len() == 1 {
		return [2]colorIndex{do(spans.At(0), tr), 0}
	} else {
		c := do(spans.At(0), tr)
		for i := 1; i < spans.Len(); i++ {
			s := spans.At(i)
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

func processorTrackSpanContextMenu(spans ptrace.Spans, cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spans))

	if spans.Len() == 1 {
		gid := cv.trace.Event((spans.At(0).Event)).G
		items = append(items, &theme.MenuItem{
			Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
			Do: func(gtx layout.Context) {
				cv.scrollToTimeline(gtx, cv.trace.G(gid))
			},
		})
	}

	return items
}

func NewProcessorTimeline(tr *Trace, cv *Canvas, p *ptrace.Processor) *Timeline {
	l := local.Sprintf("Processor %d", p.ID)
	return &Timeline{
		tracks: []Track{{spans: (p.Spans)}},

		buildTrackWidgets: func(tracks []Track) {
			for i := range tracks {
				track := &tracks[i]
				*track.TrackWidget = TrackWidget{
					spanLabel:       processorTrackSpanLabel,
					spanColor:       processorTrackSpanColor,
					spanTooltip:     processorTrackSpanTooltip,
					spanContextMenu: processorTrackSpanContextMenu,
				}
			}
		},

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return ProcessorTooltip{p, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: processorInvalidateCache,
		item:            p,
		label:           l,
		shortName:       l,
	}
}

func processorLinkContextMenu(mwin MainWindowIface, obj *ptrace.Processor) []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to processor"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink(&ProcessorLink{Processor: obj, Kind: ProcessorLinkKindScroll})
			},
		},
		{
			Label: PlainLabel("Zoom to processor"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink(&ProcessorLink{Processor: obj, Kind: ProcessorLinkKindZoom})
			},
		},
	}
}

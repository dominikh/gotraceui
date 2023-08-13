package main

import (
	"context"
	"fmt"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/container"
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
	for i := range tt.p.Spans {
		s := &tt.p.Spans[i]
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
		(len(tt.p.Spans)),
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

func processorTrackSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	g := tr.G(tr.Event(spans.At(0).Event).G)
	labels := tr.goroutineSpanLabels(g)
	return append(out, labels...)
}

func processorTrackSpanColor(spans Items[ptrace.Span], tr *Trace) (out [2]colorIndex) {
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
	}

	con, ok := spans.Container()
	assert(ok, "expected spans to have container")
	tl := con.Timeline

	var (
		cached [2]colorIndex
		found  bool
	)

	tl.spanColorCache.FindIter(spans.At(0).Start, spans.At(spans.Len()-1).End, func(node *container.RBNode[container.Interval[trace.Timestamp], container.Value[trace.Timestamp, [2]colorIndex]]) bool {
		ival := container.Interval[trace.Timestamp]{
			Min: spans.At(0).Start,
			Max: spans.At(spans.Len() - 1).End,
		}
		if node.Key.SupersetOf(ival) {
			if node.Value.Value[1] != 0 {
				cached = node.Value.Value
				found = true
				return true
			}
		}
		if ival.SupersetOf(node.Key) {
			if node.Value.Value[1] == 0 {
				cached = node.Value.Value
				found = true
				return true
			}
		}
		return false
	})

	if found {
		return cached
	}

	// Analyzing 500 spans takes around 10 μs, which is the amount of CPU time we're willing to spend on analyzing
	// merged spans. Caching these would not be worth it from a CPU/memory tradeoff perspective.
	const minCachedSize = 500

	c := do(spans.At(0), tr)
	for i := 1; i < spans.Len(); i++ {
		s := spans.At(i)
		cc := do(s, tr)
		if cc != c {
			if i > minCachedSize {
				// Store a cache entry for the non-mixed-state prefix
				// OPT(dh): don't store it if a superset of it already exists
				tl.spanColorCache.Insert(spans.At(0).Start, spans.At(i-1).End, [2]colorIndex{c, colorStateMerged})
			}

			// Store a cache entry for the non-mixed to mixed transition.
			tl.spanColorCache.Insert(spans.At(i-1).Start, spans.At(i).End, [2]colorIndex{colorStateMerged, 0})

			return [2]colorIndex{colorStateMerged, 0}
		}
	}

	if spans.Len() > minCachedSize {
		// Store a cache entry for the full non-mixed-state merged span
		tl.spanColorCache.Insert(spans.At(0).Start, spans.At(spans.Len()-1).End, [2]colorIndex{c, colorStateMerged})
	}
	return [2]colorIndex{c, colorStateMerged}
}

func processorTrackSpanContextMenu(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spans))

	if spans.Len() == 1 {
		gid := cv.trace.Event((spans.At(0).Event)).G
		items = append(items, &theme.MenuItem{
			Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
			Link: func() theme.Link {
				return (*ScrollToGoroutineLink)(cv.trace.G(gid))
			},
		})
	}

	return items
}

func NewProcessorTimeline(tr *Trace, cv *Canvas, p *ptrace.Processor) *Timeline {
	l := local.Sprintf("Processor %d", p.ID)
	tl := &Timeline{
		buildTrackWidgets: func(tracks []*Track) {
			for _, track := range tracks {
				*track.TrackWidget = TrackWidget{
					spanLabel:       processorTrackSpanLabel,
					spanColor:       processorTrackSpanColor,
					spanTooltip:     processorTrackSpanTooltip,
					spanContextMenu: processorTrackSpanContextMenu,
				}
			}
		},

		spanColorCache: container.NewIntervalTree[trace.Timestamp, [2]colorIndex](),

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return ProcessorTooltip{p, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: processorInvalidateCache,
		item:            p,
		label:           l,
		shortName:       l,
	}
	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}

	ss := SimpleItems[ptrace.Span]{
		items: p.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}
	tl.tracks[0].Start = p.Spans[0].Start
	tl.tracks[0].End = p.Spans[len(p.Spans)-1].End
	tl.tracks[0].Len = len(p.Spans)
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](ss)

	return tl
}

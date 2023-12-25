package main

import (
	"context"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
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
	// FIXME(dh): this doesn't seem right for processors that didn't start at 0
	d := time.Duration(tr.End())

	var userD, gcD time.Duration
	for i := range tt.p.Spans {
		s := &tt.p.Spans[i]
		d := s.Duration()

		_ = d

		ev := tr.Event(s.StartEvent)
		_ = ev
		// switch ev.Type {
		// case trace.EvGoStart:
		// 	userD += d
		// case trace.EvGoStartLabel:
		// 	gcD += d
		// default:
		// 	panic(fmt.Sprintf("unexepcted event type %d", ev.Type))
		// }
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
		len(tt.p.Spans),
		roundDuration(userD), userPct,
		roundDuration(gcD), gcPct,
		roundDuration(inactiveD), inactivePct,
	)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
}

func processorTrackSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	var label string
	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		ev := tr.Event(s.StartEvent)
		label = tooltipStateLabels[s.State] + "\n"
		switch s.State {
		case ptrace.StateProcRunningG, ptrace.StateProcRunningBlocked:
			g := ev.StateTransition().Resource.Goroutine()
			// OPT(dh): cache these strings
			label += local.Sprintf("Goroutine %d: %s\n", g, tr.G(g).Function)
		}
	} else {
		label = local.Sprintf("%d spans\n", spans.Len())
	}
	label += spansDurationForTooltip(spans)
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func processorTrackSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	var labels []string
	s := spans.AtPtr(0)
	switch s.State {
	case ptrace.StateProcRunningG, ptrace.StateProcRunningBlocked:
		g := tr.G(tr.Event(spans.AtPtr(0).StartEvent).StateTransition().Resource.Goroutine())
		labels = tr.goroutineSpanLabels(g)
	default:
		labels = spanStateLabels[s.State]
	}
	out = append(out, labels...)
	return out
}

func processorTrackSpanColor(span *ptrace.Span, tr *Trace) (out colorIndex) {
	if span.Tags&ptrace.SpanTagGC != 0 && span.State == ptrace.StateProcRunningG {
		return colorStateGC
	} else {
		// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
		return stateColors[span.State]
	}
}

func processorTrackSpanContextMenu(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem {
	items := []*theme.MenuItem{
		newZoomMenuItem(cv, spans),
		newOpenSpansMenuItem(spans),
	}

	if spans.Len() == 1 {
		gid := cv.trace.Event(spans.AtPtr(0).StartEvent).StateTransition().Resource.Goroutine()
		items = append(items, &theme.MenuItem{
			Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
			Action: func() theme.Action {
				return &ScrollToObjectAction{Object: cv.trace.G(gid)}
			},
		})
	}

	return items
}

func NewProcessorTimeline(tr *Trace, cv *Canvas, p *ptrace.Processor) *Timeline {
	l := local.Sprintf("Processor %d", p.ID)
	tl := &Timeline{
		cv: cv,

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return ProcessorTooltip{p, cv.trace}.Layout(win, gtx)
		},
		item:      p,
		label:     l,
		shortName: l,
	}
	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}

	ss := SimpleItems[ptrace.Span, any]{
		items: p.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}
	tl.tracks[0].Start = p.Spans[0].Start
	tl.tracks[0].End = p.Spans[len(p.Spans)-1].End
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](ss)
	tl.tracks[0].spanLabel = processorTrackSpanLabel
	tl.tracks[0].spanColor = processorTrackSpanColor
	tl.tracks[0].spanTooltip = processorTrackSpanTooltip
	tl.tracks[0].spanContextMenu = processorTrackSpanContextMenu

	addStackTracks(tl, p, tr)

	return tl
}

package main

import (
	"fmt"

	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/io/key"
	"gioui.org/io/pointer"
)

func defaultObjectLink(obj any) ObjectLink {
	switch obj := obj.(type) {
	case *ptrace.Goroutine:
		return (*GoroutineObjectLink)(obj)
	case *ptrace.Processor:
		return (*ProcessorObjectLink)(obj)
	case *trace.Timestamp:
		return (*TimestampObjectLink)(obj)
	case trace.Timestamp:
		return (*TimestampObjectLink)(&obj)
	case *ptrace.Function:
		return (*FunctionObjectLink)(obj)
	default:
		panic(fmt.Sprintf("unsupported type: %T", obj))
	}
}

type Link interface {
	Open(gtx layout.Context, mwin *MainWindow)
}

type ObjectLink interface {
	Link(ev gesture.ClickEvent) Link
	ContextMenu(mwin MainWindowIface) []*theme.MenuItem
}

type GoroutineObjectLink ptrace.Goroutine
type ProcessorObjectLink ptrace.Processor
type TimestampObjectLink trace.Timestamp
type FunctionObjectLink ptrace.Function
type SpansObjectLink struct {
	Spans Items[ptrace.Span]
}

func (l *GoroutineObjectLink) Link(ev gesture.ClickEvent) Link {
	switch ev.Modifiers {
	default:
		return (*ScrollToGoroutineLink)(l)
	case key.ModShortcut:
		return (*ZoomToGoroutineLink)(l)
	case key.ModShift:
		return (*OpenGoroutineLink)(l)
	}
}

func (l *GoroutineObjectLink) ContextMenu(mwin MainWindowIface) []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to goroutine"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink((*ScrollToGoroutineLink)(l))
			},
		},
		{
			Label: PlainLabel("Zoom to goroutine"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink((*ZoomToGoroutineLink)(l))
			},
		},
		{
			Label: PlainLabel("Show goroutine information"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink((*OpenGoroutineLink)(l))
			},
		},
	}
}

func (l *ProcessorObjectLink) Link(ev gesture.ClickEvent) Link {
	// There are no processor panels yet, so key.ModShift doesn't do anything
	switch ev.Modifiers {
	default:
		return (*ScrollToProcessorLink)(l)
	case key.ModShortcut:
		return (*ZoomToProcessorLink)(l)
	}
}

func (l *ProcessorObjectLink) ContextMenu(mwin MainWindowIface) []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to processor"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink((*ScrollToProcessorLink)(l))
			},
		},
		{
			Label: PlainLabel("Zoom to processor"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink((*ZoomToProcessorLink)(l))
			},
		},
	}
}

func (l *TimestampObjectLink) Link(ev gesture.ClickEvent) Link {
	return (*ScrollToTimestampLink)(l)
}

func (l *TimestampObjectLink) ContextMenu(mwin MainWindowIface) []*theme.MenuItem {
	return nil
}

func (l *FunctionObjectLink) Link(ev gesture.ClickEvent) Link {
	return (*OpenFunctionLink)(l)
}

func (l *FunctionObjectLink) ContextMenu(mwin MainWindowIface) []*theme.MenuItem {
	return nil
}

func (l *SpansObjectLink) Link(ev gesture.ClickEvent) Link {
	switch ev.Modifiers {
	default:
		ll := ScrollToTimestampLink(l.Spans.At(0).Start)
		return &ll
	case key.ModShortcut:
		if _, ok := l.Spans.Container(); ok {
			return (*ZoomToSpansLink)(l)
		} else {
			ll := ScrollToTimestampLink(l.Spans.At(0).Start)
			return &ll
		}
	case key.ModShift:
		return (*OpenSpansLink)(l)
	}
}

func (l *SpansObjectLink) ContextMenu(mwin MainWindowIface) []*theme.MenuItem {
	if _, ok := l.Spans.Container(); ok {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Do: func(gtx layout.Context) {
					ll := ScrollToTimestampLink(l.Spans.At(0).Start)
					mwin.OpenLink(&ll)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Do: func(gtx layout.Context) {
					ll := ScrollToTimestampLink(l.Spans.At(l.Spans.Len() - 1).End)
					mwin.OpenLink(&ll)
				},
			},
			{
				Label: PlainLabel("Zoom to span"),
				Do: func(gtx layout.Context) {
					mwin.OpenLink((*ZoomToSpansLink)(l))
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Do: func(gtx layout.Context) {
					mwin.OpenLink((*OpenSpansLink)(l))
				},
			},
		}
	} else {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Do: func(gtx layout.Context) {
					ll := ScrollToTimestampLink(l.Spans.At(0).Start)
					mwin.OpenLink(&ll)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Do: func(gtx layout.Context) {
					ll := ScrollToTimestampLink(l.Spans.At(l.Spans.Len() - 1).End)
					mwin.OpenLink(&ll)
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Do: func(gtx layout.Context) {
					mwin.OpenLink((*OpenSpansLink)(l))
				},
			},
		}
	}
}

type OpenGoroutineLink ptrace.Goroutine
type ScrollToGoroutineLink ptrace.Goroutine
type ZoomToGoroutineLink ptrace.Goroutine
type ScrollToTimestampLink trace.Timestamp
type ScrollToProcessorLink ptrace.Processor
type ZoomToProcessorLink ptrace.Processor
type OpenFunctionLink ptrace.Function
type SpansLink struct {
	Spans Items[ptrace.Span]
}
type OpenSpansLink SpansLink
type ScrollAndPanToSpansLink SpansLink
type ZoomToSpansLink SpansLink

func (l *OpenGoroutineLink) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openGoroutine((*ptrace.Goroutine)(l))
}

func (l *ScrollToGoroutineLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToObject(gtx, (*ptrace.Goroutine)(l))
}

func (l *ZoomToGoroutineLink) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.objectY(gtx, (*ptrace.Goroutine)(l))
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans[0].Start, l.Spans[len(l.Spans)-1].End, y)
}

func (l *ScrollToTimestampLink) Open(gtx layout.Context, mwin *MainWindow) {
	d := mwin.canvas.End() - mwin.canvas.start
	var off trace.Timestamp
	switch mwin.canvas.axis.anchor {
	case AxisAnchorNone:
		off = mwin.canvas.pxToTs(mwin.canvas.axis.position) - mwin.canvas.start
	case AxisAnchorStart:
		off = 0
	case AxisAnchorCenter:
		off = d / 2
	case AxisAnchorEnd:
		off = d
	}
	mwin.canvas.navigateTo(gtx, trace.Timestamp(*l)-off, mwin.canvas.nsPerPx, mwin.canvas.y)
}

func (l *ScrollToProcessorLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToObject(gtx, (*ptrace.Processor)(l))
}

func (l *ZoomToProcessorLink) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.objectY(gtx, (*ptrace.Processor)(l))
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans[0].Start, l.Spans[len(l.Spans)-1].End, y)
}

func (l *OpenFunctionLink) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openFunction((*ptrace.Function)(l))
}

func (l *OpenSpansLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openSpan(l.Spans)
}

func (l *ScrollAndPanToSpansLink) Open(gtx layout.Context, mwin *MainWindow) {
	c, ok := l.Spans.Container()
	assert(ok, "expected container")
	mwin.canvas.scrollToTimeline(gtx, c.Timeline)
	d := mwin.canvas.End() - mwin.canvas.start
	ts := l.Spans.At(0).Start + trace.Timestamp(SpansDuration(l.Spans)/2)
	mwin.canvas.navigateTo(gtx, ts-d/2, mwin.canvas.nsPerPx, mwin.canvas.animateTo.targetY)
}

func (l *ZoomToSpansLink) Open(gtx layout.Context, mwin *MainWindow) {
	c, ok := l.Spans.Container()
	assert(ok, "expected container")
	mwin.canvas.scrollToTimeline(gtx, c.Timeline)
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans.At(0).Start, LastSpan(l.Spans).End, mwin.canvas.animateTo.targetY)
}

func handleLinkClick(win *theme.Window, mwin MainWindowIface, ev TextEvent) {
	if ev.Event.Type == gesture.TypeClick && ev.Event.Button == pointer.ButtonPrimary {
		link := ev.Span.ObjectLink.Link(ev.Event)
		if link != nil {
			mwin.OpenLink(link)
		}
	} else if ev.Event.Type == gesture.TypePress && ev.Event.Button == pointer.ButtonSecondary {
		menu := ev.Span.ObjectLink.ContextMenu(mwin)
		if len(menu) != 0 {
			win.SetContextMenu(menu)
		}
	}
}

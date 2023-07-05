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

type OpenGoroutineLink ptrace.Goroutine
type ScrollToGoroutineLink ptrace.Goroutine
type ZoomToGoroutineLink ptrace.Goroutine

func (l *OpenGoroutineLink) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openGoroutine((*ptrace.Goroutine)(l))
}

func (l *ScrollToGoroutineLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, (*ptrace.Goroutine)(l))
}

func (l *ZoomToGoroutineLink) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.timelineY(gtx, (*ptrace.Goroutine)(l))
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans.Start(), l.Spans.End(), y)
}

type ScrollToTimestampLink trace.Timestamp

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

type ScrollToProcessorLink ptrace.Processor
type ZoomToProcessorLink ptrace.Processor

func (l *ScrollToProcessorLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, (*ptrace.Processor)(l))
}

func (l *ZoomToProcessorLink) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.timelineY(gtx, (*ptrace.Processor)(l))
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans.Start(), l.Spans.End(), y)
}

type OpenFunctionLink ptrace.Function

func (l *OpenFunctionLink) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openFunction((*ptrace.Function)(l))
}

type SpansLink struct {
	Spans     ptrace.Spans
	Container SpanContainer
}

type ScrollAndPanToSpansLink SpansLink
type ZoomToSpansLink SpansLink

func (l *ScrollAndPanToSpansLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, l.Container.Timeline.item)
	d := mwin.canvas.End() - mwin.canvas.start
	ts := l.Spans.At(0).Start + trace.Timestamp(SpansDuration(l.Spans)/2)
	mwin.canvas.navigateTo(gtx, ts-d/2, mwin.canvas.nsPerPx, mwin.canvas.animateTo.targetY)
}

func (l *ZoomToSpansLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, l.Container.Timeline.item)
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

package main

import (
	"fmt"
	"os"
	"runtime"
	rdebug "runtime/debug"
	"runtime/pprof"
	"time"

	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/io/key"
	"gioui.org/io/pointer"
)

type MainWindowLink interface {
	theme.Link
	Open(gtx layout.Context, mwin *MainWindow)
}

type ObjectLink interface {
	Link(ev gesture.ClickEvent) theme.Link
	ContextMenu() []*theme.MenuItem
}

type OpenGoroutineLink ptrace.Goroutine
type ScrollToGoroutineLink ptrace.Goroutine
type ZoomToGoroutineLink ptrace.Goroutine
type ScrollToTimestampLink trace.Timestamp
type ScrollToProcessorLink ptrace.Processor
type ZoomToProcessorLink ptrace.Processor
type OpenFunctionLink ptrace.Function
type SpansLink struct{ Spans Items[ptrace.Span] }
type OpenSpansLink SpansLink
type ScrollAndPanToSpansLink SpansLink
type ZoomToSpansLink SpansLink
type ScrollToTimelineLink Timeline
type CanvasJumpToBeginningLink struct{}
type CanvasScrollToTopLink struct{}
type CanvasUndoNavigationLink struct{}
type CanvasZoomToFitCurrentViewLink struct{}
type OpenFlameGraphLink struct{}
type OpenHeatmapLink struct{}
type OpenHighlightSpansDialogLink struct{}
type CanvasToggleTimelineLabelsLink struct{}
type CanvasToggleCompactDisplayLink struct{}
type CanvasToggleStackTracksLink struct{}
type OpenScrollToTimelineDialog struct{}
type OpenFileOpenDialog struct{}
type ExitLink struct{}
type WriteMemoryProfileLink struct{}
type RunGarbageCollectionLink struct{}
type RunFreeOSMemoryLink struct{}
type StartCPUProfileLink struct{}
type StopCPUProfileLink struct{}
type OpenPanelLink struct{ Panel theme.Panel }
type PrevPanelLink struct{}
type GoroutineObjectLink ptrace.Goroutine
type ProcessorObjectLink ptrace.Processor
type TimestampObjectLink trace.Timestamp
type FunctionObjectLink ptrace.Function
type SpansObjectLink struct{ Spans Items[ptrace.Span] }

func (OpenGoroutineLink) IsLink()              {}
func (ScrollToGoroutineLink) IsLink()          {}
func (ZoomToGoroutineLink) IsLink()            {}
func (ScrollToTimestampLink) IsLink()          {}
func (ScrollToProcessorLink) IsLink()          {}
func (ZoomToProcessorLink) IsLink()            {}
func (OpenFunctionLink) IsLink()               {}
func (SpansLink) IsLink()                      {}
func (OpenSpansLink) IsLink()                  {}
func (ScrollAndPanToSpansLink) IsLink()        {}
func (ZoomToSpansLink) IsLink()                {}
func (ScrollToTimelineLink) IsLink()           {}
func (CanvasJumpToBeginningLink) IsLink()      {}
func (CanvasScrollToTopLink) IsLink()          {}
func (CanvasUndoNavigationLink) IsLink()       {}
func (CanvasZoomToFitCurrentViewLink) IsLink() {}
func (OpenFlameGraphLink) IsLink()             {}
func (OpenHeatmapLink) IsLink()                {}
func (OpenHighlightSpansDialogLink) IsLink()   {}
func (CanvasToggleTimelineLabelsLink) IsLink() {}
func (CanvasToggleCompactDisplayLink) IsLink() {}
func (CanvasToggleStackTracksLink) IsLink()    {}
func (OpenScrollToTimelineDialog) IsLink()     {}
func (OpenFileOpenDialog) IsLink()             {}
func (ExitLink) IsLink()                       {}
func (WriteMemoryProfileLink) IsLink()         {}
func (RunGarbageCollectionLink) IsLink()       {}
func (RunFreeOSMemoryLink) IsLink()            {}
func (StartCPUProfileLink) IsLink()            {}
func (StopCPUProfileLink) IsLink()             {}
func (*OpenPanelLink) IsLink()                 {}
func (PrevPanelLink) IsLink()                  {}

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

func (l *GoroutineObjectLink) Link(ev gesture.ClickEvent) theme.Link {
	switch ev.Modifiers {
	default:
		return (*ScrollToGoroutineLink)(l)
	case key.ModShortcut:
		return (*ZoomToGoroutineLink)(l)
	case key.ModShift:
		return (*OpenGoroutineLink)(l)
	}
}

func (l *GoroutineObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to goroutine"),
			Link: func() theme.Link {
				return (*ScrollToGoroutineLink)(l)
			},
		},
		{
			Label: PlainLabel("Zoom to goroutine"),
			Link: func() theme.Link {
				return (*ZoomToGoroutineLink)(l)
			},
		},
		{
			Label: PlainLabel("Show goroutine information"),
			Link: func() theme.Link {
				return (*OpenGoroutineLink)(l)
			},
		},
	}
}

func (l *ProcessorObjectLink) Link(ev gesture.ClickEvent) theme.Link {
	// There are no processor panels yet, so key.ModShift doesn't do anything
	switch ev.Modifiers {
	default:
		return (*ScrollToProcessorLink)(l)
	case key.ModShortcut:
		return (*ZoomToProcessorLink)(l)
	}
}

func (l *ProcessorObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to processor"),
			Link: func() theme.Link {
				return (*ScrollToProcessorLink)(l)
			},
		},
		{
			Label: PlainLabel("Zoom to processor"),
			Link: func() theme.Link {
				return (*ZoomToProcessorLink)(l)
			},
		},
	}
}

func (l *TimestampObjectLink) Link(ev gesture.ClickEvent) theme.Link {
	return (*ScrollToTimestampLink)(l)
}

func (l *TimestampObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *FunctionObjectLink) Link(ev gesture.ClickEvent) theme.Link {
	return (*OpenFunctionLink)(l)
}

func (l *FunctionObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *SpansObjectLink) Link(ev gesture.ClickEvent) theme.Link {
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

func (l *SpansObjectLink) ContextMenu() []*theme.MenuItem {
	if _, ok := l.Spans.Container(); ok {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Link: func() theme.Link {
					return ScrollToTimestampLink(l.Spans.At(0).Start)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Link: func() theme.Link {
					return ScrollToTimestampLink(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: PlainLabel("Zoom to span"),
				Link: func() theme.Link {
					return (*ZoomToSpansLink)(l)
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Link: func() theme.Link {
					return (*OpenSpansLink)(l)
				},
			},
		}
	} else {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Link: func() theme.Link {
					return ScrollToTimestampLink(l.Spans.At(0).Start)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Link: func() theme.Link {
					return ScrollToTimestampLink(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Link: func() theme.Link {
					return (*OpenSpansLink)(l)
				},
			},
		}
	}
}

func (l *ScrollToTimelineLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, (*Timeline)(l))
}

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

func handleLinkClick(win *theme.Window, ev TextEvent) {
	if ev.Event.Type == gesture.TypeClick && ev.Event.Button == pointer.ButtonPrimary {
		link := ev.Span.ObjectLink.Link(ev.Event)
		win.EmitLink(link)
	} else if ev.Event.Type == gesture.TypePress && ev.Event.Button == pointer.ButtonSecondary {
		menu := ev.Span.ObjectLink.ContextMenu()
		if len(menu) != 0 {
			win.SetContextMenu(menu)
		}
	}
}

func (l CanvasJumpToBeginningLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.JumpToBeginning(gtx)
}
func (l CanvasScrollToTopLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ScrollToTop(gtx)
}
func (l CanvasUndoNavigationLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.UndoNavigation(gtx)
}
func (l CanvasZoomToFitCurrentViewLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ZoomToFitCurrentView(gtx)
}
func (l OpenFlameGraphLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openFlameGraph()
}
func (l OpenHeatmapLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openHeatmap()
}
func (l OpenHighlightSpansDialogLink) Open(gtx layout.Context, mwin *MainWindow) {
	displayHighlightSpansDialog(mwin.twin, &mwin.canvas.timeline.filter)
}
func (l CanvasToggleTimelineLabelsLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleTimelineLabels()
}
func (l CanvasToggleCompactDisplayLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleCompactDisplay()
}
func (l CanvasToggleStackTracksLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleStackTracks()
}
func (l OpenScrollToTimelineDialog) Open(gtx layout.Context, mwin *MainWindow) {
	pl := theme.CommandPalette{Prompt: "Scroll to timeline"}
	pl.Set(GotoTimelineCommandProvider{mwin.twin, mwin.canvas.timelines})
	mwin.twin.SetModal(pl.Layout)
}
func (l OpenFileOpenDialog) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.showFileOpenDialog()
}
func (l ExitLink) Open(gtx layout.Context, mwin *MainWindow) {
	os.Exit(0)
}
func (l WriteMemoryProfileLink) Open(gtx layout.Context, mwin *MainWindow) {
	path, err := func() (string, error) {
		runtime.GC()
		path := fmt.Sprintf("mem-%d.pprof", time.Now().Unix())
		f, err := os.Create(path)
		if err != nil {
			return "", err
		}
		if err := pprof.WriteHeapProfile(f); err != nil {
			return "", err
		}
		return path, f.Close()
	}()
	if err == nil {
		mwin.twin.ShowNotification(gtx, fmt.Sprintf("Wrote memory profile to %s", path))
	} else {
		mwin.twin.ShowNotification(gtx, fmt.Sprintf("Couldn't write memory profile: %s", err))
	}
}
func (l RunGarbageCollectionLink) Open(gtx layout.Context, mwin *MainWindow) {
	start := time.Now()
	runtime.GC()
	d := time.Since(start)
	mwin.twin.ShowNotification(gtx, fmt.Sprintf("Ran garbage collection in %s", d))
}
func (l RunFreeOSMemoryLink) Open(gtx layout.Context, mwin *MainWindow) {
	rdebug.FreeOSMemory()
	mwin.twin.ShowNotification(gtx, "Returned unused memory to OS")
}
func (l StartCPUProfileLink) Open(gtx layout.Context, mwin *MainWindow) {
	if mwin.cpuProfile == nil {
		f, path, err := func() (*os.File, string, error) {
			path := fmt.Sprintf("cpu-%d.pprof", time.Now().Unix())
			f, err := os.Create(path)
			if err != nil {
				return nil, "", err
			}
			if err := pprof.StartCPUProfile(f); err != nil {
				f.Close()
				return nil, "", err
			}
			return f, path, nil
		}()
		if err == nil {
			mwin.twin.ShowNotification(gtx, fmt.Sprintf("Writing CPU profile to %s…", path))
		} else {
			mwin.twin.ShowNotification(gtx, fmt.Sprintf("Couldn't start CPU profile: %s", err))
		}
		mwin.cpuProfile = f
	}
}
func (l StopCPUProfileLink) Open(gtx layout.Context, mwin *MainWindow) {
	if mwin.cpuProfile != nil {
		pprof.StopCPUProfile()
		if err := mwin.cpuProfile.Close(); err == nil {
			mwin.twin.ShowNotification(gtx, fmt.Sprintf("Wrote CPU profile to %s", mwin.cpuProfile.Name()))
		} else {
			mwin.twin.ShowNotification(gtx, fmt.Sprintf("Couldn't write CPU profile: %s", err))
		}
		mwin.cpuProfile = nil
	}
}

func (l *OpenPanelLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openPanel(l.Panel)
}

func (PrevPanelLink) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.prevPanel()
}

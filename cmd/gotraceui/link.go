package main

import (
	"fmt"
	"os"
	"runtime"
	rdebug "runtime/debug"
	"runtime/pprof"
	"time"

	mycolor "honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/io/key"
	"gioui.org/io/pointer"
)

var colorLink = mycolor.Oklch{L: 0.7862, C: 0.104, H: 270, Alpha: 1}

type MainWindowAction interface {
	theme.Action
	Open(gtx layout.Context, mwin *MainWindow)
}

type ObjectLink interface {
	Action(ev gesture.ClickEvent) theme.Action
	ContextMenu() []*theme.MenuItem
	Commands() []theme.Command
}

type OpenGoroutineAction struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type ScrollToGoroutineAction struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type ZoomToGoroutineAction struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type ScrollToTimestampAction trace.Timestamp
type ScrollToProcessorAction struct {
	Processor  *ptrace.Processor
	Provenance string
}
type ZoomToProcessorAction struct {
	Processor  *ptrace.Processor
	Provenance string
}
type OpenFunctionAction struct {
	Function   *ptrace.Function
	Provenance string
}
type SpansAction struct{ Spans Items[ptrace.Span] }
type OpenSpansAction SpansAction
type ScrollAndPanToSpansAction SpansAction
type ZoomToSpansAction SpansAction
type ScrollToTimelineAction Timeline
type CanvasJumpToBeginningAction struct{}
type CanvasScrollToTopAction struct{}
type CanvasUndoNavigationAction struct{}
type CanvasZoomToFitCurrentViewAction struct{}
type OpenFlameGraphAction struct{}
type OpenHeatmapAction struct{}
type OpenHighlightSpansDialogAction struct{}
type CanvasToggleTimelineLabelsAction struct{}
type CanvasToggleCompactDisplayAction struct{}
type CanvasToggleStackTracksAction struct{}
type OpenScrollToTimelineAction struct{}
type OpenFileOpenAction struct{}
type ExitAction struct{}
type WriteMemoryProfileAction struct{}
type RunGarbageCollectionAction struct{}
type RunFreeOSMemoryAction struct{}
type StartCPUProfileAction struct{}
type StopCPUProfileAction struct{}
type OpenPanelAction struct{ Panel theme.Panel }
type PrevPanelAction struct{}
type GoroutineObjectLink struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type ProcessorObjectLink struct {
	Processor  *ptrace.Processor
	Provenance string
}
type TimestampObjectLink struct {
	Timestamp  trace.Timestamp
	Provenance string
}
type FunctionObjectLink struct {
	Function   *ptrace.Function
	Provenance string
}
type SpansObjectLink struct{ Spans Items[ptrace.Span] }

func (OpenGoroutineAction) IsAction()              {}
func (ScrollToGoroutineAction) IsAction()          {}
func (ZoomToGoroutineAction) IsAction()            {}
func (ScrollToTimestampAction) IsAction()          {}
func (ScrollToProcessorAction) IsAction()          {}
func (ZoomToProcessorAction) IsAction()            {}
func (OpenFunctionAction) IsAction()               {}
func (SpansAction) IsAction()                      {}
func (OpenSpansAction) IsAction()                  {}
func (ScrollAndPanToSpansAction) IsAction()        {}
func (ZoomToSpansAction) IsAction()                {}
func (ScrollToTimelineAction) IsAction()           {}
func (CanvasJumpToBeginningAction) IsAction()      {}
func (CanvasScrollToTopAction) IsAction()          {}
func (CanvasUndoNavigationAction) IsAction()       {}
func (CanvasZoomToFitCurrentViewAction) IsAction() {}
func (OpenFlameGraphAction) IsAction()             {}
func (OpenHeatmapAction) IsAction()                {}
func (OpenHighlightSpansDialogAction) IsAction()   {}
func (CanvasToggleTimelineLabelsAction) IsAction() {}
func (CanvasToggleCompactDisplayAction) IsAction() {}
func (CanvasToggleStackTracksAction) IsAction()    {}
func (OpenScrollToTimelineAction) IsAction()       {}
func (OpenFileOpenAction) IsAction()               {}
func (ExitAction) IsAction()                       {}
func (WriteMemoryProfileAction) IsAction()         {}
func (RunGarbageCollectionAction) IsAction()       {}
func (RunFreeOSMemoryAction) IsAction()            {}
func (StartCPUProfileAction) IsAction()            {}
func (StopCPUProfileAction) IsAction()             {}
func (*OpenPanelAction) IsAction()                 {}
func (PrevPanelAction) IsAction()                  {}

func defaultObjectLink(obj any, provenance string) ObjectLink {
	switch obj := obj.(type) {
	case *ptrace.Goroutine:
		return &GoroutineObjectLink{obj, provenance}
	case *ptrace.Processor:
		return &ProcessorObjectLink{obj, provenance}
	case *trace.Timestamp:
		return &TimestampObjectLink{*obj, provenance}
	case trace.Timestamp:
		return &TimestampObjectLink{obj, provenance}
	case *ptrace.Function:
		return &FunctionObjectLink{obj, provenance}
	default:
		panic(fmt.Sprintf("unsupported type: %T", obj))
	}
}

func (l *GoroutineObjectLink) Action(ev gesture.ClickEvent) theme.Action {
	switch ev.Modifiers {
	default:
		return (*ScrollToGoroutineAction)(l)
	case key.ModShortcut:
		return (*ZoomToGoroutineAction)(l)
	case key.ModShift:
		return (*OpenGoroutineAction)(l)
	}
}

func (l *GoroutineObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to goroutine"),
			Action: func() theme.Action {
				return (*ScrollToGoroutineAction)(l)
			},
		},
		{
			Label: PlainLabel("Zoom to goroutine"),
			Action: func() theme.Action {
				return (*ZoomToGoroutineAction)(l)
			},
		},
		{
			Label: PlainLabel("Show goroutine information"),
			Action: func() theme.Action {
				return (*OpenGoroutineAction)(l)
			},
		},
	}
}

func (l *ProcessorObjectLink) Action(ev gesture.ClickEvent) theme.Action {
	// There are no processor panels yet, so key.ModShift doesn't do anything
	switch ev.Modifiers {
	default:
		return (*ScrollToProcessorAction)(l)
	case key.ModShortcut:
		return (*ZoomToProcessorAction)(l)
	}
}

func (l *ProcessorObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to processor"),
			Action: func() theme.Action {
				return (*ScrollToProcessorAction)(l)
			},
		},
		{
			Label: PlainLabel("Zoom to processor"),
			Action: func() theme.Action {
				return (*ZoomToProcessorAction)(l)
			},
		},
	}
}

func (l *TimestampObjectLink) Action(ev gesture.ClickEvent) theme.Action {
	return ScrollToTimestampAction(l.Timestamp)
}

func (l *TimestampObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *FunctionObjectLink) Action(ev gesture.ClickEvent) theme.Action {
	return (*OpenFunctionAction)(l)
}

func (l *FunctionObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *SpansObjectLink) Action(ev gesture.ClickEvent) theme.Action {
	switch ev.Modifiers {
	default:
		ll := ScrollToTimestampAction(l.Spans.At(0).Start)
		return &ll
	case key.ModShortcut:
		if _, ok := l.Spans.Container(); ok {
			return (*ZoomToSpansAction)(l)
		} else {
			ll := ScrollToTimestampAction(l.Spans.At(0).Start)
			return &ll
		}
	case key.ModShift:
		return (*OpenSpansAction)(l)
	}
}

func (l *SpansObjectLink) ContextMenu() []*theme.MenuItem {
	if _, ok := l.Spans.Container(); ok {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(0).Start)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: PlainLabel("Zoom to span"),
				Action: func() theme.Action {
					return (*ZoomToSpansAction)(l)
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Action: func() theme.Action {
					return (*OpenSpansAction)(l)
				},
			},
		}
	} else {
		return []*theme.MenuItem{
			{
				Label: PlainLabel("Scroll to span start"),
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(0).Start)
				},
			},
			{
				Label: PlainLabel("Scroll to span end"),
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: PlainLabel("Show span information"),
				Action: func() theme.Action {
					return (*OpenSpansAction)(l)
				},
			},
		}
	}
}

func (l *ScrollToTimelineAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, (*Timeline)(l))
}

func (l *OpenGoroutineAction) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openGoroutine(l.Goroutine)
}

func (l *ScrollToGoroutineAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToObject(gtx, l.Goroutine)
}

func (l *ZoomToGoroutineAction) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.objectY(gtx, l.Goroutine)
	mwin.canvas.navigateToStartAndEnd(gtx, l.Goroutine.Spans[0].Start, l.Goroutine.Spans[len(l.Goroutine.Spans)-1].End, y)
}

func (l ScrollToTimestampAction) Open(gtx layout.Context, mwin *MainWindow) {
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
	mwin.canvas.navigateTo(gtx, trace.Timestamp(l)-off, mwin.canvas.nsPerPx, mwin.canvas.y)
}

func (l *ScrollToProcessorAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToObject(gtx, l.Processor)
}

func (l *ZoomToProcessorAction) Open(gtx layout.Context, mwin *MainWindow) {
	y := mwin.canvas.objectY(gtx, l.Processor)
	mwin.canvas.navigateToStartAndEnd(gtx, l.Processor.Spans[0].Start, l.Processor.Spans[len(l.Processor.Spans)-1].End, y)
}

func (l *OpenFunctionAction) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openFunction(l.Function)
}

func (l *OpenSpansAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openSpan(l.Spans)
}

func (l *ScrollAndPanToSpansAction) Open(gtx layout.Context, mwin *MainWindow) {
	c, ok := l.Spans.Container()
	assert(ok, "expected container")
	mwin.canvas.scrollToTimeline(gtx, c.Timeline)
	d := mwin.canvas.End() - mwin.canvas.start
	ts := l.Spans.At(0).Start + trace.Timestamp(SpansDuration(l.Spans)/2)
	mwin.canvas.navigateTo(gtx, ts-d/2, mwin.canvas.nsPerPx, mwin.canvas.animateTo.targetY)
}

func (l *ZoomToSpansAction) Open(gtx layout.Context, mwin *MainWindow) {
	c, ok := l.Spans.Container()
	assert(ok, "expected container")
	mwin.canvas.scrollToTimeline(gtx, c.Timeline)
	mwin.canvas.navigateToStartAndEnd(gtx, l.Spans.At(0).Start, LastSpan(l.Spans).End, mwin.canvas.animateTo.targetY)
}

func handleLinkClick(win *theme.Window, ev TextEvent) {
	if ev.Event.Type == gesture.TypeClick && ev.Event.Button == pointer.ButtonPrimary {
		link := ev.Span.ObjectLink.Action(ev.Event)
		win.EmitAction(link)
	} else if ev.Event.Type == gesture.TypePress && ev.Event.Button == pointer.ButtonSecondary {
		menu := ev.Span.ObjectLink.ContextMenu()
		if len(menu) != 0 {
			win.SetContextMenu(menu)
		}
	}
}

func (l CanvasJumpToBeginningAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.JumpToBeginning(gtx)
}
func (l CanvasScrollToTopAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ScrollToTop(gtx)
}
func (l CanvasUndoNavigationAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.UndoNavigation(gtx)
}
func (l CanvasZoomToFitCurrentViewAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ZoomToFitCurrentView(gtx)
}
func (l OpenFlameGraphAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openFlameGraph()
}
func (l OpenHeatmapAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openHeatmap()
}
func (l OpenHighlightSpansDialogAction) Open(gtx layout.Context, mwin *MainWindow) {
	displayHighlightSpansDialog(mwin.twin, &mwin.canvas.timeline.filter)
}
func (l CanvasToggleTimelineLabelsAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleTimelineLabels()
}
func (l CanvasToggleCompactDisplayAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleCompactDisplay()
}
func (l CanvasToggleStackTracksAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.ToggleStackTracks()
}
func (l OpenScrollToTimelineAction) Open(gtx layout.Context, mwin *MainWindow) {
	pl := theme.CommandPalette{Prompt: "Scroll to timeline"}
	pl.Set(GotoTimelineCommandProvider{mwin.twin, mwin.canvas.timelines})
	mwin.twin.SetModal(pl.Layout)
}
func (l OpenFileOpenAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.showFileOpenDialog()
}
func (l ExitAction) Open(gtx layout.Context, mwin *MainWindow) {
	os.Exit(0)
}
func (l WriteMemoryProfileAction) Open(gtx layout.Context, mwin *MainWindow) {
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
func (l RunGarbageCollectionAction) Open(gtx layout.Context, mwin *MainWindow) {
	start := time.Now()
	runtime.GC()
	d := time.Since(start)
	mwin.twin.ShowNotification(gtx, fmt.Sprintf("Ran garbage collection in %s", d))
}
func (l RunFreeOSMemoryAction) Open(gtx layout.Context, mwin *MainWindow) {
	rdebug.FreeOSMemory()
	mwin.twin.ShowNotification(gtx, "Returned unused memory to OS")
}
func (l StartCPUProfileAction) Open(gtx layout.Context, mwin *MainWindow) {
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
func (l StopCPUProfileAction) Open(gtx layout.Context, mwin *MainWindow) {
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

func (l *OpenPanelAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openPanel(l.Panel)
}

func (PrevPanelAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.prevPanel()
}

func (l *GoroutineObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Scroll to goroutine %d: %s", l.Goroutine.ID, l.Goroutine.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"goto", "go to"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*ScrollToGoroutineAction)(l)
			},
		},

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Zoom to goroutine %d: %s", l.Goroutine.ID, l.Goroutine.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*ZoomToGoroutineAction)(l)
			},
		},

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Show information for goroutine %d: %s", l.Goroutine.ID, l.Goroutine.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"open"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*OpenGoroutineAction)(l)
			},
		},
	}
}
func (l *ProcessorObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Scroll to processor %d", l.Processor.ID),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"goto", "go to"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*ScrollToProcessorAction)(l)
			},
		},

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Zoom to processor %d", l.Processor.ID),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*ZoomToProcessorAction)(l)
			},
		},
	}
}
func (l *TimestampObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Pan to %d ns", l.Timestamp),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"goto", "go to", "scroll", fmt.Sprintf("%d", l.Timestamp)},
			Color:          colorLink,
			Fn: func() theme.Action {
				return ScrollToTimestampAction(l.Timestamp)
			},
		},
	}
}
func (l *FunctionObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   fmt.Sprintf("Show information for function %s", l.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"open"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*OpenFunctionAction)(l)
			},
		},
	}
}
func (l *SpansObjectLink) Commands() []theme.Command { return nil }

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

type HoveredLinker interface {
	HoveredLink() *TextSpan
}

type NavigationAction interface {
	IsNavigationAction()
}

type OpenAction interface {
	IsOpenAction()
}

type MainWindowAction interface {
	theme.Action
	Open(gtx layout.Context, mwin *MainWindow)
}

type ObjectLink interface {
	Action(mods key.Modifiers) theme.Action
	ContextMenu() []*theme.MenuItem
	Commands() []theme.Command
}

type OpenGoroutineAction struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type OpenGoroutineFlameGraphAction struct {
	Goroutine  *ptrace.Goroutine
	Provenance string
}
type ScrollToTimestampAction trace.Timestamp
type OpenFunctionAction struct {
	Function   *ptrace.Function
	Provenance string
}
type SpansAction struct{ Spans Items[ptrace.Span] }
type OpenSpansAction SpansAction
type ScrollAndPanToSpansAction SpansAction
type ZoomToSpansAction SpansAction
type ScrollToTimelineAction struct {
	Timeline   *Timeline
	Provenance string
}
type ZoomToTimelineAction struct {
	Timeline   *Timeline
	Provenance string
}
type ScrollToObjectAction struct {
	Object     any
	Provenance string
}
type ZoomToObjectAction struct {
	Object     any
	Provenance string
}
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
type OpenPanelAction struct {
	Panel Panel
}
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
type GCObjectLink struct {
	GC         *GC
	Provenance string
}
type STWObjectLink struct {
	STW        *STW
	Provenance string
}
type SpansObjectLink struct {
	Spans Items[ptrace.Span]
}

func (*OpenGoroutineAction) IsAction()              {}
func (*OpenGoroutineFlameGraphAction) IsAction()    {}
func (ScrollToTimestampAction) IsAction()           {}
func (*OpenFunctionAction) IsAction()               {}
func (*SpansAction) IsAction()                      {}
func (*OpenSpansAction) IsAction()                  {}
func (*ScrollAndPanToSpansAction) IsAction()        {}
func (*ZoomToSpansAction) IsAction()                {}
func (*ScrollToTimelineAction) IsAction()           {}
func (*ZoomToTimelineAction) IsAction()             {}
func (*ScrollToObjectAction) IsAction()             {}
func (*ZoomToObjectAction) IsAction()               {}
func (*CanvasJumpToBeginningAction) IsAction()      {}
func (*CanvasScrollToTopAction) IsAction()          {}
func (*CanvasUndoNavigationAction) IsAction()       {}
func (*CanvasZoomToFitCurrentViewAction) IsAction() {}
func (*OpenFlameGraphAction) IsAction()             {}
func (*OpenHeatmapAction) IsAction()                {}
func (*OpenHighlightSpansDialogAction) IsAction()   {}
func (*CanvasToggleTimelineLabelsAction) IsAction() {}
func (*CanvasToggleCompactDisplayAction) IsAction() {}
func (*CanvasToggleStackTracksAction) IsAction()    {}
func (*OpenScrollToTimelineAction) IsAction()       {}
func (*OpenFileOpenAction) IsAction()               {}
func (*ExitAction) IsAction()                       {}
func (*WriteMemoryProfileAction) IsAction()         {}
func (*RunGarbageCollectionAction) IsAction()       {}
func (*RunFreeOSMemoryAction) IsAction()            {}
func (*StartCPUProfileAction) IsAction()            {}
func (*StopCPUProfileAction) IsAction()             {}
func (*OpenPanelAction) IsAction()                  {}
func (*PrevPanelAction) IsAction()                  {}

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
	case *GC:
		return &GCObjectLink{obj, provenance}
	case *STW:
		return &STWObjectLink{obj, provenance}
	default:
		panic(fmt.Sprintf("unsupported type: %T", obj))
	}
}

func (l *GoroutineObjectLink) Action(mods key.Modifiers) theme.Action {
	switch mods {
	default:
		return (*OpenGoroutineAction)(l)
	case key.ModShortcut:
		return &ZoomToObjectAction{
			Object:     l.Goroutine,
			Provenance: l.Provenance,
		}
	case key.ModShift:
		return &ScrollToObjectAction{
			Object:     l.Goroutine,
			Provenance: l.Provenance,
		}
	}
}

func (l *GoroutineObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: "Scroll to goroutine",
			Action: func() theme.Action {
				return &ScrollToObjectAction{
					Object:     l.Goroutine,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Zoom to goroutine",
			Action: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.Goroutine,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Show goroutine information",
			Action: func() theme.Action {
				return (*OpenGoroutineAction)(l)
			},
		},
		{
			Label: "Open flame graph",
			Action: func() theme.Action {
				return (*OpenGoroutineFlameGraphAction)(l)
			},
		},
	}
}

func (l *ProcessorObjectLink) Action(mods key.Modifiers) theme.Action {
	// There are no processor panels yet, so key.ModShift doesn't do anything
	switch mods {
	default:
		return &ScrollToObjectAction{
			Object:     l.Processor,
			Provenance: l.Provenance,
		}
	case key.ModShortcut:
		return &ZoomToObjectAction{
			Object:     l.Processor,
			Provenance: l.Provenance,
		}
	}
}

func (l *ProcessorObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: "Scroll to processor",
			Action: func() theme.Action {
				return &ScrollToObjectAction{
					Object:     l.Processor,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Zoom to processor",
			Action: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.Processor,
					Provenance: l.Provenance,
				}
			},
		},
	}
}

func (l *TimestampObjectLink) Action(mods key.Modifiers) theme.Action {
	return ScrollToTimestampAction(l.Timestamp)
}

func (l *TimestampObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *FunctionObjectLink) Action(mods key.Modifiers) theme.Action {
	return (*OpenFunctionAction)(l)
}

func (l *FunctionObjectLink) ContextMenu() []*theme.MenuItem {
	return nil
}

func (l *GCObjectLink) Action(mods key.Modifiers) theme.Action {
	switch mods {
	default:
		return &OpenSpansAction{
			Spans: l.GC.Spans,
		}
	case key.ModShortcut:
		return &ZoomToObjectAction{
			Object:     l.GC,
			Provenance: l.Provenance,
		}
	case key.ModShift:
		return &ScrollToObjectAction{
			Object:     l.GC,
			Provenance: l.Provenance,
		}
	}
}

func (l *GCObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: "Scroll to GC timeline",
			Action: func() theme.Action {
				return &ScrollToObjectAction{
					Object:     l.GC,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Zoom to GC timeline",
			Action: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.GC,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Show GC information",
			Action: func() theme.Action {
				return &OpenSpansAction{
					Spans: l.GC.Spans,
				}
			},
		},
	}
}

func (l *STWObjectLink) Action(mods key.Modifiers) theme.Action {
	switch mods {
	default:
		return &OpenSpansAction{
			Spans: l.STW.Spans,
		}
	case key.ModShortcut:
		return &ZoomToObjectAction{
			Object:     l.STW,
			Provenance: l.Provenance,
		}
	case key.ModShift:
		return &ScrollToObjectAction{
			Object:     l.STW,
			Provenance: l.Provenance,
		}
	}
}

func (l *STWObjectLink) ContextMenu() []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: "Scroll to STW timeline",
			Action: func() theme.Action {
				return &ScrollToObjectAction{
					Object:     l.STW,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Zoom to STW timeline",
			Action: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.STW,
					Provenance: l.Provenance,
				}
			},
		},
		{
			Label: "Show STW information",
			Action: func() theme.Action {
				return &OpenSpansAction{
					Spans: l.STW.Spans,
				}
			},
		},
	}
}

func (l *SpansObjectLink) Action(mods key.Modifiers) theme.Action {
	switch mods {
	default:
		return (*OpenSpansAction)(l)
	case key.ModShortcut:
		if _, ok := l.Spans.Container(); ok {
			return (*ZoomToSpansAction)(l)
		} else {
			ll := ScrollToTimestampAction(l.Spans.At(0).Start)
			return &ll
		}
	case key.ModShift:
		ll := ScrollToTimestampAction(l.Spans.At(0).Start)
		return &ll
	}
}

func (l *SpansObjectLink) ContextMenu() []*theme.MenuItem {
	if _, ok := l.Spans.Container(); ok {
		return []*theme.MenuItem{
			{
				Label: "Scroll to span start",
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(0).Start)
				},
			},
			{
				Label: "Scroll to span end",
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: "Zoom to span",
				Action: func() theme.Action {
					return (*ZoomToSpansAction)(l)
				},
			},
			{
				Label: "Show span information",
				Action: func() theme.Action {
					return (*OpenSpansAction)(l)
				},
			},
		}
	} else {
		return []*theme.MenuItem{
			{
				Label: "Scroll to span start",
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(0).Start)
				},
			},
			{
				Label: "Scroll to span end",
				Action: func() theme.Action {
					return ScrollToTimestampAction(l.Spans.At(l.Spans.Len() - 1).End)
				},
			},
			{
				Label: "Show span information",
				Action: func() theme.Action {
					return (*OpenSpansAction)(l)
				},
			},
		}
	}
}

func (l *ScrollToTimelineAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.canvas.scrollToTimeline(gtx, l.Timeline)
}

func (l *ZoomToTimelineAction) Open(gtx layout.Context, mwin *MainWindow) {
	// TODO(dh): this assumes that the first track is always the longest
	tr := l.Timeline.tracks[0]
	y := mwin.canvas.timelineY(gtx, l.Timeline)
	mwin.canvas.navigateToStartAndEnd(gtx, tr.Start, tr.End, y)
}

func (l *ScrollToObjectAction) Open(gtx layout.Context, mwin *MainWindow) {
	// OPT(dh): don't be O(n)
	for _, tl := range mwin.canvas.timelines {
		if tl.item == l.Object {
			mwin.canvas.scrollToTimeline(gtx, tl)
			return
		}
	}
}

func (l *ZoomToObjectAction) Open(gtx layout.Context, mwin *MainWindow) {
	// TODO(dh): this assumes that the first track is always the longest
	// OPT(dh): don't be O(n)
	for _, tl := range mwin.canvas.timelines {
		if tl.item == l.Object {
			tr := tl.tracks[0]
			y := mwin.canvas.timelineY(gtx, tl)
			mwin.canvas.navigateToStartAndEnd(gtx, tr.Start, tr.End, y)
			return
		}
	}
}

func (l *OpenGoroutineAction) Open(_ layout.Context, mwin *MainWindow) {
	mwin.openGoroutine(l.Goroutine)
}

func (l *OpenGoroutineFlameGraphAction) Open(gtx layout.Context, mwin *MainWindow) {
	mwin.openFlameGraph(l.Goroutine)
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
		link := ev.Span.ObjectLink.Action(ev.Event.Modifiers)
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
	mwin.openFlameGraph(nil)
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
	pl.Set(ScrollToTimelineCommandProvider{mwin.twin, mwin.canvas.timelines})
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

func (*PrevPanelAction) Open(gtx layout.Context, mwin *MainWindow) {
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
				return &ScrollToObjectAction{
					Object:     l.Goroutine,
					Provenance: l.Provenance,
				}
			},
		},

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Zoom to goroutine %d: %s", l.Goroutine.ID, l.Goroutine.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Color:          colorLink,
			Fn: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.Goroutine,
					Provenance: l.Provenance,
				}
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

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Open flame graph for goroutine %d: %s", l.Goroutine.ID, l.Goroutine.Function.Fn),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"show", "flamegraph"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return (*OpenGoroutineFlameGraphAction)(l)
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
				return &ScrollToObjectAction{
					Object:     l.Processor,
					Provenance: l.Provenance,
				}
			},
		},

		theme.NormalCommand{
			PrimaryLabel:   local.Sprintf("Zoom to processor %d", l.Processor.ID),
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Color:          colorLink,
			Fn: func() theme.Action {
				return &ZoomToObjectAction{
					Object:     l.Processor,
					Provenance: l.Provenance,
				}
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
func (l *GCObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   "Show GC spans",
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"open"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return &OpenSpansAction{l.GC.Spans}
			},
		},
	}
}
func (l *STWObjectLink) Commands() []theme.Command {
	return []theme.Command{
		theme.NormalCommand{
			PrimaryLabel:   "Show STW spans",
			SecondaryLabel: l.Provenance,
			Category:       "Link",
			Aliases:        []string{"open"},
			Color:          colorLink,
			Fn: func() theme.Action {
				return &OpenSpansAction{l.STW.Spans}
			},
		},
	}
}
func (l *SpansObjectLink) Commands() []theme.Command { return nil }

func (*OpenGoroutineAction) IsOpenAction()                    {}
func (*OpenGoroutineFlameGraphAction) IsOpenAction()          {}
func (ScrollToTimestampAction) IsNavigationAction()           {}
func (*OpenFunctionAction) IsOpenAction()                     {}
func (*SpansAction) IsOpenAction()                            {}
func (*OpenSpansAction) IsOpenAction()                        {}
func (*ScrollAndPanToSpansAction) IsNavigationAction()        {}
func (*ZoomToSpansAction) IsNavigationAction()                {}
func (*ScrollToTimelineAction) IsNavigationAction()           {}
func (*ZoomToTimelineAction) IsNavigationAction()             {}
func (*ScrollToObjectAction) IsNavigationAction()             {}
func (*ZoomToObjectAction) IsNavigationAction()               {}
func (*CanvasJumpToBeginningAction) IsNavigationAction()      {}
func (*CanvasScrollToTopAction) IsNavigationAction()          {}
func (*CanvasUndoNavigationAction) IsNavigationAction()       {}
func (*CanvasZoomToFitCurrentViewAction) IsNavigationAction() {}
func (*OpenFlameGraphAction) IsOpenAction()                   {}
func (*OpenHeatmapAction) IsOpenAction()                      {}
func (*OpenHighlightSpansDialogAction) IsOpenAction()         {}
func (*OpenScrollToTimelineAction) IsOpenAction()             {}
func (*OpenFileOpenAction) IsOpenAction()                     {}
func (*OpenPanelAction) IsOpenAction()                        {}

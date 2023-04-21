package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"image"
	"io"
	"log"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	rdebug "runtime/debug"
	"runtime/pprof"
	rtrace "runtime/trace"
	"strconv"
	"strings"
	"sync/atomic"
	"time"
	"unicode"

	"honnef.co/go/gotraceui/cmd/gotraceui/assets"
	ourfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/profile"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/x/component"
	"gioui.org/x/explorer"
	"gioui.org/x/styledtext"
	"golang.org/x/text/message"
)

func debugCaching(gtx layout.Context) {
	if false {
		paint.Fill(gtx.Ops, rgba(0xFF00FFAA))
	}
}

// A note on Ps
//
// Not all events have a P. For example, when sysmon wakes up the scavenger, it doesn't have a P while unblocking
// goroutines.

/*
   Goroutine window, things to display:

   - [X] ID, function name
   - [X] stack of where it was created
   - Link to span that created it
   - [X] First, last timestamp, duration
   - [X] Per-state statistics (how long blocked, waiting, etc, number of state transitions)
   - [X] List of all spans
   - [X] List of all events of all spans
     - Syscalls
     - Outgoing unblocks
     - Incoming unblocks
   - List of other goroutines of the same function (actually this belongs in the function window instead)
   - [X] Link to function window
   - List of procs it ran on
   - List of user regions
   - How much memory we sweeped/reclaimed
   - Maybe something about MMU?
*/

// TODO(dh): disable navigation keybindings such as Home when we're dragging
// TODO(dh): support exporting an image of the entire trace, at a zoom level that shows all details
// TODO(dh): clicking on a goroutine in the per-P view should bring up the goroutine window
// TODO(dh): allow computing statistics for a selectable region of time
// TODO(dh): use the GC-purple color in the GC and STW timelines
// TODO(dh): toggleable behavior for hovering spans in goroutine timelines. For example, hovering a blocked span could
//   highlight the span that unblocks it (or maybe when hovering the "runnable" span, but same idea). Hovering a running
//   span could highlight all the spans it unblocks.
// TODO(dh): the Event.Stk is meaningless for goroutines that already existed when tracing started, i.e. ones that get a
//   GoWaiting event. The GoCreate event will be caused by starting the trace, and the stack of the event will be that
//   leading up to starting the trace. It will in no way reflect the code that actually, historically, started the
//   goroutine. To avoid confusion, we should remove those stacks altogether.

// This boolean guards all code involving displaying machine timelines. That feature is currently broken, because the
// trace parser only produces an event ordering that is consistent for goroutines, but not for machines. For example, it
// may try to start a P on an M that is currently blocked on a syscall.
const supportMachineTimelines = false

var (
	cpuprofile         string
	memprofileLoad     string
	memprofileExit     string
	traceFile          string
	disableCaching     bool
	exitAfterLoading   bool
	exitAfterParsing   bool
	measureFrameAllocs bool
	invalidateFrames   bool
)

type reusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *reusableOps) get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
}

func (mwin *MainWindow) openGoroutine(g *ptrace.Goroutine) {
	gi := NewGoroutineInfo(mwin, g)
	mwin.openPanel(gi)
}

func (mwin *MainWindow) openFunction(fn *ptrace.Function) {
	fi := NewFunctionInfo(mwin, fn)
	mwin.openPanel(fi)
}

func (mwin *MainWindow) openSpan(s ptrace.Spans, tl *Timeline, tr *Track, allEvents []ptrace.EventID) {
	var labels []string
	var label string
	if tr.spanLabel != nil {
		labels = tr.spanLabel(s, tl.cv.trace, nil)
	}
	if len(labels) > 0 {
		label = labels[0]
	}

	cfg := SpansInfoConfig{
		Label: label,
		Container: SpanContainer{
			Timeline: tl,
			Track:    tr,
		},
	}
	si := NewSpansInfo(cfg, mwin, s, allEvents)
	mwin.openPanel(si)
}

func (mwin *MainWindow) openPanel(p theme.Panel) {
	if mwin.panel != nil {
		mwin.panelHistory = append(mwin.panelHistory, mwin.panel)
		if len(mwin.panelHistory) == 101 {
			copy(mwin.panelHistory, mwin.panelHistory[1:])
			mwin.panelHistory = mwin.panelHistory[:100]
		}
	}
	p.SetWindowed(false)
	mwin.panel = p
}

func (mwin *MainWindow) prevPanel() bool {
	if len(mwin.panelHistory) == 0 {
		return false
	}
	p := mwin.panelHistory[len(mwin.panelHistory)-1]
	mwin.panelHistory = mwin.panelHistory[:len(mwin.panelHistory)-1]
	mwin.panel = p
	return true
}

func (mwin *MainWindow) closePanel() {
	mwin.panel = nil
}

type PanelWindow struct {
	MainWindow *MainWindow
	Panel      theme.Panel
}

func (pwin *PanelWindow) Run(win *app.Window) error {
	var ops op.Ops
	tWin := theme.NewWindow(win)

	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			tWin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				paint.Fill(gtx.Ops, tWin.Theme.Palette.Background)
				return pwin.Panel.Layout(win, gtx)
			})

			if pwin.Panel.Closed() {
				win.Perform(system.ActionClose)
			} else if pwin.Panel.Attached() {
				pwin.MainWindow.openPanel(pwin.Panel)
				win.Perform(system.ActionClose)
			}

			ev.Frame(&ops)
		}
	}

	return nil
}

func (mwin *MainWindow) openPanelWindow(p theme.Panel) {
	win := &PanelWindow{MainWindow: mwin, Panel: p}
	p.SetWindowed(true)
	go func() {
		// XXX handle error?
		win.Run(app.NewWindow(app.Title("gotraceui - " + p.Title())))
	}()

}

func (mwin *MainWindow) openHeatmap() {
	win := &HeatmapWindow{
		trace: mwin.trace,
	}
	go func() {
		// XXX handle error?
		win.Run(app.NewWindow(app.Title("gotraceui - heatmap")))
	}()
}

func shortenFunctionName(s string) string {
	fields := strings.Split(s, ".")
	return fields[len(fields)-1]
}

type Command func(*MainWindow, layout.Context)

type MainWindow struct {
	canvas          Canvas
	trace           *Trace
	commands        chan Command
	explorer        *explorer.Explorer
	showingExplorer atomic.Bool

	// Channel used by goroutines to report critical errors.
	errs chan error

	panel        theme.Panel
	panelHistory []theme.Panel

	pointerAt f32.Point

	win  *app.Window
	twin *theme.Window
	// TODO(dh): use enum for state
	state          string
	progress       float64
	progressStage  int
	progressStages []string
	ww             *theme.ListWindow
	err            error

	debugWindow *DebugWindow
}

func NewMainWindow() *MainWindow {
	mwin := &MainWindow{
		commands:    make(chan Command, 128),
		debugWindow: NewDebugWindow(),
		errs:        make(chan error),
	}

	return mwin
}

type timelineFilter struct {
	invalid bool
	parts   []struct {
		prefix string
		value  struct {
			s string
			n uint64
		}
	}
}

func newTimelineFilter(s string) theme.Filter {
	out := &timelineFilter{}
	for _, field := range strings.Fields(s) {
		prefix, value, found := strings.Cut(field, ":")
		if !found {
			prefix, value = value, prefix
		}

		var v struct {
			s string
			n uint64
		}
		switch prefix {
		case "gid", "pid":
			var err error
			v.n, err = strconv.ParseUint(value, 10, 64)
			if err != nil {
				out.invalid = true
			}
		default:
			v.s = value
		}

		out.parts = append(out.parts, struct {
			prefix string
			value  struct {
				s string
				n uint64
			}
		}{prefix, v})
	}
	return out
}

func (f *timelineFilter) Filter(item theme.ListWindowItem) bool {
	if f.invalid {
		return false
	}

	for _, p := range f.parts {
		switch p.prefix {
		case "gid":
			if item, ok := item.Item.(*ptrace.Goroutine); !ok || item.ID != p.value.n {
				return false
			}

		case "pid":
			if item, ok := item.Item.(*ptrace.Processor); !ok || uint64(item.ID) != p.value.n {
				return false
			}

		case "":
			ss := item.FilterLabels
			any := false
			for _, s := range ss {
				if strings.Contains(s, p.value.s) {
					any = true
					break
				}
			}
			if !any {
				return false
			}

		default:
			return false
		}
	}
	return true
}

// OpenTrace initiates loading of a trace. It changes the state to loadingTrace, loads the trace, and notifies the
// window when it's done. OpenTrace should be called from a different goroutine than the render loop.
func (mwin *MainWindow) OpenTrace(r io.Reader) {
	// Unset the memory limit in case we've already loaded a trace but this trace needs more memory.
	rdebug.SetMemoryLimit(-1)

	mwin.SetState("loadingTrace")
	res, err := loadTrace(r, mwin)
	if memprofileLoad != "" {
		writeMemprofile(memprofileLoad)
	}
	if err == errExitAfterParsing {
		mwin.errs <- err
		return
	}
	if exitAfterLoading {
		mwin.errs <- errExitAfterLoading
		return
	}
	if err != nil {
		mwin.SetError(fmt.Errorf("couldn't load trace: %w", err))
		return
	}

	// At this point we've allocated most long-lived memory. For large traces, the GC goal will be a lot higher than the
	// memory needed for the "static" data, causing our memory usage to grow a lot over time as we make short-lived
	// allocations when rendering frames. To avoid this we get the current memory usage, add a GiB on top, and set that as
	// the soft memory limit. Go will try to stay within the limit, which should be easy in our case, as each frame
	// produces a modest amount of garbage.
	//
	// It doesn't matter if our soft limit is vastly higher than the GC goal (which happens if the loaded trace was very
	// small), as the memory limit doesn't disable or overwrite the GC goal. That is, if memory usage is 50 MiB, we set
	// the limit to 1074 MiB, and the GC goal is 100 MiB, then Go will still try to stay within the 100 MiB limit.
	//
	// There are only two cases in which this memory limit could lead to the GC running to often: if our set of
	// long-lived allocations grows a lot, or if we allocate 1 GiB of short-lived allocations in a very short time. The
	// former would be a bug, and the former would ultimately lead to bad performance, anyway.
	runtime.GC()
	var mem runtime.MemStats
	runtime.ReadMemStats(&mem)
	limit := int64(mem.Sys-mem.HeapReleased) + 1024*1024*1024 // 1 GiB
	rdebug.SetMemoryLimit(limit)
	mwin.LoadTrace(res)
}

func (mwin *MainWindow) setState(state string) {
	mwin.state = state
	mwin.progress = 0.0
	mwin.ww = nil
}

func (mwin *MainWindow) SetState(state string) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.twin.CloseModal()
		mwin.twin.Menu.Close()
		mwin.setState(state)
	}
}

func (mwin *MainWindow) SetError(err error) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.err = err
		mwin.setState("error")
	}
}

func (mwin *MainWindow) SetProgress(p float64) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.progress = p
	}
}

func (mwin *MainWindow) SetProgressStages(names []string) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.progressStages = names
	}
}

func (mwin *MainWindow) SetProgressStage(idx int) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.progressStage = idx
		mwin.progress = 0
	}
}

func (mwin *MainWindow) SetProgressLossy(p float64) {
	fn := func(mwin *MainWindow, _ layout.Context) {
		mwin.progress = p
	}
	select {
	case mwin.commands <- fn:
	default:
	}
}

func (mwin *MainWindow) LoadTrace(res loadTraceResult) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.loadTraceImpl(res)
		mwin.setState("main")
	}
}

func (mwin *MainWindow) OpenLink(l Link) {
	mwin.commands <- func(mwin *MainWindow, gtx layout.Context) {
		// TODO(dh): links should probably have a method that take a MainWindow and run the appropriate commands on it
		switch l := l.(type) {
		case *GoroutineLink:
			switch l.Kind {
			case GoroutineLinkKindOpen:
				mwin.openGoroutine(l.Goroutine)
			case GoroutineLinkKindScroll:
				mwin.canvas.scrollToTimeline(gtx, l.Goroutine)
			case GoroutineLinkKindZoom:
				y := mwin.canvas.timelineY(gtx, l.Goroutine)
				mwin.canvas.navigateToStartAndEnd(gtx, l.Goroutine.Spans.Start(), l.Goroutine.Spans.End(), y)
			default:
				panic(l.Kind)
			}

		case *ProcessorLink:
			switch l.Kind {
			case ProcessorLinkKindScroll:
				mwin.canvas.scrollToTimeline(gtx, l.Processor)
			case ProcessorLinkKindZoom:
				y := mwin.canvas.timelineY(gtx, l.Processor)
				mwin.canvas.navigateToStartAndEnd(gtx, l.Processor.Spans.Start(), l.Processor.Spans.End(), y)
			default:
				panic(l.Kind)
			}

		case *FunctionLink:
			mwin.openFunction(l.Fn)

		case *TimestampLink:
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
			mwin.canvas.navigateTo(gtx, l.Ts-off, mwin.canvas.nsPerPx, mwin.canvas.y)

		case *SpansLink:
			switch l.Kind {
			case SpanLinkKindScrollAndPan:
				mwin.canvas.scrollToTimeline(gtx, l.Timeline.item)
				d := mwin.canvas.End() - mwin.canvas.start
				ts := l.Spans.At(0).Start + trace.Timestamp(SpansDuration(l.Spans)/2)
				mwin.canvas.navigateTo(gtx, ts-d/2, mwin.canvas.nsPerPx, mwin.canvas.animateTo.targetY)
			case SpanLinkKindZoom:
				mwin.canvas.scrollToTimeline(gtx, l.Timeline.item)
				mwin.canvas.navigateToStartAndEnd(gtx, l.Spans.At(0).Start, LastSpan(l.Spans).End, mwin.canvas.animateTo.targetY)
			default:
				panic(l.Kind)
			}

		default:
			panic(fmt.Sprintf("unsupported type: %T", l))
		}
	}
}

type ToggleLable struct {
	// LabelTrue = "Disable compact display"
	LabelTrue, LabelFalse string
	Value                 *bool
}

func ToggleLabel(t, f string, b *bool) func() string {
	return func() string {
		if *b {
			return t
		} else {
			return f
		}
	}
}
func PlainLabel(s string) func() string { return func() string { return s } }

type MainMenu struct {
	File struct {
		OpenTrace theme.MenuItem
		Quit      theme.MenuItem
	}

	Display struct {
		UndoNavigation       theme.MenuItem
		ScrollToTop          theme.MenuItem
		ZoomToFit            theme.MenuItem
		JumpToBeginning      theme.MenuItem
		HighlightSpans       theme.MenuItem
		ToggleCompactDisplay theme.MenuItem
		ToggleTimelineLabels theme.MenuItem
		ToggleStackTracks    theme.MenuItem
	}

	Analyze struct {
		OpenHeatmap theme.MenuItem
	}

	Debug struct {
		Memprofile theme.MenuItem
	}

	menu *theme.Menu
}

func NewMainMenu(mwin *MainWindow, win *theme.Window) *MainMenu {
	m := &MainMenu{}

	m.File.OpenTrace = theme.MenuItem{Label: PlainLabel("Open trace")}
	m.File.Quit = theme.MenuItem{Label: PlainLabel("Quit")}

	notMainDisabled := func() bool { return mwin.state != "main" }
	m.Display.UndoNavigation = theme.MenuItem{Shortcut: key.ModShortcut.String() + "+Z", Label: PlainLabel("Undo previous navigation"), Disabled: notMainDisabled}
	m.Display.ScrollToTop = theme.MenuItem{Shortcut: "Home", Label: PlainLabel("Scroll to top of canvas"), Disabled: notMainDisabled}
	m.Display.ZoomToFit = theme.MenuItem{Shortcut: key.ModShortcut.String() + "+Home", Label: PlainLabel("Zoom to fit visible timelines"), Disabled: notMainDisabled}
	m.Display.JumpToBeginning = theme.MenuItem{Shortcut: "Shift+Home", Label: PlainLabel("Jump to beginning of timeline"), Disabled: notMainDisabled}
	m.Display.HighlightSpans = theme.MenuItem{Shortcut: "H", Label: PlainLabel("Highlight spans…"), Disabled: notMainDisabled}
	m.Display.ToggleCompactDisplay = theme.MenuItem{Shortcut: "C", Label: ToggleLabel("Disable compact display", "Enable compact display", &mwin.canvas.timeline.compact), Disabled: notMainDisabled}
	m.Display.ToggleTimelineLabels = theme.MenuItem{Shortcut: "X", Label: ToggleLabel("Hide timeline labels", "Show timeline labels", &mwin.canvas.timeline.displayAllLabels), Disabled: notMainDisabled}
	m.Display.ToggleStackTracks = theme.MenuItem{Shortcut: "S", Label: ToggleLabel("Hide stack frames", "Show stack frames", &mwin.canvas.timeline.displayStackTracks), Disabled: notMainDisabled}

	m.Debug.Memprofile = theme.MenuItem{Label: PlainLabel("Write memory profile")}

	m.Analyze.OpenHeatmap = theme.MenuItem{Label: PlainLabel("Open processor utilization heatmap"), Disabled: notMainDisabled}

	m.menu = &theme.Menu{
		Groups: []theme.MenuGroup{
			{
				Label: "File",
				Items: []theme.Widget{
					theme.NewMenuItemStyle(win.Theme, &m.File.OpenTrace).Layout,
					theme.NewMenuItemStyle(win.Theme, &m.File.Quit).Layout,
				},
			},
			{
				Label: "Display",
				Items: []theme.Widget{
					// TODO(dh): disable Undo menu item when there are no more undo steps
					theme.NewMenuItemStyle(win.Theme, &m.Display.UndoNavigation).Layout,

					theme.MenuDivider(win.Theme).Layout,

					theme.NewMenuItemStyle(win.Theme, &m.Display.ScrollToTop).Layout,
					theme.NewMenuItemStyle(win.Theme, &m.Display.ZoomToFit).Layout,
					theme.NewMenuItemStyle(win.Theme, &m.Display.JumpToBeginning).Layout,

					theme.MenuDivider(win.Theme).Layout,

					theme.NewMenuItemStyle(win.Theme, &m.Display.HighlightSpans).Layout,

					theme.MenuDivider(win.Theme).Layout,

					theme.NewMenuItemStyle(win.Theme, &m.Display.ToggleCompactDisplay).Layout,
					theme.NewMenuItemStyle(win.Theme, &m.Display.ToggleTimelineLabels).Layout,
					theme.NewMenuItemStyle(win.Theme, &m.Display.ToggleStackTracks).Layout,
					// TODO(dh): add items for STW and GC overlays
					// TODO(dh): add item for tooltip display
				},
			},
			{
				Label: "Analyze",
				Items: []theme.Widget{
					theme.NewMenuItemStyle(win.Theme, &m.Analyze.OpenHeatmap).Layout,
				},
			},
		},
	}

	if debug {
		m.menu.Groups = append(m.menu.Groups, theme.MenuGroup{
			Label: "Debug",
			Items: []theme.Widget{
				theme.NewMenuItemStyle(win.Theme, &m.Debug.Memprofile).Layout,
			},
		})
	}

	return m
}

func displayHighlightSpansDialog(win *theme.Window, filter *Filter) {
	hd := HighlightDialog(win, filter)
	win.SetModal(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		return theme.Dialog(win.Theme, "Highlight spans").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = gtx.Constraints.Constrain(image.Pt(1000, 500))
			gtx.Constraints.Max = gtx.Constraints.Min
			return hd.Layout(win, gtx)
		})
	})
}

func (mwin *MainWindow) Run(win *app.Window) error {
	mwin.twin = theme.NewWindow(win)
	mainMenu := NewMainMenu(mwin, mwin.twin)
	mwin.win = win
	mwin.explorer = explorer.NewExplorer(win)

	profileTag := new(int)
	var ops op.Ops
	var shortcuts int

	var commands []Command
	mwin.twin.Menu = mainMenu.menu

	resize := component.Resize{
		Axis:  layout.Horizontal,
		Ratio: 0.70,
	}

	var prevTotalAlloc uint64
	var prevMallocs uint64
	var mem runtime.MemStats
	var frameCounter uint64
	var openTraceButton widget.PrimaryClickable

	for {
		select {
		case cmd := <-mwin.commands:
			commands = append(commands, cmd)
			mwin.win.Invalidate()

		case e := <-win.Events():
			mwin.explorer.ListenEvents(e)

			switch ev := e.(type) {
			case system.DestroyEvent:
				return ev.Err
			case system.FrameEvent:
				if measureFrameAllocs {
					frameCounter++
					if frameCounter%60 == 0 {
						runtime.ReadMemStats(&mem)
						log.Printf("%10.2f bytes/frame; %10.2f allocs/frame",
							float64(mem.TotalAlloc-prevTotalAlloc)/float64(60),
							float64(mem.Mallocs-prevMallocs)/float64(60),
						)
						prevTotalAlloc = mem.TotalAlloc
						prevMallocs = mem.Mallocs
					}
				}

				mwin.twin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
					gtx.Constraints.Min = image.Point{}

					for _, cmd := range commands {
						cmd(mwin, gtx)
					}
					commands = commands[:0]

					for _, ev := range gtx.Events(&mwin.pointerAt) {
						mwin.pointerAt = ev.(pointer.Event).Position
					}
					pointer.InputOp{Tag: &mwin.pointerAt, Types: pointer.Move | pointer.Drag | pointer.Enter}.Add(gtx.Ops)

				commandLoop:
					for {
						select {
						case cmd := <-mwin.commands:
							cmd(mwin, gtx)
						default:
							break commandLoop
						}
					}

					for _, ev := range gtx.Events(profileTag) {
						// Yup, profile.Event only contains a string. No structured access to data.
						fields := strings.Fields(ev.(profile.Event).Timings)
						if len(fields) > 0 && strings.HasPrefix(fields[0], "tot:") {
							var s string
							if fields[0] == "tot:" {
								s = fields[1]
							} else {
								s = strings.TrimPrefix(fields[0], "tot:")
							}
							// Either it parses fine, or d is undefined and will likely be obvious in the debug grpah.
							d, _ := time.ParseDuration(s)
							// We're using gtx.Now because events don't have timestamps associated with them. Hopefully
							// event creation isn't too far removed from this code.
							mwin.debugWindow.frametimes.addValue(gtx.Now, float64(d)/float64(time.Millisecond))
						}
					}
					profile.Op{Tag: profileTag}.Add(gtx.Ops)

					// Fill background
					paint.Fill(gtx.Ops, mwin.twin.Theme.Palette.Background)

					if mainMenu.File.Quit.Clicked() {
						win.Menu.Close()
						os.Exit(0)
					}

					if mainMenu.File.OpenTrace.Clicked() {
						win.Menu.Close()
						mwin.showFileOpenDialog()
					}

					switch mwin.state {
					case "empty":
						return layout.Dimensions{}

					case "start":
						gtx.Constraints.Min = gtx.Constraints.Max

						for openTraceButton.Clicked() {
							mwin.showFileOpenDialog()
						}
						return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							gtx.Constraints.Min.X = gtx.Constraints.Max.X
							return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									return layout.Center.Layout(gtx, widget.Image{Src: assets.Image(gtx, "logo", 128), Scale: 1.0 / gtx.Metric.PxPerDp}.Layout)
								}),

								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									return layout.Center.Layout(gtx, theme.Dumb(win, theme.Button(win.Theme, &openTraceButton.Clickable, "Open trace").Layout))
								}),
							)
						})

					case "error":
						gtx.Constraints.Min = gtx.Constraints.Max
						return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							return theme.Dialog(win.Theme, "Error").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
								return widget.Label{}.Layout(gtx, mwin.twin.Theme.Shaper, font.Font{}, win.Theme.TextSize, mwin.err.Error(), widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
							})
						})

					case "loadingTrace":
						paint.ColorOp{Color: mwin.twin.Theme.Palette.Foreground}.Add(gtx.Ops)

						// OPT(dh): cache this computation
						var maxNameWidth int
						for _, name := range mwin.progressStages {
							m := op.Record(gtx.Ops)
							dims := widget.Label{}.Layout(gtx, mwin.twin.Theme.Shaper, font.Font{}, mwin.twin.Theme.TextSize, name, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
							if dims.Size.X > maxNameWidth {
								maxNameWidth = dims.Size.X
							}
							m.Stop()
						}
						maxLabelWidth := maxNameWidth
						{
							m := op.Record(gtx.Ops)
							dims := widget.Label{}.Layout(gtx, mwin.twin.Theme.Shaper, font.Font{}, mwin.twin.Theme.TextSize, fmt.Sprintf("100.00%% | (%d/%d) ", len(mwin.progressStages), len(mwin.progressStages)), widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
							maxLabelWidth += dims.Size.X
							m.Stop()
						}

						gtx.Constraints.Min = gtx.Constraints.Max
						return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							return theme.Dialog(win.Theme, "Opening trace").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
								return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
									layout.Rigid(func(gtx layout.Context) layout.Dimensions {
										name := mwin.progressStages[mwin.progressStage]
										gtx.Constraints.Min.X = gtx.Constraints.Constrain(image.Pt(maxLabelWidth, 0)).X
										gtx.Constraints.Max.X = gtx.Constraints.Min.X
										pct := fmt.Sprintf("%5.2f%%", mwin.progress*100)
										// Replace space with figure space for correct alignment
										pct = strings.ReplaceAll(pct, " ", "\u2007")
										return widget.Label{}.Layout(gtx, mwin.twin.Theme.Shaper, font.Font{}, mwin.twin.Theme.TextSize, fmt.Sprintf("%s | (%d/%d) %s", pct, mwin.progressStage+1, len(mwin.progressStages), name), widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
									}),
									layout.Rigid(func(gtx layout.Context) layout.Dimensions {
										gtx.Constraints.Min = gtx.Constraints.Constrain(image.Pt(maxLabelWidth, 15))
										gtx.Constraints.Max = gtx.Constraints.Min
										return theme.ProgressBar(mwin.twin.Theme, float32(mwin.progress)).Layout(gtx)
									}))
							})
						})

					case "main":
						for _, ev := range gtx.Events(&shortcuts) {
							switch ev := ev.(type) {
							case key.Event:
								if ev.State == key.Press && mwin.ww == nil {
									switch ev.Name {
									case "G":
										mwin.ww = theme.NewListWindow(mwin.twin.Theme)
										items := make([]theme.ListWindowItem, 0, len(mwin.canvas.timelines))
										items = append(items,
											theme.ListWindowItem{
												Item:  mwin.canvas.timelines[0].item,
												Label: mwin.canvas.timelines[0].label,
											},

											theme.ListWindowItem{
												Item:  mwin.canvas.timelines[1].item,
												Label: mwin.canvas.timelines[1].label,
											},
										)
										for _, p := range mwin.trace.Processors {
											items = append(items, theme.ListWindowItem{
												Item:         p,
												Label:        local.Sprintf("processor %d", p.ID),
												FilterLabels: mwin.trace.processorFilterLabels(p),
											})
										}
										for _, g := range mwin.trace.Goroutines {
											var label string
											if g.Function.Fn == "" {
												// At least GCSweepStart can happen on g0
												label = local.Sprintf("goroutine %d", g.ID)
											} else {
												label = local.Sprintf("goroutine %d: %s", g.ID, g.Function)
											}
											items = append(items, theme.ListWindowItem{
												Item:         g,
												Label:        label,
												FilterLabels: mwin.trace.goroutineFilterLabels(g),
											})
										}
										mwin.ww.SetItems(items)
										mwin.ww.BuildFilter = newTimelineFilter
										win.SetModal(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
											return theme.Dialog(win.Theme, "Go to timeline").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
												gtx.Constraints.Max = gtx.Constraints.Constrain(image.Pt(1000, 500))
												return mwin.ww.Layout(gtx)
											})
										})

									case "H":
										displayHighlightSpansDialog(win, &mwin.canvas.timeline.filter)
									}
								}
							}
						}

						if mainMenu.Display.UndoNavigation.Clicked() {
							win.Menu.Close()
							mwin.canvas.UndoNavigation(gtx)
						}
						if mainMenu.Display.ScrollToTop.Clicked() {
							win.Menu.Close()
							mwin.canvas.ScrollToTop(gtx)
						}
						if mainMenu.Display.ZoomToFit.Clicked() {
							win.Menu.Close()
							mwin.canvas.ZoomToFitCurrentView(gtx)
						}
						if mainMenu.Display.JumpToBeginning.Clicked() {
							win.Menu.Close()
							mwin.canvas.JumpToBeginning(gtx)
						}
						if mainMenu.Display.HighlightSpans.Clicked() {
							win.Menu.Close()
							displayHighlightSpansDialog(win, &mwin.canvas.timeline.filter)
						}
						if mainMenu.Display.ToggleCompactDisplay.Clicked() {
							win.Menu.Close()
							mwin.canvas.ToggleCompactDisplay()
						}
						if mainMenu.Display.ToggleTimelineLabels.Clicked() {
							win.Menu.Close()
							mwin.canvas.ToggleTimelineLabels()
						}
						if mainMenu.Display.ToggleStackTracks.Clicked() {
							win.Menu.Close()
							mwin.canvas.ToggleStackTracks()
						}
						if mainMenu.Analyze.OpenHeatmap.Clicked() {
							win.Menu.Close()
							mwin.openHeatmap()
						}
						if mainMenu.Debug.Memprofile.Clicked() {
							win.Menu.Close()
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
								win.ShowNotification(gtx, fmt.Sprintf("Wrote memory profile to %s", path))
							} else {
								win.ShowNotification(gtx, fmt.Sprintf("Couldn't write memory profile: %s", err))
							}
						}

						if mwin.panel != nil {
							if mwin.panel.Closed() {
								mwin.closePanel()
							} else if mwin.panel.Detached() {
								mwin.openPanelWindow(mwin.panel)
								mwin.closePanel()
							}
						}

						key.InputOp{Tag: &shortcuts, Keys: "G|H"}.Add(gtx.Ops)

						if mwin.ww != nil {
							if item, ok := mwin.ww.Confirmed(); ok {
								mwin.canvas.scrollToTimeline(gtx, item)
								mwin.ww = nil
								win.CloseModal()
							} else if mwin.ww.Cancelled() {
								mwin.ww = nil
								win.CloseModal()
							}
						}

						mwin.debugWindow.cvStart.addValue(gtx.Now, float64(mwin.canvas.start))
						mwin.debugWindow.cvEnd.addValue(gtx.Now, float64(mwin.canvas.End()))
						mwin.debugWindow.cvY.addValue(gtx.Now, float64(mwin.canvas.y))

						var dims layout.Dimensions
						if mwin.panel == nil {
							dims = mwin.canvas.Layout(win, gtx)
						} else {
							dims = theme.Resize(win.Theme, &resize).Layout(win, gtx, mwin.canvas.Layout, mwin.panel.Layout)
						}

						for _, g := range mwin.canvas.clickedGoroutineTimelines {
							mwin.openGoroutine(g)
						}
						for _, clicked := range mwin.canvas.clickedSpans {
							mwin.openSpan(clicked.Spans, clicked.Timeline, clicked.Track, clicked.AllEvents)
						}

						return dims

					default:
						return layout.Dimensions{}
					}
				})

				if invalidateFrames {
					op.InvalidateOp{}.Add(&ops)
				}
				ev.Frame(&ops)

				mwin.twin.Futures.Sweep()
			}
		}
	}
}

func (mwin *MainWindow) showFileOpenDialog() {
	if mwin.showingExplorer.CompareAndSwap(false, true) {
		go func() {
			rc, err := mwin.explorer.ChooseFile()
			mwin.showingExplorer.Store(false)
			if err != nil {
				switch err {
				case explorer.ErrUserDecline:
					return
				case explorer.ErrNotAvailable:
					//lint:ignore ST1005 This error is only used for display in the UI. It probably shouldn't be of type error though.
					err = errors.New("Opening file system dialogs isn't supported on this system. Please pass the trace file as an argument to gotraceui instead.")
				}
				mwin.commands <- func(mwin *MainWindow, gtx layout.Context) {
					mwin.SetError(err)
				}
				return
			}
			defer rc.Close()
			mwin.OpenTrace(rc)
		}()
	}
}

func (mwin *MainWindow) loadTraceImpl(res loadTraceResult) {
	NewCanvasInto(&mwin.canvas, mwin.debugWindow, res.trace)
	mwin.canvas.start = res.start
	mwin.canvas.memoryGraph = res.plot
	mwin.canvas.timelines = append(mwin.canvas.timelines, res.timelines...)
	mwin.trace = res.trace
	mwin.panel = nil
	mwin.panelHistory = nil
	mwin.ww = nil
}

type durationNumberFormat uint8

const (
	durationNumberFormatSITable durationNumberFormat = iota
	durationNumberFormatSI
	durationNumberFormatScientific
	durationNumberFormatExact
)

func roundDuration(d time.Duration) time.Duration {
	switch {
	case d < time.Millisecond:
		return d
	case d < time.Second:
		return d.Round(time.Microsecond)
	default:
		return d.Round(time.Millisecond)
	}
}

// fmtFrac formats the fraction of v/10**prec (e.g., ".12345") into the
// tail of buf, omitting trailing zeros. It omits the decimal
// point too when the fraction is 0. It returns the index where the
// output bytes begin and the value v/10**prec.
func fmtFrac(buf []byte, v uint64, prec int, numSig int) (nw int, nv uint64) {
	// Omit trailing zeros up to and including decimal point.
	w := len(buf)
	print := false
	for i := 0; i < prec; i++ {
		digit := v % 10
		print = prec-i-1 < numSig
		if print {
			w--
			buf[w] = byte(digit) + '0'
		}
		v /= 10
	}
	if print {
		w--
		buf[w] = '.'
	}
	return w, v
}

// fmtInt formats v into the tail of buf.
// It returns the index where the output begins.
func fmtInt(buf []byte, v uint64) int {
	w := len(buf)
	if v == 0 {
		w--
		buf[w] = '0'
	} else {
		for v > 0 {
			w--
			buf[w] = byte(v%10) + '0'
			v /= 10
		}
	}
	return w
}

func (nf durationNumberFormat) format(d time.Duration) (value string, unit string) {
	switch nf {
	case durationNumberFormatScientific:
		return scientificDuration(d, 2), ""
	case durationNumberFormatSI:
		s := roundDuration(d).String()
		idx := strings.IndexFunc(s, unicode.IsLetter)
		return s[:idx], s[idx:]
	case durationNumberFormatSITable:
		// Largest time is 9145440610.000  s
		var buf [24]byte
		w := len(buf)

		u := uint64(roundDuration(d))

		// Special case: if duration is smaller than a second,
		// use smaller units, like 1.2ms
		var prec int
		w--
		switch {
		case u == 0:
			return "0.000", " s"
		case u < uint64(time.Microsecond):
			// Format nanoseconds as microseconds so that rows of numbers look neater, without a lot of whitespace to
			// line up the decimal separator.
			fallthrough
		case u < uint64(time.Millisecond):
			// print microseconds
			prec = 3
			unit = "µs"
		case u < uint64(time.Second):
			// print milliseconds
			prec = 6
			unit = "ms"
		default:
			// print seconds
			prec = 9
			unit = " s"
		}
		w, u = fmtFrac(buf[:w], u, prec, 3)
		w = fmtInt(buf[:w], u)

		return string(buf[w:]), unit
	case durationNumberFormatExact:
		return fmt.Sprintf("%.9f", d.Seconds()), ""
	default:
		panic("unreachable")
	}
}

func scientificDuration(d time.Duration, digits int) string {
	// TODO(dh): don't convert to float to use %e, implement our own algorithm
	return fmt.Sprintf("%.*e", digits, d.Seconds())
}

type Window interface {
	Run(win *app.Window) error
}

type Link interface{ isLink() }

type aLink struct{}

func (aLink) isLink() {}

type TimestampLink struct {
	aLink
	Ts trace.Timestamp
}

type GoroutineLinkKind uint8

const (
	GoroutineLinkKindOpen GoroutineLinkKind = iota
	GoroutineLinkKindScroll
	GoroutineLinkKindZoom
)

type GoroutineLink struct {
	aLink

	Goroutine *ptrace.Goroutine
	Kind      GoroutineLinkKind
}

type ProcessorLinkKind uint8

const (
	ProcessorLinkKindScroll ProcessorLinkKind = iota
	ProcessorLinkKindZoom
)

type ProcessorLink struct {
	aLink

	Processor *ptrace.Processor
	Kind      ProcessorLinkKind
}

func span(th *theme.Theme, text string) styledtext.SpanStyle {
	return styledtext.SpanStyle{
		Content: text,
		Size:    th.TextSize,
		Color:   th.Palette.Foreground,
		Font:    ourfont.Collection()[0].Font,
	}
}

func spanWith(th *theme.Theme, text string, fn func(styledtext.SpanStyle) styledtext.SpanStyle) styledtext.SpanStyle {
	return fn(span(th, text))
}

var local = message.NewPrinter(message.MatchLanguage("en"))

func formatTimestamp(ts trace.Timestamp) string {
	return local.Sprintf("%d ns", ts)
}

func openTraceFromCmdline(mwin *MainWindow) {
	f, err := os.Open(flag.Args()[0])
	if err != nil {
		mwin.SetError(fmt.Errorf("couldn't load trace: %w", err))
		return
	}
	// Set state explicitly so user doesn't see a flash of the start state.
	mwin.SetState("loadingTrace")
	go func() {
		defer f.Close()
		mwin.OpenTrace(f)
	}()
}

func usage(name string, fs *flag.FlagSet) func() {
	return func() {
		fmt.Fprintf(os.Stderr, "Usage: %s [flags] [trace file]\n", name)

		fmt.Fprintln(os.Stderr)
		fmt.Fprintln(os.Stderr, "Flags:")
		printDefaults(fs)
	}
}

// this function has been copied from the Go standard library's 'flag' package and modified to skip debug flags.
func printDefaults(fs *flag.FlagSet) {
	fs.VisitAll(func(f *flag.Flag) {
		// Don't print debug flags
		if strings.HasPrefix(f.Name, "debug.") {
			return
		}

		var b strings.Builder
		fmt.Fprintf(&b, "  -%s", f.Name) // Two spaces before -; see next two comments.
		name, usage := flag.UnquoteUsage(f)
		if len(name) > 0 {
			b.WriteString(" ")
			b.WriteString(name)
		}
		// Boolean flags of one ASCII letter are so common we
		// treat them specially, putting their usage on the same line.
		if b.Len() <= 4 { // space, space, '-', 'x'.
			b.WriteString("\t")
		} else {
			// Four spaces before the tab triggers good alignment
			// for both 4- and 8-space tab stops.
			b.WriteString("\n    \t")
		}
		b.WriteString(strings.ReplaceAll(usage, "\n", "\n    \t"))

		if !isZeroValue(f, f.DefValue) {
			if T := reflect.TypeOf(f.Value); T.Name() == "*stringValue" && T.PkgPath() == "flag" {
				// put quotes on the value
				fmt.Fprintf(&b, " (default %q)", f.DefValue)
			} else {
				fmt.Fprintf(&b, " (default %v)", f.DefValue)
			}
		}
		fmt.Fprint(fs.Output(), b.String(), "\n")
	})
}

// isZeroValue determines whether the string represents the zero
// value for a flag.
//
// this function has been copied from the Go standard library's 'flag' package.
func isZeroValue(f *flag.Flag, value string) bool {
	// Build a zero value of the flag's Value type, and see if the
	// result of calling its String method equals the value passed in.
	// This works unless the Value type is itself an interface type.
	typ := reflect.TypeOf(f.Value)
	var z reflect.Value
	if typ.Kind() == reflect.Ptr {
		z = reflect.New(typ.Elem())
	} else {
		z = reflect.Zero(typ)
	}
	return value == z.Interface().(flag.Value).String()
}

func main() {
	flag.Usage = usage("gotraceui", flag.CommandLine)
	flag.StringVar(&cpuprofile, "debug.cpuprofile", "", "write CPU profile to this file")
	flag.StringVar(&memprofileLoad, "debug.memprofile-load", "", "write memory profile to this file after loading trace")
	flag.StringVar(&memprofileExit, "debug.memprofile-exit", "", "write meory profile to this file when exiting")
	flag.StringVar(&traceFile, "debug.trace", "", "write runtime trace to this file")
	flag.BoolVar(&disableCaching, "debug.disable-caching", false, "Disable caching")
	flag.BoolVar(&exitAfterLoading, "debug.exit-after-loading", false, "Exit after parsing and processing trace")
	flag.BoolVar(&exitAfterParsing, "debug.exit-after-parsing", false, "Exit after parsing trace")
	flag.BoolVar(&measureFrameAllocs, "debug.measure-frame-allocs", false, "Measure the number of allocations per frame")
	flag.BoolVar(&invalidateFrames, "debug.invalidate-frames", false, "Invalidate frame after drawing it")
	fv := flag.Bool("version", false, "Print version and exit")
	fdv := flag.Bool("debug.version", false, "Print extended version information and exit")
	flag.Parse()

	if *fv {
		PrintVersion(Version)
		return
	} else if *fdv {
		PrintVerboseVersion(Version)
		return
	}

	go func() {
		if cpuprofile != "" {
			f, err := os.Create(cpuprofile)
			if err == nil {
				pprof.StartCPUProfile(f)
			} else {
				fmt.Fprintln(os.Stderr, "couldn't write CPU profile:", err)
			}
		}
		if traceFile != "" {
			f, err := os.Create(traceFile)
			if err == nil {
				rtrace.Start(f)
			} else {
				fmt.Fprintln(os.Stderr, "couldn't write trace:", err)
			}
		}
	}()

	mwin := NewMainWindow()
	if debug {
		go func() {
			win := app.NewWindow(app.Title("gotraceui - debug window"))
			mwin.debugWindow.Run(win)
		}()
	}

	mwin.SetState("start")

	if len(flag.Args()) > 0 {
		openTraceFromCmdline(mwin)
	}

	go func() {
		win := app.NewWindow(app.Title("gotraceui"))
		mwin.errs <- mwin.Run(win)
	}()

	go func() {
		err := <-mwin.errs
		if err != nil {
			log.Println(err)
		}

		if cpuprofile != "" {
			pprof.StopCPUProfile()
		}
		if traceFile != "" {
			rtrace.Stop()
		}
		if memprofileExit != "" {
			writeMemprofile(memprofileExit)
		}
		os.Exit(0)
	}()
	app.Main()
}

type loadTraceResult struct {
	trace      *Trace
	plot       Plot
	start, end trace.Timestamp
	timelines  []*Timeline
}

func loadTrace(f io.Reader, mwin *MainWindow) (loadTraceResult, error) {
	names := []string{
		"Parsing trace",
		"Parsing trace",
		"Processing",
		"Processing",
		"Processing",
		"Processing",
		"Processing",
		"Processing",
	}

	mwin.SetProgressStages(names)

	mwin.SetProgressStage(0)
	t, err := trace.Parse(f, mwin.SetProgressLossy)
	if err != nil {
		return loadTraceResult{}, err
	}
	if exitAfterParsing {
		return loadTraceResult{}, errExitAfterParsing
	}

	mwin.SetProgressStage(1)
	pt, err := ptrace.Parse(t, mwin.SetProgressLossy)
	if err != nil {
		return loadTraceResult{}, err
	}

	mwin.SetProgressStage(2)
	// Assign GC tag to all GC spans so we can later determine their span colors cheaply.
	for i, proc := range pt.Processors {
		for j := 0; j < proc.Spans.Len(); j++ {
			fn := pt.G(pt.Events[proc.Spans.At(j).Event].G).Function
			if fn == nil {
				continue
			}
			switch fn.Fn {
			case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
				proc.Spans.AtPtr(j).Tags |= ptrace.SpanTagGC
			}
		}
		mwin.SetProgressLossy(float64(i+1) / float64(len(pt.Processors)))
	}

	mwin.SetProgressStage(3)
	tr := &Trace{Trace: pt}
	if len(pt.Goroutines) != 0 {
		tr.allGoroutineSpanLabels = make([][]string, len(pt.Goroutines))
		tr.allGoroutineFilterLabels = make([][]string, len(pt.Goroutines))

		for seqID, g := range pt.Goroutines {
			// Populate goroutine span labels
			localPrefixedID := local.Sprintf("g%d", g.ID)
			localUnprefixedID := localPrefixedID[1:]
			fmtPrefixedID := fmt.Sprintf("g%d", g.ID)
			fmtUnprefixedID := fmtPrefixedID[1:]

			var spanLabels []string
			if g.Function.Fn != "" {
				short := shortenFunctionName(g.Function.Fn)
				spanLabels = append(spanLabels, localPrefixedID+": "+g.Function.Fn)
				if short != g.Function.Fn {
					spanLabels = append(spanLabels, localPrefixedID+": ."+short)
					spanLabels = append(spanLabels, localPrefixedID)
				} else {
					// This branch is probably impossible; all functions should be fully qualified.
					spanLabels = append(spanLabels, localPrefixedID)
				}
			} else {
				spanLabels = append(spanLabels, localPrefixedID)
			}
			tr.allGoroutineSpanLabels[seqID] = spanLabels

			// Populate goroutine filter filterLabels
			filterLabels := []string{
				fmtUnprefixedID,
				localUnprefixedID,
				fmtPrefixedID,
				localPrefixedID,
				g.Function.Fn,
				strings.ToLower(g.Function.Fn),
				"goroutine", // allow queries like "goroutine 1234" to work
			}
			tr.allGoroutineFilterLabels[g.SeqID] = filterLabels
			mwin.SetProgressLossy(float64(seqID+1) / float64(len(pt.Goroutines)))
		}
	}

	mwin.SetProgressStage(4)
	if len(pt.Processors) != 0 {
		tr.allProcessorSpanLabels = make([][]string, len(pt.Processors))
		tr.allProcessorFilterLabels = make([][]string, len(pt.Processors))

		for seqID, proc := range pt.Processors {
			localPrefixedID := local.Sprintf("p%d", proc.ID)
			localUnprefixedID := localPrefixedID[1:]
			fmtPrefixedID := fmt.Sprintf("p%d", proc.ID)
			fmtUnprefixedID := fmtPrefixedID[1:]

			tr.allProcessorSpanLabels[seqID] = append(tr.allProcessorSpanLabels[seqID], localPrefixedID)

			filterLabels := []string{
				fmtUnprefixedID,
				localUnprefixedID,
				fmtPrefixedID,
				localPrefixedID,
				"processor", // allow queries like "processor 1234" to work
			}
			tr.allProcessorFilterLabels[proc.SeqID] = filterLabels
			mwin.SetProgressLossy(float64(seqID+1) / float64(len(pt.Processors)))
		}
	}

	// TODO(dh): preallocate
	var timelines []*Timeline

	mwin.SetProgressStage(5)
	if supportMachineTimelines {
		for i, m := range tr.Machines {
			timelines = append(timelines, NewMachineTimeline(tr, &mwin.canvas, m))
			mwin.SetProgressLossy(float64(i+1) / float64(len(tr.Machines)))
		}
	}

	mwin.SetProgressStage(6)
	for i, proc := range tr.Processors {
		timelines = append(timelines, NewProcessorTimeline(tr, &mwin.canvas, proc))
		mwin.SetProgressLossy(float64(i+1) / float64(len(tr.Processors)))
	}

	mwin.SetProgressStage(7)
	for i, g := range tr.Goroutines {
		timelines = append(timelines, NewGoroutineTimeline(tr, &mwin.canvas, g))
		mwin.SetProgressLossy(float64(i+1) / float64(len(tr.Goroutines)))
	}

	// We no longer need this.
	tr.CPUSamples = nil

	end := tr.Events[len(tr.Events)-1].Ts

	// Zoom out slightly beyond the end of the trace, so that the user can immediately tell that they're looking at the
	// entire trace.
	slack := float64(end) * 0.05
	start := trace.Timestamp(-slack)
	end = trace.Timestamp(float64(end) + slack)

	mg := Plot{
		Name: "Memory usage",
		Unit: "bytes",
	}
	mg.AddSeries(
		PlotSeries{
			Name:   "Heap size",
			Points: pt.HeapSize,
			Filled: true,
			Color:  rgba(0x7EB072FF),
		},
		PlotSeries{
			Name:   "Heap goal",
			Points: pt.HeapGoal,
			Filled: false,
			Color:  colors[colorStateBlockedGC],
		},
	)

	var goroot, gopath string
	for _, fn := range tr.Functions {
		if strings.HasPrefix(fn.Fn, "runtime.") && strings.Count(fn.Fn, ".") == 1 && strings.Contains(fn.File, filepath.Join("go", "src", "runtime")) && !strings.ContainsRune(fn.Fn, os.PathSeparator) {
			idx := strings.LastIndex(fn.File, filepath.Join("go", "src", "runtime"))
			goroot = fn.File[0 : idx+len("go")]
			break
		}
	}

	// goroot will be empty for executables with trimmed paths. In that case we cannot detect GOPATH, either.
	if goroot != "" {
		// We detect GOROOT and GOPATH separately because we make use of GOROOT to reliably detect GOPATH.
		candidates := map[string]int{}
		for _, fn := range tr.Functions {
			if !strings.HasPrefix(fn.File, goroot) && strings.ContainsRune(fn.Fn, os.PathSeparator) {
				// TODO(dh): support Windows paths
				dir, pkgAndFn, _ := strings.Cut(fn.Fn, string(os.PathSeparator))
				pkg, _, _ := strings.Cut(pkgAndFn, ".")
				idx := strings.LastIndex(fn.File, filepath.Join("src", dir, pkg))
				if idx == -1 {
					idx = strings.LastIndex(fn.File, filepath.Join("pkg", "mod", dir))
					if idx == -1 {
						continue
					}
				}
				p := fn.File[:idx]
				candidates[p]++
			}
		}

		var max int
		for c, n := range candidates {
			if n > max {
				gopath = c
				max = n
			}
		}
	}

	tr.GOROOT = goroot
	tr.GOPATH = gopath

	return loadTraceResult{
		trace:     tr,
		plot:      mg,
		start:     start,
		end:       end,
		timelines: timelines,
	}, nil
}

const allocatorBucketSize = 64

type allocator[T any] struct {
	n       int
	buckets [][]T
}

func (l *allocator[T]) Allocate(v T) *T {
	a, _ := l.index(l.n)
	if a >= len(l.buckets) {
		l.buckets = append(l.buckets, make([]T, 0, allocatorBucketSize))
	}
	l.buckets[a] = append(l.buckets[a], v)
	ptr := &l.buckets[a][len(l.buckets[a])-1]
	l.n++
	return ptr
}

func (l *allocator[T]) index(i int) (int, int) {
	return i / allocatorBucketSize, i % allocatorBucketSize
}

func (l *allocator[T]) Ptr(i int) *T {
	a, b := l.index(i)
	return &l.buckets[a][b]
}

func (l *allocator[T]) Get(i int) T {
	a, b := l.index(i)
	return l.buckets[a][b]
}

func (l *allocator[T]) Set(i int, v T) {
	a, b := l.index(i)
	l.buckets[a][b] = v
}

func (l *allocator[T]) Len() int {
	return l.n
}

func (l *allocator[T]) Reset() {
	for i := range l.buckets {
		l.buckets[i] = l.buckets[i][:0]
	}
	l.n = 0
}

func (l *allocator[T]) Truncate(n int) {
	if n >= l.n {
		return
	}
	a, b := l.index(n)
	l.buckets[a] = l.buckets[a][:b]
	for i := a + 1; i < len(l.buckets); i++ {
		l.buckets[i] = l.buckets[i][:0]
	}
	l.n = n
}

type Text struct {
	// The theme must only be used for building the Text, with methods like Span. The Layout function has to use the
	// theme provided to it, to avoid race conditions when texts transition from widgets to independent windows.
	//
	// The theme must be reset in Reset.
	styles    []styledtext.SpanStyle
	Alignment text.Alignment
	TextBuilder

	events []TextEvent

	// Clickables we use for spans and reuse between frames. We allocate them one by one because it really doesn't
	// matter; we have hundreds of these at most. This won't make the GC sweat, and it avoids us having to do a bunch of
	// semi-manual memory management.
	clickables []*gesture.Click
}

type TextBuilder struct {
	Theme *theme.Theme
	Spans []TextSpan
}

type TextEvent struct {
	Span  *TextSpan
	Event gesture.ClickEvent
}

type TextSpan struct {
	styledtext.SpanStyle
	Object any

	Click *gesture.Click
}

func (txt *TextBuilder) Span(label string) *TextSpan {
	style := styledtext.SpanStyle{
		Content: label,
		Size:    txt.Theme.TextSize,
		Color:   txt.Theme.Palette.Foreground,
		Font:    ourfont.Collection()[0].Font,
	}
	s := TextSpan{
		SpanStyle: style,
	}
	txt.Spans = append(txt.Spans, s)
	return &txt.Spans[len(txt.Spans)-1]
}

func (txt *TextBuilder) SpanWith(label string, fn func(s *TextSpan)) *TextSpan {
	s := txt.Span(label)
	fn(s)
	return s
}

func (txt *TextBuilder) Bold(label string) *TextSpan {
	s := txt.Span(label)
	s.Font.Weight = font.Bold
	return s
}

func (txt *TextBuilder) Link(label string, obj any) *TextSpan {
	s := txt.Span(label)
	s.Color = txt.Theme.Palette.Link
	s.Object = obj
	return s
}

func (txt *Text) Reset(th *theme.Theme) {
	txt.Theme = th
	txt.events = txt.events[:0]
	txt.styles = txt.styles[:0]
	txt.Spans = txt.Spans[:0]
	txt.Alignment = 0
}

func (txt *Text) Events() []TextEvent {
	return txt.events
}

func (txt *Text) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Text.Layout").End()

	var clickableIdx int
	for i := range txt.Spans {
		s := &txt.Spans[i]
		if s.Object != nil {
			var clk *gesture.Click
			if clickableIdx < len(txt.clickables) {
				clk = txt.clickables[clickableIdx]
				clickableIdx++
			} else {
				clk = &gesture.Click{}
				txt.clickables = append(txt.clickables, clk)
				clickableIdx++
			}
			s.Click = clk
		}
	}

	txt.events = txt.events[:0]
	for i := range txt.Spans {
		s := &txt.Spans[i]
		if s.Click != nil {
			for _, ev := range s.Click.Events(gtx.Queue) {
				txt.events = append(txt.events, TextEvent{s, ev})
			}
		}
	}

	txt.styles = txt.styles[:0]
	for _, s := range txt.Spans {
		txt.styles = append(txt.styles, s.SpanStyle)
	}
	ptxt := styledtext.Text(win.Theme.Shaper, txt.styles...)
	ptxt.Alignment = txt.Alignment
	if txt.Alignment == text.Start {
		gtx.Constraints.Max.X = 1e6
	}
	return ptxt.Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
		defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
		s := &txt.Spans[i]
		if s.Object != nil {
			s.Click.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
		}
	})
}

func defaultLink(obj any) Link {
	switch obj := obj.(type) {
	case *ptrace.Processor:
		// There are no processor panels yet, so we default to scrolling
		return &ProcessorLink{Processor: obj, Kind: ProcessorLinkKindScroll}
	case *trace.Timestamp:
		return &TimestampLink{Ts: *obj}
	case trace.Timestamp:
		return &TimestampLink{Ts: obj}
	case *ptrace.Function:
		return &FunctionLink{Fn: obj}
	default:
		panic(fmt.Sprintf("unsupported type: %T", obj))
	}
}

func handleLinkClick(win *theme.Window, mwin *MainWindow, ev TextEvent) {
	if ev.Event.Type == gesture.TypeClick && ev.Event.Button == pointer.ButtonPrimary {
		if obj, ok := ev.Span.Object.(*ptrace.Goroutine); ok {
			switch ev.Event.Modifiers {
			case 0:
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindScroll})
			case key.ModShortcut:
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindZoom})
			case key.ModShift:
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindOpen})
			}
		} else {
			mwin.OpenLink(defaultLink(ev.Span.Object))
		}
	} else if ev.Event.Type == gesture.TypePress && ev.Event.Button == pointer.ButtonSecondary {
		switch obj := ev.Span.Object.(type) {
		case *ptrace.Goroutine:
			win.SetContextMenu(goroutineLinkContextMenu(mwin, obj))
		case *ptrace.Processor:
			win.SetContextMenu(processorLinkContextMenu(mwin, obj))
		}
	}
}

type Recording struct {
	Call       op.CallOp
	Dimensions layout.Dimensions
}

func (r Recording) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Recording.Layout").End()

	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	r.Call.Add(gtx.Ops)
	return r.Dimensions
}

func Record(win *theme.Window, gtx layout.Context, w theme.Widget) Recording {
	m := op.Record(gtx.Ops)
	dims := w(win, gtx)
	c := m.Stop()

	return Recording{c, dims}
}

type Description struct {
	Attributes []DescriptionAttribute
	text       Text
}

type DescriptionAttribute struct {
	Key   string
	Value *theme.Future[TextSpan]
}

func (desc *Description) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	desc.text.Reset(win.Theme)
	for _, attr := range desc.Attributes {
		desc.text.Bold(fmt.Sprintf("%s: ", attr.Key))
		if s, ok := attr.Value.Result(); ok {
			desc.text.Spans = append(desc.text.Spans, s)
		} else {
			switch (gtx.Now.UnixMilli() / 500) % 3 {
			case 0:
				desc.text.Span(".")
			case 1:
				desc.text.Span("..")
			case 2:
				desc.text.Span("...")
			}
			op.InvalidateOp{}.Add(gtx.Ops)
		}
		desc.text.Span("\n")
	}

	return desc.text.Layout(win, gtx)
}

func (desc *Description) Events() []TextEvent {
	return desc.text.Events()
}

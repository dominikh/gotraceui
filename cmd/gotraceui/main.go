package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"image"
	"io"
	"log"
	"math"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"runtime/pprof"
	rtrace "runtime/trace"
	"strconv"
	"strings"
	"sync/atomic"
	"time"
	"unicode"

	"honnef.co/go/gotraceui/cmd/gotraceui/assets"
	mycolor "honnef.co/go/gotraceui/color"
	ourfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/mysync"
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
	"golang.org/x/exp/slices"
	"golang.org/x/text/message"
)

var (
	uint64SliceCache        = mem.NewConcurrentSliceCache[uint64, []uint64]()
	boolSliceCache          = mem.NewConcurrentSliceCache[bool, []bool]()
	spanSliceCache          = mem.NewConcurrentSliceCache[ptrace.Span, []ptrace.Span]()
	stackSpanMetaSliceCache = mem.NewConcurrentSliceCache[stackSpanMeta, []stackSpanMeta]()
)
var colorPanel = mycolor.Oklch{L: 0.7862, C: 0.104, H: 140, Alpha: 1}

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
	softDebug          bool
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

func (mwin *MainWindow) openGoroutine(g *ptrace.Goroutine) {
	gi := NewGoroutineInfo(mwin.trace, mwin.twin, &mwin.canvas, g, mwin.canvas.timelines)
	mwin.openPanel(gi)
}

func (mwin *MainWindow) openFunction(fn *ptrace.Function) {
	fi := NewFunctionInfo(mwin.trace, mwin.twin, fn)
	mwin.openPanel(fi)
}

func (mwin *MainWindow) openSpan(s Items[ptrace.Span]) {
	var labels []string
	var label string

	if c, ok := s.Container(); ok && c.Track.spanLabel != nil {
		labels = c.Track.spanLabel(s, c.Timeline.cv.trace, nil)
	}
	if len(labels) > 0 {
		label = labels[0]
	}

	cfg := SpansInfoConfig{
		Label: label,
	}
	si := NewSpansInfo(cfg, mwin.trace, mwin.twin, theme.Immediate[Items[ptrace.Span]](s), mwin.canvas.timelines)
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

func (mwin *MainWindow) ClosePanel() {
	mwin.panel = nil
}

type PanelWindow struct {
	MainWindow *theme.Window
	Panel      theme.Panel
}

func (pwin *PanelWindow) Run(win *app.Window) error {
	var ops op.Ops
	tWin := theme.NewWindow(win)

	var dead bool
	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			if dead {
				// Don't render if we're waiting for the DestroyEvent because the panel was attached to the main window.
				continue
			}
			tWin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				for _, l := range win.Actions() {
					switch l := l.(type) {
					case MainWindowAction:
						pwin.MainWindow.EmitAction(l)
					case theme.ExecuteAction:
						l(gtx)
					default:
						panic(fmt.Sprintf("%T", l))
					}
				}

				paint.Fill(gtx.Ops, tWin.Theme.Palette.Background)
				return pwin.Panel.Layout(win, gtx)
			})

			if pwin.Panel.Closed() {
				win.Perform(system.ActionClose)
			} else if pwin.Panel.Attached() {
				pwin.MainWindow.EmitAction(&OpenPanelAction{pwin.Panel})
				win.Perform(system.ActionClose)
				dead = true
			}

			ev.Frame(&ops)
		}
	}

	return nil
}

func (mwin *MainWindow) openPanelWindow(p theme.Panel) {
	win := &PanelWindow{MainWindow: mwin.twin, Panel: p}
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

func (mwin *MainWindow) openFlameGraph() {
	win := &FlameGraphWindow{}
	go func() {
		// XXX handle error?
		win.Run(app.NewWindow(app.Title("gotraceui - flame graph")), mwin.trace.Trace)
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
	explorer        *explorer.Explorer
	showingExplorer atomic.Bool

	cpuProfile *os.File

	// Channel used by goroutines to report critical errors.
	errs chan error

	panel        theme.Panel
	panelHistory []theme.Panel

	pointerAt f32.Point

	win  *app.Window
	twin *theme.Window
	// TODO(dh): use enum for state
	state          string
	progress       atomic.Uint64
	progressStage  int
	progressStages []string
	err            error

	debugWindow *DebugWindow
}

func NewMainWindow() *MainWindow {
	mwin := &MainWindow{
		debugWindow: NewDebugWindow(),
		errs:        make(chan error),
	}

	return mwin
}

// OpenTrace initiates loading of a trace. It changes the state to loadingTrace, loads the trace, and notifies the
// window when it's done. OpenTrace should be called from a different goroutine than the render loop.
func (mwin *MainWindow) OpenTrace(r io.Reader) {
	mwin.SetState("loadingTrace")
	res, err := loadTrace(r, mwin, &mwin.canvas)
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

	mwin.LoadTrace(res)
}

func (mwin *MainWindow) setState(state string) {
	mwin.state = state
	mwin.progress.Store(0)
}

func (mwin *MainWindow) SetState(state string) {
	mwin.twin.EmitAction(theme.ExecuteAction(func(gtx layout.Context) {
		mwin.twin.CloseModal()
		mwin.setState(state)
	}))
}

func (mwin *MainWindow) SetError(err error) {
	mwin.twin.EmitAction(theme.ExecuteAction(func(gtx layout.Context) {
		mwin.err = err
		mwin.setState("error")
	}))
}

func (mwin *MainWindow) SetProgress(p float64) {
	mwin.progress.Store(math.Float64bits(p))
}

func (mwin *MainWindow) SetProgressStages(names []string) {
	mwin.twin.EmitAction(theme.ExecuteAction(func(gtx layout.Context) {
		mwin.progressStages = names
	}))
}

func (mwin *MainWindow) SetProgressStage(idx int) {
	mwin.twin.EmitAction(theme.ExecuteAction(func(gtx layout.Context) {
		mwin.progressStage = idx
		mwin.progress.Store(0)
	}))
}

func (mwin *MainWindow) LoadTrace(res loadTraceResult) {
	mwin.twin.EmitAction(theme.ExecuteAction(func(gtx layout.Context) {
		mwin.loadTraceImpl(res)
		mwin.setState("main")
	}))
}

func (mwin *MainWindow) openLink(gtx layout.Context, l theme.Action) {
	switch l := l.(type) {
	case MainWindowAction:
		l.Open(gtx, mwin)
	case theme.ExecuteAction:
		l(gtx)
	default:
		panic(fmt.Sprintf("unsupported link type %T", l))
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

func (mwin *MainWindow) Run() error {
	win := mwin.win
	profileTag := new(int)
	var ops op.Ops

	var commands []Command

	resize := component.Resize{
		Axis:  layout.Horizontal,
		Ratio: 0.70,
	}

	var prevTotalAlloc uint64
	var prevMallocs uint64
	var mem runtime.MemStats
	var frameCounter uint64
	var openTraceButton widget.PrimaryClickable

	for e := range win.Events() {
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
				for _, l := range win.Actions() {
					mwin.openLink(gtx, l)
				}
				commands = commands[:0]

				for _, ev := range gtx.Events(&mwin.pointerAt) {
					mwin.pointerAt = ev.(pointer.Event).Position
				}
				pointer.InputOp{Tag: &mwin.pointerAt, Types: pointer.Move | pointer.Drag | pointer.Enter}.Add(gtx.Ops)

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

					// OPT(dh): only compute this once
					var maxNameWidth int
					for _, name := range mwin.progressStages {
						width := win.TextLength(gtx, widget.Label{}, font.Font{}, win.Theme.TextSize, name)
						if width > maxNameWidth {
							maxNameWidth = width
						}
					}
					maxLabelWidth := maxNameWidth
					{
						width := win.TextLength(gtx, widget.Label{}, font.Font{}, win.Theme.TextSize, fmt.Sprintf("100.00%% | (%d/%d) ", len(mwin.progressStages), len(mwin.progressStages)))
						maxLabelWidth += width
					}

					gtx.Constraints.Min = gtx.Constraints.Max
					return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return theme.Dialog(win.Theme, "Opening trace").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
							progress := math.Float64frombits(mwin.progress.Load())
							return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									var name string
									if mwin.progressStage < len(mwin.progressStages) {
										name = mwin.progressStages[mwin.progressStage]
									} else {
										name = "Unknown"
									}
									gtx.Constraints.Min.X = gtx.Constraints.Constrain(image.Pt(maxLabelWidth, 0)).X
									gtx.Constraints.Max.X = gtx.Constraints.Min.X
									pct := fmt.Sprintf("%5.2f%%", progress*100)
									// Replace space with figure space for correct alignment
									pct = strings.ReplaceAll(pct, " ", "\u2007")
									return widget.Label{}.Layout(gtx, mwin.twin.Theme.Shaper, font.Font{}, mwin.twin.Theme.TextSize, fmt.Sprintf("%s | (%d/%d) %s", pct, mwin.progressStage+1, len(mwin.progressStages), name), widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
								}),
								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									gtx.Constraints.Min = gtx.Constraints.Constrain(image.Pt(maxLabelWidth, 15))
									gtx.Constraints.Max = gtx.Constraints.Min
									return theme.ProgressBar(mwin.twin.Theme, float32(progress)).Layout(gtx)
								}))
						})
					})

				case "main":
					win.AddShortcut(theme.Shortcut{Name: "G"})
					win.AddShortcut(theme.Shortcut{Name: "H"})
					win.AddShortcut(theme.Shortcut{Modifiers: key.ModShortcut, Name: "Space"})

					for _, s := range win.PressedShortcuts() {
						switch s {
						case theme.Shortcut{Name: "G"}:
							pl := &theme.CommandPalette{Prompt: "Scroll to timeline"}
							pl.Set(GotoTimelineCommandProvider{mwin.twin, mwin.canvas.timelines})
							win.SetModal(pl.Layout)

						case theme.Shortcut{Name: "H"}:
							displayHighlightSpansDialog(win, &mwin.canvas.timeline.filter)

						case theme.Shortcut{Modifiers: key.ModShortcut, Name: "Space"}:
							cmd := &theme.CommandPalette{}
							cmd.Set(theme.MultiCommandProvider{win.CommandProviders()})
							win.SetModal(cmd.Layout)
						}
					}

					if mwin.panel != nil {
						if mwin.panel.Closed() {
							mwin.ClosePanel()
						} else if mwin.panel.Detached() {
							mwin.openPanelWindow(mwin.panel)
							mwin.ClosePanel()
						}
					}

					mwin.debugWindow.cvStart.addValue(gtx.Now, float64(mwin.canvas.start))
					mwin.debugWindow.cvEnd.addValue(gtx.Now, float64(mwin.canvas.End()))
					mwin.debugWindow.cvY.addValue(gtx.Now, float64(mwin.canvas.y))

					win.AddCommandProvider(mwin.defaultCommands())
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
						mwin.openSpan(clicked)
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
		}
	}

	return nil
}

func (mwin *MainWindow) defaultCommands() theme.CommandProvider {
	var (
		colorDisplay    = mycolor.Oklch{L: 0.7862, C: 0.104, H: 219.74, Alpha: 1}
		colorAnalysis   = mycolor.Oklch{L: 0.7862, C: 0.104, H: 82.85, Alpha: 1}
		colorNavigation = mycolor.Oklch{L: 0.7862, C: 0.104, H: 175.7, Alpha: 1}
		colorDebug      = mycolor.Oklch{}
		colorGeneral    = mycolor.Oklch{L: 0.7862, C: 0.104, H: 120, Alpha: 1}
	)

	cmds := theme.CommandSlice{
		theme.NormalCommand{
			Category:     "Navigation",
			PrimaryLabel: "Scroll to timeline…",
			Aliases:      []string{"goto", "go to"},
			Shortcut:     "G",
			Color:        colorNavigation,
			Fn: func() theme.Action {
				return &OpenScrollToTimelineAction{}
			}},

		theme.NormalCommand{
			Category:     "Navigation",
			PrimaryLabel: "Undo previous navigation",
			Shortcut:     key.ModShortcut.String() + "+Z",
			Color:        colorNavigation,
			Fn: func() theme.Action {
				return &CanvasUndoNavigationAction{}
			}},

		theme.NormalCommand{
			Category:     "Navigation",
			PrimaryLabel: "Scroll to top of canvas",
			Aliases:      []string{"jump", "beginning"},
			Shortcut:     "Home",
			Color:        colorNavigation,
			Fn: func() theme.Action {
				return &CanvasScrollToTopAction{}
			}},

		theme.NormalCommand{
			Category:     "Navigation",
			PrimaryLabel: "Zoom to fit visible timelines",
			Shortcut:     key.ModShortcut.String() + "+Home",
			Color:        colorNavigation,
			Fn: func() theme.Action {
				return &CanvasZoomToFitCurrentViewAction{}
			}},

		theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Highlight spans…",
			Shortcut:     "H",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &OpenHighlightSpansDialogAction{}
			}},

		theme.NormalCommand{
			Category:     "Navigation",
			PrimaryLabel: "Pan to beginning of time",
			Aliases:      []string{"scroll", "jump"},
			Shortcut:     "Shift+Home",
			Color:        colorNavigation,
			Fn: func() theme.Action {
				return &CanvasJumpToBeginningAction{}
			}},

		theme.NormalCommand{
			Category:     "Analysis",
			PrimaryLabel: "Open processor utilization heatmap",
			Color:        colorAnalysis,
			Fn: func() theme.Action {
				return &OpenHeatmapAction{}
			}},

		theme.NormalCommand{
			Category:     "Analysis",
			PrimaryLabel: "Open flame graph",
			Aliases:      []string{"flamegraph"},
			Color:        colorAnalysis,
			Fn: func() theme.Action {
				return &OpenFlameGraphAction{}
			}},

		theme.NormalCommand{
			Category:     "General",
			PrimaryLabel: "Open trace",
			Color:        colorGeneral,
			Fn: func() theme.Action {
				return &OpenFileOpenAction{}
			}},

		theme.NormalCommand{
			Category:     "General",
			PrimaryLabel: "Quit",
			Color:        colorGeneral,
			Fn: func() theme.Action {
				return &ExitAction{}
			}},
	}

	if mwin.canvas.timeline.displayStackTracks {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Hide stack tracks",
			Shortcut:     "S",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleStackTracksAction{}
			},
		})
	} else {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Show stack tracks",
			Shortcut:     "S",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleStackTracksAction{}
			},
		})
	}

	if mwin.canvas.timeline.compact {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Disable compact display",
			Shortcut:     "C",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleCompactDisplayAction{}
			}})
	} else {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Enable compact display",
			Shortcut:     "C",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleCompactDisplayAction{}
			}})
	}

	if mwin.canvas.timeline.displayAllLabels {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Hide timeline labels",
			Shortcut:     "X",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleTimelineLabelsAction{}
			}})
	} else {
		cmds = append(cmds, theme.NormalCommand{
			Category:     "Display",
			PrimaryLabel: "Show timeline labels",
			Shortcut:     "X",
			Color:        colorDisplay,
			Fn: func() theme.Action {
				return &CanvasToggleTimelineLabelsAction{}
			}})
	}

	if softDebug {
		cmds = append(cmds,
			theme.NormalCommand{
				Category:     "Debug",
				PrimaryLabel: "Write memory profile",
				Color:        colorDebug,
				Fn: func() theme.Action {
					return &WriteMemoryProfileAction{}
				}},

			theme.NormalCommand{
				Category:     "Debug",
				PrimaryLabel: "Force garbage collection",
				Aliases:      []string{"run", "gc"},
				Color:        colorDebug,
				Fn: func() theme.Action {
					return &RunGarbageCollectionAction{}
				}},

			theme.NormalCommand{
				Category:     "Debug",
				PrimaryLabel: "Force garbage collection & return unused memory to OS",
				Aliases:      []string{"run", "gc", "free", "operating system"},
				Color:        colorDebug,
				Fn: func() theme.Action {
					return &RunFreeOSMemoryAction{}
				}},
		)

		if mwin.cpuProfile == nil {
			cmds = append(cmds, theme.NormalCommand{
				Category:     "Debug",
				PrimaryLabel: "Start CPU profile",
				Color:        colorDebug,
				Fn: func() theme.Action {
					return &StartCPUProfileAction{}
				}})
		} else {
			cmds = append(cmds, theme.NormalCommand{
				Category:     "Debug",
				PrimaryLabel: "Stop CPU profile",
				Color:        colorDebug,
				Fn: func() theme.Action {
					return &StopCPUProfileAction{}
				}})
		}
	}

	slices.SortFunc(cmds, func(a, b theme.Command) bool {
		an := a.(theme.NormalCommand)
		bn := b.(theme.NormalCommand)

		if an.Category < bn.Category {
			return true
		} else if an.Category > bn.Category {
			return false
		} else {
			return an.PrimaryLabel < bn.PrimaryLabel
		}
	})

	return cmds
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
				mwin.SetError(err)
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

	for _, tl := range res.timelines {
		assert(tl.item != nil, "unexpected nil item")
		mwin.canvas.itemToTimeline[tl.item] = tl
	}

	mwin.trace = res.trace
	mwin.panel = nil
	mwin.panelHistory = nil
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
	Invalidate()
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
		if f.Name == "debug" || strings.HasPrefix(f.Name, "debug.") {
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
	flag.BoolVar(&softDebug, "debug", debug, "Enable basic debug functionality")
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
	mwin.win = app.NewWindow(app.Title("gotraceui"))
	mwin.twin = theme.NewWindow(mwin.win)
	mwin.explorer = explorer.NewExplorer(mwin.win)

	if debug {
		go func() {
			win := app.NewWindow(app.Title("gotraceui - debug window"))
			mwin.debugWindow.Run(win)
		}()
	}

	mwin.setState("start")

	if len(flag.Args()) > 0 {
		openTraceFromCmdline(mwin)
	}

	go func() {
		mwin.errs <- mwin.Run()
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

type progresser interface {
	SetProgressStages(names []string)
	SetProgressStage(stage int)
	SetProgress(p float64)
}

func loadTrace(f io.Reader, p progresser, cv *Canvas) (loadTraceResult, error) {
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

	p.SetProgressStages(names)

	p.SetProgressStage(0)
	t, err := trace.Parse(f, p.SetProgress)
	if err != nil {
		return loadTraceResult{}, err
	}
	if exitAfterParsing {
		return loadTraceResult{}, errExitAfterParsing
	}

	p.SetProgressStage(1)
	pt, err := ptrace.Parse(t, p.SetProgress)
	if err != nil {
		return loadTraceResult{}, err
	}

	p.SetProgressStage(2)
	// Assign GC tag to all GC spans so we can later determine their span colors cheaply.
	for i, proc := range pt.Processors {
		for j := 0; j < len(proc.Spans); j++ {
			fn := pt.G(pt.Events[proc.Spans[j].Event].G).Function
			if fn == nil {
				continue
			}
			switch fn.Fn {
			case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
				proc.Spans[j].Tags |= ptrace.SpanTagGC
			}
		}
		p.SetProgress(float64(i+1) / float64(len(pt.Processors)))
	}

	p.SetProgressStage(3)
	tr := &Trace{Trace: pt}
	if len(pt.Goroutines) != 0 {
		tr.allGoroutineSpanLabels = make([][]string, len(pt.Goroutines))

		for seqID, g := range pt.Goroutines {
			// Populate goroutine span labels
			localPrefixedID := local.Sprintf("g%d", g.ID)

			var spanLabels []string
			if g.Function.Fn != "" {
				short := shortenFunctionName(g.Function.Fn)
				spanLabels = append(spanLabels, localPrefixedID+": "+g.Function.Fn)
				if short != g.Function.Fn {
					spanLabels = append(spanLabels, localPrefixedID+": ."+short)
				} else {
					// This branch is probably impossible; all functions should be fully qualified.
					spanLabels = append(spanLabels, localPrefixedID)
				}
			} else {
				spanLabels = append(spanLabels, localPrefixedID)
			}
			tr.allGoroutineSpanLabels[seqID] = spanLabels

			p.SetProgress(float64(seqID+1) / float64(len(pt.Goroutines)))
		}
	}

	p.SetProgressStage(4)
	if len(pt.Processors) != 0 {
		tr.allProcessorSpanLabels = make([][]string, len(pt.Processors))

		for seqID, proc := range pt.Processors {
			localPrefixedID := local.Sprintf("p%d", proc.ID)
			tr.allProcessorSpanLabels[seqID] = append(tr.allProcessorSpanLabels[seqID], localPrefixedID)
			p.SetProgress(float64(seqID+1) / float64(len(pt.Processors)))
		}
	}

	// TODO(dh): preallocate
	var timelines []*Timeline

	p.SetProgressStage(5)
	if supportMachineTimelines {
		for i, m := range tr.Machines {
			timelines = append(timelines, NewMachineTimeline(tr, cv, m))
			p.SetProgress(float64(i+1) / float64(len(tr.Machines)))
		}
	}

	p.SetProgressStage(6)
	for i, proc := range tr.Processors {
		timelines = append(timelines, NewProcessorTimeline(tr, cv, proc))
		p.SetProgress(float64(i+1) / float64(len(tr.Processors)))
	}

	p.SetProgressStage(7)
	baseTimeline := len(timelines)
	timelines = slices.Grow(timelines, len(tr.Goroutines))[:len(timelines)+len(tr.Goroutines)]
	var progress atomic.Uint64
	mysync.Distribute(tr.Goroutines, 0, func(group int, step int, subitems []*ptrace.Goroutine) error {
		for j, g := range subitems {
			timelines[baseTimeline+group*step+j] = NewGoroutineTimeline(tr, cv, g)
			pr := progress.Add(1)
			p.SetProgress(float64(pr) / float64(len(tr.Goroutines)))
		}
		return nil
	})

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

type Text struct {
	// The theme must only be used for building the Text, with methods like Span. The Layout function has to use the
	// theme provided to it, to avoid race conditions when texts transition from widgets to independent windows.
	//
	// The theme must be reset in Reset.
	styles    []styledtext.SpanStyle
	Alignment text.Alignment

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
	Object     any
	ObjectLink ObjectLink

	Click *gesture.Click
}

func (txt *TextBuilder) Add(s TextSpan) {
	txt.Spans = append(txt.Spans, s)
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

func (txt *TextBuilder) DefaultLink(label, provenance string, obj any) *TextSpan {
	return txt.Link(label, obj, defaultObjectLink(obj, provenance))
}

func (txt *TextBuilder) Link(label string, obj any, link ObjectLink) *TextSpan {
	s := txt.Span(label)
	s.Color = txt.Theme.Palette.Link
	s.Object = obj
	s.ObjectLink = link
	return s
}

func (txt *Text) Reset(th *theme.Theme) {
	txt.events = txt.events[:0]
	txt.styles = txt.styles[:0]
	txt.Alignment = 0
}

func (txt *Text) Events() []TextEvent {
	return txt.events
}

func (txt *Text) Layout(win *theme.Window, gtx layout.Context, spans []TextSpan) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Text.Layout").End()

	var clickableIdx int
	for i := range spans {
		s := &spans[i]
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
	for i := range spans {
		s := &spans[i]
		if s.Click != nil {
			for _, ev := range s.Click.Events(gtx.Queue) {
				txt.events = append(txt.events, TextEvent{s, ev})
			}
		}
	}

	txt.styles = txt.styles[:0]
	for _, s := range spans {
		txt.styles = append(txt.styles, s.SpanStyle)
	}
	ptxt := styledtext.Text(win.Theme.Shaper, txt.styles...)
	ptxt.Alignment = txt.Alignment
	if txt.Alignment == text.Start {
		gtx.Constraints.Max.X = 1e6
	}
	return ptxt.Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
		defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
		s := &spans[i]
		if s.Object != nil {
			s.Click.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
		}
	})
}

type Description struct {
	Attributes []DescriptionAttribute
}

type DescriptionAttribute struct {
	Key   string
	Value TextSpan
}

func (desc Description) Layout(win *theme.Window, gtx layout.Context, txt *Text) layout.Dimensions {
	txt.Reset(win.Theme)
	// OPT(dh): reuse space
	tb := TextBuilder{Theme: win.Theme}
	for _, attr := range desc.Attributes {
		tb.Bold(fmt.Sprintf("%s: ", attr.Key))
		tb.Add(attr.Value)
		tb.Span("\n")
	}

	return txt.Layout(win, gtx, tb.Spans)
}

type ScrollToTimelineCommand struct {
	MainWindow *theme.Window
	Timeline   *Timeline
}

func (cmd ScrollToTimelineCommand) Layout(win *theme.Window, gtx layout.Context, current bool) layout.Dimensions {
	var (
		numSpans   int
		start, end trace.Timestamp
	)

	// TODO(dh): instead of this switch we should have a method on the interface for returning the spans
	switch item := cmd.Timeline.item.(type) {
	case *GC:
		numSpans = item.Spans.Len()
		start = item.Spans.At(0).Start
		end = LastSpan(item.Spans).End
	case *STW:
		numSpans = item.Spans.Len()
		start = item.Spans.At(0).Start
		end = LastSpan(item.Spans).End
	case *ptrace.Goroutine:
		numSpans = len(item.Spans)
		start = item.Spans[0].Start
		end = item.Spans[len(item.Spans)-1].End
	case *ptrace.Processor:
		numSpans = len(item.Spans)
		start = item.Spans[0].Start
		end = item.Spans[len(item.Spans)-1].End
	default:
		panic(fmt.Sprintf("%T", item))
	}
	return theme.NormalCommand{
		PrimaryLabel:   cmd.Timeline.label,
		SecondaryLabel: local.Sprintf("%d spans\n%d ns—%d ns (%s)", numSpans, start, end, roundDuration(time.Duration(end-start))),
		Color:          mycolor.Oklch{L: 0.7862, C: 0.104, H: 139.8, Alpha: 1},
	}.Layout(win, gtx, current)
}

func (cmd ScrollToTimelineCommand) Link() theme.Action {
	return (*ScrollToTimelineAction)(cmd.Timeline)
}

func (cmd ScrollToTimelineCommand) Filter(input string) bool {
	for _, f := range strings.Fields(input) {
		b := func() bool {
			if strings.HasPrefix(f, "g") {
				if f == "g" {
					if _, ok := cmd.Timeline.item.(*ptrace.Goroutine); ok {
						return true
					}
				} else {
					id := strings.ReplaceAll(f[len("g"):], ",", "")
					if n, err := strconv.ParseUint(id, 10, 64); err == nil {
						if g, ok := cmd.Timeline.item.(*ptrace.Goroutine); ok {
							if g.ID == n {
								return true
							}
						}
					}
				}
			}
			// TODO(dh): deduplicate code
			if strings.HasPrefix(f, "p") {
				if f == "p:" {
					if _, ok := cmd.Timeline.item.(*ptrace.Processor); ok {
						return true
					}
				} else {
					id := strings.ReplaceAll(f[len("p"):], ",", "")
					if n, err := strconv.ParseUint(id, 10, 64); err == nil {
						if p, ok := cmd.Timeline.item.(*ptrace.Processor); ok {
							if n <= math.MaxInt32 {
								if p.ID == int32(n) {
									return true
								}
							}
						}
					}
				}
			}

			// OPT(dh): don't repeatedly lowercase the label
			if strings.Contains(strings.ToLower(cmd.Timeline.label), strings.ToLower(f)) {
				return true
			}
			return false
		}()
		if !b {
			return false
		}
	}
	return true
}

type GotoTimelineCommandProvider struct {
	MainWindow *theme.Window
	Timelines  []*Timeline
}

func (p GotoTimelineCommandProvider) Len() int {
	return len(p.Timelines)
}

func (p GotoTimelineCommandProvider) At(idx int) theme.Command {
	return ScrollToTimelineCommand{
		MainWindow: p.MainWindow,
		Timeline:   p.Timelines[idx],
	}
}

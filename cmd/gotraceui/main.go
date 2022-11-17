package main

import (
	"context"
	"flag"
	"fmt"
	"image"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	rtrace "runtime/trace"
	"strconv"
	"strings"
	"time"

	mylayout "honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font/gofont"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/profile"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/x/styledtext"
	"golang.org/x/text/message"
)

/*
   Notes on trace consistency

   Tracing can start and stop in the middle of the program's lifetime, repeatedly. The following corner cases can arise
   due to this (not an exhaustive list):

   - The user stops tracing before ending a user region or user task. This means we can't rely on all create events
     having valid links to end events.

   - The user stops tracing before all goroutines have ended. This happens virtually all the time for the runtime.main
     goroutine.

   - User task IDs are not reset between traces.

   - The user starts a region before starting tracing, and ends it after starting tracing. runtime/trace handles this
     for us; StartRegion returns a "no-op region" if tracing isn't enabled, ending which doesn't emit any event.

   - The user starts tracing, starts a region, stops tracing, starts tracing, stops the region. If we only look at the
     second trace, then we see that the region ends, but we never see its creation.

   - The same tracing states as in the previous two examples, but starting and ending tasks instead of regions. For
     tasks, we can see the end of unknown tasks in both cases, because there are no "no-op tasks".

   - The user creates a task with a parent task that we didn't see, either because tracing wasn't enabled or because the
     parent was enabled in a previous trace.
*/

// A note on Ps
//
// Not all events have a P. For example, when sysmon wakes up the scavenger, it doesn't have a P while unblocking
// goroutines.

/*
   GC notes:
   - The only use of p=1000004 is for GCStart
   - GCDone happens on other procs, probably whichever proc was running the background worker that determined that we're done
   - A similar thing applies to GCSTWStart and GCSTWDone
   - The second GCSTWDone can happen after GCDone

   GC can get started by normal user goroutines, when they allocate and detect that the GC trigger condition has been
   met. The lucky goroutine will be responsible for running the runtime code that stops the world, sets up write
   barriers and so on. It returns to the user's code once the first STW phase is over and the concurrent mark phase has
   started. Since multiple goroutines may allocate in parallel, multiple goroutines might detect the GC trigger and
   attempt to start GC. All but one will block trying to acquire the semaphore, then not start GC once they unblock.

   The GCStart and the first pair of STWStart + STWStop will be sent from that goroutine, but note that these events
   form a timeline separate from normal goroutine events. In particular, seeing STWStart on the goroutine doesn't mean
   that its previous GoStart has been superseded, and we'll not see another GoStart after we see STWStop.

   The second STW phase and GCDone are sent from another goroutine, probably the background mark worker that determined
   that we're done.
*/

/*
   Goroutine window, things to display:

   - [X] ID, function name
   - [X] stack of where it was created
   - Link to span that created it
   - [X] First, last timestamp, duration
   - Per-state statistics (how long blocked, waiting, etc, number of state transitions)
   - List of all spans
   - List of all events of all spans
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

// TODO(dh): in a lot of places we react to clicks on the Press event, but we should really detect proper clicks, which
//   is press + release without dragging inbetween.
// TODO(dh): standardize whether we pass Theme as an argument to Layout or store it in the type
// FIXME(dh): in ListWindow, when all items got filtered away and we change the filter so there are items again, no item
//   will be selected, and pressing enter will panic, trying to access index -1
// TODO(dh): disable navigation keybindings such as Home when we're dragging
// XXX how do we have a minimum inactive span of length 0?
// OPT(dh): optimize highlighting hovered goroutine in per-processor view when there are merged spans with lots of children
// TODO(dh): support exporting an image of the entire trace, at a zoom level that shows all details
// TODO(dh): clicking on a goroutine in the per-P view should bring up the goroutine window
// OPT(dh): the goroutine span tooltip should cache the stats. for the bgsweep goroutine in the staticcheck-std trace,
//   rendering the tooltip alone takes ~16ms
// TODO(dh): allow computing statistics for a selectable region of time
// TODO(dh): hovering over spans in the goroutine timelines highlights goroutines in the processor timelines. that's a
//   happy accident. however, it doesn't work reliably, because we just look at trace.Event.G for the matching, and for
//   some events, like unblocking, that's the wrong G.
// TODO(dh): use the GC-purple color in the GC and STW timelines
// TODO(dh): toggleable behavior for hovering spans in goroutine timelines. For example, hovering a blocked span could
//   highlight the span that unblocks it (or maybe when hovering the "runnable" span, but same idea). Hovering a running
//   span could highlight all the spans it unblocks.
// TODO(dh): the Event.Stk is meaningless for goroutines that already existed when tracing started, i.e. ones that get a
//   GoWaiting event. The GoCreate event will be caused by starting the trace, and the stack of the event will be that
//   leading up to starting the trace. It will in no way reflect the code that actually, historically, started the
//   goroutine. To avoid confusion, we should remove those stacks altogether.

var (
	cpuprofile       string
	memprofileLoad   string
	memprofileExit   string
	traceFile        string
	disableCaching   bool
	exitAfterLoading bool
	exitAfterParsing bool
)

var goFonts = gofont.Collection()

type EventID uint64

type reusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *reusableOps) get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
}

func (w *MainWindow) openGoroutineWindow(g *Goroutine) {
	_, ok := w.goroutineWindows[g.id]
	if ok {
		// XXX try to activate (bring to the front) the existing window
	} else {
		win := &GoroutineWindow{
			// Note that we cannot use a.theme, because text.Shaper isn't safe for concurrent use.
			theme: theme.NewTheme(gofont.Collection()),
			mwin:  w,
			trace: w.trace,
			g:     g,
		}
		win.stats = NewGoroutineStats(g, w.trace)
		w.goroutineWindows[g.id] = win
		// XXX computing the label is duplicated with rendering the activity widget
		var l string
		if g.function != "" {
			l = local.Sprintf("goroutine %d: %s", g.id, g.function)
		} else {
			l = local.Sprintf("goroutine %d", g.id)
		}
		go func() {
			// XXX handle error?
			win.Run(app.NewWindow(app.Title(fmt.Sprintf("gotraceui - %s", l))))
			w.notifyGoroutineWindowClosed <- g.id
		}()
	}
}

func (w *MainWindow) openHeatmap() {
	win := &HeatmapWindow{
		theme: w.theme,
		trace: w.trace,
	}
	go func() {
		// XXX handle error?
		win.Run(app.NewWindow(app.Title("gotraceui - heatmap")))
	}()
}

func shortenFunctionName(s string) string {
	fields := strings.Split(s, ".")
	short := fields[len(fields)-1]
	// TODO(dh): the short name isn't ideal for anonymous functions, as we turn
	// "pkg.(type).fn.func1" into ".func1", when really it should be ".fn.func1".
	return short
}

type Command func(*MainWindow, layout.Context)

type MainWindow struct {
	tl       Timeline
	theme    *theme.Theme
	trace    *Trace
	commands chan Command

	pointerAt f32.Point

	win *app.Window
	// TODO(dh): use enum for state
	state    string
	progress float32
	ww       *theme.ListWindow[fmt.Stringer]
	err      error

	notifyGoroutineWindowClosed chan uint64
	goroutineWindows            map[uint64]*GoroutineWindow

	debugWindow *DebugWindow
}

func NewMainWindow() *MainWindow {
	win := &MainWindow{
		theme:                       theme.NewTheme(gofont.Collection()),
		commands:                    make(chan Command, 128),
		notifyGoroutineWindowClosed: make(chan uint64, 16),
		goroutineWindows:            make(map[uint64]*GoroutineWindow),
		debugWindow:                 NewDebugWindow(),
	}

	win.tl.axis.tl = &win.tl

	return win
}

type activityFilter struct {
	invalid bool
	parts   []struct {
		prefix string
		value  struct {
			s string
			n uint64
		}
	}
}

func newActivityFilter(s string) theme.Filter[fmt.Stringer] {
	out := &activityFilter{}
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

func (f *activityFilter) Filter(item fmt.Stringer) bool {
	if f.invalid {
		return false
	}

	for _, p := range f.parts {
		switch p.prefix {
		case "gid":
			if item, ok := item.(*Goroutine); !ok || item.id != p.value.n {
				return false
			}

		case "pid":
			if item, ok := item.(*Processor); !ok || uint64(item.id) != p.value.n {
				return false
			}

		case "":
			// TODO(dh): support case insensitive search
			var s string
			switch item := item.(type) {
			case *Goroutine:
				s = item.function
			case *Processor:
				s = strconv.FormatUint(uint64(item.id), 10)
			default:
				panic(fmt.Sprintf("unhandled type %T", item))
			}
			if !strings.Contains(s, p.value.s) {
				return false
			}

		default:
			return false
		}
	}
	return true
}

func (mwin *MainWindow) setState(state string) {
	mwin.state = state
	mwin.progress = 0.0
	mwin.ww = nil
}

func (mwin *MainWindow) SetState(state string) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.setState(state)
	}
}

func (mwin *MainWindow) SetError(err error) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.err = err
		mwin.setState("error")
	}
}

func (mwin *MainWindow) SetProgress(p float32) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.progress = p
	}
}

func (mwin *MainWindow) SetProgressLossy(p float32) {
	fn := func(mwin *MainWindow, _ layout.Context) {
		mwin.progress = p
	}
	select {
	case mwin.commands <- fn:
	default:
	}
}

func (mwin *MainWindow) LoadTrace(tr *Trace) {
	mwin.commands <- func(mwin *MainWindow, _ layout.Context) {
		mwin.loadTraceImpl(tr)
		mwin.setState("main")
	}
}

func (mwin *MainWindow) OpenLink(l Link) {
	mwin.commands <- func(mwin *MainWindow, gtx layout.Context) {
		// TODO(dh): links should probably have a method that take a MainWindow and run the appropriate commands on it
		switch l := l.(type) {
		case *GoroutineLink:
			switch l.Kind {
			case GoroutineLinkKindOpenWindow:
				mwin.openGoroutineWindow(l.Goroutine)
			case GoroutineLinkKindScroll:
				mwin.tl.scrollToActivity(gtx, l.Goroutine)
			case GoroutineLinkKindZoom:
				y := mwin.tl.activityY(gtx, l.Goroutine)
				mwin.tl.navigateTo(gtx, l.Goroutine.spans.Start(mwin.trace), l.Goroutine.spans.End(), y)
			default:
				panic(l.Kind)
			}
		case *TimestampLink:
			d := mwin.tl.end - mwin.tl.start
			mwin.tl.navigateTo(gtx, l.Ts, l.Ts+d, mwin.tl.y)
		default:
			panic(l)
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
		Quit theme.MenuItem
	}

	Display struct {
		UndoNavigation       theme.MenuItem
		ScrollToTop          theme.MenuItem
		ZoomToFit            theme.MenuItem
		JumpToBeginning      theme.MenuItem
		ToggleCompactDisplay theme.MenuItem
		ToggleActivityLabels theme.MenuItem
		ToggleSampleTracks   theme.MenuItem
	}

	Analyze struct {
		OpenHeatmap theme.MenuItem
	}

	Debug struct {
		Memprofile theme.MenuItem
	}

	menu *theme.Menu
}

func NewMainMenu(w *MainWindow) *MainMenu {
	m := &MainMenu{}

	m.File.Quit = theme.MenuItem{Label: PlainLabel("Quit")}

	m.Display.UndoNavigation = theme.MenuItem{Shortcut: "Ctrl+Z", Label: PlainLabel("Undo previous navigation")}
	m.Display.ScrollToTop = theme.MenuItem{Shortcut: "Home", Label: PlainLabel("Scroll to top of activity list")}
	m.Display.ZoomToFit = theme.MenuItem{Shortcut: "Ctrl+Home", Label: PlainLabel("Zoom to fit visible activities")}
	m.Display.JumpToBeginning = theme.MenuItem{Shortcut: "Shift+Home", Label: PlainLabel("Jump to beginning of timeline")}
	m.Display.ToggleCompactDisplay = theme.MenuItem{Shortcut: "C", Label: ToggleLabel("Disable compact display", "Enable compact display", &w.tl.activity.compact)}
	m.Display.ToggleActivityLabels = theme.MenuItem{Shortcut: "X", Label: ToggleLabel("Hide activity labels", "Show activity labels", &w.tl.activity.displayAllLabels)}
	m.Display.ToggleSampleTracks = theme.MenuItem{Shortcut: "S", Label: ToggleLabel("Hide sample tracks", "Display sample tracks", &w.tl.activity.displaySampleTracks)}

	m.Debug.Memprofile = theme.MenuItem{Label: PlainLabel("Write memory profile")}

	m.Analyze.OpenHeatmap = theme.MenuItem{Label: PlainLabel("Open processor utilization heatmap")}

	m.menu = &theme.Menu{
		Theme: w.theme,
		Groups: []theme.MenuGroup{
			{
				Label: "File",
				Items: []theme.Widget{
					m.File.Quit.Layout,
				},
			},
			{
				Label: "Display",
				Items: []theme.Widget{
					// TODO(dh): disable Undo menu item when there are no more undo steps
					m.Display.UndoNavigation.Layout,

					theme.MenuDivider{}.Layout,

					m.Display.ScrollToTop.Layout,
					m.Display.ZoomToFit.Layout,
					m.Display.JumpToBeginning.Layout,

					theme.MenuDivider{}.Layout,

					m.Display.ToggleCompactDisplay.Layout,
					m.Display.ToggleActivityLabels.Layout,
					m.Display.ToggleSampleTracks.Layout,
					// TODO(dh): add items for STW and GC overlays
					// TODO(dh): add item for tooltip display
				},
			},
			{
				Label: "Analyze",
				Items: []theme.Widget{
					m.Analyze.OpenHeatmap.Layout,
				},
			},
		},
	}

	if debug {
		m.menu.Groups = append(m.menu.Groups, theme.MenuGroup{
			Label: "Debug",
			Items: []theme.Widget{
				m.Debug.Memprofile.Layout,
			},
		})
	}

	return m
}

func (w *MainWindow) Run(win *app.Window) error {
	mainMenu := NewMainMenu(w)
	w.win = win

	profileTag := new(int)
	var ops op.Ops
	var shortcuts int

	var commands []Command
	tWin := &theme.Window{
		Theme: w.theme,
		// XXX the majority of menu items should be disabled while we're not in the main state, i.e. while a trace
		// hasn't been loaded yet
		Menu: mainMenu.menu,
	}
	for {
		select {
		case cmd := <-w.commands:
			commands = append(commands, cmd)
			w.win.Invalidate()

		case gid := <-w.notifyGoroutineWindowClosed:
			delete(w.goroutineWindows, gid)

		case e := <-win.Events():
			switch ev := e.(type) {
			case system.DestroyEvent:
				return ev.Err
			case system.FrameEvent:
				tWin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
					gtx.Constraints.Min = image.Point{}

					for _, cmd := range commands {
						cmd(w, gtx)
					}
					commands = commands[:0]

					for _, ev := range gtx.Events(&w.pointerAt) {
						w.pointerAt = ev.(pointer.Event).Position
					}
					pointer.InputOp{Tag: &w.pointerAt, Types: pointer.Move | pointer.Drag | pointer.Enter}.Add(gtx.Ops)

				commandLoop:
					for {
						select {
						case cmd := <-w.commands:
							cmd(w, gtx)
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
							w.debugWindow.frametimes.addValue(gtx.Now, float64(d)/float64(time.Millisecond))
						}
					}
					profile.Op{Tag: profileTag}.Add(gtx.Ops)

					// Fill background
					paint.Fill(gtx.Ops, colors[colorBackground])

					switch w.state {
					case "empty":
						return layout.Dimensions{}

					case "error":
						paint.ColorOp{Color: w.theme.Palette.Foreground}.Add(gtx.Ops)
						m := op.Record(gtx.Ops)
						dims := widget.Label{}.Layout(gtx, w.theme.Shaper, text.Font{}, w.theme.TextSize, fmt.Sprintf("Error: %s", w.err))
						call := m.Stop()
						op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
						call.Add(gtx.Ops)
						return layout.Dimensions{Size: gtx.Constraints.Max}

					case "loadingTrace":
						paint.ColorOp{Color: w.theme.Palette.Foreground}.Add(gtx.Ops)
						m := op.Record(gtx.Ops)
						dims := widget.Label{}.Layout(gtx, w.theme.Shaper, text.Font{}, w.theme.TextSize, "Loading trace...")
						op.Offset(image.Pt(0, dims.Size.Y)).Add(gtx.Ops)

						func() {
							gtx := gtx
							gtx.Constraints.Min = image.Pt(dims.Size.X, 15)
							gtx.Constraints.Max = gtx.Constraints.Min
							theme.ProgressBar(w.theme, w.progress).Layout(gtx)
						}()

						call := m.Stop()
						op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
						call.Add(gtx.Ops)
						return layout.Dimensions{Size: gtx.Constraints.Max}

					case "main":
						for _, ev := range gtx.Events(&shortcuts) {
							switch ev := ev.(type) {
							case key.Event:
								if ev.State == key.Press && w.ww == nil {
									switch ev.Name {
									case "G":
										w.ww = theme.NewListWindow[fmt.Stringer](w.theme)
										items := make([]fmt.Stringer, 0, 2+len(w.trace.ps)+len(w.trace.gs))
										// XXX the GC and STW widgets should also be added here
										for _, p := range w.trace.ps {
											items = append(items, p)
										}
										for _, g := range w.trace.gs {
											items = append(items, g)
										}
										w.ww.SetItems(items)
										w.ww.BuildFilter = newActivityFilter
									}
								}
							}
						}

						if mainMenu.File.Quit.Clicked() {
							win.Menu.Close()
							os.Exit(0)
						}
						if mainMenu.Display.UndoNavigation.Clicked() {
							win.Menu.Close()
							w.tl.UndoNavigation(gtx)
						}
						if mainMenu.Display.ScrollToTop.Clicked() {
							win.Menu.Close()
							w.tl.ScrollToTop(gtx)
						}
						if mainMenu.Display.ZoomToFit.Clicked() {
							win.Menu.Close()
							w.tl.ZoomToFitCurrentView(gtx)
						}
						if mainMenu.Display.JumpToBeginning.Clicked() {
							win.Menu.Close()
							w.tl.JumpToBeginning(gtx)
						}
						if mainMenu.Display.ToggleCompactDisplay.Clicked() {
							win.Menu.Close()
							w.tl.ToggleCompactDisplay()
						}
						if mainMenu.Display.ToggleActivityLabels.Clicked() {
							win.Menu.Close()
							w.tl.ToggleActivityLabels()
						}
						if mainMenu.Display.ToggleSampleTracks.Clicked() {
							win.Menu.Close()
							w.tl.ToggleSampleTracks()
						}
						if mainMenu.Analyze.OpenHeatmap.Clicked() {
							win.Menu.Close()
							w.openHeatmap()
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

						for _, g := range w.tl.clickedGoroutineActivities {
							w.openGoroutineWindow(g)
						}

						key.InputOp{Tag: &shortcuts, Keys: "G"}.Add(gtx.Ops)

						if w.ww != nil {
							if item, ok := w.ww.Confirmed(); ok {
								w.tl.scrollToActivity(gtx, item)
								w.ww = nil
							} else if w.ww.Cancelled() {
								w.ww = nil
							} else {
								macro := op.Record(gtx.Ops)
								// XXX use constant for color
								// XXX maintain modal state so the modal can be closed
								(&theme.Modal{Background: rgba(0x000000DD)}).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
									return mylayout.PixelInset{
										Top:    gtx.Constraints.Max.Y/2 - 500/2,
										Bottom: gtx.Constraints.Max.Y/2 - 500/2,
										Left:   gtx.Constraints.Max.X/2 - 1000/2,
										Right:  gtx.Constraints.Max.X/2 - 1000/2,
									}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
										return w.ww.Layout(gtx)
									})
								})

								op.Defer(gtx.Ops, macro.Stop())
							}
						}

						if cpuprofile != "" {
							op.InvalidateOp{}.Add(&ops)
						}

						w.debugWindow.tlStart.addValue(gtx.Now, float64(w.tl.start))
						w.debugWindow.tlEnd.addValue(gtx.Now, float64(w.tl.end))
						w.debugWindow.tlY.addValue(gtx.Now, float64(w.tl.y))

						return w.tl.Layout(win, gtx)

					default:
						return layout.Dimensions{}
					}
				})

				ev.Frame(&ops)
			}
		}
	}
}

func (w *MainWindow) loadTraceImpl(t *Trace) {
	var end trace.Timestamp
	for _, g := range t.gs {
		if len(g.spans) > 0 {
			if d := g.spans.End(); d > end {
				end = d
			}
		}
	}
	for _, p := range t.ps {
		if len(p.spans) > 0 {
			if d := p.spans.End(); d > end {
				end = d
			}
		}
	}

	// Zoom out slightly beyond the end of the trace, so that the user can immediately tell that they're looking at the
	// entire trace.
	slack := float64(end) * 0.05
	start := trace.Timestamp(-slack)
	end = trace.Timestamp(float64(end) + slack)

	w.tl = Timeline{
		start:       start,
		end:         end,
		trace:       t,
		debugWindow: w.debugWindow,
	}

	w.tl.activity.displayAllLabels = true
	w.tl.axis = Axis{tl: &w.tl, theme: w.theme}
	w.tl.activities = make([]*ActivityWidget, 2, len(t.gs)+len(t.ps)+2)
	w.tl.activities[0] = NewGCWidget(&w.tl, t, t.gc)
	w.tl.activities[1] = NewSTWWidget(&w.tl, t, t.stw)
	for _, p := range t.ps {
		w.tl.activities = append(w.tl.activities, NewProcessorWidget(&w.tl, p))
	}
	for _, g := range t.gs {
		// FIXME(dh): NewGoroutineWidget is expensive, because it has to compute sample tracks. This causes the UI to
		// freeze, because loadTraceImpl runs in the UI goroutine.
		w.tl.activities = append(w.tl.activities, NewGoroutineWidget(&w.tl, g))
	}

	w.trace = t
}

//gcassert:inline
func withOps(gtx layout.Context, ops *op.Ops) layout.Context {
	gtx.Ops = ops
	return gtx
}

type durationNumberFormat uint8

const (
	durationNumberFormatSI durationNumberFormat = iota
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

func (nf durationNumberFormat) format(d time.Duration) string {
	switch nf {
	case durationNumberFormatScientific:
		return scientificDuration(d, 2)
	case durationNumberFormatSI:
		return roundDuration(d).String()
	case durationNumberFormatExact:
		return fmt.Sprintf("%.9f", d.Seconds())
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

type TimestampLink struct {
	Ts trace.Timestamp
}

func (*TimestampLink) isLink() {}

type Link interface{ isLink() }

type GoroutineLinkKind uint8

const (
	GoroutineLinkKindOpenWindow GoroutineLinkKind = iota
	GoroutineLinkKindScroll
	GoroutineLinkKindZoom
)

type GoroutineLink struct {
	Goroutine *Goroutine
	Kind      GoroutineLinkKind
}

func (*GoroutineLink) isLink() {}

func span(th *theme.Theme, text string) styledtext.SpanStyle {
	return styledtext.SpanStyle{
		Content: text,
		Size:    th.TextSize,
		Color:   th.Palette.Foreground,
		Font:    goFonts[0].Font,
	}
}

func spanWith(th *theme.Theme, text string, fn func(styledtext.SpanStyle) styledtext.SpanStyle) styledtext.SpanStyle {
	return fn(span(th, text))
}

var local = message.NewPrinter(message.MatchLanguage("en"))

func formatTimestamp(ts trace.Timestamp) string {
	return local.Sprintf("%d ns", ts)
}

func main() {
	flag.StringVar(&cpuprofile, "debug.cpuprofile", "", "write CPU profile to this file")
	flag.StringVar(&memprofileLoad, "debug.memprofile-load", "", "write memory profile to this file after loading trace")
	flag.StringVar(&memprofileExit, "debug.memprofile-exit", "", "write meory profile to this file when exiting")
	flag.StringVar(&traceFile, "debug.trace", "", "write runtime trace to this file")
	flag.BoolVar(&disableCaching, "debug.disable-caching", false, "Disable caching")
	flag.BoolVar(&exitAfterLoading, "debug.exit-after-loading", false, "Exit after parsing and processing trace")
	flag.BoolVar(&exitAfterParsing, "debug.exit-after-parsing", false, "Exit after parsing trace")
	flag.Parse()

	if len(flag.Args()) != 1 {
		fmt.Fprintln(os.Stderr, "need one argument: path to trace")
		os.Exit(1)
	}

	mwin := NewMainWindow()
	if debug {
		go func() {
			win := app.NewWindow(app.Title("gotraceui - debug window"))
			mwin.debugWindow.Run(win)
		}()
	}

	errs := make(chan error)
	go func() {
		mwin.SetState("loadingTrace")
		f, err := os.Open(flag.Args()[0])
		if err != nil {
			mwin.SetError(fmt.Errorf("couldn't load trace: %w", err))
			return
		}
		defer f.Close()
		t, err := loadTrace(f, mwin)
		if memprofileLoad != "" {
			writeMemprofile(memprofileLoad)
		}
		if err == errExitAfterLoading || err == errExitAfterParsing {
			errs <- err
			return
		}
		if err != nil {
			mwin.SetError(fmt.Errorf("couldn't load trace: %w", err))
			return
		}
		mwin.LoadTrace(t)
	}()
	go func() {
		win := app.NewWindow(app.Title("gotraceui"))
		errs <- mwin.Run(win)
	}()

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

		err := <-errs
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
	theme     *theme.Theme
	styles    []styledtext.SpanStyle
	Spans     []TextSpan
	Alignment text.Alignment

	// Clickables we use for spans and reuse between frames. We allocate them one by one because it really doesn't
	// matter; we have hundreds of these at most. This won't make the GC sweat, and it avoids us having to do a bunch of
	// semi-manual memory management.
	clickables []*widget.Clickable
}

type TextSpan struct {
	*styledtext.SpanStyle
	Link Link

	Clickable *widget.Clickable
}

func (txt *Text) Span(label string) *TextSpan {
	style := styledtext.SpanStyle{
		Content: label,
		Size:    txt.theme.TextSize,
		Color:   txt.theme.Palette.Foreground,
		Font:    goFonts[0].Font,
	}
	txt.styles = append(txt.styles, style)
	s := TextSpan{
		SpanStyle: &txt.styles[len(txt.styles)-1],
	}
	txt.Spans = append(txt.Spans, s)
	return &txt.Spans[len(txt.Spans)-1]
}

func (txt *Text) SpanWith(label string, fn func(s *TextSpan)) *TextSpan {
	s := txt.Span(label)
	fn(s)
	return s
}

func (txt *Text) Bold(label string) *TextSpan {
	s := txt.Span(label)
	s.Font.Weight = text.Bold
	return s
}

func (txt *Text) Link(label string, link Link) *TextSpan {
	s := txt.Span(label)
	s.Color = txt.theme.Palette.Link
	s.Link = link
	return s
}

func (txt *Text) Reset() {
	txt.styles = txt.styles[:0]
	txt.Spans = txt.Spans[:0]
	txt.Alignment = 0
}

func (txt *Text) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Text.Layout").End()

	var clickableIdx int
	for i := range txt.Spans {
		s := &txt.Spans[i]
		if s.Link != nil {
			var clk *widget.Clickable
			if clickableIdx < len(txt.clickables) {
				clk = txt.clickables[clickableIdx]
				clickableIdx++
			} else {
				clk = &widget.Clickable{}
				txt.clickables = append(txt.clickables, clk)
				clickableIdx++
			}
			s.Clickable = clk
		}
	}

	ptxt := styledtext.Text(txt.theme.Shaper, txt.styles...)
	ptxt.Alignment = txt.Alignment
	return ptxt.Layout(gtx, func(_ layout.Context, i int, dims layout.Dimensions) {
		s := &txt.Spans[i]
		if s.Link != nil {
			s.Clickable.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				pointer.CursorPointer.Add(gtx.Ops)
				return dims
			})
		}
	})
}

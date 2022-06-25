package main

import (
	"fmt"
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font/gofont"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
	"honnef.co/go/gotraceui/trace"
)

const debug = true

// XXX investigate if there can be gaps in goroutine IDs

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistance = 24
	tickHeight      = 50
	tickWidth       = 3
)

const stateBarHeight = unit.Dp(10)

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration

	// State for dragging the timeline
	Drag struct {
		ClickAt f32.Point
		Active  bool
		Start   time.Duration
		End     time.Duration
	}

	// State for zooming to a selection
	ZoomSelection struct {
		Active  bool
		ClickAt f32.Point
	}

	// Frame-local state set by Layout and read by various helpers
	nsPerPx float64

	cursorPos f32.Point
}

func (tl *Timeline) startZoomSelection(pos f32.Point) {
	tl.ZoomSelection.Active = true
	tl.ZoomSelection.ClickAt = pos
}

func (tl *Timeline) abortZoomSelection() {
	tl.ZoomSelection.Active = false
}

func (tl *Timeline) endZoomSelection(gtx layout.Context, pos f32.Point) {
	tl.ZoomSelection.Active = false
	one := int(math.Round(float64(tl.ZoomSelection.ClickAt.X)))
	two := int(math.Round(float64(pos.X)))
	start := tl.pxToTs(gtx, min(one, two))
	end := tl.pxToTs(gtx, max(one, two))
	if start == end {
		// Cannot zoom to a zero width area
		return
	}

	tl.Start = start
	tl.End = end
}

func (tl *Timeline) startDrag(pos f32.Point) {
	tl.Drag.ClickAt = pos
	tl.Drag.Active = true
	tl.Drag.Start = tl.Start
	tl.Drag.End = tl.End
}

func (tl *Timeline) endDrag() {
	tl.Drag.Active = false
}

func (tl *Timeline) dragTo(gtx layout.Context, pos f32.Point) {
	d := time.Duration(math.Round(tl.nsPerPx * float64(tl.Drag.ClickAt.X-pos.X)))
	tl.Start = tl.Drag.Start + d
	tl.End = tl.Drag.End + d
}

func (tl *Timeline) zoom(gtx layout.Context, ticks float32, at f32.Point) {
	// TODO(dh): scale scroll amount by current zoom and by value of ev.Scroll.Y
	// XXX stop scrolling at some extreme point, so that nsperPx * our scroll multiplier is >=1
	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		tl.Start += time.Duration(tl.nsPerPx * 100 * ratio)
		tl.End -= time.Duration(tl.nsPerPx * 100 * (1 - ratio))
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		tl.Start -= time.Duration(tl.nsPerPx * 100 * ratio)
		tl.End += time.Duration(tl.nsPerPx * 100 * (1 - ratio))
	}
}

func (tl *Timeline) tickInterval(gtx layout.Context) time.Duration {
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(math.Round(float64(t) / tl.nsPerPx))
		if tickDistance >= minTickDistance {
			return t
		}
	}
	panic("unreachable")
}

//gcassert:inline
func (tl *Timeline) tsToPx(gtx layout.Context, t time.Duration) float64 {
	return float64(t-tl.Start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(gtx layout.Context, px int) time.Duration {
	return time.Duration(math.Round(float64(px)*tl.nsPerPx + float64(tl.Start)))
}

func (tl *Timeline) Layout(gtx layout.Context) layout.Dimensions {
	for _, ev := range gtx.Events(tl) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Type {
			case pointer.Press:
				if ev.Buttons&pointer.ButtonTertiary != 0 {
					if ev.Modifiers&key.ModShift != 0 {
						tl.startZoomSelection(ev.Position)
					} else {
						tl.startDrag(ev.Position)
					}
				}

			case pointer.Scroll:
				tl.abortZoomSelection()
				tl.zoom(gtx, ev.Scroll.Y, ev.Position)

			case pointer.Drag:
				tl.cursorPos = ev.Position
				if ev.Buttons&pointer.ButtonTertiary != 0 {
					if tl.Drag.Active {
						tl.dragTo(gtx, ev.Position)
					}
				}

			case pointer.Release:
				// For pointer.Release, ev.Buttons contains the buttons still being pressed, not the ones that have been
				// released.
				if ev.Buttons&pointer.ButtonTertiary == 0 {
					if tl.Drag.Active {
						tl.endDrag()
					} else if tl.ZoomSelection.Active {
						// XXX zoom to selection
						tl.endZoomSelection(gtx, ev.Position)
					}
				}

			case pointer.Move:
				tl.cursorPos = ev.Position
			}
		}
	}

	tl.nsPerPx = float64(tl.End-tl.Start) / float64(gtx.Constraints.Max.X)

	if debug {
		if tl.End < tl.Start {
			panic("XXX")
		}
		if tl.nsPerPx <= 0 {
			panic("XXX")
		}
	}

	// Fill background
	paint.Fill(gtx.Ops, toColor(0xAAAAAAFF))

	// Set up event handlers
	pointer.InputOp{
		Tag: tl,
		Types: pointer.Scroll |
			pointer.Drag |
			pointer.Press |
			pointer.Release |
			pointer.Move,
		ScrollBounds: image.Rectangle{Min: image.Point{-1, -1}, Max: image.Point{1, 1}},
	}.Add(gtx.Ops)

	// Draw axis

	// TODO don't bother with tl.Start and tl.End, we just have to subtract them in some places again. Just compute how
	// many ticks go in a line.
	//
	// TODO don't allow for labels to overlap
	// TODO round labels to pleasing precision
	tickInterval := tl.tickInterval(gtx)
	for t := tl.Start; t < tl.End; t += tickInterval {
		start := int(math.Round(tl.tsToPx(gtx, t) - tickWidth/2))
		end := int(math.Round(tl.tsToPx(gtx, t) + tickWidth/2))
		rect := clip.Rect{
			Min: image.Point{X: start, Y: 0},
			Max: image.Point{X: end, Y: tickHeight},
		}
		paint.FillShape(gtx.Ops, toColor(0x000000FF), rect.Op())

		macro := op.Record(gtx.Ops)
		label := fmt.Sprintf("+%s", t-tl.Start)
		dims := widget.Label{MaxLines: 1}.Layout(gtx, shaper, text.Font{}, 14, label)
		call := macro.Stop()

		// XXX leftmost and rightmost tick shouldn't be centered

		stack := op.Offset(image.Point{
			X: start - dims.Size.X/2,
			Y: tickHeight,
		}).Push(gtx.Ops)
		call.Add(gtx.Ops)
		stack.Pop()
	}

	func() {
		defer op.Offset(image.Point{Y: tickHeight * 2}).Push(gtx.Ops).Pop()

		// XXX make sure our rounding is stable and doesn't jitter
		// XXX handle spans that would be smaller than 1 unit

		// Draw goroutine lifetimes
		for gid, spans := range sspans {
			if gid > 100 {
				continue
			}
			func() {
				defer op.Offset(image.Point{X: 0, Y: 12 * int(gid)}).Push(gtx.Ops).Pop()
				// OPT(dh): use binary search
				first := -1
				last := -1
				for i, s := range spans {
					visible := (s.Start >= tl.Start && s.Start < tl.End) ||
						(s.End >= tl.Start && s.End < tl.End) ||
						(s.Start <= tl.Start && s.End >= tl.End)
					if first == -1 {
						if visible {
							first = i
						}
					} else {
						if !visible {
							last = i
							break
						}
					}
				}

				if last == -1 {
					last = len(spans)
				}
				if debug && first == last {
					panic("first == last")
				}

				widthOfSpan := func(s Span) float64 {
					return float64(s.End-s.Start) / tl.nsPerPx
				}

				if first != -1 {
					spans := spans[first:last]
					for i := 0; i < len(spans); i++ {
						s := spans[i]
						startPx := float64(tl.tsToPx(gtx, s.Start))
						endPx := float64(tl.tsToPx(gtx, s.End))

						var c color.NRGBA
						if int(s.State) >= len(stateColors) {
							c = toColor(colorStateUnknown)
						} else {
							c = stateColors[s.State]
						}

						const minSpanWidth = 5

						if endPx-startPx < minSpanWidth {
							c = toColor(colorStateUnknown) // XXX use different color
							// Collect enough spans until we've filled the minimum width
							for {
								i++
								if i == len(spans) {
									// We've run out of spans
									break
								}

								if widthOfSpan(spans[i]) >= minSpanWidth {
									// Don't merge spans that can stand on their own
									i--
									break
								}

								endPx = tl.tsToPx(gtx, spans[i].End)
							}
						}

						// XXX .5 needs to be rounded up or down for the end or start
						rect := clip.Rect{
							Min: image.Point{max(int(math.Round(startPx)), 0), 0},
							Max: image.Point{min(int(math.Round(endPx)), gtx.Constraints.Max.X), int(stateBarHeight)},
						}
						paint.FillShape(gtx.Ops, c, rect.Op())
					}
				}
			}()
		}
	}()

	// Draw zoom selection
	if tl.ZoomSelection.Active {
		one := int(math.Round(float64(tl.ZoomSelection.ClickAt.X)))
		two := int(math.Round(float64(tl.cursorPos.X)))
		rect := clip.Rect{
			Min: image.Point{X: min(one, two), Y: 0},
			Max: image.Point{X: max(one, two), Y: gtx.Constraints.Max.Y},
		}
		paint.FillShape(gtx.Ops, toColor(0x0089B099), rect.Op())
	}

	// Draw cursor
	rect := clip.Rect{
		Min: image.Point{X: int(math.Round(float64(tl.cursorPos.X))), Y: 0},
		Max: image.Point{X: int(math.Round(float64(tl.cursorPos.X + 1))), Y: gtx.Constraints.Max.Y},
	}
	paint.FillShape(gtx.Ops, toColor(0x000000FF), rect.Op())

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

// XXX goroutine 0 seems to be special and doesn't get (un)scheduled. look into that.

// TODO(dh): How should resizing the window affect the zoom level? When making the window wider, should it display more
// time or should it display the same time, stretched to fill the new space? Tracy does the latter.

// NOTE: how Tracy deals with spans that are too small to see at a zoom level: there's a minimum width for the first
// span, and consecutive spans that would fall into that span get merged into it

type Span struct {
	Start time.Duration
	End   time.Duration
	State schedulingState
}

// XXX in the real code we'll want to directly process the parsed events, not transform them to another
// representation. We're doing this only because it's easy to prototype with.
var sspans = map[uint64][]Span{}

// TODO(dh): avoid global state, bundle this in a Theme, much like gioui.org/widget/material does
var shaper text.Shaper

func main() {
	shaper = text.NewCache(gofont.Collection())

	r, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}
	res, err := trace.Parse(r, "")
	if err != nil {
		log.Fatal(err)
	}

	for _, ev := range res.Events {
		var g uint64
		var state schedulingState

		switch ev.Type {
		case trace.EvGoCreate:
			// ev.G creates ev.Args[0]
			g = ev.Args[0]
			state = stateInactive
		case trace.EvGoStart:
			// ev.G starts running
			g = ev.G
			state = stateActive
		case trace.EvGoStartLabel:
			// ev.G starts running
			// TODO(dh): make use of the label
			g = ev.G
			state = stateActive
		case trace.EvGoStop:
			// ev.G is stopping
			g = ev.G
			state = stateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			g = ev.G
			state = stateDone
		case trace.EvGoSched:
			// ev.G calls Gosched
			g = ev.G
			state = stateInactive
		case trace.EvGoSleep:
			// ev.G calls Sleep
			g = ev.G
			state = stateInactive
		case trace.EvGoPreempt:
			// ev.G got preempted
			g = ev.G
			state = stateInactive
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoBlockGC, trace.EvGoBlock:
			// ev.G is blocking
			g = ev.G
			state = stateBlocked
		case trace.EvGoWaiting:
			// ev.G is blocked when tracing starts
			g = ev.G
			state = stateBlocked
		case trace.EvGoUnblock:
			// ev.G is unblocking ev.Args[0]
			g = ev.Args[0]
			state = stateWaiting
		case trace.EvGoSysCall:
			// From the runtime's documentation:
			//
			// Syscall tracing:
			// At the start of a syscall we emit traceGoSysCall to capture the stack trace.
			// If the syscall does not block, that is it, we do not emit any other events.
			// If the syscall blocks (that is, P is retaken), retaker emits traceGoSysBlock;
			// when syscall returns we emit traceGoSysExit and when the goroutine starts running
			// (potentially instantly, if exitsyscallfast returns true) we emit traceGoStart.

			// TODO(dh): denote syscall somehow
			continue
		case trace.EvGoSysBlock:
			// TODO(dh): have a special state for this
			g = ev.G
			state = stateBlockedSyscall
		case trace.EvGoSysExit:
			g = ev.G
			state = stateWaiting
		case trace.EvGCMarkAssistStart:
			// TODO(dh): add a state for this
			continue
		case trace.EvGCMarkAssistDone:
			// TODO(dh): add a state for this
			continue
		case trace.EvProcStart, trace.EvProcStop:
			// TODO(dh): implement a per-proc timeline
			continue
		case trace.EvGCStart, trace.EvGCDone, trace.EvGCSTWStart, trace.EvGCSTWDone,
			trace.EvGCSweepStart, trace.EvGCSweepDone, trace.EvHeapAlloc, trace.EvHeapGoal:
			// TODO(dh): implement a GC timeline
			continue
		case trace.EvGomaxprocs:
			// TODO(dh): graph GOMAXPROCS
			continue
		case trace.EvUserTaskCreate, trace.EvUserTaskEnd, trace.EvUserRegion, trace.EvUserLog:
			// TODO(dh): implement a per-task timeline
			// TODO(dh): incorporate regions and logs in per-goroutine timeline
			continue
		default:
			panic(fmt.Sprintf("unhandled event %d", ev.Type))
		}

		if debug {
			if s := sspans[g]; len(s) > 0 {
				if len(s) == 1 && ev.Type == trace.EvGoWaiting && s[0].State == stateInactive {
					// The execution trace emits GoCreate + GoWaiting for goroutines that already exist at the start of
					// tracing if they're in a blocked state. This causes a transition from inactive to blocked, which we
					// wouldn't normally permit.
				} else {
					prevState := s[len(s)-1].State
					if !legalStateTransitions[prevState][state] {
						panic(fmt.Sprintf("illegal state transition %d -> %d for goroutine %d, offset %d", prevState, state, g, ev.Off))
					}
				}
			}
		}

		sspans[g] = append(sspans[g], Span{Start: time.Duration(ev.Ts), State: state})
	}

	for gid, spans := range sspans {
		for i := range spans[:len(spans)-1] {
			spans[i].End = spans[i+1].Start
		}
		last := spans[len(spans)-1]
		if last.State == stateDone {
			// The goroutine has ended
			// XXX the event probably has a stack associated with it, which we shouldn't discard.
			sspans[gid] = spans[:len(spans)-1]
		} else {
			// XXX somehow encode open-ended traces
			spans[len(spans)-1].End = time.Duration(res.Events[len(res.Events)-1].Ts)
		}
	}

	go func() {
		w := app.NewWindow()
		err := run(w)
		if err != nil {
			log.Fatal(err)
		}
		os.Exit(0)
	}()
	app.Main()
}

const (
	colorStateInactive       = 0x404040FF
	colorStateActive         = 0x00FF00FF
	colorStateBlocked        = 0xFF0000FF
	colorStateBlockedSyscall = 0x6F0000FF
	colorStateWaiting        = 0x0000FFFF
	colorStateStuck          = 0x000000FF
	colorStateUnknown        = 0xFFFF00FF
)

type schedulingState int

const (
	stateInactive schedulingState = iota
	stateActive
	stateBlocked
	stateBlockedSyscall
	stateStuck
	stateWaiting
	stateDone
	stateLast
)

var stateColors = [...]color.NRGBA{
	stateInactive:       toColor(colorStateInactive),
	stateActive:         toColor(colorStateActive),
	stateBlocked:        toColor(colorStateBlocked),
	stateBlockedSyscall: toColor(colorStateBlockedSyscall),
	stateStuck:          toColor(colorStateStuck),
	stateWaiting:        toColor(colorStateWaiting),
}

var legalStateTransitions = [stateLast][stateLast]bool{
	stateInactive: {
		stateActive:  true,
		stateWaiting: true,
	},
	stateActive: {
		stateInactive:       true,
		stateBlocked:        true,
		stateBlockedSyscall: true,
		stateStuck:          true,
		stateDone:           true,
	},
	stateWaiting: {
		stateActive: true,
	},
	stateBlocked: {
		stateWaiting: true,
	},
	stateBlockedSyscall: {
		stateWaiting: true,
	},
}

//gcassert:inline
func toColor(c uint32) color.NRGBA {
	// XXX does endianness matter?
	return color.NRGBA{
		A: uint8(c & 0xFF),
		B: uint8(c >> 8 & 0xFF),
		G: uint8(c >> 16 & 0xFF),
		R: uint8(c >> 24 & 0xFF),
	}
}

func run(w *app.Window) error {
	tl := Timeline{
		Start: 0,
		End:   100 * time.Millisecond,
	}

	// TODO(dh): handle window resize events and update nsPerPx
	var ops op.Ops
	for {
		e := <-w.Events()
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			gtx := layout.NewContext(&ops, ev)
			gtx.Constraints.Min = image.Point{}
			gtx.Metric.PxPerDp = 2 // XXX

			tl.Layout(gtx)

			ev.Frame(&ops)
		}
	}
}

func min(a, b int) int {
	if a <= b {
		return a
	} else {
		return b
	}
}

func max(a, b int) int {
	if a >= b {
		return a
	} else {
		return b
	}
}

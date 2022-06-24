package main

import (
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"honnef.co/go/gotraceui/trace"
)

const (
	minTickDistance = 48
	maxTickDistance = 144
	midTickDistance = (maxTickDistance - minTickDistance) / 2
)

const stateBarHeight = unit.Dp(10)

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration

	// State for dragging the timeline
	Drag struct {
		ClickAt f32.Point
		Start   time.Duration
		End     time.Duration
	}

	TickInterval time.Duration
}

func (tl *Timeline) startDrag(pos f32.Point) {
	tl.Drag.ClickAt = pos
	tl.Drag.Start = tl.Start
	tl.Drag.End = tl.End
}

func (tl *Timeline) dragTo(gtx layout.Context, pos f32.Point) {
	nsPerPx := tl.nsPerPx(gtx)
	d := time.Duration(math.Round(nsPerPx * float64(tl.Drag.ClickAt.X-pos.X)))
	if tl.Drag.Start+d < 0 {
		d = -tl.Drag.Start
	}
	tl.Start = tl.Drag.Start + d
	tl.End = tl.Drag.End + d
}

func (tl *Timeline) zoom(gtx layout.Context, ticks float32, at f32.Point) {
	nsPerPx := tl.nsPerPx(gtx)

	// TODO(dh): scale scroll amount by current zoom and by value of ev.Scroll.Y
	// XXX stop scrolling at some extreme point, so that nsperPx * our scroll multiplier is >=1
	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		tl.Start += time.Duration(nsPerPx * 100 * ratio)
		tl.End -= time.Duration(nsPerPx * 100 * (1 - ratio))
		if tl.End < 0 {
			tl.End = 0
		}
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		tl.Start -= time.Duration(nsPerPx * 100 * ratio)
		tl.End += time.Duration(nsPerPx * 100 * (1 - ratio))
		if tl.Start < 0 {
			tl.Start = 0
		}
	}
}

// The number of nanoseconds a pixel represents.
func (tl *Timeline) nsPerPx(gtx layout.Context) float64 {
	return float64(tl.End-tl.Start) / float64(gtx.Constraints.Max.X)
}

func (tl *Timeline) Layout(gtx layout.Context) layout.Dimensions {
	for _, ev := range gtx.Events(tl) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Type {
			case pointer.Press:
				if ev.Buttons&pointer.ButtonTertiary != 0 {
					tl.startDrag(ev.Position)
				}

			case pointer.Scroll:
				tl.zoom(gtx, ev.Scroll.Y, ev.Position)

			case pointer.Drag:
				if ev.Buttons&pointer.ButtonTertiary != 0 {
					tl.dragTo(gtx, ev.Position)
				}
			}
		}
	}

	log.Printf("displaying %s–%s (%ens per px)", tl.Start, tl.End, tl.nsPerPx(gtx))

	if tl.Start < 0 {
		panic("XXX")
	}
	if tl.End < tl.Start {
		panic("XXX")
	}
	if tl.nsPerPx(gtx) <= 0 {
		panic("XXX")
	}

	paint.ColorOp{Color: color.NRGBA{R: 0xAA, G: 0xAA, B: 0xAA, A: 0xFF}}.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)

	pointer.InputOp{Tag: tl, Types: pointer.Scroll | pointer.Drag | pointer.Press, ScrollBounds: image.Rectangle{Min: image.Point{-1, -1}, Max: image.Point{1, 1}}}.Add(gtx.Ops)

	// XXX make sure our rounding is stable and doesn't jitter
	// XXX handle spans that would be smaller than 1 unit

	nsPerPx := tl.nsPerPx(gtx)

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
			if first == last {
				panic("first == last")
			}

			widthOfSpan := func(s Span) float64 {
				return float64(s.End-s.Start) / nsPerPx
			}

			if first != -1 {
				spans := spans[first:last]
				for i := 0; i < len(spans); i++ {
					s := spans[i]
					startPx := float64(s.Start-tl.Start) / nsPerPx
					endPx := float64(s.End-tl.Start) / nsPerPx

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

							endPx = float64(spans[i].End-tl.Start) / nsPerPx
						}
					}

					// XXX .5 needs to be rounded up or down for the end or start
					rect := clip.Rect{
						Min: image.Point{max(int(math.Round(startPx)), 0), 0},
						Max: image.Point{min(int(math.Round(endPx)), gtx.Constraints.Max.X), int(stateBarHeight)},
					}
					stack := rect.Push(gtx.Ops)
					paint.ColorOp{Color: c}.Add(gtx.Ops)
					paint.PaintOp{}.Add(gtx.Ops)
					stack.Pop()
				}
			}
		}()
	}
	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

// TODO(dh): Support negative timeline. Right now, ts = 0 can only be displayed on the far left of the window, because
// we cannot render negative timestamps. That's inconvenient, we want to be able to focus on a span in the middle of our
// window. It would also make zooming out more intuitive, as it wouldn't shift the center once we're displaying the
// leftmost timestamp.

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

func main() {
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
		case trace.EvGoStop:
			// ev.G is stopping
			g = ev.G
			state = stateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			g = ev.G
			state = -1
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
		default:
			continue
		}

		sspans[g] = append(sspans[g], Span{Start: time.Duration(ev.Ts), State: state})
	}

	for gid, spans := range sspans {
		for i := range spans[:len(spans)-1] {
			spans[i].End = spans[i+1].Start
		}
		last := spans[len(spans)-1]
		if last.State == -1 {
			// The goroutine has ended
			// XXX the event probably has a stack associated with it, which we shouldn't discard.
			sspans[gid] = spans[:len(spans)-1]
		} else {
			// XXX somehow encode open-ended traces
			spans[len(spans)-1].End = time.Hour
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
	colorStateInactive = 0x404040FF
	colorStateActive   = 0x00FF00FF
	colorStateBlocked  = 0xFF0000FF
	colorStateWaiting  = 0x0000FFFF
	colorStateStuck    = 0x000000FF
	colorStateUnknown  = 0xFFFF00FF
)

type schedulingState int

const (
	stateInactive = iota
	stateActive
	stateBlocked
	stateStuck
	stateWaiting
)

var stateColors = [...]color.NRGBA{
	stateInactive: toColor(colorStateInactive),
	stateActive:   toColor(colorStateActive),
	stateBlocked:  toColor(colorStateBlocked),
	stateStuck:    toColor(colorStateStuck),
	stateWaiting:  toColor(colorStateWaiting),
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
		switch e := e.(type) {
		case system.DestroyEvent:
			return e.Err
		case system.FrameEvent:
			ops.Reset()
			gtx := layout.NewContext(&ops, e)
			gtx.Metric.PxPerDp = 2 // XXX

			tl.Layout(gtx)

			e.Frame(&ops)
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

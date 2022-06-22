package main

import (
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"time"

	"gioui.org/app"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"honnef.co/go/gotraceui/trace"
)

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
var spans []Span

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
		if ev.G != 3 {
			continue
		}

		var state schedulingState
		switch ev.Type {
		case trace.EvGoStart:
			state = stateActive
		case trace.EvGoStop, trace.EvGoEnd, trace.EvGoSched, trace.EvGoSleep:
			state = stateInactive
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoSysBlock, trace.EvGoBlockGC, trace.EvGoPreempt, trace.EvGoBlock:
			state = stateBlocked
		case trace.EvGoInSyscall:
			panic("unsupported")
		case trace.EvGoUnblockLocal:
			panic("unsupported")
		case trace.EvGoWaiting:
			state = stateBlocked
		case trace.EvGoUnblock:
			state = stateWaiting
		default:
			continue
		}

		spans = append(spans, Span{Start: time.Duration(ev.Ts), State: state})
	}

	if len(spans) == 0 {
		// XXX
		log.Fatal("boring trace")
	}

	for i := range spans[:len(spans)-1] {
		spans[i].End = spans[i+1].Start
	}
	// XXX what's the event for goroutine destruction?
	spans[len(spans)-1].End = time.Hour

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
	colorStateUnknown  = 0xFFFF00FF
)

type schedulingState int

const (
	stateInactive = iota
	stateActive
	stateBlocked
	stateWaiting
)

var stateColors = [...]color.NRGBA{
	stateInactive: toColor(colorStateInactive),
	stateActive:   toColor(colorStateActive),
	stateBlocked:  toColor(colorStateBlocked),
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
	var (
		// The region of the timeline that we're displaying, measured in nanoseconds
		tlStart, tlEnd time.Duration
		// The number of nanoseconds a pixel represents.
		nsPerPx float64
	)

	tlStart = 0
	// XXX find out how long the last span is
	tlEnd = 30 * time.Millisecond
	// tlEnd = spans[len(spans)-1].End

	const stateBarHeight = unit.Dp(10)

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

			for _, ev := range gtx.Queue.Events(&tlStart) {
				switch ev := ev.(type) {
				case pointer.Event:
					// TODO(dh): scale scroll amount by current zoom and by value of ev.Scroll.Y
					// TODO(dh): scroll centered on the mouse position
					// XXX stop scrolling at some extreme point, so that nsperPx * our scroll multiplier is >=1
					if ev.Scroll.Y < 0 {
						// Scrolling up, into the screen, zooming in
						tlStart += time.Duration(nsPerPx * 100)
						tlEnd -= time.Duration(nsPerPx * 100)
						if tlEnd < 0 {
							tlEnd = 0
						}
					} else if ev.Scroll.Y > 0 {
						// Scrolling down, out of the screen, zooming out
						tlStart -= time.Duration(nsPerPx * 100)
						tlEnd += time.Duration(nsPerPx * 100)
						if tlStart < 0 {
							tlStart = 0
						}
					}

				case key.Event:
					if ev.State == key.Press {
						// TODO(dh): don't move if we're at the limit
						switch ev.Name {
						case "←":
							tlStart -= time.Duration(10 * nsPerPx)
							tlEnd -= time.Duration(10 * nsPerPx)
							if tlStart < 0 {
								tlStart = 0
							}
						case "→":
							tlStart += time.Duration(10 * nsPerPx)
							tlEnd += time.Duration(10 * nsPerPx)
						}
					}
				}
			}

			nsPerPx = float64(tlEnd-tlStart) / float64(gtx.Constraints.Max.X)
			log.Printf("displaying %s–%s (%ens per px)", tlStart, tlEnd, nsPerPx)

			if tlStart < 0 {
				panic("XXX")
			}
			if tlEnd < tlStart {
				panic("XXX")
			}
			if nsPerPx <= 0 {
				panic("XXX")
			}

			pointer.InputOp{Tag: &tlStart, Types: pointer.Scroll, ScrollBounds: image.Rectangle{Min: image.Point{-1, -1}, Max: image.Point{1, 1}}}.Add(gtx.Ops)
			key.InputOp{Tag: &tlStart, Keys: "←|→"}.Add(gtx.Ops)

			// XXX make sure our rounding is stable and doesn't jitter
			// XXX handle spans that would be smaller than 1 unit

			// OPT(dh): use binary search
			first := -1
			last := -1
			for i, s := range spans {
				visible := (s.Start >= tlStart && s.Start < tlEnd) ||
					(s.End >= tlStart && s.End < tlEnd) ||
					(s.Start <= tlStart && s.End >= tlEnd)
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

			if first == -1 {
				log.Println("found no spans")
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
				log.Printf("rendering %d spans", len(spans[first:last]))

				spans := spans[first:last]
				for i := 0; i < len(spans); i++ {
					s := spans[i]
					startPx := float64(s.Start-tlStart) / nsPerPx
					endPx := float64(s.End-tlStart) / nsPerPx

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

							endPx = float64(spans[i].End-tlStart) / nsPerPx
						}
					}

					// XXX .5 needs to be rounded up or down for the end or start
					rect := clip.Rect{
						Min: image.Point{int(math.Round(startPx)), 0},
						Max: image.Point{int(math.Round(endPx)), int(stateBarHeight)},
					}
					stack := rect.Push(gtx.Ops)
					paint.ColorOp{Color: c}.Add(gtx.Ops)
					paint.PaintOp{}.Add(gtx.Ops)
					stack.Pop()
				}
			}

			e.Frame(&ops)
		}
	}
}

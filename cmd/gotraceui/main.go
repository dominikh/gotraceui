package main

import (
	"image"
	"image/color"
	"log"
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

/*
TODO:
   Components of a goroutine timeline:
   - markers displaying incoming/outgoing scheduling events and user events
   - thin bar displaying a goroutine's state (active, inactive, blocked, …)
     - clicking a segment of the bar should bring up information about that segment
       - duration of the segment, event that triggered the segment, event that triggered its end
   - area showing stack samples and user regions
     - clicking samples and regions should bring up information about them
   - info block fixed on the left side, displaying goroutine ID
     - clicking it should display information such as
       - event that created the goroutine
       - duration
       - time spent in the various states
       - counts of events etc

The various displays of information should start as popups that can be "undocked" into their own windows, MDI style.

*/

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
	colorStateUnknown  = 0xFF00FFFF
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
	// How much time one device-independent pixel represents
	timeStep := 200 * time.Nanosecond
	// Offset into the trace
	var offset time.Duration

	const stateBarHeight = unit.Dp(10)

	var ops op.Ops
	for {
		e := <-w.Events()
		switch e := e.(type) {
		case system.DestroyEvent:
			return e.Err
		case system.FrameEvent:
			ops.Reset()
			gtx := layout.NewContext(&ops, e)

			for _, ev := range gtx.Queue.Events(&timeStep) {
				ev := ev.(pointer.Event)
				d := timeStep / 100 * time.Duration(ev.Scroll.Y)
				if d == 0 {
					if ev.Scroll.Y < 0 {
						d = -1
					} else {
						d = 1
					}
				}
				timeStep += d
				if timeStep <= 0 {
					timeStep = 1
				}

			}

			for _, ev := range gtx.Queue.Events(&offset) {
				if ev, ok := ev.(key.Event); ok {
					if ev.State == key.Press {
						switch ev.Name {
						case "←":
							offset -= 10 * timeStep
							if offset < 0 {
								offset = 0
							}
						case "→":
							offset += 10 * timeStep
						}
					}
				}
			}

			pointer.InputOp{Tag: &timeStep, Types: pointer.Scroll, ScrollBounds: image.Rectangle{Min: image.Point{-1, -1}, Max: image.Point{1, 1}}}.Add(gtx.Ops)
			key.InputOp{Tag: &offset, Keys: "←|→"}.Add(gtx.Ops)

			// XXX make sure our rounding is stable and doesn't jitter
			// XXX handle spans that would be smaller than 1 unit

			var off int
			for _, s := range spans {
				if off > gtx.Constraints.Max.X {
					// No point drawing spans outside the visible region
					break
				}

				if s.End < offset {
					// OPT(dh): find starting point in spans based on offset, instead of always iterating from the beginning.
					continue
				}

				if s.Start < offset {
					s.Start = offset
				}

				width := gtx.Metric.Dp(unit.Dp((s.End - s.Start) / timeStep))
				// if width == 0 {
				// 	// XXX right now this screws up the timeline, because consecutive tiny spans will add up to
				// 	// significant size. We'll have to do what Tracy does and merge consecutive spans if we enlargened
				// 	// one.
				// 	width = 1
				// }
				end := off + width
				if end > gtx.Constraints.Max.X {
					end = gtx.Constraints.Max.X
				}
				stack := clip.Rect{
					Min: image.Point{off, 0},
					Max: image.Point{end, gtx.Metric.Dp(stateBarHeight)},
				}.Push(gtx.Ops)
				off += width

				var c color.NRGBA
				if int(s.State) >= len(stateColors) {
					c = toColor(colorStateUnknown)
				} else {
					c = stateColors[s.State]
				}
				paint.ColorOp{Color: c}.Add(gtx.Ops)
				paint.PaintOp{}.Add(gtx.Ops)

				stack.Pop()
			}

			e.Frame(&ops)
		}
	}
}

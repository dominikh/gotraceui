package main

import (
	"fmt"
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

type Span struct {
	Start int64
	End   int64
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

		spans = append(spans, Span{Start: ev.Ts * 1000, State: state})
	}

	if len(spans) == 0 {
		// XXX
		log.Fatal("boring trace")
	}

	for i := range spans[:len(spans)-1] {
		spans[i].End = spans[i+1].Start
	}
	// XXX what's the event for goroutine destruction?
	spans[len(spans)-1].End = int64(time.Hour) * 1000

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
	// How much time one device-independent pixel represents, in picoseconds
	timeStep := int64(1000000000000)
	// Offset into the trace, in picoseconds
	var offset int64

	const stateBarHeight = unit.Dp(10)

	var ops op.Ops
	for {
		e := <-w.Events()
		switch e := e.(type) {
		case system.DestroyEvent:
			return e.Err
		case system.FrameEvent:
			t := time.Now()

			ops.Reset()
			gtx := layout.NewContext(&ops, e)
			gtx.Metric.PxPerDp = 2 // XXX

			for _, ev := range gtx.Queue.Events(&timeStep) {
				ev := ev.(pointer.Event)
				d := timeStep / 100 * int64(ev.Scroll.Y)
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

				log.Printf("1 dp = %d ps", timeStep)
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

			widthInDp := unit.Dp(gtx.Constraints.Max.X / int(gtx.Metric.PxPerDp))
			widthInPs := int64(widthInDp) * timeStep

			log.Printf("timespan: %d - %d", offset, offset+widthInPs)

			// OPT(dh): use binary search
			first := -1
			last := -1
			for i, s := range spans {
				visible := (s.Start >= offset && s.Start < offset+widthInPs) ||
					(s.End >= offset && s.End < offset+widthInPs) ||
					(s.Start <= offset && s.End >= offset+widthInPs)
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

			if first != -1 {
				log.Printf("rendering %d spans", len(spans[first:last]))

				// OPT(dh): for large time steps and long traces, we may attempt to render hundreds of thousands of
				// spans, a lot of which may map to the same point on account of being tiny. We should introduce some
				// indexed data structure that lets us cull them.

				// prev records the previously drawn span, to handle drawing of overlapping spans. If spans do overlap,
				// prev will record the larger of the two.
				prev := clip.Rect{Min: image.Point{-1, -1}, Max: image.Point{-1, -1}}
				prevMerged := false

				for _, s := range spans[first:last] {
					// XXX if timeStep doesn't align cleanly with nanoseconds, then we need to round
					// XXX when multiple spans map to the same dp, then we shouldn't just overdraw, we should visually merge them
					startInPs := s.Start - offset
					endInPs := s.End - offset
					startInDp := unit.Dp(startInPs / timeStep)
					endInDp := unit.Dp(endInPs / timeStep)

					if startInDp < 0 {
						startInDp = 0
					}
					if endInDp > widthInDp {
						endInDp = widthInDp
					}

					if endInDp == startInDp {
						endInDp += 1
					}

					var c color.NRGBA
					if int(s.State) >= len(stateColors) {
						c = toColor(colorStateUnknown)
					} else {
						c = stateColors[s.State]
					}

					rect := clip.Rect{
						Min: image.Point{gtx.Metric.Dp(startInDp), 0},
						Max: image.Point{gtx.Metric.Dp(endInDp), gtx.Metric.Dp(stateBarHeight)},
					}

					// XXX this algorithm sucks. Changing the zoom level can change which spans overlap, simply due to
					// rounding errors. This means that while zooming in, spans may flicker between being merged and not
					// being merged. Can't we just fudge the pixels? Being a pixel or two off shouldn't matter, people
					// aren't using a ruler on their screen to measure the width of spans; and we already force spans to
					// be at least 1 pixel wide, which isn't accurate, either. At least we should be able to fudge those
					// spans that we didn't have to widen to be 1 pixel wide. For the others, we can't, because
					// otherwise the widening adds up.
					//
					// XXX another fun problem is spans in the leftmost column that are 0 pixels wide and get widened.
					// these are bound to overlap with the next span, causing to flicker when scrolling.
					//
					// XXX what if we just merge all spans that come out to a width of 0 and then do… something?
					if rect.Min.X == prev.Min.X {
						// Our spans - before being mapped to pixels - never overlap. A span n will always begin exactly
						// after span n-1 ends. When mapping to pixels, however, multiple spans may have the same
						// starting point, because they may round down to it. This is the only way in which spans can
						// overlap. Even in pixel space, span n cannot begin in the middle of span n-1, only its
						// beginning (or its end.)

						if prev == rect {
							if !prevMerged {
								// The two spans are exactly the same. We only need to draw the new span as a merged span,
								// which will hide the other span.
								stack := rect.Push(gtx.Ops)
								paint.ColorOp{Color: toColor(0xFF00FFFF)}.Add(gtx.Ops)
								paint.PaintOp{}.Add(gtx.Ops)
								stack.Pop()
								prevMerged = true
							}
						} else {
							if rect.Max.X > prev.Max.X {
								// The new span is wider than the previous span. Draw it as merged, hiding the previous span.
								c = toColor(0xFF00FFFF)
								stack := rect.Push(gtx.Ops)
								paint.ColorOp{Color: c}.Add(gtx.Ops)
								paint.PaintOp{}.Add(gtx.Ops)
								stack.Pop()
								prev = rect
							} else {
								// The previous span is wider than the new span. Redraw it as merged.
								c = toColor(0xFF00FFFF)
								stack := prev.Push(gtx.Ops)
								paint.ColorOp{Color: c}.Add(gtx.Ops)
								paint.PaintOp{}.Add(gtx.Ops)
								stack.Pop()
							}
						}
					} else {
						prev = rect
						prevMerged = false
						stack := rect.Push(gtx.Ops)
						paint.ColorOp{Color: c}.Add(gtx.Ops)
						paint.PaintOp{}.Add(gtx.Ops)
						stack.Pop()
					}
				}
			}

			fmt.Println(time.Since(t))
			e.Frame(&ops)
		}
	}
}

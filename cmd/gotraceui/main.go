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

// TODO(dh): switch to float32
// TODO(dh): use unit.Dp for all sizes

const debug = true

// XXX investigate if there can be gaps in goroutine IDs

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistance = 24
	tickHeight      = 50
	tickWidth       = 3
)

const minSpanWidth = spanBorderWidth*2 + unit.Dp(1)
const stateBarHeight = 20
const spanBorderWidth = unit.Dp(1)

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

	Global struct {
		cursorPos f32.Point
	}
	Goroutines struct {
		cursorPos f32.Point
	}
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
	// FIXME(dh): repeatedly zooming in and out doesn't cancel each other out. Fix that.
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

func (tl *Timeline) zoomToClickedSpan(gtx layout.Context, at f32.Point) {
	// XXX avoid magic constants
	// XXX don't allow clicking in the space between goroutines
	gid := uint64(at.Y / (stateBarHeight * 2))
	spans, ok := sspans[gid]
	if !ok {
		// Not a known goroutine
		return
	}

	spans = tl.visibleSpans(spans)
	it := renderedSpansIterator{
		tl:    tl,
		spans: spans,
	}
	for {
		dspSpans, startPx, endPx, ok := it.next(gtx)
		if !ok {
			break
		}

		start := dspSpans[0].Start
		end := dspSpans[len(dspSpans)-1].End

		if float64(at.X) >= startPx && float64(at.X) < endPx {
			tl.Start = start
			tl.End = end
			return
		}
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

func (tl *Timeline) visibleSpans(spans []Span) []Span {
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

	if first == -1 {
		return nil
	}
	return spans[first:last]
}

//gcassert:inline
func (tl *Timeline) tsToPx(gtx layout.Context, t time.Duration) float64 {
	return float64(t-tl.Start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(gtx layout.Context, px int) time.Duration {
	return time.Duration(math.Round(float64(px)*tl.nsPerPx + float64(tl.Start)))
}

type renderedSpansIterator struct {
	offset int
	tl     *Timeline
	spans  []Span
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spans []Span, startPx, endPx float64, ok bool) {
	if it.offset >= len(it.spans) {
		return nil, 0, 0, false
	}

	startOffset := it.offset

	s := it.spans[it.offset]
	it.offset++
	startPx = it.tl.tsToPx(gtx, s.Start)
	endPx = it.tl.tsToPx(gtx, s.End)
	if int(math.Round(endPx-startPx)) < gtx.Metric.Dp(minSpanWidth) {
		// Collect enough spans until we've filled the minimum width
		for {
			if it.offset == len(it.spans) {
				// We've run out of spans
				break
			}

			widthOfSpan := func(s Span) float64 {
				return float64(s.End-s.Start) / it.tl.nsPerPx
			}
			if int(math.Round(widthOfSpan(it.spans[it.offset]))) >= gtx.Metric.Dp(minSpanWidth) {
				// Don't merge spans that can stand on their own
				break
			}

			endPx = it.tl.tsToPx(gtx, it.spans[it.offset].End)
			it.offset++
		}
	}

	if int(math.Round(endPx-startPx)) < gtx.Metric.Dp(minSpanWidth) {
		endPx = startPx + float64(gtx.Metric.Dp(minSpanWidth))
	}

	return it.spans[startOffset:it.offset], startPx, endPx, true
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
				tl.Global.cursorPos = ev.Position
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
						tl.endZoomSelection(gtx, ev.Position)
					}
				}

			case pointer.Move:
				tl.Global.cursorPos = ev.Position
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
	paint.Fill(gtx.Ops, toColor(colorBackground))

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

	tl.layoutAxis(gtx)
	tl.layoutGoroutines(gtx)

	// Draw zoom selection
	if tl.ZoomSelection.Active {
		one := int(math.Round(float64(tl.ZoomSelection.ClickAt.X)))
		two := int(math.Round(float64(tl.Global.cursorPos.X)))
		rect := clip.Rect{
			Min: image.Point{X: min(one, two), Y: 0},
			Max: image.Point{X: max(one, two), Y: gtx.Constraints.Max.Y},
		}
		paint.FillShape(gtx.Ops, toColor(colorZoomSelection), rect.Op())
	}

	// Draw cursor
	rect := clip.Rect{
		Min: image.Point{X: int(math.Round(float64(tl.Global.cursorPos.X))), Y: 0},
		Max: image.Point{X: int(math.Round(float64(tl.Global.cursorPos.X + 1))), Y: gtx.Constraints.Max.Y},
	}
	paint.FillShape(gtx.Ops, toColor(colorCursor), rect.Op())

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (tl *Timeline) layoutAxis(gtx layout.Context) {
	// TODO don't bother with tl.Start and tl.End, we just have to subtract them in some places again. Just compute how
	// many ticks go in a line.
	//
	// TODO don't allow for labels to overlap
	tickInterval := tl.tickInterval(gtx)
	for t := tl.Start; t < tl.End; t += tickInterval {
		start := int(math.Round(tl.tsToPx(gtx, t) - tickWidth/2))
		end := int(math.Round(tl.tsToPx(gtx, t) + tickWidth/2))
		rect := clip.Rect{
			Min: image.Point{X: start, Y: 0},
			Max: image.Point{X: end, Y: tickHeight},
		}
		paint.FillShape(gtx.Ops, toColor(colorTick), rect.Op())

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
}

func (tl *Timeline) layoutGoroutines(gtx layout.Context) {
	defer op.Offset(image.Point{Y: tickHeight * 2}).Push(gtx.Ops).Pop()
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	// Dragging doesn't produce Move events, even if we're not listening for dragging
	pointer.InputOp{Tag: &tl.Goroutines, Types: pointer.Move | pointer.Drag | pointer.Press}.Add(gtx.Ops)
	for _, e := range gtx.Events(&tl.Goroutines) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Move, pointer.Drag:
			tl.Goroutines.cursorPos = ev.Position
		case pointer.Press:
			if ev.Modifiers&key.ModCtrl != 0 {
				tl.zoomToClickedSpan(gtx, ev.Position)
			}
		}
	}

	// XXX make sure our rounding is stable and doesn't jitter

	var tooltip []Span

	// Draw goroutine lifetimes
	for gid, spans := range sspans {
		if gid > 100 {
			continue
		}
		func() {
			// Our goroutines aren't sorted, causing our offsets to jump all over the place. That's why we calculate
			// absolute offsets and pop them after each iteration.
			defer op.Offset(image.Point{X: 0, Y: stateBarHeight * 2 * int(gid)}).Push(gtx.Ops).Pop()
			it := renderedSpansIterator{
				tl:    tl,
				spans: tl.visibleSpans(spans),
			}
			first := true
			for {
				dspSpans, startPx, endPx, ok := it.next(gtx)
				if !ok {
					break
				}

				var c color.NRGBA
				if len(dspSpans) == 1 {
					s := dspSpans[0]
					if int(s.State) >= len(stateColors) {
						c = toColor(colorStateUnknown)
					} else {
						c = stateColors[s.State]
					}
				} else {
					c = toColor(colorStateUnknown) // XXX use different color
				}

				rect := clip.Rect{
					Min: image.Point{max(int(math.Round(startPx)), 0), 0},
					Max: image.Point{min(int(math.Round(endPx)), gtx.Constraints.Max.X), stateBarHeight},
				}
				paint.FillShape(gtx.Ops, toColor(0x000000FF), rect.Op())
				if first {
					// We don't want two borders right next to each other
					rect.Min.X += gtx.Metric.Dp(spanBorderWidth)
				}
				rect.Min.Y += gtx.Metric.Dp(spanBorderWidth)
				rect.Max.X -= gtx.Metric.Dp(spanBorderWidth)
				rect.Max.Y -= gtx.Metric.Dp(spanBorderWidth)
				paint.FillShape(gtx.Ops, c, rect.Op())

				// XXX Make sure this is the goroutine under point

				if float64(tl.Goroutines.cursorPos.X) >= startPx && float64(tl.Goroutines.cursorPos.X) < endPx &&
					// XXX factor out the math for finding the goroutine from the Y position, the same is used for clicking spans
					// XXX consider the padding between goroutines
					tl.Goroutines.cursorPos.Y >= float32(stateBarHeight*2*gid) && tl.Goroutines.cursorPos.Y < float32(stateBarHeight*2*(gid+1)) {
					tooltip = dspSpans
				}

				first = false
			}
		}()
	}

	if len(tooltip) != 0 {
		// TODO have a gap between the cursor and the tooltip
		// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
		macro := op.Record(gtx.Ops)
		SpanTooltip{tooltip}.Layout(gtx)
		call := macro.Stop()
		defer op.Offset(tl.Goroutines.cursorPos.Round()).Push(gtx.Ops).Pop()
		call.Add(gtx.Ops)
	}
}

type SpanTooltip struct {
	Spans []Span
}

func (tt SpanTooltip) Layout(gtx layout.Context) layout.Dimensions {
	// XXX support tooltips covering multiple spans
	label := "State: "
	if len(tt.Spans) == 1 {
		switch state := tt.Spans[0].State; state {
		case stateInactive:
			label += "inactive"
		case stateActive:
			label += "active"
		case stateBlocked:
			label += "blocked"
		case stateBlockedSyscall:
			label += "syscall (blocked)"
		case stateStuck:
			label += "stuck"
		case stateWaiting:
			label += "waiting"
		default:
			panic(fmt.Sprintf("unhandled state %d", state))
		}
	} else {
		label += "mixed"
	}
	label += "\n"
	d := tt.Spans[len(tt.Spans)-1].End - tt.Spans[0].Start
	label += fmt.Sprintf("Duration: %s", d)

	// TODO(dh): display reason why we're in this state
	// TODO(dh): make tooltip actually look good

	macro := op.Record(gtx.Ops)
	paint.ColorOp{Color: toColor(colorTooltipText)}.Add(gtx.Ops)
	// XXX can we ensure that widget.Label only uses our newlines and doesn't attempt to word-wrap for us?
	dims := widget.Label{}.Layout(gtx, shaper, text.Font{}, 14, label)
	call := macro.Stop()

	rect := clip.Rect{
		Max: dims.Size,
	}
	paint.FillShape(gtx.Ops, toColor(colorTooltipBackground), rect.Op())
	call.Add(gtx.Ops)

	return dims
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

//gcassert:inline
func (s Span) Duration() time.Duration {
	return s.End - s.Start
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
	colorStateInactive       = 0x888888FF
	colorStateActive         = 0x448844FF
	colorStateBlocked        = 0xbb5d5dFF
	colorStateBlockedSyscall = 0x6F0000FF
	colorStateWaiting        = 0x4BACB8FF
	colorStateStuck          = 0x000000FF
	colorStateUnknown        = 0xFFFF00FF
)

const (
	colorBackground        = 0xffffeaFF
	colorZoomSelection     = 0xeeee9e99
	colorCursor            = 0x000000FF
	colorTick              = 0x000000FF
	colorTooltipText       = 0x000000FF
	colorTooltipBackground = 0xFFFFFFFF
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

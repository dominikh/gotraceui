package main

import (
	"fmt"
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"time"

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
	"gioui.org/unit"
	"gioui.org/widget"
)

// TODO(dh): switch to float32
// TODO(dh): use unit.Dp for all sizes
// TODO(dh): figure out what puts us in the generic "blocked" state

// TODO(dh): the Event.Stk is meaningless for goroutines that already existed when tracing started, i.e. ones that get a
// GoWaiting event. The GoCreate event will be caused by starting the trace, and the stack of the event will be that
// leading up to starting the trace. It will in no way reflect the code that actually, historically, started the
// goroutine. To avoid confusion, we should remove those stacks altogether.

// TODO(dh): Go 1.19 adds CPU samples to the execution trace (if profiling is enabled). This adds the new event
// EvCPUSample, and updates the trace's version to Go 1.19.

const debug = true

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistance = 24
	tickHeight      = 50
	tickWidth       = 3
)

const minSpanWidth = spanBorderWidth*2 + unit.Dp(1)

const (
	goroutineHeight = 20
	goroutineGap    = 10
)

const spanBorderWidth = unit.Dp(1)
const minTickLabelDistance = 15

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration
	// Imagine we're drawing all goroutines onto an infinitely long canvas. Timeline.Y specifies the Y of that infinite
	// canvas that the goroutine section's Y == 0 is displaying.
	Y int

	// State for dragging the timeline
	Drag struct {
		ClickAt f32.Point
		Active  bool
		Start   time.Duration
		End     time.Duration
		StartY  int
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
	start := tl.pxToTs(min(one, two))
	end := tl.pxToTs(max(one, two))
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
	tl.Drag.StartY = tl.Y
}

func (tl *Timeline) endDrag() {
	tl.Drag.Active = false
}

func (tl *Timeline) dragTo(gtx layout.Context, pos f32.Point) {
	td := time.Duration(math.Round(tl.nsPerPx * float64(tl.Drag.ClickAt.X-pos.X)))
	tl.Start = tl.Drag.Start + td
	tl.End = tl.Drag.End + td

	yd := int(math.Round(float64(tl.Drag.ClickAt.Y - pos.Y)))
	tl.Y = tl.Drag.StartY + yd
	if tl.Y < 0 {
		tl.Y = 0
	}
	// XXX don't allow dragging tl.Y beyond the end
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

// gidAtPoint returns the goroutine ID at a point. The point should be relative to the
// goroutine section of the timeline.
func (tl *Timeline) gidAtPoint(at f32.Point) (uint64, bool) {
	if !tl.isOnGoroutine(at) {
		return 0, false
	}
	return uint64((at.Y + float32(tl.Y)) / (goroutineHeight + goroutineGap)), true
}

// isOnGoroutine reports whether there's a goroutine under a point. The point should be relative to the goroutine
// section of the timeline.
func (tl *Timeline) isOnGoroutine(at f32.Point) bool {
	rem := math.Mod(float64(at.Y)+float64(tl.Y), (goroutineHeight + goroutineGap))
	return rem <= goroutineHeight
}

// zoomToCLickedSpan zooms to the span at a point, if any. The point should be relative to the goroutine section of the
// timeline.
func (tl *Timeline) zoomToClickedSpan(gtx layout.Context, at f32.Point) {
	// XXX avoid magic constants
	gid, ok := tl.gidAtPoint(at)
	if !ok {
		return
	}
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
func (tl *Timeline) tsToPx(t time.Duration) float64 {
	return float64(t-tl.Start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(px int) time.Duration {
	return time.Duration(math.Round(float64(px)*tl.nsPerPx + float64(tl.Start)))
}

type renderedSpansIterator struct {
	offset int
	tl     *Timeline
	spans  []Span
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut []Span, startPx, endPx float64, ok bool) {
	offset := it.offset
	spans := it.spans

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	minSpanWidthDp := gtx.Metric.Dp(minSpanWidth)
	startOffset := offset
	nsPerPx := it.tl.nsPerPx
	tlStart := it.tl.Start

	s := it.spans[offset]
	offset++
	startPx = float64(s.Start-tlStart) / nsPerPx
	endPx = float64(s.End-tlStart) / nsPerPx

	if int(math.Round(endPx-startPx)) < minSpanWidthDp {
		// Collect enough spans until we've filled the minimum width
		for {
			if offset == len(it.spans) {
				// We've run out of spans
				break
			}

			s := spans[offset]
			widthOfSpan := int(math.Round(float64(s.End-s.Start) / nsPerPx))
			if widthOfSpan >= minSpanWidthDp {
				// Don't merge spans that can stand on their own
				break
			}

			endPx = float64(s.End-tlStart) / nsPerPx
			offset++
		}
	}

	if int(math.Round(endPx-startPx)) < minSpanWidthDp {
		endPx = startPx + float64(minSpanWidthDp)
	}

	it.offset = offset
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
	// TODO don't allow for labels to overlap
	// TODO draw smaller ticks between larger ticks
	tickInterval := tl.tickInterval(gtx)
	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	prevLabelEnd := -1
	for t := tl.Start; t < tl.End; t += tickInterval {
		start := int(math.Round(tl.tsToPx(t) - tickWidth/2))
		end := int(math.Round(tl.tsToPx(t) + tickWidth/2))
		rect := clip.Rect{
			Min: image.Point{X: start, Y: 0},
			Max: image.Point{X: end, Y: tickHeight},
		}
		paint.FillShape(gtx.Ops, toColor(colorTick), rect.Op())

		if t == tl.Start {
			// TODO print thousands separator
			label := fmt.Sprintf("%d ns", t)
			stack := op.Offset(image.Point{Y: tickHeight}).Push(gtx.Ops)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, shaper, text.Font{}, 14, label)
			prevLabelEnd = dims.Size.X
			stack.Pop()
		} else {
			macro := op.Record(gtx.Ops)
			// TODO separate value and unit symbol with a space
			label := fmt.Sprintf("+%s", t-tl.Start)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, shaper, text.Font{}, 14, label)
			call := macro.Stop()

			if start-dims.Size.X/2 > prevLabelEnd+minTickLabelDistance {
				prevLabelEnd = start + dims.Size.X/2
				if start+dims.Size.X/2 <= gtx.Constraints.Max.X {
					stack := op.Offset(image.Point{
						X: start - dims.Size.X/2,
						Y: tickHeight,
					}).Push(gtx.Ops)
					call.Add(gtx.Ops)
					stack.Pop()
				}
			}
		}
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
	// TODO(dh): draw a scrollbar
	for gid, spans := range sspans {
		y := (goroutineHeight+goroutineGap)*int(gid) - tl.Y
		if y < -goroutineHeight || y > gtx.Constraints.Max.Y {
			// Don't draw goroutines that would be fully hidden, but do draw partially hidden ones
			continue
		}
		func() {
			// Our goroutines aren't sorted, causing our offsets to jump all over the place. That's why we calculate
			// absolute offsets and pop them after each iteration.
			defer op.Offset(image.Point{X: 0, Y: y}).Push(gtx.Ops).Pop()

			gidAtPoint, isOnGoroutine := tl.gidAtPoint(tl.Goroutines.cursorPos)

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
					c = toColor(colorStateMerged)
				}

				rect := clip.Rect{
					Min: image.Point{max(int(math.Round(startPx)), 0), 0},
					Max: image.Point{min(int(math.Round(endPx)), gtx.Constraints.Max.X), goroutineHeight},
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

				if float64(tl.Goroutines.cursorPos.X) >= startPx && float64(tl.Goroutines.cursorPos.X) < endPx && isOnGoroutine && gidAtPoint == gid {
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
	label := "State: "
	if len(tt.Spans) == 1 {
		switch state := tt.Spans[0].State; state {
		case stateInactive:
			label += "inactive"
			label += "\nReason: " + tt.Spans[0].Reason
		case stateActive:
			label += "active"
		case stateBlocked:
			label += "blocked"
		case stateBlockedSend:
			label += "blocked on channel send"
		case stateBlockedRecv:
			label += "blocked on channel recv"
		case stateBlockedSelect:
			label += "blocked on select"
		case stateBlockedSync:
			label += "blocked on mutex"
		case stateBlockedCond:
			label += "blocked on condition variable"
		case stateBlockedNet:
			label += "blocked on network"
		case stateBlockedGC:
			label += "blocked on GC assist"
		case stateBlockedSyscall:
			label += "blocked on syscall"
		case stateStuck:
			label += "stuck"
		case stateWaiting:
			label += "waiting"
		default:
			panic(fmt.Sprintf("unhandled state %d", state))
		}
	} else {
		label += fmt.Sprintf("mixed (%d spans)", len(tt.Spans))
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
	// TODO(dh): use an enum for Reason
	Reason string
}

//gcassert:inline
func (s Span) Duration() time.Duration {
	return s.End - s.Start
}

// TODO(dh): avoid global state
var sspans = map[uint64][]Span{}

// TODO(dh): avoid global state, bundle this in a Theme, much like gioui.org/widget/material does
var shaper text.Shaper

func main() {
	shaper = text.NewCache(gofont.Collection())

	r, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Loading trace...")

	res, err := trace.Parse(r, "")
	if err != nil {
		log.Fatal(err)
	}

	var evTypeToState = [...]schedulingState{
		trace.EvGoBlockSend:   stateBlockedSend,
		trace.EvGoBlockRecv:   stateBlockedRecv,
		trace.EvGoBlockSelect: stateBlockedSelect,
		trace.EvGoBlockSync:   stateBlockedSync,
		trace.EvGoBlockCond:   stateBlockedCond,
		trace.EvGoBlockNet:    stateBlockedNet,
		trace.EvGoBlockGC:     stateBlockedGC,
		trace.EvGoBlock:       stateBlocked,
	}

	for _, ev := range res.Events {
		var g uint64
		var state schedulingState
		var reason string

		switch ev.Type {
		case trace.EvGoCreate:
			// ev.G creates ev.Args[0]
			g = ev.Args[0]
			state = stateInactive
			reason = "newly created"
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
			reason = "called runtime.Gosched"
		case trace.EvGoSleep:
			// ev.G calls Sleep
			g = ev.G
			state = stateInactive
			reason = "called time.Sleep"
		case trace.EvGoPreempt:
			// ev.G got preempted
			g = ev.G
			state = stateInactive
			reason = "got preempted"
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoBlockGC, trace.EvGoBlock:
			// ev.G is blocking
			g = ev.G
			state = evTypeToState[ev.Type]
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

		sspans[g] = append(sspans[g], Span{Start: time.Duration(ev.Ts), State: state, Reason: reason})
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

	fmt.Println("Starting UI...")

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
	colorStateInactive = 0x888888FF
	colorStateActive   = 0x448844FF

	colorStateBlocked              = 0xBA4141FF
	colorStateBlockedHappensBefore = 0xBB6363FF
	colorStateBlockedNet           = 0xBB5D5DFF
	colorStateBlockedGC            = 0xBB554FFF
	colorStateBlockedSyscall       = 0xBA4F41FF

	colorStateWaiting = 0x4BACB8FF
	colorStateStuck   = 0x000000FF
	colorStateMerged  = 0xB9BB63FF
	colorStateUnknown = 0xFFFF00FF
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
	stateBlockedSend
	stateBlockedRecv
	stateBlockedSelect
	stateBlockedSync
	stateBlockedCond
	stateBlockedNet
	stateBlockedGC
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
	stateBlockedSend:    toColor(colorStateBlockedHappensBefore),
	stateBlockedRecv:    toColor(colorStateBlockedHappensBefore),
	stateBlockedSelect:  toColor(colorStateBlockedHappensBefore),
	stateBlockedSync:    toColor(colorStateBlockedHappensBefore),
	stateBlockedCond:    toColor(colorStateBlockedHappensBefore),
	stateBlockedNet:     toColor(colorStateBlockedNet),
	stateBlockedGC:      toColor(colorStateBlockedGC),
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
		stateBlockedSend:    true,
		stateBlockedRecv:    true,
		stateBlockedSelect:  true,
		stateBlockedSync:    true,
		stateBlockedCond:    true,
		stateBlockedNet:     true,
		stateBlockedGC:      true,
		stateBlockedSyscall: true,
		stateStuck:          true,
		stateDone:           true,
	},
	stateWaiting: {
		stateActive: true,
	},
	stateBlocked:       {stateWaiting: true},
	stateBlockedSend:   {stateWaiting: true},
	stateBlockedRecv:   {stateWaiting: true},
	stateBlockedSelect: {stateWaiting: true},
	stateBlockedSync:   {stateWaiting: true},
	stateBlockedCond:   {stateWaiting: true},
	stateBlockedNet:    {stateWaiting: true},
	stateBlockedGC:     {stateWaiting: true},
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
	var end time.Duration
	for _, spans := range sspans {
		if len(spans) > 0 {
			d := spans[len(spans)-1].End
			if d > end {
				end = d
			}
		}
	}

	// Zoom out slightly beyond the end of the trace, so that the user can immediately tell that they're looking at the
	// entire trace.
	slack := float64(end) * 0.05
	start := time.Duration(-slack)
	end = time.Duration(float64(end) + slack)
	tl := Timeline{
		Start: start,
		End:   end,
	}

	profileTag := new(int)
	var ops op.Ops
	for {
		e := <-w.Events()
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			gtx := layout.NewContext(&ops, ev)
			gtx.Constraints.Min = image.Point{}
			// XXX detect HiDPI
			gtx.Metric.PxPerDp = 2

			for _, ev := range gtx.Events(profileTag) {
				fmt.Println(ev)
			}
			profile.Op{Tag: profileTag}.Add(gtx.Ops)

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

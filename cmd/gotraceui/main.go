package main

import (
	"bufio"
	"fmt"
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"sort"
	"strings"
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
	"gioui.org/widget/material"
)

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
	minTickDistanceDp      unit.Dp = 20
	tickHeightDp           unit.Dp = 12
	tickWidthDp            unit.Dp = 1
	minTickLabelDistanceDp unit.Dp = 8
	tickLabelFontSizeSp    unit.Sp = 14

	// XXX the label height depends on the font used
	goroutineLabelHeightDp unit.Dp = 20
	goroutineStateHeightDp unit.Dp = 12
	goroutineGapDp         unit.Dp = 5
	goroutineHeightDp      unit.Dp = goroutineStateHeightDp + goroutineLabelHeightDp

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 4

	spanBorderWidthDp unit.Dp = 1

	tooltipFontSizeSp unit.Sp = 14

	goroutineLabelFontSizeSp unit.Sp = 14
)

type Axis struct {
	tl *Timeline

	prevFrame struct {
		ops    op.Ops
		call   op.CallOp
		labels []string
		dims   layout.Dimensions
	}
}

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration
	// Imagine we're drawing all goroutines onto an infinitely long canvas. Timeline.Y specifies the Y of that infinite
	// canvas that the goroutine section's Y == 0 is displaying.
	Y int

	Gs []*GoroutineWidget

	Theme *material.Theme

	Scrollbar widget.Scrollbar
	Axis      Axis

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
	nsPerPx float32

	Global struct {
		cursorPos f32.Point
	}
	Goroutines struct {
		DisplayAllLabels bool
		Compact          bool
		_                [0]int
	}

	// prevFrame records the timeline's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the timeline hasn't changed.
	prevFrame struct {
		Start        time.Duration
		End          time.Duration
		Y            int
		nsPerPx      float32
		compact      bool
		displayedGws []*GoroutineWidget
		dspSpans     map[uint64][]struct {
			dspSpans       []Span
			startPx, endPx float32
		}
	}
}

func (tl *Timeline) unchanged() bool {
	return tl.prevFrame.Start == tl.Start && tl.prevFrame.End == tl.End && tl.prevFrame.nsPerPx == tl.nsPerPx && tl.prevFrame.Y == tl.Y && tl.prevFrame.compact == tl.Goroutines.Compact
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
	one := tl.ZoomSelection.ClickAt.X
	two := pos.X
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
	td := time.Duration(round32(tl.nsPerPx * (tl.Drag.ClickAt.X - pos.X)))
	tl.Start = tl.Drag.Start + td
	tl.End = tl.Drag.End + td

	yd := int(round32(tl.Drag.ClickAt.Y - pos.Y))
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
		ratio := at.X / float32(gtx.Constraints.Max.X)
		tl.Start += time.Duration(tl.nsPerPx * 100 * ratio)
		tl.End -= time.Duration(tl.nsPerPx * 100 * (1 - ratio))
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := at.X / float32(gtx.Constraints.Max.X)
		tl.Start -= time.Duration(tl.nsPerPx * 100 * ratio)
		tl.End += time.Duration(tl.nsPerPx * 100 * (1 - ratio))
	}
}

func (tl *Timeline) goroutineHeight(gtx layout.Context) int {
	if tl.Goroutines.Compact {
		return gtx.Metric.Dp(goroutineHeightDp) - gtx.Metric.Dp(goroutineLabelHeightDp)
	} else {
		return gtx.Metric.Dp(goroutineHeightDp)
	}
}

func (tl *Timeline) visibleSpans(spans []Span) []Span {
	// Visible spans have to end after tl.Start and begin before tl.End
	start := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.End > tl.Start
	})
	if start == len(spans) {
		return nil
	}
	end := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.Start >= tl.End
	})

	return spans[start:end]
}

//gcassert:inline
func (tl *Timeline) tsToPx(t time.Duration) float32 {
	return float32(t-tl.Start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(px float32) time.Duration {
	return time.Duration(round32(px*tl.nsPerPx + float32(tl.Start)))
}

type renderedSpansIterator struct {
	offset         int
	tl             *Timeline
	spans          []Span
	prevExtendedBy float32
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut []Span, startPx, endPx float32, ok bool) {
	offset := it.offset
	spans := it.spans

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	minSpanWidth := float32(gtx.Metric.Dp(minSpanWidthDp))
	startOffset := offset
	nsPerPx := float32(it.tl.nsPerPx)
	tlStart := it.tl.Start

	s := it.spans[offset]
	offset++
	startPx = float32(s.Start-tlStart)/nsPerPx + it.prevExtendedBy
	endPx = float32(s.End-tlStart) / nsPerPx

	if endPx-startPx < minSpanWidth {
		// Collect enough spans until we've filled the minimum width

		// Compute the minimum duration for a span to stand on its own. We subtract 1 from minSpanWidth to account for
		// lucky rounding to pixel boundaries.
		minStandaloneDuration := (minSpanWidth - 1) * nsPerPx

		for {
			if offset == len(it.spans) {
				// We've run out of spans
				break
			}

			s := spans[offset]
			if float32(s.End-s.Start) < minStandaloneDuration {
				// Even under ideal conditions - no extension and no truncation - this span wouldn't be able to stand on
				// its own. Avoid doing expensive math.
			} else {
				// Assume that we stop at this span. Compute the final size and the future prevExtendedBy. Use that to see
				// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
				var extended float32
				if endPx-startPx < minSpanWidth {
					extended = minSpanWidth - (endPx - startPx)
				}
				theirStartPx := float32(s.Start-tlStart)/nsPerPx + extended
				theirEndPx := float32(s.End-tlStart) / nsPerPx
				if theirEndPx-theirStartPx >= minSpanWidth {
					// Don't merge spans that can stand on their own
					break
				}
			}

			endPx = float32(s.End-tlStart) / nsPerPx
			offset++
		}
	}

	if endPx-startPx < minSpanWidth {
		it.prevExtendedBy = minSpanWidth - (endPx - startPx)
		endPx = startPx + minSpanWidth
	} else {
		it.prevExtendedBy = 0
	}

	it.offset = offset
	return it.spans[startOffset:it.offset], startPx, endPx, true
}

func Stack(gtx layout.Context, widgets ...layout.Widget) {
	for _, w := range widgets {
		dims := w(gtx)
		gtx.Constraints.Max.Y -= dims.Size.Y
		defer op.Offset(image.Pt(0, dims.Size.Y)).Push(gtx.Ops).Pop()
	}
}

func (tl *Timeline) zoomToFitCurrentView(gtx layout.Context) {
	var first, last time.Duration = -1, -1
	for _, gw := range tl.visibleGoroutines(gtx) {
		if len(gw.g.Spans) == 0 {
			continue
		}
		if t := gw.g.Spans[0].Start; t < first || first == -1 {
			first = t
		}
		if t := gw.g.Spans[len(gw.g.Spans)-1].End; t > last {
			last = t
		}
	}
	if first != -1 && last == -1 {
		panic("unreachable")
	}
	tl.Start = first
	tl.End = last
}

func (tl *Timeline) Layout(gtx layout.Context) layout.Dimensions {
	for _, ev := range gtx.Events(tl) {
		switch ev := ev.(type) {
		case key.Event:
			if ev.State == key.Press {
				switch ev.Name {
				case key.NameHome:
					switch {
					case ev.Modifiers&key.ModShift != 0:
						tl.zoomToFitCurrentView(gtx)
					case ev.Modifiers&key.ModCtrl != 0:
						d := tl.End - tl.Start
						tl.Start = 0
						tl.End = tl.Start + d
					case ev.Modifiers == 0:
						tl.Y = 0
					}

				case "X":
					tl.Goroutines.DisplayAllLabels = !tl.Goroutines.DisplayAllLabels

				case "C":
					// FIXME(dh): adjust tl.Y so that the top visible goroutine stays the same
					tl.Goroutines.Compact = !tl.Goroutines.Compact
				}
			}
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

	{
		goroutineHeight := tl.goroutineHeight(gtx)
		goroutineGap := gtx.Metric.Dp(goroutineGapDp)
		// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
		d := tl.Scrollbar.ScrollDistance()
		totalHeight := float32(len(gs) * (goroutineHeight + goroutineGap))
		tl.Y += int(round32(d * totalHeight))
		if tl.Y < 0 {
			tl.Y = 0
		}
	}

	for _, gw := range tl.prevFrame.displayedGws {
		if spans := gw.ClickedSpans; len(spans) > 0 {
			start := spans[0].Start
			end := spans[len(spans)-1].End
			tl.Start = start
			tl.End = end
			break
		}
	}

	// FIXME(dh): the axis is wider than the canvas because of a scrollbar. this means that tl.End is slightly outside
	// the visible area. that's generally fine, but means that zooming to a span, or to fit the visible goroutines, is
	// off by a couple pixels.

	tl.nsPerPx = float32(tl.End-tl.Start) / float32(gtx.Constraints.Max.X)

	if debug {
		if tl.End < tl.Start {
			panic("XXX")
		}
		if tl.nsPerPx <= 0 {
			panic("XXX")
		}
	}

	// Fill background
	paint.Fill(gtx.Ops, colors[colorBackground])

	// Set up event handlers
	pointer.InputOp{
		Tag: tl,
		Types: pointer.Scroll |
			pointer.Drag |
			pointer.Press |
			pointer.Release |
			pointer.Move,
		ScrollBounds: image.Rectangle{Min: image.Pt(-1, -1), Max: image.Pt(1, 1)},
	}.Add(gtx.Ops)
	key.InputOp{Tag: tl, Keys: "C|X|(Shift)-(Ctrl)-" + key.NameHome}.Add(gtx.Ops)
	key.FocusOp{Tag: tl}.Add(gtx.Ops)

	// Draw axis and goroutines
	Stack(gtx, tl.Axis.Layout, func(gtx layout.Context) layout.Dimensions {
		dims, gws := tl.layoutGoroutines(gtx)
		tl.prevFrame.displayedGws = gws
		return dims
	})

	// Draw zoom selection
	if tl.ZoomSelection.Active {
		one := tl.ZoomSelection.ClickAt.X
		two := tl.Global.cursorPos.X
		rect := FRect{
			Min: f32.Pt(min(one, two), 0),
			Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
		}
		paint.FillShape(gtx.Ops, colors[colorZoomSelection], rect.Op(gtx.Ops))
	}

	// Draw cursor
	rect := clip.Rect{
		Min: image.Pt(int(round32(tl.Global.cursorPos.X)), 0),
		Max: image.Pt(int(round32(tl.Global.cursorPos.X+1)), gtx.Constraints.Max.Y),
	}
	paint.FillShape(gtx.Ops, colors[colorCursor], rect.Op())

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (a *Axis) tickInterval(gtx layout.Context) time.Duration {
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	minTickDistance := gtx.Metric.Dp(minTickDistanceDp)
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(round32(float32(t) / a.tl.nsPerPx))
		if tickDistance >= minTickDistance {
			return t
		}
	}
	panic("unreachable")
}

func (a *Axis) Layout(gtx layout.Context) (dims layout.Dimensions) {
	// TODO draw smaller ticks between larger ticks
	tickInterval := a.tickInterval(gtx)
	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	prevLabelEnd := float32(-1)
	// TODO(dh): calculating the label height on each frame risks that it changes between frames, which will cause the
	// goroutines to shift around as the axis section grows and shrinks.
	labelHeight := 0
	tickWidth := float32(gtx.Metric.Dp(tickWidthDp))
	tickHeight := float32(gtx.Metric.Dp(tickHeightDp))
	minTickLabelDistance := float32(gtx.Metric.Dp(minTickLabelDistanceDp))

	var labels []string
	if a.tl.unchanged() {
		a.prevFrame.call.Add(gtx.Ops)
		return a.prevFrame.dims
	} else if a.tl.prevFrame.nsPerPx == a.tl.nsPerPx {
		// Panning only changes the first label
		labels = a.prevFrame.labels
		// TODO print thousands separator
		labels[0] = fmt.Sprintf("%d ns", a.tl.Start)
	} else {
		for t := a.tl.Start; t < a.tl.End; t += tickInterval {
			if t == a.tl.Start {
				// TODO print thousands separator
				labels = append(labels, fmt.Sprintf("%d ns", t))
			} else {
				// TODO separate value and unit symbol with a space
				labels = append(labels, fmt.Sprintf("+%s", t-a.tl.Start))
			}
		}
		a.prevFrame.labels = labels
	}

	origOps := gtx.Ops
	gtx.Ops = &a.prevFrame.ops
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		a.prevFrame.call = call
		a.prevFrame.dims = dims
	}()

	var ticksPath clip.Path
	var ticksOps op.Ops
	ticksPath.Begin(&ticksOps)
	i := 0
	for t := a.tl.Start; t < a.tl.End; t += tickInterval {
		start := a.tl.tsToPx(t) - tickWidth/2
		end := a.tl.tsToPx(t) + tickWidth/2
		rect := FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := a.tl.tsToPx(t+(tickInterval/10)*time.Duration(j)) - tickWidth/2
			smallEnd := a.tl.tsToPx(t+(tickInterval/10)*time.Duration(j)) + tickWidth/2
			smallTickHeight := tickHeight / 3
			if j == 5 {
				smallTickHeight = tickHeight / 2
			}
			rect := FRect{
				Min: f32.Pt(smallStart, 0),
				Max: f32.Pt(smallEnd, smallTickHeight),
			}
			rect.IntoPath(&ticksPath)
		}

		if t == a.tl.Start {
			label := labels[i]
			stack := op.Offset(image.Pt(0, int(tickHeight))).Push(gtx.Ops)
			paint.ColorOp{Color: colors[colorTickLabel]}.Add(gtx.Ops)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, a.tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
			if dims.Size.Y > labelHeight {
				labelHeight = dims.Size.Y
			}
			prevLabelEnd = float32(dims.Size.X)
			stack.Pop()
		} else {
			macro := op.Record(gtx.Ops)
			// TODO separate value and unit symbol with a space
			label := labels[i]
			paint.ColorOp{Color: colors[colorTickLabel]}.Add(gtx.Ops)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, a.tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
			call := macro.Stop()

			if start-float32(dims.Size.X/2) > prevLabelEnd+minTickLabelDistance {
				prevLabelEnd = start + float32(dims.Size.X/2)
				if start+float32(dims.Size.X/2) <= float32(gtx.Constraints.Max.X) {
					stack := op.Offset(image.Pt(int(round32(start-float32(dims.Size.X/2))), int(tickHeight))).Push(gtx.Ops)
					call.Add(gtx.Ops)
					stack.Pop()
				}
			}
		}
		i++
	}

	paint.FillShape(gtx.Ops, colors[colorTick], clip.Outline{Path: ticksPath.End()}.Op())

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight)+labelHeight)}
}

type GoroutineWidget struct {
	Theme *material.Theme

	tl              *Timeline
	g               *Goroutine
	pointerAt       f32.Point
	hovered         bool
	hoveredActivity bool

	ClickedSpans []Span

	prevFrame struct {
		// State for reusing the previous frame's ops, to avoid redrawing from scratch if no relevant state has changed.
		hovered         bool
		hoveredActivity bool
		forceLabel      bool
		compact         bool
		ops             op.Ops
		call            op.CallOp
	}
}

func (gw *GoroutineWidget) Layout(gtx layout.Context, forceLabel bool, compact bool) layout.Dimensions {
	goroutineHeight := gw.tl.goroutineHeight(gtx)
	goroutineStateHeight := gtx.Metric.Dp(goroutineStateHeightDp)
	spanBorderWidth := gtx.Metric.Dp(spanBorderWidthDp)

	gw.ClickedSpans = nil

	var trackPos f32.Point
	var trackClick bool

	// FIXME(dh): update tooltip position when dragging
	for _, e := range gtx.Events(&gw.hoveredActivity) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			gw.hoveredActivity = true
			gw.pointerAt = ev.Position
		case pointer.Leave, pointer.Cancel:
			gw.hoveredActivity = false
		case pointer.Press:
			if ev.Buttons&pointer.ButtonTertiary != 0 && ev.Modifiers&key.ModCtrl != 0 {
				trackPos = ev.Position
				trackClick = true
			}
		}
	}
	for _, ev := range gtx.Events(&gw.hovered) {
		switch ev.(pointer.Event).Type {
		case pointer.Enter, pointer.Move:
			gw.hovered = true
		case pointer.Leave, pointer.Cancel:
			gw.hovered = false
		}
	}

	if !trackClick &&
		gw.tl.unchanged() &&
		!gw.hoveredActivity &&
		!gw.prevFrame.hoveredActivity &&
		!gw.hovered &&
		!gw.prevFrame.hovered &&
		forceLabel == gw.prevFrame.forceLabel &&
		compact == gw.prevFrame.compact {

		// OPT(dh): instead of avoiding cached ops completely when the goroutine is hovered, draw the tooltip
		// separately.
		gw.prevFrame.call.Add(gtx.Ops)
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, goroutineHeight)}
	}

	gw.prevFrame.hovered = gw.hovered
	gw.prevFrame.hoveredActivity = gw.hoveredActivity
	gw.prevFrame.forceLabel = forceLabel
	gw.prevFrame.compact = compact

	origOps := gtx.Ops
	gtx.Ops = &gw.prevFrame.ops
	gtx.Ops.Reset()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		gw.prevFrame.call = call
	}()

	func() {
		defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, goroutineHeight)}.Push(gtx.Ops).Pop()
		pointer.InputOp{Tag: &gw.hovered, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel}.Add(gtx.Ops)
	}()

	if !compact {
		c := colors[colorGoroutineLabel]
		paint.ColorOp{Color: c}.Add(gtx.Ops)
		var l string
		if gw.g.Function != nil {
			l = fmt.Sprintf("goroutine %d: %s", gw.g.ID, gw.g.Function.Fn)
		} else {
			l = fmt.Sprintf("goroutine %d", gw.g.ID)
		}

		macro := op.Record(gtx.Ops)
		dims := widget.Label{}.Layout(gtx, gw.tl.Theme.Shaper, text.Font{}, goroutineLabelFontSizeSp, l)
		call := macro.Stop()

		if gw.hovered || forceLabel {
			call.Add(gtx.Ops)
		}

		defer op.Offset(image.Pt(0, dims.Size.Y)).Push(gtx.Ops).Pop()
		if gw.hovered || forceLabel {
			paint.FillShape(gtx.Ops, c, clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Metric.Dp(1))}.Op())
		}
		defer op.Offset(image.Pt(0, gtx.Metric.Dp(1)*2)).Push(gtx.Ops).Pop()
	}

	func() {
		defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, goroutineStateHeight)}.Push(gtx.Ops).Pop()
		defer pointer.PassOp{}.Push(gtx.Ops).Pop()
		pointer.InputOp{Tag: &gw.hoveredActivity, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel}.Add(gtx.Ops)
	}()

	// TODO(dh): track click events per goroutine, then use that to implement zooming to a span. it's less efficient but
	// more decoupled.

	// Draw goroutine lifetimes
	//
	// We batch draw operations by color to avoid making thousands of draw calls. See
	// https://lists.sr.ht/~eliasnaur/gio/%3C871qvbdx5r.fsf%40honnef.co%3E#%3C87v8smctsd.fsf@honnef.co%3E
	//
	ops := [...]op.Ops{
		colorStateInactive:             {},
		colorStateActive:               {},
		colorStateBlocked:              {},
		colorStateBlockedHappensBefore: {},
		colorStateBlockedNet:           {},
		colorStateBlockedGC:            {},
		colorStateBlockedSyscall:       {},
		colorStateReady:                {},
		colorStateStuck:                {},
		colorStateMerged:               {},
		colorStateUnknown:              {},
	}
	//gcassert:noescape
	paths := [...]clip.Path{
		colorStateInactive:             {},
		colorStateActive:               {},
		colorStateBlocked:              {},
		colorStateBlockedHappensBefore: {},
		colorStateBlockedNet:           {},
		colorStateBlockedGC:            {},
		colorStateBlockedSyscall:       {},
		colorStateReady:                {},
		colorStateStuck:                {},
		colorStateMerged:               {},
		colorStateUnknown:              {},
	}

	type path struct {
		ops  op.Ops
		path clip.Path
	}
	eventsPath := &path{}
	eventsPath.path.Begin(&eventsPath.ops)

	for i := range paths {
		paths[i].Begin(&ops[i])
	}

	firstStart := float32(-1)
	lastEnd := float32(-1)
	first := true

	doSpans := func(dspSpans []Span, startPx, endPx float32) {
		if trackClick && trackPos.X >= startPx && trackPos.X < endPx {
			gw.ClickedSpans = dspSpans
			trackClick = false
		}

		if first {
			firstStart = startPx
		}
		lastEnd = endPx

		var c colorIndex
		if len(dspSpans) == 1 {
			s := dspSpans[0]
			if int(s.State) >= len(stateColors) {
				c = colorStateUnknown
			} else {
				c = stateColors[s.State]
			}
		} else {
			c = colorStateMerged
		}

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt((max(startPx, 0)), 0)
		maxP = f32.Pt((min(endPx, float32(gtx.Constraints.Max.X))), float32(goroutineStateHeight))
		if first && startPx >= 0 {
			// We don't want two borders right next to each other, nor do we want a border for truncated spans
			minP.X += float32(spanBorderWidth)
		}
		minP.Y += float32(spanBorderWidth)
		if endPx <= float32(gtx.Constraints.Max.X) {
			maxP.X -= float32(spanBorderWidth)
		}
		maxP.Y -= float32(spanBorderWidth)

		p := &paths[c]
		p.MoveTo(minP)
		p.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
		p.LineTo(maxP)
		p.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
		p.Close()

		if spanHasEvents(gw.g.Events, dspSpans[0].Start, dspSpans[len(dspSpans)-1].End) {
			p := eventsPath
			minP := minP
			maxP := maxP
			minP.Y += float32((goroutineStateHeight - spanBorderWidth*2) / 2)

			p.path.MoveTo(minP)
			p.path.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			p.path.LineTo(maxP)
			p.path.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			p.path.Close()
		}

		if gw.hoveredActivity && gw.pointerAt.X >= startPx && gw.pointerAt.X < endPx {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(gw.pointerAt.Round()).Push(gtx.Ops)
			SpanTooltip{dspSpans, gw.Theme.Shaper}.Layout(gtx)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
		}

		first = false
	}

	if gw.tl.unchanged() {
		for _, prevSpans := range gw.tl.prevFrame.dspSpans[gw.g.ID] {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := gw.tl.prevFrame.dspSpans[gw.g.ID][:0]
		it := renderedSpansIterator{
			tl:    gw.tl,
			spans: gw.tl.visibleSpans(gw.g.Spans),
		}
		for {
			dspSpans, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}
			allDspSpans = append(allDspSpans, struct {
				dspSpans       []Span
				startPx, endPx float32
			}{dspSpans, startPx, endPx})
			doSpans(dspSpans, startPx, endPx)
		}
		gw.tl.prevFrame.dspSpans[gw.g.ID] = allDspSpans
	}

	if !first {
		// TODO(dh): use path type for outlines, too
		var outlinesPath clip.Path
		outlinesPath.Begin(gtx.Ops)
		// Outlines are not grouped with other spans of the same color because they have to be drawn before spans.
		firstStart = max(firstStart, 0)
		lastEnd = min(lastEnd, float32(gtx.Constraints.Max.X))
		outlinesPath.MoveTo(f32.Pt(firstStart, 0))
		outlinesPath.LineTo(f32.Pt(lastEnd, 0))
		outlinesPath.LineTo(f32.Pt(lastEnd, float32(goroutineStateHeight)))
		outlinesPath.LineTo(f32.Pt(firstStart, float32(goroutineStateHeight)))
		outlinesPath.Close()
		paint.FillShape(gtx.Ops, colors[colorSpanOutline], clip.Outline{Path: outlinesPath.End()}.Op())
	} else {
		// No spans for this goroutine
	}

	for cIdx := range paths {
		p := &paths[cIdx]
		paint.FillShape(gtx.Ops, colors[cIdx], clip.Outline{Path: p.End()}.Op())
	}
	paint.FillShape(gtx.Ops, colors[colorSpanWithEvents], clip.Outline{Path: eventsPath.path.End()}.Op())

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, goroutineHeight)}
}

func (tl *Timeline) visibleGoroutines(gtx layout.Context) []*GoroutineWidget {
	goroutineHeight := tl.goroutineHeight(gtx)
	goroutineGap := gtx.Metric.Dp(goroutineGapDp)

	start := -1
	end := -1
	// OPT(dh): at least use binary search to find the range of goroutines we need to draw
	// OPT(dh): we can probably compute the indices directly
	for i := range tl.Gs {
		y := (goroutineHeight+goroutineGap)*int(i) - tl.Y
		// Don't draw goroutines that would be fully hidden, but do draw partially hidden ones
		if y < -goroutineHeight {
			continue
		}
		if start == -1 {
			start = i
		}
		if y > gtx.Constraints.Max.Y {
			end = i
			break
		}
	}

	return tl.Gs[start:end]
}

func (tl *Timeline) layoutGoroutines(gtx layout.Context) (layout.Dimensions, []*GoroutineWidget) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	goroutineHeight := tl.goroutineHeight(gtx)
	goroutineGap := gtx.Metric.Dp(goroutineGapDp)

	// Draw a scrollbar, then clip to smaller area. We've already computed nsPerPx, so clipping the goroutine area will
	// not bring us out of alignment with the axis.
	{
		// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
		totalHeight := float32((len(gs) + 1) * (goroutineHeight + goroutineGap))
		fraction := float32(gtx.Constraints.Max.Y) / totalHeight
		offset := float32(tl.Y) / totalHeight
		sb := material.Scrollbar(tl.Theme, &tl.Scrollbar)
		stack := op.Offset(image.Pt(gtx.Constraints.Max.X-gtx.Metric.Dp(sb.Width()), 0)).Push(gtx.Ops)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
		stack.Pop()

		gtx.Constraints.Max.X -= gtx.Metric.Dp(sb.Width())
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	}

	// OPT(dh): at least use binary search to find the range of goroutines we need to draw
	start := -1
	end := -1
	for i, gw := range tl.Gs {
		y := (goroutineHeight+goroutineGap)*int(i) - tl.Y
		// Don't draw goroutines that would be fully hidden, but do draw partially hidden ones
		if y < -goroutineHeight {
			continue
		}
		end = i
		if y > gtx.Constraints.Max.Y {
			break
		}
		if start == -1 {
			start = i
		}

		stack := op.Offset(image.Pt(0, y)).Push(gtx.Ops)
		gw.Layout(gtx, tl.Goroutines.DisplayAllLabels, tl.Goroutines.Compact)
		stack.Pop()
	}

	var out []*GoroutineWidget
	if start != -1 {
		out = tl.Gs[start:end]
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}, out
}

type SpanTooltip struct {
	Spans  []Span
	shaper text.Shaper
}

// For debugging
func dumpFrames(frames []*trace.Frame) {
	if len(frames) == 0 {
		fmt.Println("no frames")
	}
	for _, f := range frames {
		fmt.Println(f)
	}
}

func (tt SpanTooltip) Layout(gtx layout.Context) layout.Dimensions {
	var tooltipBorderWidth = gtx.Metric.Dp(2)

	label := "State: "
	var at *trace.Frame
	if len(tt.Spans) == 1 {
		s := tt.Spans[0]
		at = s.At
		if at == nil && len(s.Stack) > 0 {
			at = s.Stack[0]
		}
		switch state := s.State; state {
		case stateInactive:
			label += "inactive"
			label += "\nReason: " + s.Reason
		case stateActive:
			label += "active"
		case stateBlocked:
			label += "blocked"
		case stateBlockedSend:
			label += "blocked on channel send"
		case stateBlockedWaitingForTraceData:
			label += "waiting for trace data"
		case stateBlockedRunfinqWaiting:
			label += "runfinq: waiting for finalizer to run"
		case stateBlockedRecv:
			label += "blocked on channel recv"
		case stateBlockedSelect:
			label += "blocked on select"
		case stateBlockedSync:
			label += "blocked on mutex"
		case stateBlockedSyncOnce:
			label += "blocked on sync.Once"
		case stateBlockedSyncTriggeringGC:
			label += "blocked triggering GC"
		case stateBlockedCond:
			label += "blocked on condition variable"
		case stateBlockedNet:
			label += "blocked on polled I/O"
			dumpFrames(s.Stack)
		case stateBlockedGC:
			label += "blocked on GC assist"
		case stateBlockedSyscall:
			label += "blocked on syscall"
		case stateStuck:
			label += "stuck"
		case stateReady:
			label += "ready"
		default:
			panic(fmt.Sprintf("unhandled state %d", state))
		}

		tags := make([]string, 0, 4)
		if s.Tags&spanTagRead != 0 {
			tags = append(tags, "read")
		}
		if s.Tags&spanTagAccept != 0 {
			tags = append(tags, "accept")
		}
		if s.Tags&spanTagDial != 0 {
			tags = append(tags, "dial")
		}
		if s.Tags&spanTagNetwork != 0 {
			tags = append(tags, "network")
		}
		if s.Tags&spanTagTCP != 0 {
			tags = append(tags, "TCP")
		}
		if s.Tags&spanTagTLS != 0 {
			tags = append(tags, "TLS")
		}
		if s.Tags&spanTagHTTP != 0 {
			tags = append(tags, "HTTP")
		}
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}
	} else {
		label += fmt.Sprintf("mixed (%d spans)", len(tt.Spans))
	}
	label += "\n"
	d := tt.Spans[len(tt.Spans)-1].End - tt.Spans[0].Start
	label += fmt.Sprintf("Duration: %s", d)

	if at != nil {
		label += fmt.Sprintf("\nIn: %s", at.Fn)
	}

	// TODO(dh): display reason why we're in this state
	// TODO(dh): make tooltip actually look good

	macro := op.Record(gtx.Ops)
	paint.ColorOp{Color: colors[colorTooltipText]}.Add(gtx.Ops)
	// XXX can we ensure that widget.Label only uses our newlines and doesn't attempt to word-wrap for us?
	dims := widget.Label{}.Layout(gtx, tt.shaper, text.Font{}, tooltipFontSizeSp, label)
	call := macro.Stop()

	rect := clip.Rect{
		Max: image.Pt(dims.Size.X+2*tooltipBorderWidth, dims.Size.Y+2*tooltipBorderWidth),
	}
	paint.FillShape(gtx.Ops, colors[colorTooltipBorder], rect.Op())

	rect = clip.Rect{
		Min: image.Pt(tooltipBorderWidth, tooltipBorderWidth),
		Max: image.Pt(dims.Size.X+tooltipBorderWidth, dims.Size.Y+tooltipBorderWidth),
	}
	paint.FillShape(gtx.Ops, colors[colorTooltipBackground], rect.Op())
	stack := op.Offset(image.Pt(tooltipBorderWidth, tooltipBorderWidth)).Push(gtx.Ops)
	call.Add(gtx.Ops)
	stack.Pop()

	return layout.Dimensions{
		Baseline: dims.Baseline,
		Size:     image.Pt(dims.Size.X+2*tooltipBorderWidth, dims.Size.Y+2*tooltipBorderWidth),
	}
}

// XXX goroutine 0 seems to be special and doesn't get (un)scheduled. look into that.

// TODO(dh): How should resizing the window affect the zoom level? When making the window wider, should it display more
// time or should it display the same time, stretched to fill the new space? Tracy does the latter.

// NOTE: how Tracy deals with spans that are too small to see at a zoom level: there's a minimum width for the first
// span, and consecutive spans that would fall into that span get merged into it

type Goroutine struct {
	ID       uint64
	Function *trace.Frame
	Spans    []Span
	Events   []*trace.Event
}

type Span struct {
	Start time.Duration
	End   time.Duration
	State schedulingState
	// TODO(dh): use an enum for Reason
	Reason string
	Events []*trace.Event
	Stack  []*trace.Frame
	Tags   spanTags
	At     *trace.Frame
}

//gcassert:inline
func (s Span) Duration() time.Duration {
	return s.End - s.Start
}

// TODO(dh): avoid global state
var gs []*Goroutine

func spanHasEvents(events []*trace.Event, start, end time.Duration) bool {
	// OPT(dh): use at least binary search. Ideally we'd just store events with spans and avoid the computation
	for _, ev := range events {
		if time.Duration(ev.Ts) >= end {
			return false
		}
		if time.Duration(ev.Ts) >= start {
			return true
		}
	}
	return false
}

func eventsForSpan(events []*trace.Event, start, end time.Duration) []*trace.Event {
	// OPT(dh): use at least binary search. Ideally we'd just store events with spans and avoid the computation
	first := -1
	last := -1
	for i, ev := range events {
		if time.Duration(ev.Ts) >= start && time.Duration(ev.Ts) < end {
			if first == -1 {
				first = i
			}
		} else {
			if first != -1 {
				last = i
				break
			}
		}
	}
	if first == -1 {
		return nil
	}
	if last == -1 {
		last = len(events)
	}
	return events[first:last]
}

func main() {
	r, err := os.Open(os.Args[1])
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Loading trace...")

	res, err := trace.Parse(bufio.NewReader(r), "")
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

	gsByID := map[uint64]*Goroutine{}
	getG := func(gid uint64) *Goroutine {
		g, ok := gsByID[gid]
		if ok {
			return g
		}
		g = &Goroutine{ID: gid}
		gsByID[gid] = g
		return g
	}

	lastSyscall := map[uint64][]*trace.Frame{}

	for _, ev := range res.Events {
		var gid uint64
		var state schedulingState
		var reason string

		switch ev.Type {
		case trace.EvGoCreate:
			// ev.G creates ev.Args[0]
			getG(ev.G).Events = append(getG(ev.G).Events, ev)
			gid = ev.Args[0]
			if ev.Args[1] != 0 {
				stack := res.Stacks[ev.Args[1]]
				if len(stack) != 0 {
					getG(gid).Function = stack[0]
				}
			}
			state = stateInactive
			reason = "newly created"
		case trace.EvGoStart:
			// ev.G starts running
			gid = ev.G
			state = stateActive
		case trace.EvGoStartLabel:
			// ev.G starts running
			// TODO(dh): make use of the label
			gid = ev.G
			state = stateActive
		case trace.EvGoStop:
			// ev.G is stopping
			gid = ev.G
			state = stateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			gid = ev.G
			state = stateDone
		case trace.EvGoSched:
			// ev.G calls Gosched
			gid = ev.G
			state = stateInactive
			reason = "called runtime.Gosched"
		case trace.EvGoSleep:
			// ev.G calls Sleep
			gid = ev.G
			state = stateInactive
			reason = "called time.Sleep"
		case trace.EvGoPreempt:
			// ev.G got preempted
			gid = ev.G
			state = stateInactive
			reason = "got preempted"
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoBlockGC, trace.EvGoBlock:
			// ev.G is blocking
			gid = ev.G
			state = evTypeToState[ev.Type]
		case trace.EvGoWaiting:
			// ev.G is blocked when tracing starts
			gid = ev.G
			state = stateBlocked
		case trace.EvGoUnblock:
			// ev.G is unblocking ev.Args[0]
			getG(ev.G).Events = append(getG(ev.G).Events, ev)
			gid = ev.Args[0]
			state = stateReady
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
			lastSyscall[ev.G] = ev.Stk
			continue
		case trace.EvGoSysBlock:
			// TODO(dh): have a special state for this
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoInSyscall:
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoSysExit:
			gid = ev.G
			state = stateReady
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
			if s := getG(gid).Spans; len(s) > 0 {
				if len(s) == 1 && ev.Type == trace.EvGoWaiting && s[0].State == stateInactive {
					// The execution trace emits GoCreate + GoWaiting for goroutines that already exist at the start of
					// tracing if they're in a blocked state. This causes a transition from inactive to blocked, which we
					// wouldn't normally permit.
				} else {
					prevState := s[len(s)-1].State
					if !legalStateTransitions[prevState][state] {
						panic(fmt.Sprintf("illegal state transition %d -> %d for goroutine %d, offset %d", prevState, state, gid, ev.Off))
					}
				}
			}
		}

		s := Span{Start: time.Duration(ev.Ts), State: state, Reason: reason, Stack: ev.Stk}
		if ev.Type == trace.EvGoSysBlock {
			s.Stack = lastSyscall[ev.G]
		}
		s = applyPatterns(s)
		getG(gid).Spans = append(getG(gid).Spans, s)
	}

	for _, g := range gsByID {
		if len(g.Spans) == 0 {
			continue
		}
		for i := range g.Spans[:len(g.Spans)-1] {
			g.Spans[i].End = g.Spans[i+1].Start
		}
		last := g.Spans[len(g.Spans)-1]
		if last.State == stateDone {
			// The goroutine has ended
			// XXX the event probably has a stack associated with it, which we shouldn't discard.
			g.Spans = g.Spans[:len(g.Spans)-1]
		} else {
			// XXX somehow encode open-ended traces
			g.Spans[len(g.Spans)-1].End = time.Duration(res.Events[len(res.Events)-1].Ts)
		}

		gs = append(gs, g)
	}

	sort.Slice(gs, func(i, j int) bool {
		return gs[i].ID < gs[j].ID
	})

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

var colors = [...]color.NRGBA{
	colorStateInactive: toColor(0x888888FF),
	colorStateActive:   toColor(0x448844FF),

	colorStateBlocked:              toColor(0xBA4141FF),
	colorStateBlockedHappensBefore: toColor(0xBB6363FF),
	colorStateBlockedNet:           toColor(0xBB5D5DFF),
	colorStateBlockedGC:            toColor(0xBB554FFF),
	colorStateBlockedSyscall:       toColor(0xBA4F41FF),

	colorStateReady:   toColor(0x4BACB8FF),
	colorStateStuck:   toColor(0x000000FF),
	colorStateMerged:  toColor(0xB9BB63FF),
	colorStateUnknown: toColor(0xFFFF00FF),

	colorBackground:        toColor(0xffffeaFF),
	colorZoomSelection:     toColor(0xeeee9e99),
	colorCursor:            toColor(0x000000FF),
	colorTick:              toColor(0x000000FF),
	colorTickLabel:         toColor(0x000000FF),
	colorTooltipText:       toColor(0x000000FF),
	colorTooltipBackground: toColor(0xEEFFEEFF),
	colorTooltipBorder:     toColor(0x57A8A8FF),

	colorGoroutineLabel: toColor(0x888888FF),

	// TODO(dh): find a nice color for this
	colorSpanWithEvents: toColor(0xFF00FFFF),
	colorSpanOutline:    toColor(0x000000FF),
}

type colorIndex int

const (
	colorStateUnknown colorIndex = iota

	colorStateInactive
	colorStateActive

	colorStateBlocked
	colorStateBlockedHappensBefore
	colorStateBlockedNet
	colorStateBlockedGC
	colorStateBlockedSyscall

	colorStateReady
	colorStateStuck
	colorStateMerged

	colorBackground
	colorZoomSelection
	colorCursor
	colorTick
	colorTickLabel
	colorTooltipText
	colorTooltipBackground
	colorTooltipBorder

	colorGoroutineLabel

	colorSpanWithEvents
	colorSpanOutline
)

type schedulingState int

const (
	stateNone schedulingState = iota
	stateInactive
	stateActive
	stateBlocked
	stateBlockedRunfinqWaiting
	stateBlockedWaitingForTraceData // 5
	stateBlockedSend
	stateBlockedRecv
	stateBlockedSelect
	stateBlockedSync
	stateBlockedSyncOnce
	stateBlockedSyncTriggeringGC // 11
	stateBlockedCond
	stateBlockedNet
	stateBlockedGC
	stateBlockedSyscall
	stateStuck
	stateReady // 17
	stateDone
	stateLast
)

var stateColors = [...]colorIndex{
	stateInactive:       colorStateInactive,
	stateActive:         colorStateActive,
	stateBlocked:        colorStateBlocked,
	stateBlockedSend:    colorStateBlockedHappensBefore,
	stateBlockedRecv:    colorStateBlockedHappensBefore,
	stateBlockedSelect:  colorStateBlockedHappensBefore,
	stateBlockedSync:    colorStateBlockedHappensBefore,
	stateBlockedCond:    colorStateBlockedHappensBefore,
	stateBlockedNet:     colorStateBlockedNet,
	stateBlockedGC:      colorStateBlockedGC,
	stateBlockedSyscall: colorStateBlockedSyscall,
	stateStuck:          colorStateStuck,
	stateReady:          colorStateReady,
}

var legalStateTransitions = [stateLast][stateLast]bool{
	stateInactive: {
		stateActive:         true,
		stateReady:          true,
		stateBlockedSyscall: true,
	},
	stateActive: {
		stateInactive:                   true,
		stateBlocked:                    true,
		stateBlockedSend:                true,
		stateBlockedRecv:                true,
		stateBlockedSelect:              true,
		stateBlockedSync:                true,
		stateBlockedSyncOnce:            true,
		stateBlockedSyncTriggeringGC:    true,
		stateBlockedRunfinqWaiting:      true,
		stateBlockedWaitingForTraceData: true,
		stateBlockedCond:                true,
		stateBlockedNet:                 true,
		stateBlockedGC:                  true,
		stateBlockedSyscall:             true,
		stateStuck:                      true,
		stateDone:                       true,
	},
	stateReady: {
		stateActive: true,
	},
	stateBlocked:                    {stateReady: true},
	stateBlockedSend:                {stateReady: true},
	stateBlockedRecv:                {stateReady: true},
	stateBlockedSelect:              {stateReady: true},
	stateBlockedSync:                {stateReady: true},
	stateBlockedSyncOnce:            {stateReady: true},
	stateBlockedSyncTriggeringGC:    {stateReady: true},
	stateBlockedRunfinqWaiting:      {stateReady: true},
	stateBlockedWaitingForTraceData: {stateReady: true},
	stateBlockedCond:                {stateReady: true},
	stateBlockedNet:                 {stateReady: true},
	stateBlockedGC:                  {stateReady: true},
	stateBlockedSyscall: {
		stateReady: true,
	},
}

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
	for _, g := range gs {
		if len(g.Spans) > 0 {
			d := g.Spans[len(g.Spans)-1].End
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
	tl := &Timeline{
		Start: start,
		End:   end,
		Theme: material.NewTheme(gofont.Collection()),
	}
	tl.Axis = Axis{tl: tl}
	tl.Gs = make([]*GoroutineWidget, len(gs))
	for i, g := range gs {
		tl.Gs[i] = &GoroutineWidget{
			Theme: tl.Theme,
			tl:    tl,
			g:     g,
		}
	}
	tl.prevFrame.dspSpans = map[uint64][]struct {
		dspSpans []Span
		startPx  float32
		endPx    float32
	}{}

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

			for _, ev := range gtx.Events(profileTag) {
				fmt.Println(ev)
			}
			profile.Op{Tag: profileTag}.Add(gtx.Ops)

			tl.Layout(gtx)
			tl.prevFrame.Start = tl.Start
			tl.prevFrame.End = tl.End
			tl.prevFrame.nsPerPx = tl.nsPerPx
			tl.prevFrame.Y = tl.Y
			tl.prevFrame.compact = tl.Goroutines.Compact

			ev.Frame(&ops)
		}
	}
}

func min(a, b float32) float32 {
	if a <= b {
		return a
	} else {
		return b
	}
}

func max(a, b float32) float32 {
	if a >= b {
		return a
	} else {
		return b
	}
}

type FRect struct {
	Min f32.Point
	Max f32.Point
}

func (r FRect) Path(ops *op.Ops) clip.PathSpec {
	var p clip.Path
	p.Begin(ops)
	r.IntoPath(&p)
	return p.End()
}

func (r FRect) IntoPath(p *clip.Path) {
	p.MoveTo(r.Min)
	p.LineTo(f32.Pt(r.Max.X, r.Min.Y))
	p.LineTo(r.Max)
	p.LineTo(f32.Pt(r.Min.X, r.Max.Y))
	p.LineTo(r.Min)
}

func (r FRect) Op(ops *op.Ops) clip.Op {
	return clip.Outline{Path: r.Path(ops)}.Op()
}

func round32(f float32) float32 {
	return float32(math.Round(float64(f)))
}

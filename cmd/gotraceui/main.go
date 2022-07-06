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
	"gioui.org/x/eventx"
)

// XXX parsing failures and other format violations shouldn't cause panics, but instead return errors that we can
// present in the UI.

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

	tooltipFontSizeSp    unit.Sp = 14
	tooltipPaddingDp     unit.Dp = 2
	tooltipBorderWidthDp unit.Dp = 2

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
	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := at.X / float32(gtx.Constraints.Max.X)
		ds := time.Duration(tl.nsPerPx * 100 * ratio)
		de := time.Duration(tl.nsPerPx * 100 * (1 - ratio))
		tl.Start += ds
		tl.End -= de
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := at.X / float32(gtx.Constraints.Max.X)
		ds := time.Duration(tl.nsPerPx * 100 * ratio)
		de := time.Duration(tl.nsPerPx * 100 * (1 - ratio))

		// Make sure the user can always zoom out
		if ds < 1 {
			ds = 1
		}
		if de < 1 {
			de = 1
		}

		start := tl.Start - time.Duration(ds)
		end := tl.End + time.Duration(de)

		// Limit timeline to roughly one day. There's rno reason to zoom out this far, and zooming out further will lead
		// to edge cases and eventually overflow.
		if end-start < 24*time.Hour {
			tl.Start = start
			tl.End = end
		}
	}

	if tl.Start > tl.End {
		tl.Start = tl.End - 1
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

func (tl *Timeline) scrollToGoroutine(gtx layout.Context, g *Goroutine) {
	// OPT(dh): don't be O(n)
	off := 0
	for _, og := range tl.Gs {
		if g == og.g {
			// TODO(dh): show goroutine at center of window, not the top
			tl.Y = off
			return
		}
		off += tl.goroutineHeight(gtx) + gtx.Metric.Dp(goroutineGapDp)
	}
	panic("unreachable")
}

func (tl *Timeline) Layout(gtx layout.Context) layout.Dimensions {
	for _, ev := range gtx.Events(tl) {
		switch ev := ev.(type) {
		case key.Event:
			if ev.State == key.Press {
				switch ev.Name {
				case key.NameHome:
					switch {
					case ev.Modifiers&key.ModCtrl != 0:
						tl.zoomToFitCurrentView(gtx)
					case ev.Modifiers&key.ModShift != 0:
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
		totalHeight := float32(len(tl.Gs) * (goroutineHeight + goroutineGap))
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
	hoveredLabel    bool

	ClickedSpans []Span

	prevFrame struct {
		// State for reusing the previous frame's ops, to avoid redrawing from scratch if no relevant state has changed.
		hovered         bool
		hoveredActivity bool
		hoveredLabel    bool
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
	for _, ev := range gtx.Events(&gw.hoveredLabel) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Type {
			case pointer.Enter, pointer.Move:
				gw.hoveredLabel = true
				gw.pointerAt = ev.Position
			case pointer.Leave, pointer.Cancel:
				gw.hoveredLabel = false
			case pointer.Press:
				if ev.Buttons&pointer.ButtonTertiary != 0 && ev.Modifiers&key.ModCtrl != 0 {
					gw.ClickedSpans = gw.g.Spans
				}
			}
		}
	}

	if !trackClick &&
		gw.tl.unchanged() &&
		!gw.hoveredActivity &&
		!gw.prevFrame.hoveredActivity &&
		!gw.hoveredLabel &&
		!gw.prevFrame.hoveredLabel &&
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
	gw.prevFrame.hoveredLabel = gw.hoveredLabel
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

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, goroutineHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: &gw.hovered, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel}.Add(gtx.Ops)

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

			stack := clip.Rect{Max: dims.Size}.Push(gtx.Ops)
			pointer.InputOp{Tag: &gw.hoveredLabel, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Cancel | pointer.Move}.Add(gtx.Ops)
			stack.Pop()
		}

		if gw.hoveredLabel {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(gw.pointerAt.Round()).Push(gtx.Ops)
			GoroutineTooltip{gw.g, gw.Theme.Shaper}.Layout(gtx)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
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

	if start == -1 {
		// No visible goroutines
		return nil
	}

	if end == -1 {
		end = len(tl.Gs)
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
		totalHeight := float32((len(tl.Gs) + 1) * (goroutineHeight + goroutineGap))
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

type GoroutineTooltip struct {
	G      *Goroutine
	shaper text.Shaper
}

func (tt GoroutineTooltip) Layout(gtx layout.Context) layout.Dimensions {
	start := tt.G.Spans[0].Start
	end := tt.G.Spans[len(tt.G.Spans)-1].End
	d := end - start

	// OPT(dh): compute these statistics when parsing the trace, instead of on each frame.
	var blockedD, inactiveD, runningD, gcAssistD time.Duration
	for _, s := range tt.G.Spans {
		switch s.State {
		case stateInactive:
			inactiveD += s.Duration()
		case stateActive:
			runningD += s.Duration()
		case stateBlocked:
			blockedD += s.Duration()
		case stateBlockedRunfinqWaiting:
			inactiveD += s.Duration()
		case stateBlockedWaitingForTraceData:
			inactiveD += s.Duration()
		case stateBlockedSend:
			blockedD += s.Duration()
		case stateBlockedRecv:
			blockedD += s.Duration()
		case stateBlockedSelect:
			blockedD += s.Duration()
		case stateBlockedSync:
			blockedD += s.Duration()
		case stateBlockedSyncOnce:
			blockedD += s.Duration()
		case stateBlockedSyncTriggeringGC:
			blockedD += s.Duration()
		case stateBlockedCond:
			blockedD += s.Duration()
		case stateBlockedNet:
			blockedD += s.Duration()
		case stateBlockedGC:
			blockedD += s.Duration()
		case stateBlockedSyscall:
			blockedD += s.Duration()
		case stateStuck:
			blockedD += s.Duration()
		case stateReady:
			inactiveD += s.Duration()
		case stateCreated:
			inactiveD += s.Duration()
		case stateGCMarkAssist:
			gcAssistD += s.Duration()
		case stateGCSweep:
			gcAssistD += s.Duration()
		case stateDone:
		default:
			panic(fmt.Sprintf("unknown state %d", s.State))
		}
	}
	blockedPct := float32(blockedD) / float32(d) * 100
	inactivePct := float32(inactiveD) / float32(d) * 100
	runningPct := float32(runningD) / float32(d) * 100
	gcAssistPct := float32(gcAssistD) / float32(d) * 100
	l := fmt.Sprintf("Goroutine %d: %s\n\n"+
		"Appeared at: %s\n"+
		"Disappeared at: %s\n"+
		"Lifetime: %s\n"+
		"Time in blocked states: %s (%.2f%%)\n"+
		"Time in inactive states: %s (%.2f%%)\n"+
		"Time in GC assist: %s (%.2f%%)\n"+
		"Time in running states: %s (%.2f%%)",
		tt.G.ID, tt.G.Function.Fn,
		start,
		end,
		d,
		blockedD, blockedPct,
		inactiveD, inactivePct,
		gcAssistD, gcAssistPct,
		runningD, runningPct)

	return Tooltip{shaper: tt.shaper}.Layout(gtx, l)
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
	label := "State: "
	var at *trace.Frame
	if len(tt.Spans) == 1 {
		s := tt.Spans[0]
		if at == nil && len(s.Stack) > 0 {
			at = s.Stack[s.At]
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
		case stateBlockedGC:
			label += "blocked on GC assist"
		case stateBlockedSyscall:
			label += "blocked on syscall"
		case stateStuck:
			label += "stuck"
		case stateReady:
			label += "ready"
		case stateCreated:
			label += "ready"
		case stateGCMarkAssist:
			label += "GC mark assist"
		case stateGCSweep:
			label += "GC sweep"
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
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("\nIn: %s", at.Fn)
	}

	return Tooltip{shaper: tt.shaper}.Layout(gtx, label)
}

type Tooltip struct {
	shaper text.Shaper
}

func (tt Tooltip) Layout(gtx layout.Context, l string) layout.Dimensions {
	var padding = gtx.Metric.Dp(tooltipPaddingDp)
	var tooltipBorderWidth = gtx.Metric.Dp(tooltipBorderWidthDp)

	macro := op.Record(gtx.Ops)
	paint.ColorOp{Color: colors[colorTooltipText]}.Add(gtx.Ops)
	// XXX can we ensure that widget.Label only uses our newlines and doesn't attempt to word-wrap for us?
	dims := widget.Label{}.Layout(gtx, tt.shaper, text.Font{}, tooltipFontSizeSp, l)
	call := macro.Stop()

	total := clip.Rect{
		Min: image.Pt(0, 0),
		Max: image.Pt(dims.Size.X+2*tooltipBorderWidth+2*padding, dims.Size.Y+2*tooltipBorderWidth+2*padding),
	}
	paint.FillShape(gtx.Ops, colors[colorTooltipBorder], total.Op())

	content := total
	content.Min.X += tooltipBorderWidth
	content.Min.Y += tooltipBorderWidth
	content.Max.X -= tooltipBorderWidth
	content.Max.Y -= tooltipBorderWidth
	paint.FillShape(gtx.Ops, colors[colorTooltipBackground], content.Op())

	stack := op.Offset(image.Pt(tooltipBorderWidth+padding, tooltipBorderWidth+padding)).Push(gtx.Ops)
	call.Add(gtx.Ops)
	stack.Pop()

	return layout.Dimensions{
		Baseline: dims.Baseline,
		Size:     total.Max,
	}
}

// XXX goroutine 0 seems to be special and doesn't get (un)scheduled. look into that.

// TODO(dh): How should resizing the window affect the zoom level? When making the window wider, should it display more
// time or should it display the same time, stretched to fill the new space? Tracy does the latter.

type Goroutine struct {
	ID       uint64
	Function *trace.Frame
	Spans    []Span
	Events   []*trace.Event
}

func (g *Goroutine) String() string {
	if g.Function == nil {
		// At least GCSweepStart can happen on g0
		return fmt.Sprintf("goroutine %d", g.ID)
	} else {
		return fmt.Sprintf("goroutine %d: %s", g.ID, g.Function.Fn)
	}
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
	At     int
}

//gcassert:inline
func (s Span) Duration() time.Duration {
	return s.End - s.Start
}

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
	var gs []*Goroutine

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
	inMarkAssist := map[uint64]struct{}{}

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
			// FIXME(dh): when tracing starts after goroutines have already been created then we receive an EvGoCreate
			// for them. But those goroutines may not necessarily be in a non-running state. We do receive EvGoWaiting
			// and EvGoInSyscall for goroutines that are blocked or in a syscall when tracing starts; does that mean
			// that any goroutine that doesn't receive this event is currently running? If so we'd have to detect which
			// goroutines receive neither EvGoWaiting or EvGoInSyscall, and which were already running.
			//
			// EvGoWaiting is emitted when we're in _Gwaiting, and EvGoInSyscall when we're in _Gsyscall. Critically
			// this doesn't cover _Gidle and _Grunnable, which means we don't know if it's running or waiting to run. If
			// there's another event then we can deduce it (we can't go from _Grunnable to _Gblocked, for example), but
			// if there are no more events, then we cannot tell if the goroutine was always running or always runnable.
			state = stateCreated
			reason = "newly created"
		case trace.EvGoStart:
			// ev.G starts running
			gid = ev.G

			if _, ok := inMarkAssist[gid]; ok {
				state = stateGCMarkAssist
			} else {
				state = stateActive
			}
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
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoInSyscall:
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoSysExit:
			gid = ev.G
			state = stateReady
		case trace.EvProcStart, trace.EvProcStop:
			// TODO(dh): implement a per-proc timeline
			continue

		case trace.EvGCMarkAssistStart:
			// User goroutines may be asked to assist the GC's mark phase. This happens when the goroutine allocates
			// memory and some condition is true. When that happens, the tracer emits EvGCMarkAssistStart for that
			// goroutine.
			//
			// Note that this event is not preceeded by an EvGoBlock or similar. Similarly, EvGCMarkAssistDone is not
			// succeeded by an EvGoStart or similar. The mark assist events are laid over the normal goroutine
			// scheduling events.
			//
			// We instead turn these into proper goroutine states and split the current span in two to make room for
			// mark assist. This needs special care because mark assist can be preempted, so we might GoStart into mark
			// assist.

			gid = ev.G
			state = stateGCMarkAssist
			inMarkAssist[gid] = struct{}{}
		case trace.EvGCMarkAssistDone:
			// The counterpart to EvGCMarkAssistStop.

			gid = ev.G
			state = stateActive
			delete(inMarkAssist, gid)
		case trace.EvGCSweepStart:
			// This is similar to mark assist, but for sweeping spans. When a goroutine would need to allocate a new
			// span, it first sweeps other spans of the same size to find a free one.
			//
			// Unlike mark assist, sweeping cannot be preempted, simplifying our state tracking.

			gid = ev.G
			state = stateGCSweep
		case trace.EvGCSweepDone:
			// The counterpart to EvGcSweepStart.

			// XXX apparently this can happen on g0, in which case going to stateActive is probably wrong.
			gid = ev.G
			state = stateActive
		case trace.EvGCStart, trace.EvGCDone, trace.EvGCSTWStart, trace.EvGCSTWDone:
			// TODO(dh): implement a GC timeline
			//
			// These seem to always happen on g0
			continue
		case trace.EvHeapAlloc:
			// Instant measurement of currently allocated memory
			continue
		case trace.EvHeapGoal:
			// Instant measurement of new heap goal

			// TODO(dh): implement
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
		err := run(w, gs)
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
	colorStateGCMarkAssist:         toColor(0x9C6FD6FF),
	colorStateGCSweep:              toColor(0x9C6FD6FF),

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
	colorStateGCMarkAssist
	colorStateGCSweep

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
	stateBlockedWaitingForTraceData
	stateBlockedSend
	stateBlockedRecv
	stateBlockedSelect
	stateBlockedSync
	stateBlockedSyncOnce
	stateBlockedSyncTriggeringGC
	stateBlockedCond
	stateBlockedNet
	stateBlockedGC
	stateBlockedSyscall
	stateStuck
	stateReady
	stateCreated
	stateDone
	stateGCMarkAssist
	stateGCSweep
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
	stateCreated:        colorStateReady,
	stateGCMarkAssist:   colorStateGCMarkAssist,
	stateGCSweep:        colorStateGCSweep,
}

var legalStateTransitions = [stateLast][stateLast]bool{
	stateInactive: {
		stateActive:         true,
		stateReady:          true,
		stateBlockedSyscall: true,

		// Starting back into preempted mark assist
		stateGCMarkAssist: true,
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
		stateGCMarkAssist:               true,
		stateGCSweep:                    true,
	},
	stateCreated: {
		stateActive: true,

		// FIXME(dh): These two transitions are only valid for goroutines that already existed when tracing started.
		// eventually we'll make it so those goroutines don't end up in stateReady, at which point we should remove
		// these entries.
		stateBlocked:        true,
		stateBlockedSyscall: true,
	},
	stateReady: {
		stateActive:       true,
		stateGCMarkAssist: true,
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

	stateGCMarkAssist: {
		stateActive:      true, // back to the goroutine's previous state
		stateInactive:    true, // mark assist can be preempted
		stateBlocked:     true,
		stateBlockedSync: true,
		stateBlockedGC:   true, // XXX what does this transition mean?
	},

	stateGCSweep: {
		stateActive: true, // back to the goroutine's previous state
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

func run(w *app.Window, gs []*Goroutine) error {
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

	var ww *ListWindow[*Goroutine]

	var shortcuts int

	for {
		e := <-w.Events()
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			gtx := layout.NewContext(&ops, ev)
			gtx.Constraints.Min = image.Point{}

			clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops)

			for _, ev := range gtx.Events(&shortcuts) {
				switch ev := ev.(type) {
				case key.Event:
					if ev.State == key.Press && ev.Name == "G" && ww == nil {
						ww = NewListWindow[*Goroutine](tl.Theme)
						ww.SetItems(gs)
						ww.Filter = func(item *Goroutine, f string) bool {
							// XXX implement a much better filtering function that can do case-insensitive fuzzy search,
							// and allows matching goroutines by ID.
							return strings.Contains(item.Function.Fn, f)
						}
					}
				}
			}

			key.InputOp{Tag: &shortcuts, Keys: "G"}.Add(gtx.Ops)

			if ww != nil {
				if item, ok := ww.Confirmed(); ok {
					tl.scrollToGoroutine(gtx, item)
					ww = nil
				} else if ww.Cancelled() {
					ww = nil
				} else {
					macro := op.Record(gtx.Ops)

					// Draw full-screen overlay that prevents input to the timeline and closed the window if clicking
					// outside of it.
					//
					// XXX use constant for color
					paint.Fill(gtx.Ops, toColor(0x000000DD))
					pointer.InputOp{Tag: ww}.Add(gtx.Ops)

					offset := image.Pt(gtx.Constraints.Max.X/2-1000/2, gtx.Constraints.Max.Y/2-500/2)
					stack := op.Offset(offset).Push(gtx.Ops)
					gtx := gtx
					// XXX compute constraints from window size
					// XXX also set a minimum width
					gtx.Constraints.Max.X = 1000
					gtx.Constraints.Max.Y = 500
					ww.Layout(gtx)
					stack.Pop()
					op.Defer(gtx.Ops, macro.Stop())
				}
			}

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

type listWindowItem[T any] struct {
	index int
	item  T
	s     string
	click widget.Clickable
}

type ListWindow[T fmt.Stringer] struct {
	Filter func(item T, f string) bool

	items []listWindowItem[T]

	filtered []int
	// index of the selected item in the filtered list
	index     int
	done      bool
	cancelled bool

	theme *material.Theme
	input widget.Editor
	list  widget.List
}

func NewListWindow[T fmt.Stringer](th *material.Theme) *ListWindow[T] {
	return &ListWindow[T]{
		theme: th,
		input: widget.Editor{
			SingleLine: true,
			Submit:     true,
		},
		list: widget.List{
			List: layout.List{
				Axis: layout.Vertical,
			},
		},
	}
}

func (w *ListWindow[T]) SetItems(items []T) {
	w.items = make([]listWindowItem[T], len(items))
	w.filtered = make([]int, len(items))
	for i, item := range items {
		w.items[i] = listWindowItem[T]{
			item:  item,
			index: i,
			s:     item.String(),
		}
		w.filtered[i] = i
	}
}

func (w *ListWindow[T]) Cancelled() bool { return w.cancelled }
func (w *ListWindow[T]) Confirmed() (T, bool) {
	if !w.done {
		var zero T
		return zero, false
	}
	w.done = false
	return w.items[w.filtered[w.index]].item, true
}

func (w *ListWindow[T]) Layout(gtx layout.Context) layout.Dimensions {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	// XXX use constant for color
	paint.Fill(gtx.Ops, toColor(0xFFFFFFFF))

	key.InputOp{Tag: w, Keys: "↓|↑|⎋"}.Add(gtx.Ops)

	var spy *eventx.Spy

	dims := widget.Border{
		// XXX use a dedicated constant
		Color:        colors[colorTooltipBorder],
		CornerRadius: 0,
		Width:        1,
	}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		spy, gtx = eventx.Enspy(gtx)
		gtx.Constraints.Min.X = gtx.Constraints.Max.X

		fn2 := func(gtx layout.Context) layout.Dimensions {
			return material.List(w.theme, &w.list).Layout(gtx, len(w.filtered), func(gtx layout.Context, index int) layout.Dimensions {
				// XXX use constants for colors
				item := &w.items[w.filtered[index]]
				return item.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					if index == w.index {
						// XXX make this pretty, don't just change the font color
						paint.ColorOp{Color: toColor(0xFF0000FF)}.Add(gtx.Ops)
					} else if item.click.Hovered() {
						// XXX make this pretty, don't just change the font color
						paint.ColorOp{Color: toColor(0xFF00FFFF)}.Add(gtx.Ops)
					} else {
						paint.ColorOp{Color: toColor(0x000000FF)}.Add(gtx.Ops)
					}
					return widget.Label{MaxLines: 1}.Layout(gtx, w.theme.Shaper, text.Font{}, 14, item.s)
				})
			})
		}

		flex := layout.Flex{
			Axis: layout.Vertical,
		}
		editor := material.Editor(w.theme, &w.input, "")
		editor.Editor.Focus()
		return flex.Layout(gtx, layout.Rigid(editor.Layout), layout.Flexed(1, fn2))
	})

	// The editor widget selectively handles the up and down arrow keys, depending on the contents of the text field and
	// the position of the cursor. This means that our own InputOp won't always be getting all events. But due to the
	// selectiveness of the editor's InputOp, we can't fully rely on it, either. We need to combine the events of the
	// two.
	//
	// To be consistent, we handle all events after layout of the nested widgets, to have the same frame latency for all
	// events.
	handleKey := func(ev key.Event) {
		if ev.State == key.Press {
			firstVisible := w.list.Position.First
			lastVisible := w.list.Position.First + w.list.Position.Count - 1
			if w.list.Position.Offset > 0 {
				// The last element might be barely visible, even just one pixel. and we still want to scroll in that
				// case
				firstVisible++
			}
			if w.list.Position.OffsetLast < 0 {
				// The last element might be barely visible, even just one pixel. and we still want to scroll in that
				// case
				lastVisible--
			}
			visibleCount := lastVisible - firstVisible + 1

			switch ev.Name {
			case "↑":
				w.index--
				if w.index < firstVisible {
					// XXX compute the correct position. the user might have scrolled the list via its scrollbar.
					w.list.Position.First--
				}
				if w.index < 0 {
					w.index = len(w.filtered) - 1
					w.list.Position.First = w.index - visibleCount + 1
				}
			case "↓":
				w.index++
				if w.index > lastVisible {
					// XXX compute the correct position. the user might have scrolled the list via its scrollbar.
					w.list.Position.First++
				}
				if w.index >= len(w.filtered) {
					w.index = 0
					w.list.Position.First = 0
					w.list.Position.Offset = 0
				}
			case "⎋": // Escape
				w.cancelled = true
			}
		}
	}
	for _, evs := range spy.AllEvents() {
		for _, ev := range evs.Items {
			if ev, ok := ev.(key.Event); ok {
				handleKey(ev)
			}
		}
	}
	for _, ev := range w.input.Events() {
		switch ev.(type) {
		case widget.ChangeEvent:
			w.filtered = w.filtered[:0]
			f := w.input.Text()
			for _, item := range w.items {
				if w.Filter(item.item, f) {
					w.filtered = append(w.filtered, item.index)
				}
			}
			// TODO(dh): if the previously selected entry hasn't been filtered away, then it should stay selected.
			if w.index >= len(w.filtered) {
				// XXX if there are no items, then this sets w.index to -1, causing two bugs: hitting return will panic,
				// and once there are items again, none of them will be selected
				w.index = len(w.filtered) - 1
			}
		case widget.SubmitEvent:
			if len(w.filtered) != 0 {
				w.done = true
			}
		}
	}
	for i, idx := range w.filtered {
		if w.items[idx].click.Clicked() {
			w.index = i
			w.done = true
		}
	}

	for _, ev := range gtx.Events(w) {
		switch ev := ev.(type) {
		case key.Event:
			handleKey(ev)
		}
	}

	return dims
}

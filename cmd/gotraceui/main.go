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

// TODO(dh): switch to float32
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

	goroutineStateHeightDp unit.Dp = 10
	goroutineGapDp         unit.Dp = 5
	goroutineHeightDp      unit.Dp = goroutineStateHeightDp + goroutineGapDp

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 2

	spanBorderWidthDp unit.Dp = 1

	tooltipFontSizeSp unit.Sp = 14
)

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration
	// Imagine we're drawing all goroutines onto an infinitely long canvas. Timeline.Y specifies the Y of that infinite
	// canvas that the goroutine section's Y == 0 is displaying.
	Y int

	Theme *material.Theme

	Scrollbar widget.Scrollbar

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
		hovered   bool
	}

	// prevFrame records the timeline's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the timeline hasn't changed.
	prevFrame struct {
		Start    time.Duration
		End      time.Duration
		Y        int
		nsPerPx  float64
		dspSpans map[uint64][]struct {
			dspSpans       []Span
			startPx, endPx int
		}
		tickLabels []string
	}
}

func (tl *Timeline) unchanged() bool {
	return tl.prevFrame.Start == tl.Start && tl.prevFrame.End == tl.End && tl.prevFrame.nsPerPx == tl.nsPerPx && tl.prevFrame.Y == tl.Y
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
func (tl *Timeline) gidAtPoint(gtx layout.Context, at f32.Point) (uint64, bool) {
	if !tl.isOnGoroutine(gtx, at) {
		return 0, false
	}
	return uint64((at.Y + float32(tl.Y)) / float32(gtx.Metric.Dp(goroutineHeightDp))), true
}

// isOnGoroutine reports whether there's a goroutine under a point. The point should be relative to the goroutine
// section of the timeline.
func (tl *Timeline) isOnGoroutine(gtx layout.Context, at f32.Point) bool {
	goroutineHeight := gtx.Metric.Dp(goroutineHeightDp)
	goroutineStateHeight := gtx.Metric.Dp(goroutineStateHeightDp)
	rem := math.Mod(float64(at.Y)+float64(tl.Y), float64(goroutineHeight))
	return rem <= float64(goroutineStateHeight)
}

// zoomToCLickedSpan zooms to the span at a point, if any. The point should be relative to the goroutine section of the
// timeline.
func (tl *Timeline) zoomToClickedSpan(gtx layout.Context, at f32.Point) {
	gid, ok := tl.gidAtPoint(gtx, at)
	if !ok {
		return
	}
	g, ok := gs[gid]
	if !ok {
		// Not a known goroutine
		return
	}

	do := func(dspSpans []Span, startPx, endPx int) bool {
		start := dspSpans[0].Start
		end := dspSpans[len(dspSpans)-1].End

		if int(at.X) >= startPx && int(at.X) < endPx {
			tl.Start = start
			tl.End = end
			return true
		}

		return false
	}

	if tl.unchanged() {
		for _, prevSpans := range tl.prevFrame.dspSpans[gid] {
			if do(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx) {
				return
			}
		}
	} else {
		spans := tl.visibleSpans(g.Spans)
		it := renderedSpansIterator{
			tl:    tl,
			spans: spans,
		}
		for {
			dspSpans, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}

			if do(dspSpans, startPx, endPx) {
				return
			}
		}
	}
}

func (tl *Timeline) tickInterval(gtx layout.Context) time.Duration {
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	minTickDistance := gtx.Metric.Dp(minTickDistanceDp)
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(math.Round(float64(t) / tl.nsPerPx))
		if tickDistance >= minTickDistance {
			return t
		}
	}
	panic("unreachable")
}

func (tl *Timeline) visibleSpans(spans []Span) []Span {
	// Visible spans have to end after tl.Start and begin before tl.End
	start := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.End >= tl.Start
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
func (tl *Timeline) tsToPx(t time.Duration) float64 {
	return float64(t-tl.Start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(px int) time.Duration {
	return time.Duration(math.Round(float64(px)*tl.nsPerPx + float64(tl.Start)))
}

type renderedSpansIterator struct {
	offset         int
	tl             *Timeline
	spans          []Span
	prevExtendedBy int
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut []Span, startPx, endPx int, ok bool) {
	offset := it.offset
	spans := it.spans

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	minSpanWidth := gtx.Metric.Dp(minSpanWidthDp)
	startOffset := offset
	nsPerPx := it.tl.nsPerPx
	tlStart := it.tl.Start

	s := it.spans[offset]
	offset++
	startPx = int(math.Round(float64(s.Start-tlStart)/nsPerPx)) + it.prevExtendedBy
	endPx = int(math.Round(float64(s.End-tlStart) / nsPerPx))

	if endPx-startPx < minSpanWidth {
		// Collect enough spans until we've filled the minimum width

		// Compute the minimum duration for a span to stand on its own. We subtract 1 from minSpanWidth to account for
		// lucky rounding to pixel boundaries.
		minStandaloneDuration := float64(minSpanWidth-1) * nsPerPx

		for {
			if offset == len(it.spans) {
				// We've run out of spans
				break
			}

			s := spans[offset]
			if float64(s.End-s.Start) < minStandaloneDuration {
				// Even under ideal conditions - no extension and no truncation - this span wouldn't be able to stand on
				// its own. Avoid doing expensive math.
			} else {
				// Assume that we stop at this span. Compute the final size and the future prevExtendedBy. Use that to see
				// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
				var extended int
				if endPx-startPx < minSpanWidth {
					extended = minSpanWidth - (endPx - startPx)
				}
				theirStartPx := int(math.Round(float64(s.Start-tlStart)/nsPerPx)) + extended
				theirEndPx := int(math.Round(float64(s.End-tlStart) / nsPerPx))
				if theirEndPx-theirStartPx >= minSpanWidth {
					// Don't merge spans that can stand on their own
					break
				}
			}

			endPx = int(math.Round(float64(s.End-tlStart) / nsPerPx))
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

	{
		goroutineHeight := gtx.Metric.Dp(goroutineHeightDp)
		// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
		var maxGid uint64
		for gid := range gs {
			if gid > maxGid {
				maxGid = gid
			}
		}

		d := tl.Scrollbar.ScrollDistance()
		totalHeight := float32(maxGid * uint64(goroutineHeight))
		tl.Y += int(math.Round(float64(d * totalHeight)))
		if tl.Y < 0 {
			tl.Y = 0
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

	// Draw axis and goroutines
	Stack(gtx, tl.layoutAxis, tl.layoutGoroutines)

	// Draw zoom selection
	if tl.ZoomSelection.Active {
		one := int(math.Round(float64(tl.ZoomSelection.ClickAt.X)))
		two := int(math.Round(float64(tl.Global.cursorPos.X)))
		rect := clip.Rect{
			Min: image.Pt(min(one, two), 0),
			Max: image.Pt(max(one, two), gtx.Constraints.Max.Y),
		}
		paint.FillShape(gtx.Ops, colors[colorZoomSelection], rect.Op())
	}

	// Draw cursor
	rect := clip.Rect{
		Min: image.Pt(int(math.Round(float64(tl.Global.cursorPos.X))), 0),
		Max: image.Pt(int(math.Round(float64(tl.Global.cursorPos.X+1))), gtx.Constraints.Max.Y),
	}
	paint.FillShape(gtx.Ops, colors[colorCursor], rect.Op())

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (tl *Timeline) layoutAxis(gtx layout.Context) layout.Dimensions {
	// TODO draw smaller ticks between larger ticks
	tickInterval := tl.tickInterval(gtx)
	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	prevLabelEnd := float32(-1)
	// TODO(dh): calculating the label height on each frame risks that it changes between frames, which will cause the
	// goroutines to shift around as the axis section grows and shrinks.
	labelHeight := 0
	tickWidth := float32(gtx.Metric.Dp(tickWidthDp))
	tickHeight := float32(gtx.Metric.Dp(tickHeightDp))
	minTickLabelDistance := float32(gtx.Metric.Dp(minTickLabelDistanceDp))

	var labels []string
	if tl.unchanged() {
		labels = tl.prevFrame.tickLabels
	} else if tl.prevFrame.nsPerPx == tl.nsPerPx {
		// Panning only changes the first label
		labels = tl.prevFrame.tickLabels
		// TODO print thousands separator
		labels[0] = fmt.Sprintf("%d ns", tl.Start)
	} else {
		for t := tl.Start; t < tl.End; t += tickInterval {
			if t == tl.Start {
				// TODO print thousands separator
				labels = append(labels, fmt.Sprintf("%d ns", t))
			} else {
				// TODO separate value and unit symbol with a space
				labels = append(labels, fmt.Sprintf("+%s", t-tl.Start))
			}
		}
		tl.prevFrame.tickLabels = labels
	}

	var ticksPath clip.Path
	var ticksOps op.Ops
	ticksPath.Begin(&ticksOps)
	i := 0
	for t := tl.Start; t < tl.End; t += tickInterval {
		start := float32(tl.tsToPx(t) - float64(tickWidth/2))
		end := float32(tl.tsToPx(t) + float64(tickWidth/2))
		rect := FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := float32(tl.tsToPx(t+(tickInterval/10)*time.Duration(j))) - tickWidth/2
			smallEnd := float32(tl.tsToPx(t+(tickInterval/10)*time.Duration(j))) + tickWidth/2
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

		if t == tl.Start {
			label := labels[i]
			stack := op.Offset(image.Pt(0, int(tickHeight))).Push(gtx.Ops)
			paint.ColorOp{Color: colors[colorTickLabel]}.Add(gtx.Ops)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
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
			dims := widget.Label{MaxLines: 1}.Layout(gtx, tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
			call := macro.Stop()

			if start-float32(dims.Size.X/2) > prevLabelEnd+minTickLabelDistance {
				prevLabelEnd = start + float32(dims.Size.X/2)
				if start+float32(dims.Size.X/2) <= float32(gtx.Constraints.Max.X) {
					stack := op.Offset(image.Pt(int(math.Round(float64(start-float32(dims.Size.X/2)))), int(tickHeight))).Push(gtx.Ops)
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

func (tl *Timeline) layoutGoroutines(gtx layout.Context) layout.Dimensions {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	// Dragging doesn't produce Move events, even if we're not listening for dragging
	pointer.InputOp{Tag: &tl.Goroutines, Types: pointer.Move | pointer.Drag | pointer.Press | pointer.Leave | pointer.Enter}.Add(gtx.Ops)
	for _, e := range gtx.Events(&tl.Goroutines) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Move, pointer.Drag:
			tl.Goroutines.cursorPos = ev.Position
		case pointer.Leave:
			tl.Goroutines.hovered = false
		case pointer.Enter:
			tl.Goroutines.hovered = true
		case pointer.Press:
			if ev.Buttons&pointer.ButtonTertiary != 0 && ev.Modifiers&key.ModCtrl != 0 {
				tl.zoomToClickedSpan(gtx, ev.Position)
			}
		}
	}

	// Draw goroutine lifetimes
	// TODO(dh): draw a scrollbar
	//
	// We batch draw operations by color to avoid making thousands of draw calls. See
	// https://lists.sr.ht/~eliasnaur/gio/%3C871qvbdx5r.fsf%40honnef.co%3E#%3C87v8smctsd.fsf@honnef.co%3E
	//
	// TODO(dh): use path type for outlines, too
	var outlines op.Ops
	var outlinesPath clip.Path
	outlinesPath.Begin(&outlines)

	type path struct {
		ops  op.Ops
		path clip.Path
	}

	//gcassert:noescape
	paths := [...]path{
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

	eventsPath := &path{}
	eventsPath.path.Begin(&eventsPath.ops)

	for i := range paths {
		p := &paths[i]
		p.path.Begin(&p.ops)
	}

	goroutineHeight := gtx.Metric.Dp(goroutineHeightDp)
	goroutineStateHeight := gtx.Metric.Dp(goroutineStateHeightDp)
	spanBorderWidth := gtx.Metric.Dp(spanBorderWidthDp)

	// Draw a scrollbar, then clip to smaller area. We've already computed nsPerPx, so clipping the goroutine area will
	// not bring us out of alignment with the axis.
	{
		// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
		var maxGid uint64
		for gid := range gs {
			if gid > maxGid {
				maxGid = gid
			}
		}
		totalHeight := float32((maxGid + 1) * uint64(goroutineHeight))
		fraction := float32(gtx.Constraints.Max.Y) / totalHeight
		offset := float32(tl.Y) / totalHeight
		sb := material.Scrollbar(tl.Theme, &tl.Scrollbar)
		stack := op.Offset(image.Pt(gtx.Constraints.Max.X-gtx.Metric.Dp(sb.Width()), 0)).Push(gtx.Ops)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
		stack.Pop()

		gtx.Constraints.Max.X -= gtx.Metric.Dp(sb.Width())
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	}

	for gid, g := range gs {
		y := goroutineHeight*int(gid) - tl.Y
		if y < -goroutineStateHeight || y > gtx.Constraints.Max.Y {
			// Don't draw goroutines that would be fully hidden, but do draw partially hidden ones
			continue
		}
		func() {
			// Our goroutines aren't sorted, causing our offsets to jump all over the place. That's why we calculate
			// absolute offsets and pop them after each iteration.
			defer op.Offset(image.Pt(0, y)).Push(gtx.Ops).Pop()

			gidAtPoint, isOnGoroutine := tl.gidAtPoint(gtx, tl.Goroutines.cursorPos)

			firstStart := -1
			lastEnd := -1
			first := true

			doSpans := func(dspSpans []Span, startPx, endPx int) {
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
				minP = f32.Point{X: float32(max(startPx, 0)), Y: float32(y)}
				maxP = f32.Point{X: float32(min(endPx, gtx.Constraints.Max.X)), Y: float32(y + goroutineStateHeight)}
				if first && startPx >= 0 {
					// We don't want two borders right next to each other, nor do we want a border for truncated spans
					minP.X += float32(spanBorderWidth)
				}
				minP.Y += float32(spanBorderWidth)
				if endPx <= gtx.Constraints.Max.X {
					maxP.X -= float32(spanBorderWidth)
				}
				maxP.Y -= float32(spanBorderWidth)

				p := &paths[c]
				p.path.MoveTo(minP)
				p.path.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
				p.path.LineTo(maxP)
				p.path.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
				p.path.Close()

				if spanHasEvents(g.Events, dspSpans[0].Start, dspSpans[len(dspSpans)-1].End) {
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

				if tl.Goroutines.hovered && int(tl.Goroutines.cursorPos.X) >= startPx && int(tl.Goroutines.cursorPos.X) < endPx && isOnGoroutine && gidAtPoint == gid {
					// TODO have a gap between the cursor and the tooltip
					// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
					macro := op.Record(gtx.Ops)
					ttX := int(math.Round(float64(tl.Goroutines.cursorPos.X)))
					ttY := int(math.Round(float64(tl.Goroutines.cursorPos.Y))) - y
					stack := op.Offset(image.Pt(ttX, ttY)).Push(gtx.Ops)
					SpanTooltip{dspSpans, tl.Theme.Shaper}.Layout(gtx)
					stack.Pop()
					call := macro.Stop()
					op.Defer(gtx.Ops, call)
				}

				first = false
			}

			if tl.unchanged() {
				for _, prevSpans := range tl.prevFrame.dspSpans[gid] {
					doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
				}
			} else {
				allDspSpans := tl.prevFrame.dspSpans[gid][:0]
				it := renderedSpansIterator{
					tl:    tl,
					spans: tl.visibleSpans(g.Spans),
				}
				for {
					dspSpans, startPx, endPx, ok := it.next(gtx)
					if !ok {
						break
					}
					allDspSpans = append(allDspSpans, struct {
						dspSpans       []Span
						startPx, endPx int
					}{dspSpans, startPx, endPx})
					doSpans(dspSpans, startPx, endPx)
				}
				tl.prevFrame.dspSpans[gid] = allDspSpans
			}

			if !first {
				// Outlines are not grouped with other spans of the same color because they have to be drawn before spans.
				firstStart = max(firstStart, 0)
				lastEnd = min(lastEnd, gtx.Constraints.Max.X)
				outlinesPath.MoveTo(f32.Point{X: float32(firstStart), Y: float32(y)})
				outlinesPath.LineTo(f32.Point{X: float32(lastEnd), Y: float32(y)})
				outlinesPath.LineTo(f32.Point{X: float32(lastEnd), Y: float32(y) + float32(goroutineStateHeight)})
				outlinesPath.LineTo(f32.Point{X: float32(firstStart), Y: float32(y) + float32(goroutineStateHeight)})
				outlinesPath.Close()
			} else {
				// No spans for this goroutine
			}
		}()
	}

	paint.FillShape(gtx.Ops, color.NRGBA{A: 0xFF}, clip.Outline{Path: outlinesPath.End()}.Op())
	for cIdx, p := range paths {
		paint.FillShape(gtx.Ops, colors[cIdx], clip.Outline{Path: p.path.End()}.Op())
	}
	// TODO(dh): find a nice color for this
	paint.FillShape(gtx.Ops, toColor(0xFF00FFFF), clip.Outline{Path: eventsPath.path.End()}.Op())

	return layout.Dimensions{Size: gtx.Constraints.Max}
}

type SpanTooltip struct {
	Spans  []Span
	shaper text.Shaper
}

func (tt SpanTooltip) Layout(gtx layout.Context) layout.Dimensions {
	var tooltipBorderWidth = gtx.Metric.Dp(2)

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
		case stateReady:
			label += "ready"
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
}

//gcassert:inline
func (s Span) Duration() time.Duration {
	return s.End - s.Start
}

// TODO(dh): avoid global state
var gs = map[uint64]*Goroutine{}

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

	getG := func(gid uint64) *Goroutine {
		g, ok := gs[gid]
		if ok {
			return g
		}
		g = &Goroutine{}
		gs[gid] = g
		return g
	}

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
			continue
		case trace.EvGoSysBlock:
			// TODO(dh): have a special state for this
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

		getG(gid).Spans = append(getG(gid).Spans, Span{Start: time.Duration(ev.Ts), State: state, Reason: reason})
	}

	for _, g := range gs {
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
}

type colorIndex int

const (
	colorStateInactive colorIndex = iota
	colorStateActive

	colorStateBlocked
	colorStateBlockedHappensBefore
	colorStateBlockedNet
	colorStateBlockedGC
	colorStateBlockedSyscall

	colorStateReady
	colorStateStuck
	colorStateMerged
	colorStateUnknown

	colorBackground
	colorZoomSelection
	colorCursor
	colorTick
	colorTickLabel
	colorTooltipText
	colorTooltipBackground
	colorTooltipBorder
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
	stateReady
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
		stateActive: true,
		stateReady:  true,
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
	stateReady: {
		stateActive: true,
	},
	stateBlocked:       {stateReady: true},
	stateBlockedSend:   {stateReady: true},
	stateBlockedRecv:   {stateReady: true},
	stateBlockedSelect: {stateReady: true},
	stateBlockedSync:   {stateReady: true},
	stateBlockedCond:   {stateReady: true},
	stateBlockedNet:    {stateReady: true},
	stateBlockedGC:     {stateReady: true},
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
	tl := Timeline{
		Start: start,
		End:   end,
		Theme: material.NewTheme(gofont.Collection()),
	}
	tl.prevFrame.dspSpans = map[uint64][]struct {
		dspSpans []Span
		startPx  int
		endPx    int
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
			// XXX detect HiDPI
			gtx.Metric.PxPerDp = 2

			for _, ev := range gtx.Events(profileTag) {
				fmt.Println(ev)
			}
			profile.Op{Tag: profileTag}.Add(gtx.Ops)

			tl.Layout(gtx)
			tl.prevFrame.Start = tl.Start
			tl.prevFrame.End = tl.End
			tl.prevFrame.nsPerPx = tl.nsPerPx
			tl.prevFrame.Y = tl.Y

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

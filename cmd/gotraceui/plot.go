package main

// OPT(dh): cache the plot the same way we cache timelines, to avoid redrawing them when nothing has changed.

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"math"
	rtrace "runtime/trace"
	"sort"
	"strings"

	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/f32"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/widget"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
)

type PlotSeries struct {
	Name   string
	Points []ptrace.Point
	Filled bool
	Color  color.NRGBA

	disabled bool
}

type Plot struct {
	Name   string
	Unit   string
	series []PlotSeries

	min uint64
	max uint64

	scratchPoints  []f32.Point
	scratchStrings []string
	pointerAt      f32.Point
	hovered        bool
	hideLegends    bool
	autoScale      bool

	// Used by drawOrthogonalLine to correctly overlap lines at changes in direction
	prevDirection uint8

	prevFrame struct {
		start, end trace.Timestamp
		// bitmap of disabled series
		disabledSeries uint64
	}
}

const (
	plotDirectionNone = iota
	plotDirectionHorizontal
	plotDirectionVertical
)

func (pl *Plot) AddSeries(series ...PlotSeries) {
	pl.series = append(pl.series, series...)
	_, max := pl.computeExtents(0, math.MaxInt64)
	pl.min = 0
	pl.max = max
}

func (pl *Plot) computeExtents(start, end trace.Timestamp) (min, max uint64) {
	min = math.MaxUint64
	max = 0

	for _, s := range pl.series {
		if s.disabled {
			continue
		}
		idx := sort.Search(len(s.Points), func(i int) bool {
			return s.Points[i].When >= start
		})
		// Decrement by one to consider a point that's out of view but extends into view
		idx--
		if idx < 0 {
			idx = 0
		}
		for _, p := range s.Points[idx:] {
			if p.When >= end {
				break
			}
			if p.Value < min {
				min = p.Value
			}
			if p.Value > max {
				max = p.Value
			}
		}
	}

	if min == max {
		min--
		max++
	}

	d := max - min
	if n := min - d/10; n <= min {
		min = n
	} else {
		min = 0
	}
	max += d / 10

	return min, max
}

func (pl *Plot) Layout(win *theme.Window, gtx layout.Context, cv *Canvas) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Plot.Layout").End()
	var clicked bool
	for _, e := range gtx.Events(pl) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			pl.hovered = true
			pl.pointerAt = ev.Position
		case pointer.Press:
			if ev.Buttons == pointer.ButtonSecondary {
				clicked = true
			}
		case pointer.Leave, pointer.Cancel:
			pl.hovered = false
		}
	}

	if pl.autoScale {
		r := rtrace.StartRegion(context.Background(), "auto-scaling")
		var bitmap uint64
		for i, s := range pl.series {
			if s.disabled {
				bitmap |= 1 << i
			}
		}
		if pl.prevFrame.start != cv.start || pl.prevFrame.end != cv.end || pl.prevFrame.disabledSeries != bitmap {
			pl.min, pl.max = pl.computeExtents(cv.start, cv.end)
		}
		pl.prevFrame.start = cv.start
		pl.prevFrame.end = cv.end
		pl.prevFrame.disabledSeries = bitmap
		r.End()
	}

	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: pl, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Press | pointer.Cancel}.Add(gtx.Ops)

	paint.Fill(gtx.Ops, rgba(0xDFFFEAFF))

	{
		r := rtrace.StartRegion(context.Background(), "draw all points")
		for _, s := range pl.series {
			if s.disabled {
				continue
			}
			pl.drawPoints(gtx, cv, s)
		}
		r.End()
	}

	if pl.hovered {
		r := rtrace.StartRegion(context.Background(), "hovered")
		// When drawing the plot, multiple points can fall on the same pixel, in which case we pick the last value for a
		// given pixel.
		//
		// When hovering, we want to get the most recent point for the hovered pixel. We do this by searching for the
		// first point whose timestamp would fall on a later pixel, and then use the point immediately before that.

		ts := cv.pxToTs(pl.pointerAt.X + 1)

		lines := pl.scratchStrings[:0]
		for _, s := range pl.series {
			if s.disabled {
				continue
			}
			idx := sort.Search(len(s.Points), func(idx int) bool {
				pt := s.Points[idx]
				return pt.When > ts
			})
			idx--
			if idx < 0 {
				continue
			}

			lines = append(lines, local.Sprintf("%s: %d %s", s.Name, s.Points[idx].Value, pl.Unit))
		}
		pl.scratchStrings = lines[:0]

		if len(lines) > 0 {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				return theme.Tooltip(win.Theme, strings.Join(lines, "\n")).Layout(win, gtx)
			})
		}
		r.End()
	}

	if clicked {
		r := rtrace.StartRegion(context.Background(), "context menu")
		items := []*theme.MenuItem{
			{
				Label: PlainLabel("Reset extents"),
				Do: func(gtx layout.Context) {
					pl.min = 0
					_, pl.max = pl.computeExtents(0, math.MaxInt64)
				},
			},
			{
				Label: PlainLabel("Set extents to global extrema"),
				Do: func(gtx layout.Context) {
					pl.min, pl.max = pl.computeExtents(0, math.MaxInt64)
				},
			},
			{
				Label: PlainLabel("Set extents to local extrema"),
				Do: func(gtx layout.Context) {
					pl.min, pl.max = pl.computeExtents(cv.start, cv.end)
				},
			},
			{
				Label: ToggleLabel("Don't auto-set extents", "Auto-set extents to local extrema", &pl.autoScale),
				Do: func(gtx layout.Context) {
					pl.autoScale = !pl.autoScale
				},
			},
			{
				Label: ToggleLabel("Show legends", "Hide legends", &pl.hideLegends),
				Do: func(gtx layout.Context) {
					pl.hideLegends = !pl.hideLegends
				},
			},
		}
		for i := range pl.series {
			s := &pl.series[i]
			var label string
			if s.disabled {
				label = fmt.Sprintf("Show %q series", s.Name)
			} else {
				label = fmt.Sprintf("Hide %q series", s.Name)
			}
			item := &theme.MenuItem{
				Label: PlainLabel(label),
				Do: func(gtx layout.Context) {
					s.disabled = !s.disabled
				},
			}
			items = append(items, item)
		}
		win.SetContextMenu(items)
		r.End()
	}

	if !pl.hideLegends {
		gtx := gtx
		gtx.Constraints.Min = image.Point{}

		r := rtrace.StartRegion(context.Background(), "legends")
		// Print legends
		m := op.Record(gtx.Ops)
		dims := widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, local.Sprintf("%d %s", pl.max, pl.Unit))
		c := m.Stop()
		paint.FillShape(gtx.Ops, rgba(0xFFFFFFFF), clip.Rect{Max: dims.Size}.Op())
		paint.ColorOp{Color: rgba(0x000000FF)}.Add(gtx.Ops)
		c.Add(gtx.Ops)

		m = op.Record(gtx.Ops)
		dims = widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, local.Sprintf("%d %s", pl.min, pl.Unit))
		c = m.Stop()
		defer op.Offset(image.Pt(0, gtx.Constraints.Max.Y-dims.Size.Y)).Push(gtx.Ops).Pop()
		paint.FillShape(gtx.Ops, rgba(0xFFFFFFFF), clip.Rect{Max: dims.Size}.Op())
		paint.ColorOp{Color: rgba(0x000000FF)}.Add(gtx.Ops)
		c.Add(gtx.Ops)
		r.End()
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}
}

func (pl *Plot) start(gtx layout.Context, cv *Canvas) int {
	// XXX check for rounding error
	// XXX this can probably overflow
	start := 0
	if v := cv.tsToPx(0); int(v) > start {
		start = int(v)
	}
	return start
}

// end returns the width (in pixels) of the canvas, capped to the actual length of the trace.
func (pl *Plot) end(gtx layout.Context, cv *Canvas) int {
	// XXX check for rounding error
	// XXX this can probably overflow

	timelineEnd := gtx.Constraints.Max.X
	lastEvent := cv.trace.Events[len(cv.trace.Events)-1]
	if end := cv.tsToPx(lastEvent.Ts); int(end) < timelineEnd {
		timelineEnd = int(end)
	}
	if timelineEnd < 0 {
		return 0
	}
	return timelineEnd
}

func (pl *Plot) drawPoints(gtx layout.Context, cv *Canvas, s PlotSeries) {
	defer rtrace.StartRegion(context.Background(), "draw points").End()
	const lineWidth = 2

	var drawLine func(p *clip.Path, pt f32.Point, width float32)
	if s.Filled {
		drawLine = func(p *clip.Path, pt f32.Point, width float32) {
			// the width doesn't matter for filled series, we will later fill the entire area
			p.LineTo(pt)
		}
	} else {
		drawLine = pl.drawOrthogonalLine
	}

	scaleValue := func(v uint64) float32 {
		y := float32(scale(float64(pl.min), float64(pl.max), float64(gtx.Constraints.Max.Y), 0, float64(v)))
		if y < 0 {
			y = 0
		}
		return y
	}

	canvasStart := pl.start(gtx, cv)
	canvasEnd := pl.end(gtx, cv)
	if canvasEnd == 0 || canvasStart >= canvasEnd {
		// No points to display
		return
	}

	var points []f32.Point
	if cap(pl.scratchPoints) >= canvasEnd {
		points = pl.scratchPoints[canvasStart:canvasEnd]
		for i := range points {
			points[i] = f32.Point{}
		}
	} else {
		pl.scratchPoints = make([]f32.Point, gtx.Constraints.Max.X)
		points = pl.scratchPoints[canvasStart:canvasEnd]
	}

	values := s.Points
	for i := range points {
		ts := cv.pxToTs(float32(i + canvasStart + 1))
		idx, _ := slices.BinarySearchFunc(values, ptrace.Point{When: ts}, func(p1, p2 ptrace.Point) int {
			return compare(p1.When, p2.When)
		})

		if idx == 0 {
			continue
		}
		points[i] = f32.Pt(float32(i+canvasStart), scaleValue(values[idx-1].Value))
	}

	var path clip.Path
	path.Begin(gtx.Ops)
	var first f32.Point
	var start int
	for i, pt := range points {
		if pt != (f32.Point{}) {
			path.MoveTo(pt)
			first = pt
			start = i + 1
			break
		}
	}

	path.MoveTo(first)
	for _, pt := range points[start:] {
		if pt == (f32.Point{}) {
			continue
		}
		drawLine(&path, f32.Pt(pt.X, path.Pos().Y), lineWidth)
		drawLine(&path, pt, lineWidth)
	}

	// Continue the last point
	if start != 0 {
		drawLine(&path, f32.Pt(float32(canvasEnd), path.Pos().Y), lineWidth)
	}

	if s.Filled {
		drawLine(&path, f32.Pt(float32(canvasEnd), float32(gtx.Constraints.Max.Y)), lineWidth)
		drawLine(&path, f32.Pt(first.X, float32(gtx.Constraints.Max.Y)), lineWidth)
		drawLine(&path, first, lineWidth)
		paint.FillShape(gtx.Ops, s.Color, clip.Outline{Path: path.End()}.Op())
	} else {
		paint.FillShape(gtx.Ops, s.Color, clip.Outline{Path: path.End()}.Op())
	}
}

func compare[T constraints.Ordered](a, b T) int {
	if a < b {
		return -1
	} else if a == b {
		return 0
	} else {
		return 1
	}
}

func (pl *Plot) drawOrthogonalLine(p *clip.Path, pt f32.Point, width float32) {
	// TODO(dh): this code can't be used with transparent colors because we draw over some regions multiple times.

	if pt == p.Pos() {
		return
	}

	if p.Pos().X == pt.X {
		// Vertical line
		left := pt.X - width/2
		right := pt.X + width/2

		if pl.prevDirection == plotDirectionHorizontal {
			p.Move(f32.Pt(0, width/2))
		}

		orig := p.Pos()
		p.Move(f32.Pt(-width/2, 0))
		p.LineTo(f32.Pt(left, pt.Y))
		p.LineTo(f32.Pt(right, pt.Y))
		p.LineTo(f32.Pt(right, orig.Y))
		p.LineTo(f32.Pt(orig.X-width, orig.Y))
		p.MoveTo(pt)

		pl.prevDirection = plotDirectionVertical
	} else if p.Pos().Y == pt.Y {
		// Horizontal line
		top := pt.Y - width/2
		bottom := pt.Y + width/2

		if pl.prevDirection == plotDirectionVertical {
			p.Move(f32.Pt(-width/2, 0))
		}

		orig := p.Pos()
		p.Move(f32.Pt(0, -width/2))
		p.LineTo(f32.Pt(pt.X, top))
		p.LineTo(f32.Pt(pt.X, bottom))
		p.LineTo(f32.Pt(orig.X, bottom))
		p.LineTo(f32.Pt(orig.X, orig.Y-width))
		p.MoveTo(pt)

		pl.prevDirection = plotDirectionHorizontal
	} else {
		panic(fmt.Sprintf("non-orthogonal line %s–%s", p.Pos(), pt))
	}
}

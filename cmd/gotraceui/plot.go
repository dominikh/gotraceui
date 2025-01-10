package main

import (
	"context"
	"fmt"
	"image"
	"iter"
	"math"
	rtrace "runtime/trace"
	"sort"
	"strings"
	"time"

	"honnef.co/go/curve"
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/f32"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	exptrace "golang.org/x/exp/trace"
)

const (
	debugProfile        = false
	debugDisableCaching = false
)

type PlotStyle uint8

const (
	PlotFilled = 1 << iota
	PlotStaircase
)

type PlotSeries struct {
	Name   string
	Metric ptrace.Metric
	Style  PlotStyle
	Color  color.Oklch

	cachedDecimation ptrace.Metric
	disabled         bool
}

type Plot struct {
	Name   string
	Unit   string
	series []PlotSeries

	min uint64
	max uint64

	click gesture.Click
	hover gesture.Hover

	scratchStrings []string
	// Scratch space used for the result of downsampling
	scratch     []int
	hideLegends bool
	autoScale   bool

	prevFrame struct {
		constraints layout.Constraints
		hideLegends bool
		autoScale   bool
		ops         mem.ReusableOps
		call        op.CallOp

		start, end exptrace.Time
		// bitmap of disabled series
		disabledSeries uint64
	}
}

const zoom1Pixels = 32768

func (pl *Plot) AddSeries(series ...PlotSeries) {
	for i := range series {
		s := &series[i]
		points := s.Metric
		if len(points.Timestamps) == 0 {
			continue
		}
		nsPerPx := float64(points.Timestamps[len(points.Timestamps)-1]) / float64(zoom1Pixels)
		// TODO make pixel count proportional to length of trace (or number of samples?)
		indices := downsample(points, 0, zoom1Pixels, nsPerPx, nil)
		decimation := ptrace.Metric{
			Timestamps: make([]exptrace.Time, len(indices)),
			Values:     make([]uint64, len(indices)),
		}
		for i, idx := range indices {
			decimation.Timestamps[i] = s.Metric.Timestamps[idx]
			decimation.Values[i] = s.Metric.Values[idx]
		}
		s.cachedDecimation = decimation
	}
	pl.series = append(pl.series, series...)
	_, max := pl.computeExtents(0, math.MaxInt64)
	pl.min = 0
	pl.max = max
}

func (pl *Plot) computeExtents(start, end exptrace.Time) (min, max uint64) {
	min = math.MaxUint64
	max = 0

	for _, s := range pl.series {
		if len(s.Metric.Timestamps) == 0 {
			continue
		}
		if s.disabled {
			continue
		}
		idx := sort.Search(len(s.Metric.Timestamps), func(i int) bool {
			return s.Metric.Timestamps[i] >= start
		})
		// Decrement by one to consider a point that's out of view but extends into view
		idx--
		if idx < 0 {
			idx = 0
		}
		values := s.Metric.Values
		timestamps := s.Metric.Timestamps
		_ = timestamps[len(values)-1]
		for i := idx; i < len(values); i++ {
			// OPT make sure this doesn't have a bounds check
			// OPT use SIMD minmax when possible
			if timestamps[i] >= end {
				break
			}
			v := s.Metric.Values[i]
			if v < min {
				min = v
			}
			if v > max {
				max = v
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
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	pl.hover.Update(gtx.Queue)
	pl.click.Add(gtx.Ops)
	pl.hover.Add(gtx.Ops)

	var clicked bool
	for _, click := range pl.click.Update(gtx.Queue) {
		if click.Kind == gesture.KindPress && click.Button == pointer.ButtonSecondary {
			clicked = true
			break
		}
	}

	if clicked {
		r := rtrace.StartRegion(context.Background(), "context menu")
		items := []*theme.MenuItem{
			{
				Label: PlainLabel("Reset extents"),
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						pl.min = 0
						_, pl.max = pl.computeExtents(0, math.MaxInt64)
						pl.autoScale = false
					})
				},
			},
			{
				Label: PlainLabel("Set extents to global extrema"),
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						pl.min, pl.max = pl.computeExtents(0, math.MaxInt64)
					})
				},
			},
			{
				Label: PlainLabel("Set extents to local extrema"),
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						pl.min, pl.max = pl.computeExtents(cv.start, cv.End())
					})
				},
			},
			{
				Label: ToggleLabel("Don't auto-set extents", "Auto-set extents to local extrema", &pl.autoScale),
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						pl.autoScale = !pl.autoScale
					})
				},
			},
			{
				Label: ToggleLabel("Show legends", "Hide legends", &pl.hideLegends),
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						pl.hideLegends = !pl.hideLegends
					})
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
				Action: func() theme.Action {
					return theme.ExecuteAction(func(gtx layout.Context) {
						s.disabled = !s.disabled
					})
				},
			}
			items = append(items, item)
		}
		win.SetContextMenu(items)
		r.End()
	}

	var bitmap uint64
	for i, s := range pl.series {
		if s.disabled {
			bitmap |= 1 << i
		}
	}

	if !debugDisableCaching &&
		cv.unchanged(gtx) &&
		gtx.Constraints == pl.prevFrame.constraints &&
		bitmap == pl.prevFrame.disabledSeries &&
		pl.hideLegends == pl.prevFrame.hideLegends &&
		pl.autoScale == pl.prevFrame.autoScale {

		pl.prevFrame.call.Add(gtx.Ops)
		debugCaching(win, gtx)
	} else {
		pl.prevFrame.constraints = gtx.Constraints
		pl.prevFrame.hideLegends = pl.hideLegends
		pl.prevFrame.disabledSeries = bitmap
		pl.prevFrame.autoScale = pl.autoScale

		origOps := gtx.Ops
		gtx.Ops = pl.prevFrame.ops.Get()
		macro := op.Record(gtx.Ops)
		defer func() {
			call := macro.Stop()
			call.Add(origOps)
			pl.prevFrame.call = call
		}()

		if pl.autoScale {
			r := rtrace.StartRegion(context.Background(), "auto-scaling")
			if pl.prevFrame.start != cv.start || pl.prevFrame.end != cv.End() || pl.prevFrame.disabledSeries != bitmap {
				pl.min, pl.max = pl.computeExtents(cv.start, cv.End())
			}
			pl.prevFrame.start = cv.start
			pl.prevFrame.end = cv.End()
			r.End()
		}

		theme.Fill(win, gtx.Ops, oklch(97.14, 0.043, 156.75))

		{
			r := rtrace.StartRegion(context.Background(), "draw all points")
			for _, s := range pl.series {
				if s.disabled {
					continue
				}
				pl.drawPoints(win, gtx, cv, s)
			}
			r.End()
		}

		if !pl.hideLegends {
			gtx := gtx
			gtx.Constraints.Min = image.Point{}

			r := rtrace.StartRegion(context.Background(), "legends")
			// Print legends
			rec := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				return theme.Label(win.Theme, local.Sprintf("%d %s", pl.max, pl.Unit)).Layout(win, gtx)
			})
			theme.FillShape(win, gtx.Ops, oklch(100, 0, 0), clip.Rect{Max: rec.Dimensions.Size}.Op())
			paint.ColorOp{Color: win.ConvertColor(oklch(0, 0, 0))}.Add(gtx.Ops)
			rec.Layout(win, gtx)

			rec = theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				return theme.Label(win.Theme, local.Sprintf("%d %s", pl.min, pl.Unit)).Layout(win, gtx)
			})
			defer op.Offset(image.Pt(0, gtx.Constraints.Max.Y-rec.Dimensions.Size.Y)).Push(gtx.Ops).Pop()
			theme.FillShape(win, gtx.Ops, oklch(100, 0, 0), clip.Rect{Max: rec.Dimensions.Size}.Op())
			paint.ColorOp{Color: win.ConvertColor(oklch(0, 0, 0))}.Add(gtx.Ops)
			rec.Layout(win, gtx)
			r.End()
		}
	}

	if pl.click.Hovered() {
		r := rtrace.StartRegion(context.Background(), "hovered")
		// When hovering, we want to get the most recent point for the hovered pixel. We do this by searching for the
		// first point whose timestamp would fall on a later pixel, and then use the point immediately before that.

		ts := cv.pxToTs(pl.hover.Pointer().X + 1)

		lines := pl.scratchStrings[:0]
		for _, s := range pl.series {
			if s.disabled {
				continue
			}
			idx := sort.Search(len(s.Metric.Timestamps), func(idx int) bool {
				return s.Metric.Timestamps[idx] > ts
			})
			idx--
			if idx < 0 {
				continue
			}

			lines = append(lines, local.Sprintf("%s: %d %s", s.Name, s.Metric.Values[idx], pl.Unit))
		}
		pl.scratchStrings = lines[:0]

		if len(lines) > 0 {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				return theme.Tooltip(win.Theme, strings.Join(lines, "\n")).Layout(win, gtx)
			})
		}
		r.End()
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}
}

// [1] https://dl.acm.org/doi/abs/10.14778/2732951.2732953
func downsample(
	points ptrace.Metric,
	start exptrace.Time,
	numBins int,
	nsPerBin float64,
	out []int,
) []int {
	defer rtrace.StartRegion(context.Background(), "downsample").End()
	return downsample2(points.Timestamps, points.Values, start, numBins, nsPerBin, out)
}

func downsample2[T ~uint64 | ~uint32 | ~uint16 | ~uint8](
	timestamps []exptrace.Time,
	values []T,
	start exptrace.Time,
	numBins int,
	nsPerBin float64,
	out []int,
) []int {
	off := sort.Search(len(timestamps), func(i int) bool {
		return timestamps[i] >= start
	})

	_ = timestamps[len(values)-1]
	for x := range numBins {
		if off == len(values) {
			break
		}

		binEndTs := exptrace.Time(math.Round(float64(x+1)*nsPerBin + float64(start)))
		binEndIdx := sort.Search(len(timestamps), func(i int) bool {
			return timestamps[i] >= binEndTs
		})

		switch {
		case binEndIdx < off:
			panic("unreachable")
		case binEndIdx == off:
			continue
		case binEndIdx == off+1:
			out = append(out, off)
			off = binEndIdx
			continue
		default:
			minIdx, maxIdx := argminmax(values[off:binEndIdx])
			minIdx += off
			maxIdx += off

			second, third := min(minIdx, maxIdx), max(minIdx, maxIdx)
			out = append(out, off)
			if second != off && second != binEndIdx-1 {
				out = append(out, second)
			}
			if third != off && third != binEndIdx-1 {
				out = append(out, third)
			}
			out = append(out, binEndIdx-1)
			off = binEndIdx
		}
	}
	if off < len(values) {
		// Include first off-screen point so we connect to it.
		out = append(out, off)
	}
	return out
}

func (pl *Plot) drawPoints(win *theme.Window, gtx layout.Context, cv *Canvas, s PlotSeries) {
	defer rtrace.StartRegion(context.Background(), "Plot.drawPoints").End()
	rtrace.Log(context.Background(), "", s.Name)

	if len(s.Metric.Values) < 2 {
		return
	}

	const lineWidth = 3

	scaleValue := func(v uint64) float64 {
		y := scale(float64(pl.min), float64(pl.max), float64(gtx.Constraints.Max.Y), 0, float64(v))
		if y < 0 {
			y = 0
		}
		return y
	}

	// We cannot use cv.End because that respects the scrollbar, which isn't visible for the plot.
	visibleStartTs := max(0, cv.start)
	visibleEndTs := min(cv.trace.End(), cv.pxToTs(float32(gtx.Constraints.Max.X)))
	if visibleStartTs >= visibleEndTs {
		return
	}
	if visibleEndTs < 0 {
		return
	}

	points := s.Metric
	zoom := float64(cv.trace.End()-cv.trace.Start()) / float64((cv.End() - cv.start))
	if zoom1Pixels/zoom >= float64(gtx.Constraints.Max.X) {
		// We cache a decimation of the entire plot down to zoom1Pixels many
		// pixels, which is a value several times larger than common window
		// widths.
		//
		// As long as zoom1Pixels * (displayed trace duration / total trace
		// duration) >= the window width, this decimation provides enough points
		// to render that portion of the trace at sufficient detail for the
		// window's width, saving us from having to downsample from all points,
		// of which there might be millions. Once the condition no longer holds,
		// the amount of points covered by the visible portion of the trace is
		// hopefully small enough that we can downsample them fast enough.
		//
		// Because the downsampling is based on evenly spaced bins and not a
		// curve simplification algorithm like Visvalingam–Whyatt, this approach
		// is not sensitive to the uneven distribution of points in a plot. That
		// is, no matter which region of the trace we zoom to, as long as the
		// ratio of displayed trace to full trace to window width is acceptable,
		// the cached decimation will provide enough detail for that region, no
		// matter if it contains millions of points or none.
		//
		// The zoom1Pixels points are downsampled a second time, for the actual
		// window width, to reduce the load on the 2D renderer.
		points = s.cachedDecimation
	}

	first := sort.Search(len(points.Timestamps), func(i int) bool {
		return points.Timestamps[i] >= cv.start
	})

	if first == 0 && points.Timestamps[0] >= visibleEndTs {
		// The first point happens later in the trace. There's nothing to do for
		// us yet. In particular, there are no off-screen points to connect,
		// because no points have existed yet.
		return
	}

	var t time.Time
	if debugProfile {
		t = time.Now()
	}
	indices := downsample(points, cv.start, gtx.Constraints.Max.X, cv.nsPerPx, pl.scratch[:0])
	pl.scratch = indices
	if debugProfile {
		fmt.Printf("downsampled %q to %d points in %s\n", s.Name, len(indices), time.Since(t))
	}

	// This step removes insignificant points. For some reason this greatly
	// improves Gio's performance, even when only 1% of points get removed. We
	// believe this isn't due to the overall number of points but some edge
	// condition involving very small areas.
	//
	// We'd like to get rid of this step eventually, as sometimes it can remove
	// very small but visible details from the plot.
	if debugProfile {
		t = time.Now()
	}
	areaFn := func(m ptrace.Metric, a, b, c int) float64 {
		xa := float64(cv.tsToPx(m.Timestamps[a]))
		xb := float64(cv.tsToPx(m.Timestamps[b]))
		xc := float64(cv.tsToPx(m.Timestamps[c]))
		ya := float64(scaleValue(m.Values[a]))
		yb := float64(scaleValue(m.Values[b]))
		yc := float64(scaleValue(m.Values[c]))
		return math.Abs(xa*yb + xb*yc + xc*ya - xa*yc - xb*ya - xc*yb)
	}
	origIndices := indices
	out := indices[:1]
	for i := 1; i < len(indices)-1; i++ {
		area := areaFn(points, out[len(out)-1], indices[i], indices[i+1])
		if area > 0.1 {
			out = append(out, indices[i])
		}
	}
	out = append(out, indices[len(indices)-1])
	indices = out
	if debugProfile {
		fmt.Printf("reduced %q from %d to %d points in %s\n", s.Name, len(origIndices), len(indices), time.Since(t))
	}

	path := func(yield func(el curve.PathElement) bool) {
		var curY float64
		if first > 0 {
			// Start one point to the left of the first point.
			pt := curve.Pt(float64(float64(points.Timestamps[first-1]-cv.start)/cv.nsPerPx), scaleValue(points.Values[first-1]))
			if !yield(curve.MoveTo(pt)) {
				return
			}
			curY = pt.Y
		} else {
			pt := curve.Pt(float64(float64(points.Timestamps[0]-cv.start)/cv.nsPerPx), scaleValue(points.Values[0]))
			if !yield(curve.MoveTo(pt)) {
				return
			}
			curY = pt.Y
			first++
		}

		for _, idx := range indices {
			ts := points.Timestamps[idx]
			v := points.Values[idx]
			cpt := curve.Pt(float64(ts-cv.start)/cv.nsPerPx, scaleValue(v))
			if s.Style&PlotStaircase != 0 {
				if !yield(curve.LineTo(curve.Pt(cpt.X, curY))) {
					return
				}
				if !yield(curve.LineTo(cpt)) {
					return
				}
			} else {
				if !yield(curve.LineTo(cpt)) {
					return
				}
			}
			curY = cpt.Y
		}
	}

	if s.Style&PlotFilled != 0 {
		var giopath clip.Path
		giopath.Begin(gtx.Ops)
		start := curveToGio(path, &giopath)
		// Close the area and fill it
		giopath.LineTo(f32.Pt(giopath.Pos().X, float32(gtx.Constraints.Max.Y)))
		giopath.LineTo(f32.Pt(float32(start.X), float32(gtx.Constraints.Max.Y)))
		giopath.Close()
		theme.FillShape(win, gtx.Ops, s.Color, clip.Outline{Path: giopath.End()}.Op())
	} else {
		// Stroke the path. We use the curve package because its stroking is
		// more robust than Gio's. Specifically, when we zoom into the plot a
		// lot, Gio gets stuck in a loop until it OOMs. We haven't debugged why
		// that is.
		reg := rtrace.StartRegion(context.Background(), "converting path")

		var t time.Time
		if debugProfile {
			t = time.Now()
		}

		stroked := curve.StrokePath(path, curve.Stroke{
			Width:    lineWidth,
			StartCap: curve.ButtCap,
			EndCap:   curve.ButtCap,
			Join:     curve.BevelJoin,
		}, curve.StrokeOpts{
			OptLevel: curve.Subdivide,
		}, 0.1)

		var giopath clip.Path
		giopath.Begin(gtx.Ops)
		curveToGio(stroked, &giopath)
		reg.End()

		if debugProfile {
			fmt.Printf("stroked and converted %q in %s\n", s.Name, time.Since(t))
		}
		theme.FillShape(win, gtx.Ops, s.Color, clip.Outline{Path: giopath.End()}.Op())
	}
}

func curveToGio(p iter.Seq[curve.PathElement], giopath *clip.Path) curve.Point {
	pointToGio := func(pt curve.Point) f32.Point {
		return f32.Pt(float32(pt.X), float32(pt.Y))
	}
	var start curve.Point
	for el := range p {
		switch el.Kind {
		case curve.MoveToKind:
			// This assumes that we'll only see a single MoveTo, which is true
			// for plots.
			start = el.P0
			giopath.MoveTo(pointToGio(el.P0))
		case curve.LineToKind:
			giopath.LineTo(pointToGio(el.P0))
		case curve.QuadToKind:
			giopath.QuadTo(pointToGio(el.P0), pointToGio(el.P1))
		case curve.CubicToKind:
			giopath.CubeTo(pointToGio(el.P0), pointToGio(el.P1), pointToGio(el.P2))
		case curve.ClosePathKind:
			giopath.Close()
		}
	}
	return start
}

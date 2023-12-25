package main

import (
	"context"
	"fmt"
	"image"
	"math"
	"math/bits"
	rtrace "runtime/trace"
	"sort"
	"strings"

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

type PlotStyle uint8

const (
	PlotFilled = 1 << iota
	PlotStaircase
)

type PlotSeries struct {
	Name   string
	Points []ptrace.Point
	Style  PlotStyle
	Color  color.Oklch

	decimated [][]ptrace.Point
	disabled  bool
}

type Plot struct {
	Name   string
	Unit   string
	series []PlotSeries

	min uint64
	max uint64

	click gesture.Click
	hover gesture.Hover

	visScratch     visvalingamScratch
	scratchStrings []string
	hideLegends    bool
	autoScale      bool

	// Used by drawOrthogonalLine to correctly overlap lines at changes in direction
	prevDirection uint8

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

const (
	plotDirectionNone = iota
	plotDirectionHorizontal
	plotDirectionVertical
)

func (pl *Plot) AddSeries(series ...PlotSeries) {
	for i := range series {
		s := &series[i]
		s.decimated = decimate(s.Points)
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

	if cv.unchanged(gtx) &&
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

	return layout.Dimensions{Size: gtx.Constraints.Max}
}

func (pl *Plot) drawPoints(win *theme.Window, gtx layout.Context, cv *Canvas, s PlotSeries) {
	if len(s.Points) < 2 {
		return
	}

	defer rtrace.StartRegion(context.Background(), "draw points").End()
	const lineWidth = 2

	var drawLine func(p *clip.Path, pt f32.Point, width float32)
	if s.Style&PlotFilled != 0 {
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

	// We cannot use cv.End because that respects the scrollbar, which isn't visible for the plot.
	visibleStartTs := max(0, cv.start)
	visibleEndTs := min(cv.trace.End(), cv.pxToTs(float32(gtx.Constraints.Max.X)))
	if visibleStartTs >= visibleEndTs {
		return
	}
	if visibleEndTs < 0 {
		return
	}

	eventsPerNs := (float64(len(s.Points)) / float64(cv.trace.End()-cv.trace.Start()))
	// eventsPerPx holds how many events we would attempt to display per pixel if we didn't decimate the
	// original set of points. If this number is >1, we scale the total number of points by 1 / eventsPerPx
	// and use that as the desired number of decimated points. That way, the visible portion of the plot will
	// end up with 1 point per pixel (assuming even spacing of points.) This means that as we zoom further
	// into the plot, we will use more and more points to maintain an event / pixel density of 1.
	//
	// This assumes that events are evenly distributed, which isn't quite true, but works in practice.
	//
	// If the number is <1 then we effectively do nothing and use all available points, to achieve the closest
	// to 1 point / pixel we can.
	eventsPerPx := float64(cv.End()-cv.start) * eventsPerNs / float64(gtx.Constraints.Max.X)
	wanted := uint64(len(s.Points))
	if eventsPerPx > 1 {
		wanted = max(2, uint64(math.Ceil(float64(wanted)/eventsPerPx)))
	}

	var points []ptrace.Point
	// Every level of decimation contains 2**(i+1) points. Choose the lowest bucket that contains at least as many
	// points as we want.
	level := uint64Log2RoundUp(wanted)
	points = s.decimated[level-1]

	first := sort.Search(len(points), func(i int) bool {
		return points[i].When >= cv.start
	})

	if first == 0 && points[0].When >= visibleEndTs {
		// The first point happens later in the trace. There's nothing to do for us yet. In particular, there are no
		// off-screen points to connect, because no points have existed yet.
		return
	}

	var path clip.Path
	path.Begin(gtx.Ops)
	var start f32.Point
	if first > 0 {
		pt := f32.Pt(cv.tsToPx(points[first-1].When), scaleValue(points[first-1].Value))
		start = pt
		path.MoveTo(pt)
	} else {
		pt := f32.Pt(cv.tsToPx(points[0].When), scaleValue(points[0].Value))
		start = pt
		path.MoveTo(pt)
		first++
	}

	var p ptrace.Point
	var cnt int
	for cnt, p = range points[first:] {
		if p.When >= visibleEndTs {
			break
		}
	}
	if first+cnt < len(points) {
		// Include the next point so that we continue to it from the last visible point.
		cnt++
	}

	points = points[first : first+cnt]

	// Our events per ns is an average over the whole trace and assumes a uniform distribution of events. This
	// can lead to us having more points than we can draw for the visible portion of the plot. When that
	// happens we apply another decimation. This isn't strictly necessary, but reduces the amount of points
	// Gio has to deal with, and can result in cleaner output if we have many more points than pixels.
	//
	// Instead of only decimating to the target number, we also discard all points with an insignificant area.
	// This will often bring us below the target number, resulting in further simplification of the path, at
	// no visual cost.
	//
	// This second decimation differs from the initial decimation in that it is sensitive to the scale of the
	// plot and can compute the actual pixel area of triangles, instead of the abstract area that the first
	// decimation has to use. This is why discarding insignificant points is only possible in this step.
	//
	// This decimation can still be quite expensive, costing around 1ms, so only do it if we have a significant excess
	// of points.
	if cnt > gtx.Constraints.Max.X*4 {
		areaFn := func(a, b, c ptrace.Point) float64 {
			xa := float64(cv.tsToPx(a.When))
			xb := float64(cv.tsToPx(b.When))
			xc := float64(cv.tsToPx(c.When))
			ya := float64(scaleValue(a.Value))
			yb := float64(scaleValue(b.Value))
			yc := float64(scaleValue(c.Value))
			return math.Abs(xa*yb + xb*yc + xc*ya - xa*yc - xb*ya - xc*yb)
		}
		vis, ok := newVisvalingam(points, areaFn, &pl.visScratch, gtx.Constraints.Max.X)
		if ok {
			vis.compress()
			points = vis.decimate(gtx.Constraints.Max.X)
		}
	}

	for _, p := range points {
		pt := f32.Pt(cv.tsToPx(p.When), scaleValue(p.Value))
		if s.Style&PlotStaircase != 0 {
			drawLine(&path, f32.Pt(pt.X, path.Pos().Y), lineWidth)
			drawLine(&path, pt, lineWidth)
		} else {
			drawLine(&path, pt, lineWidth)
		}
	}

	if len(points) == 0 || points[len(points)-1].When < visibleEndTs {
		// The last point isn't off-screen, which means it is the last point in the trace, and we should continue that
		// point until the end of the trace.
		end := min(cv.tsToPx(cv.trace.End()), float32(gtx.Constraints.Max.X))
		drawLine(&path, f32.Pt(end, path.Pos().Y), lineWidth)
	}

	if s.Style&PlotFilled != 0 {
		// Close the area and fill it
		drawLine(&path, f32.Pt(path.Pos().X, float32(gtx.Constraints.Max.Y)), lineWidth)
		drawLine(&path, f32.Pt(start.X, float32(gtx.Constraints.Max.Y)), lineWidth)
		path.Close()
		theme.FillShape(win, gtx.Ops, s.Color, clip.Outline{Path: path.End()}.Op())
	} else {
		// Stroke the path
		theme.FillShape(win, gtx.Ops, s.Color, clip.Outline{Path: path.End()}.Op())
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

type (
	pointIndex uint32
	itemIndex  uint32
	heapIndex  uint32
	pointItem  struct {
		point pointIndex
		area  float64
		prev  itemIndex
		next  itemIndex

		index heapIndex
	}
)

type visvalingam struct {
	heap   minHeap
	items  []pointItem
	points []ptrace.Point
	area   func(a, b, c ptrace.Point) float64
}

type visvalingamScratch struct {
	items []pointItem
	heap  minHeap
}

// newVisvalingam prepares decimation using the Visvalingam algorithm. The max parameter indicates the largest
// number of points we'll try to decimate down to.
func newVisvalingam(
	points []ptrace.Point,
	area func(a, b, c ptrace.Point) float64,
	scratch *visvalingamScratch,
	max int,
) (*visvalingam, bool) {
	if len(points) < 3 {
		return nil, false
	}

	if len(points) > math.MaxUint32 {
		return nil, false
	}

	var items []pointItem
	if scratch != nil && cap(scratch.items) >= len(points) {
		items = scratch.items[:len(points)]
	} else {
		items = make([]pointItem, len(points))
		if scratch != nil {
			scratch.items = items
		}
	}

	items[0] = pointItem{
		point: 0,
		area:  math.MaxFloat64,
		prev:  math.MaxUint32,
		next:  1,
	}

	for i := 1; i < len(points)-1; i++ {
		a := points[i-1]
		b := points[i]
		c := points[i+1]

		items[i] = pointItem{
			point: pointIndex(i),
			area:  area(a, b, c),
			prev:  itemIndex(i - 1),
			next:  itemIndex(i + 1),
		}
	}

	items[len(items)-1] = pointItem{
		point: pointIndex(len(points) - 1),
		area:  math.MaxFloat64,
		prev:  itemIndex(len(items) - 2),
		next:  math.MaxUint32,
	}

	var heap minHeap
	if scratch != nil && cap(scratch.heap) >= len(items) {
		heap = scratch.heap[:len(items)]
	} else {
		heap = make(minHeap, len(items))
		if scratch != nil {
			scratch.heap = heap
		}
	}
	for i := range items {
		heap[i] = &items[i]
		heap[i].index = heapIndex(i)
	}
	heap.Init()

	return &visvalingam{
		heap:   heap,
		items:  items,
		points: points,
		area:   area,
	}, true
}

// compress decimates all points that don't contribute to the shape of the plot, i.e. those that have a zero
// visible area.
func (vis *visvalingam) compress() {
	for len(vis.heap) > 0 && vis.heap[0].area <= 0.1 {
		vis.pop()
	}
}

func (vis *visvalingam) pop() {
	heap := &vis.heap
	items := vis.items
	points := vis.points

	cur := heap.Pop()
	next := &items[cur.next]
	prev := &items[cur.prev]

	prev.next = cur.next
	next.prev = cur.prev

	if prev.prev != math.MaxUint32 {
		area := vis.area(
			points[items[prev.prev].point],
			points[prev.point],
			points[next.point],
		)

		area = math.Max(area, cur.area)
		heap.Update(prev, area)
	}

	if next.next != math.MaxUint32 {
		area := vis.area(
			points[prev.point],
			points[next.point],
			points[items[next.next].point],
		)

		area = math.Max(area, cur.area)
		heap.Update(next, area)
	}
}

// decimate decimates points until only limit points are left. It is valid and efficient to call decimate
// multiple times with decreasing values of limit.
func (vis *visvalingam) decimate(limit int) []ptrace.Point {
	heap := &vis.heap
	points := vis.points

	for len(*heap) > limit {
		vis.pop()
	}

	out := make([]ptrace.Point, 0, min(len(*heap), limit))
	p := vis.items[0]
	for {
		out = append(out, points[p.point])
		if p.next == math.MaxUint32 {
			break
		}
		p = vis.items[p.next]
	}

	return out
}

func visArea(a, b, c ptrace.Point) float64 {
	xa := float64(a.When)
	xb := float64(b.When)
	xc := float64(c.When)
	ya := float64(a.Value)
	yb := float64(b.Value)
	yc := float64(c.Value)
	return math.Abs(xa*yb + xb*yc + xc*ya - xa*yc - xb*ya - xc*yb)
}

func uint64Log2RoundUp(x uint64) uint {
	log2 := uint(bits.Len64(x) - 1)
	if x&(x-1) != 0 {
		log2++
	}
	return log2
}

// decimate repeatedly decimates points to produce plotLevels levels of detail.
func decimate(points []ptrace.Point) [][]ptrace.Point {
	if len(points) <= 2 {
		return [][]ptrace.Point{points}
	}

	level := uint64Log2RoundUp(uint64(len(points)))
	out := make([][]ptrace.Point, level)
	vis, ok := newVisvalingam(points, visArea, nil, 1<<level)
	if !ok {
		for i := range out {
			out[i] = points
		}
		return out
	}

	for len(vis.heap) >= 2 && level >= 1 {
		out[level-1] = vis.decimate(1 << level)
		level--
	}

	return out
}

type minHeap []*pointItem

func (h *minHeap) Init() {
	n := len(*h)
	for i := n/2 - 1; i >= 0; i-- {
		h.down(heapIndex(i))
	}
}

func (h *minHeap) Push(item *pointItem) {
	item.index = heapIndex(len(*h))
	*h = append(*h, item)
	h.up(item.index)
}

func (h *minHeap) Pop() *pointItem {
	removed := (*h)[0]
	lastItem := (*h)[len(*h)-1]
	*h = (*h)[:len(*h)-1]

	if len(*h) > 0 {
		lastItem.index = 0
		(*h)[0] = lastItem
		h.down(0)
	}

	return removed
}

func (h minHeap) Update(item *pointItem, area float64) {
	if item.area > area {
		// area got smaller
		item.area = area
		h.up(item.index)
	} else {
		// area got larger
		item.area = area
		h.down(item.index)
	}
}

func (h *minHeap) Remove(item *pointItem) {
	i := item.index

	lastItem := (*h)[len(*h)-1]
	*h = (*h)[:len(*h)-1]

	if i != heapIndex(len(*h)) {
		lastItem.index = i
		(*h)[i] = lastItem

		if lastItem.area < item.area {
			h.up(i)
		} else {
			h.down(i)
		}
	}
}

func (h minHeap) up(i heapIndex) {
	item := h[i]
	for i > 0 {
		up := ((i + 1) >> 1) - 1
		parent := h[up]

		if parent.area <= item.area {
			// parent is smaller so we're done fixing up the heap.
			break
		}

		// swap nodes
		parent.index = i
		h[i] = parent

		item.index = up
		h[up] = item

		i = up
	}
}

func (h minHeap) down(i heapIndex) {
	item := h[i]
	for {
		right := (i + 1) << 1
		left := right - 1

		down := i
		child := h[down]

		// swap with smallest child
		if left < heapIndex(len(h)) && h[left].area < child.area {
			down = left
			child = h[down]
		}

		if right < heapIndex(len(h)) && h[right].area < child.area {
			down = right
			child = h[down]
		}

		// non smaller, so quit
		if down == i {
			break
		}

		// swap the nodes
		child.index = i
		h[child.index] = child

		item.index = down
		h[down] = item

		i = down
	}
}

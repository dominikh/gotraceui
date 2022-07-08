package main

import (
	"bufio"
	"fmt"
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"runtime/pprof"
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

/*
   GC notes:
   - The only use of p=1000004 is for GCStart
   - GCDone happens on other procs, probably whichever proc was running the background worker that determined that we're done
   - A similar thing applies to GCSTWStart and GCSTWDone
   - The second GCSTWDone can happen after GCDone
*/

// TODO(dh): provide different sortings for goroutines. One user requested sorting by "amount of activity" but couldn't
// define that. Maybe time spent scheduled? Another sorting would be by earliest timestamp, which would be almost like
// sorted by gid, but would work around gids being allocated to Ps in groups of 16. also interesting might be sorted by
// "relatedness", keeping goroutines that interact a lot close together. But I reckon that'll require a lot of tuning
// and experimentation to get right, especially once more than 2 goroutines interact.

// TODO(dh): implement popup windows that can be used to customize UI settings. e.g. instead of needing different
// shortcuts for toggling labels, compact mode, tooltips etc, have one shortcut that opens a menu that allows toggling
// these features. maybe even use a radial menu? (probably not.)

// FIXME(dh): sync.Once state has no assigned color

// TODO(dh): allow computing statistics for a selectable region of time

// TODO(dh): hovering over spans in the goroutine timelines highlights goroutines in the processor timelines. that's a
// happy accident. however, it doesn't work reliably, because we just look at trace.Event.G for the matching, and for
// some events, like unblocking, that's the wrong G.

// TODO(dh): use the GC-purple color in the GC and STW timelines, as well as for the GC goroutines in the per-P
// timelines.

// TODO(dh): toggleable behavior for hovering spans in goroutine timelines. For example, hovering a blocked span could
// highlight the span that unblocks it (or maybe when hovering the "runnable" span, but same idea). Hovering a running
// span could highlight all the spans it unblocks.

// TODO(dh): toggleable overlay that shows STW and GC phases

// TODO(dh): support pinning activity widgets at the top. for example it might be useful to see the GC and STW while
// looking at an arbitrary goroutine.

// TODO(dh): the Event.Stk is meaningless for goroutines that already existed when tracing started, i.e. ones that get a
// GoWaiting event. The GoCreate event will be caused by starting the trace, and the stack of the event will be that
// leading up to starting the trace. It will in no way reflect the code that actually, historically, started the
// goroutine. To avoid confusion, we should remove those stacks altogether.

// TODO(dh): Go 1.19 adds CPU samples to the execution trace (if profiling is enabled). This adds the new event
// EvCPUSample, and updates the trace's version to Go 1.19.

const debug = true
const cpuprofiling = true
const memprofiling = true
const profiling = cpuprofiling || memprofiling

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistanceDp      unit.Dp = 20
	tickHeightDp           unit.Dp = 12
	tickWidthDp            unit.Dp = 1
	minTickLabelDistanceDp unit.Dp = 8
	tickLabelFontSizeSp    unit.Sp = 14

	// XXX the label height depends on the font used
	activityLabelHeightDp   unit.Dp = 20
	activityStateHeightDp   unit.Dp = 12
	activityGapDp           unit.Dp = 5
	activityHeightDp        unit.Dp = activityStateHeightDp + activityLabelHeightDp
	activityLabelFontSizeSp unit.Sp = 14

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 4

	spanBorderWidthDp unit.Dp = 1

	tooltipFontSizeSp    unit.Sp = 14
	tooltipPaddingDp     unit.Dp = 2
	tooltipBorderWidthDp unit.Dp = 2
)

type reusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *reusableOps) get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
}

type Axis struct {
	tl *Timeline

	ticksOps reusableOps

	prevFrame struct {
		ops    reusableOps
		call   op.CallOp
		labels []string
		dims   layout.Dimensions
	}
}

type showTooltips uint8

const (
	showTooltipsBoth = iota
	showTooltipsSpans
	showTooltipsNone
)

type Timeline struct {
	// The region of the timeline that we're displaying, measured in nanoseconds
	Start time.Duration
	End   time.Duration
	// Imagine we're drawing all activities onto an infinitely long canvas. Timeline.Y specifies the Y of that infinite
	// canvas that the activity section's Y == 0 is displaying.
	Y int
	// All activities. Index 0 and 1 are the GC and STW timelines, followed by processors and goroutines.
	Activities []*ActivityWidget
	Theme      *material.Theme
	Scrollbar  widget.Scrollbar
	Axis       Axis

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
	Activity struct {
		DisplayAllLabels         bool
		Compact                  bool
		HighlightSpansWithEvents bool
		// Should tooltips be shown?
		ShowTooltips showTooltips

		HoveredSpans []Span
	}

	// prevFrame records the timeline's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the timeline hasn't changed.
	prevFrame struct {
		Start                    time.Duration
		End                      time.Duration
		Y                        int
		nsPerPx                  float32
		compact                  bool
		displayedAws             []*ActivityWidget
		highlightSpansWithEvents bool
		dspSpans                 map[any][]struct {
			dspSpans       []Span
			startPx, endPx float32
		}
		hoveredSpans []Span
	}
}

func (tl *Timeline) unchanged() bool {
	if profiling {
		return false
	}

	return tl.prevFrame.Start == tl.Start &&
		tl.prevFrame.End == tl.End &&
		tl.prevFrame.nsPerPx == tl.nsPerPx &&
		tl.prevFrame.Y == tl.Y &&
		tl.prevFrame.compact == tl.Activity.Compact &&
		tl.prevFrame.highlightSpansWithEvents == tl.Activity.HighlightSpansWithEvents
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

func (tl *Timeline) activityHeight(gtx layout.Context) int {
	if tl.Activity.Compact {
		return gtx.Dp(activityHeightDp) - gtx.Dp(activityLabelHeightDp)
	} else {
		return gtx.Dp(activityHeightDp)
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
	offset  int
	tl      *Timeline
	spans   []Span
	prevEnd time.Duration
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut []Span, startPx, endPx float32, ok bool) {
	offset := it.offset
	spans := it.spans

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	nsPerPx := float32(it.tl.nsPerPx)
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * float64(nsPerPx)))
	startOffset := offset
	tlStart := it.tl.Start

	s := &spans[offset]
	offset++

	start := s.Start
	end := s.End
	if it.prevEnd > start {
		// The previous span was extended and grew into this span. This shifts our start position to the right.
		start = it.prevEnd
	}

	if end-start < minSpanWidthD {
		// Merge all tiny spans until we find a span or gap that's big enough to stand on its own. We do not stop
		// merging after we've reached the minimum size because that can lead to multiple merges being next to each
		// other. Not only does this look bad, it is also prone to tiny spans toggling between two merged spans, and
		// previously merged spans becoming visible again when zooming out.
		for ; offset < len(it.spans); offset++ {
			adjustedEnd := end
			if end-start < minSpanWidthD {
				adjustedEnd = start + minSpanWidthD
			} else {
				// Our merged span is long enough now and won't need to be extended anymore. Break out of this loop and
				// go into a smaller loop that specializes on just collecting tiny spans, avoiding the comparisons
				// needed for extending.
				offset--
				break
			}

			nextSpan := &spans[offset]
			// Assume that we stop at this span. Compute the final size and extension. Use that to see
			// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
			nextStart := nextSpan.Start
			nextEnd := nextSpan.End
			if adjustedEnd > nextStart {
				// The current span would have to grow into the next span, making it smaller
				nextStart = adjustedEnd
			}
			if nextEnd-nextStart >= minSpanWidthD || nextStart-end >= minSpanWidthD {
				// Don't merge spans or gaps that can stand on their own
				break
			}

			end = nextSpan.End
		}

		for ; offset < len(it.spans); offset++ {
			nextSpan := &spans[offset]
			// Assume that we stop at this span. Compute the final size and extension. Use that to see
			// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
			nextStart := nextSpan.Start
			nextEnd := nextSpan.End
			if nextEnd-nextStart >= minSpanWidthD || nextStart-end >= minSpanWidthD {
				// Don't merge spans or gaps that can stand on their own
				break
			}

			end = nextSpan.End
		}
	}

	if end-start < minSpanWidthD {
		// We're still too small, so extend the span to its minimum size.
		end = start + minSpanWidthD
	}

	it.offset = offset
	it.prevEnd = end
	startPx = float32(start-tlStart) / nsPerPx
	endPx = float32(end-tlStart) / nsPerPx
	return spans[startOffset:it.offset], startPx, endPx, true
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
	for _, gw := range tl.visibleActivities(gtx) {
		if len(gw.AllSpans) == 0 {
			continue
		}
		if t := gw.AllSpans[0].Start; t < first || first == -1 {
			first = t
		}
		if t := gw.AllSpans[len(gw.AllSpans)-1].End; t > last {
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
	for _, og := range tl.Activities {
		if g == og.item {
			// TODO(dh): show goroutine at center of window, not the top
			tl.Y = off
			return
		}
		off += tl.activityHeight(gtx) + gtx.Dp(activityGapDp)
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
					tl.Activity.DisplayAllLabels = !tl.Activity.DisplayAllLabels

				case "C":
					// FIXME(dh): adjust tl.Y so that the top visible goroutine stays the same
					tl.Activity.Compact = !tl.Activity.Compact

				case "T":
					// TODO(dh): show an onscreen hint what setting we changed to
					tl.Activity.ShowTooltips = (tl.Activity.ShowTooltips + 1) % (showTooltipsNone + 1)

				case "E":
					tl.Activity.HighlightSpansWithEvents = !tl.Activity.HighlightSpansWithEvents
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
		activityHeight := tl.activityHeight(gtx)
		activityGap := gtx.Dp(activityGapDp)
		// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
		d := tl.Scrollbar.ScrollDistance()
		totalHeight := float32(len(tl.Activities) * (activityHeight + activityGap))
		tl.Y += int(round32(d * totalHeight))
		if tl.Y < 0 {
			tl.Y = 0
		}
	}

	tl.Activity.HoveredSpans = nil
	for _, gw := range tl.prevFrame.displayedAws {
		if spans := gw.ClickedSpans; len(spans) > 0 {
			start := spans[0].Start
			end := spans[len(spans)-1].End
			tl.Start = start
			tl.End = end
			break
		}
	}
	for _, gw := range tl.prevFrame.displayedAws {
		if spans := gw.HoveredSpans; len(spans) > 0 {
			tl.Activity.HoveredSpans = spans
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
	}

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
	key.InputOp{Tag: tl, Keys: "C|E|T|X|(Shift)-(Ctrl)-" + key.NameHome}.Add(gtx.Ops)
	key.FocusOp{Tag: tl}.Add(gtx.Ops)

	// Draw axis and goroutines
	Stack(gtx, tl.Axis.Layout, func(gtx layout.Context) layout.Dimensions {
		dims, gws := tl.layoutActivities(gtx)
		tl.prevFrame.displayedAws = gws
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

func (axis *Axis) tickInterval(gtx layout.Context) (time.Duration, bool) {
	if axis.tl.nsPerPx == 0 {
		return 0, false
	}
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	minTickDistance := gtx.Dp(minTickDistanceDp)
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(round32(float32(t) / axis.tl.nsPerPx))
		if tickDistance >= minTickDistance {
			return t, true
		}
	}
	panic("unreachable")
}

func (axis *Axis) Layout(gtx layout.Context) (dims layout.Dimensions) {
	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	prevLabelEnd := float32(-1)
	// TODO(dh): calculating the label height on each frame risks that it changes between frames, which will cause the
	// goroutines to shift around as the axis section grows and shrinks.
	labelHeight := 0
	tickWidth := float32(gtx.Dp(tickWidthDp))
	tickHeight := float32(gtx.Dp(tickHeightDp))
	minTickLabelDistance := float32(gtx.Dp(minTickLabelDistanceDp))

	tickInterval, ok := axis.tickInterval(gtx)
	if !ok {
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight))}
	}

	var labels []string
	if axis.tl.unchanged() {
		axis.prevFrame.call.Add(gtx.Ops)
		return axis.prevFrame.dims
	} else if axis.tl.prevFrame.nsPerPx == axis.tl.nsPerPx {
		// Panning only changes the first label
		labels = axis.prevFrame.labels
		// TODO print thousands separator
		labels[0] = fmt.Sprintf("%d ns", axis.tl.Start)
	} else {
		for t := axis.tl.Start; t < axis.tl.End; t += tickInterval {
			if t == axis.tl.Start {
				// TODO print thousands separator
				labels = append(labels, fmt.Sprintf("%d ns", t))
			} else {
				// TODO separate value and unit symbol with a space
				labels = append(labels, fmt.Sprintf("+%s", t-axis.tl.Start))
			}
		}
		axis.prevFrame.labels = labels
	}

	origOps := gtx.Ops
	gtx.Ops = axis.prevFrame.ops.get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		axis.prevFrame.call = call
		axis.prevFrame.dims = dims
	}()

	var ticksPath clip.Path
	ticksPath.Begin(axis.ticksOps.get())
	i := 0
	for t := axis.tl.Start; t < axis.tl.End; t += tickInterval {
		start := axis.tl.tsToPx(t) - tickWidth/2
		end := axis.tl.tsToPx(t) + tickWidth/2
		rect := FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := axis.tl.tsToPx(t+(tickInterval/10)*time.Duration(j)) - tickWidth/2
			smallEnd := axis.tl.tsToPx(t+(tickInterval/10)*time.Duration(j)) + tickWidth/2
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

		if t == axis.tl.Start {
			label := labels[i]
			stack := op.Offset(image.Pt(0, int(tickHeight))).Push(gtx.Ops)
			paint.ColorOp{Color: colors[colorTickLabel]}.Add(gtx.Ops)
			dims := widget.Label{MaxLines: 1}.Layout(gtx, axis.tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
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
			dims := widget.Label{MaxLines: 1}.Layout(gtx, axis.tl.Theme.Shaper, text.Font{}, tickLabelFontSizeSp, label)
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

type ActivityWidget struct {
	// Inputs
	AllSpans        []Span
	WidgetTooltip   func(gtx layout.Context, aw *ActivityWidget)
	MarkSpan        func(aw *ActivityWidget, spans []Span) bool
	HighlightSpan   func(aw *ActivityWidget, spans []Span) bool
	InvalidateCache func(aw *ActivityWidget) bool

	tl    *Timeline
	item  any
	label string

	pointerAt       f32.Point
	hovered         bool
	hoveredActivity bool
	hoveredLabel    bool

	ClickedSpans []Span
	HoveredSpans []Span

	// op lists get reused between frames to avoid generating garbage
	ops         [colorStateLast]op.Ops
	outlinesOps reusableOps
	eventsOps   reusableOps

	prevFrame struct {
		// State for reusing the previous frame's ops, to avoid redrawing from scratch if no relevant state has changed.
		hovered         bool
		hoveredActivity bool
		hoveredLabel    bool
		forceLabel      bool
		compact         bool
		topBorder       bool
		ops             reusableOps
		call            op.CallOp
	}
}

func NewGCWidget(tl *Timeline, spans []Span) *ActivityWidget {
	return &ActivityWidget{
		AllSpans: spans,
		tl:       tl,
		item:     spans,
		label:    "GC",
	}
}

func NewSTWWidget(tl *Timeline, spans []Span) *ActivityWidget {
	return &ActivityWidget{
		AllSpans: spans,
		tl:       tl,
		item:     spans,
		label:    "STW",
	}
}

func NewGoroutineWidget(tl *Timeline, g *Goroutine) *ActivityWidget {
	var l string
	if g.Function != nil {
		l = fmt.Sprintf("goroutine %d: %s", g.ID, g.Function.Fn)
	} else {
		l = fmt.Sprintf("goroutine %d", g.ID)
	}
	return &ActivityWidget{
		AllSpans: g.Spans,
		WidgetTooltip: func(gtx layout.Context, aw *ActivityWidget) {
			GoroutineTooltip{g, aw.tl.Theme.Shaper}.Layout(gtx)
		},
		MarkSpan: func(aw *ActivityWidget, dspSpans []Span) bool {
			return spanHasEvents(g.Events, dspSpans[0].Start, dspSpans[len(dspSpans)-1].End)
		},
		tl:    tl,
		item:  g,
		label: l,
	}
}

func NewProcessorWidget(tl *Timeline, p *Processor) *ActivityWidget {
	return &ActivityWidget{
		AllSpans:      p.Spans,
		WidgetTooltip: func(gtx layout.Context, aw *ActivityWidget) {},
		MarkSpan:      func(aw *ActivityWidget, spans []Span) bool { return false },
		HighlightSpan: func(aw *ActivityWidget, spans []Span) bool {
			if len(tl.Activity.HoveredSpans) != 1 {
				return false
			}
			// OPT(dh): don't be O(n)
			o := tl.Activity.HoveredSpans[0]
			for _, s := range spans {
				if s.Event.G == o.Event.G {
					return true
				}
			}
			return false
		},
		InvalidateCache: func(aw *ActivityWidget) bool {
			if len(tl.prevFrame.hoveredSpans) == 0 && len(tl.Activity.HoveredSpans) == 0 {
				// Nothing hovered in either frame.
				return false
			}

			if len(tl.prevFrame.hoveredSpans) > 1 && len(tl.Activity.HoveredSpans) > 1 {
				// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
				// frames, then nothing changes for rendering.
				return false
			}

			if len(tl.prevFrame.hoveredSpans) != len(tl.Activity.HoveredSpans) {
				// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
				// previously highlighted.
				//
				// The number of hovered spans changed, and at least in one frame the number was 1.
				return true
			}

			// If we got to this point, then both slices have exactly one element.
			if tl.prevFrame.hoveredSpans[0].Event.G != tl.Activity.HoveredSpans[0].Event.G {
				return true
			}

			return false
		},
		tl:    tl,
		item:  p,
		label: fmt.Sprintf("Processor %d", p.ID),
	}
}

func (aw *ActivityWidget) Layout(gtx layout.Context, forceLabel bool, compact bool, topBorder bool) layout.Dimensions {
	activityHeight := aw.tl.activityHeight(gtx)
	activityStateHeight := gtx.Dp(activityStateHeightDp)
	activityLabelHeight := gtx.Dp(activityLabelHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)

	aw.ClickedSpans = nil
	aw.HoveredSpans = nil

	var trackClick bool

	// FIXME(dh): update tooltip position when dragging
	for _, e := range gtx.Events(&aw.hoveredActivity) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			aw.hoveredActivity = true
			aw.pointerAt = ev.Position
		case pointer.Leave, pointer.Cancel:
			aw.hoveredActivity = false
		case pointer.Press:
			if ev.Buttons&pointer.ButtonTertiary != 0 && ev.Modifiers&key.ModCtrl != 0 {
				trackClick = true
			}
		}
	}
	for _, ev := range gtx.Events(&aw.hovered) {
		switch ev.(pointer.Event).Type {
		case pointer.Enter, pointer.Move:
			aw.hovered = true
		case pointer.Leave, pointer.Cancel:
			aw.hovered = false
		}
	}
	for _, ev := range gtx.Events(&aw.hoveredLabel) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Type {
			case pointer.Enter, pointer.Move:
				aw.hoveredLabel = true
				aw.pointerAt = ev.Position
			case pointer.Leave, pointer.Cancel:
				aw.hoveredLabel = false
			case pointer.Press:
				if ev.Buttons&pointer.ButtonTertiary != 0 && ev.Modifiers&key.ModCtrl != 0 {
					aw.ClickedSpans = aw.AllSpans
				}
			}
		}
	}

	if !trackClick &&
		aw.tl.unchanged() &&
		!aw.hoveredActivity &&
		!aw.prevFrame.hoveredActivity &&
		!aw.hoveredLabel &&
		!aw.prevFrame.hoveredLabel &&
		!aw.hovered &&
		!aw.prevFrame.hovered &&
		forceLabel == aw.prevFrame.forceLabel &&
		compact == aw.prevFrame.compact &&
		(aw.InvalidateCache == nil || !aw.InvalidateCache(aw)) &&
		topBorder == aw.prevFrame.topBorder {

		// OPT(dh): instead of avoiding cached ops completely when the activity is hovered, draw the tooltip
		// separately.
		aw.prevFrame.call.Add(gtx.Ops)
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
	}

	aw.prevFrame.hovered = aw.hovered
	aw.prevFrame.hoveredActivity = aw.hoveredActivity
	aw.prevFrame.hoveredLabel = aw.hoveredLabel
	aw.prevFrame.forceLabel = forceLabel
	aw.prevFrame.compact = compact
	aw.prevFrame.topBorder = topBorder

	origOps := gtx.Ops
	gtx.Ops = aw.prevFrame.ops.get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		aw.prevFrame.call = call
	}()

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, activityHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: &aw.hovered, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel}.Add(gtx.Ops)

	if !compact {
		if aw.hovered || forceLabel || topBorder {
			// Draw border at top of the activity
			paint.FillShape(gtx.Ops, colors[colorActivityBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if aw.hovered || forceLabel {
			paint.ColorOp{Color: colors[colorActivityLabel]}.Add(gtx.Ops)
			labelDims := widget.Label{}.Layout(gtx, aw.tl.Theme.Shaper, text.Font{}, activityLabelFontSizeSp, aw.label)

			stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
			pointer.InputOp{Tag: &aw.hoveredLabel, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Cancel | pointer.Move}.Add(gtx.Ops)
			stack.Pop()
		}

		if aw.WidgetTooltip != nil && aw.tl.Activity.ShowTooltips == showTooltipsBoth && aw.hoveredLabel {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(aw.pointerAt.Round()).Push(gtx.Ops)
			aw.WidgetTooltip(gtx, aw)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
		}

		defer op.Offset(image.Pt(0, activityLabelHeight)).Push(gtx.Ops).Pop()
	}

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, activityStateHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: &aw.hoveredActivity, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel}.Add(gtx.Ops)

	// Draw activity lifetimes
	//
	// We batch draw operations by color to avoid making thousands of draw calls. See
	// https://lists.sr.ht/~eliasnaur/gio/%3C871qvbdx5r.fsf%40honnef.co%3E#%3C87v8smctsd.fsf@honnef.co%3E
	//
	for i := range aw.ops {
		aw.ops[i].Reset()
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

	var outlinesPath clip.Path
	var eventsPath clip.Path
	outlinesPath.Begin(aw.outlinesOps.get())
	eventsPath.Begin(aw.eventsOps.get())

	for i := range paths {
		paths[i].Begin(&aw.ops[i])
	}

	first := true

	var prevEndPx float32
	doSpans := func(dspSpans []Span, startPx, endPx float32) {
		if aw.hoveredActivity && aw.pointerAt.X >= startPx && aw.pointerAt.X < endPx {
			if trackClick {
				aw.ClickedSpans = dspSpans
				trackClick = false
			}
			aw.HoveredSpans = dspSpans
		}

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
		maxP = f32.Pt((min(endPx, float32(gtx.Constraints.Max.X))), float32(activityStateHeight))

		// Draw outline as a rectangle, the span will draw on top of it so that only the outline remains.
		//
		// OPT(dh): for activities that have no gaps between any of the spans this can be drawn as a single rectangle
		// covering all spans.
		outlinesPath.MoveTo(minP)
		outlinesPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
		outlinesPath.LineTo(maxP)
		outlinesPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
		outlinesPath.Close()

		if first && startPx < 0 {
			// Never draw a left border for spans truncated spans
		} else if !first && startPx == prevEndPx {
			// Don't draw left border if it'd touch a right border
		} else {
			minP.X += float32(spanBorderWidth)
		}
		prevEndPx = endPx

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

		// TODO(dh): use different looks for marked spans and highlighted spans
		if (aw.tl.Activity.HighlightSpansWithEvents && aw.MarkSpan(aw, dspSpans)) || (aw.HighlightSpan != nil && aw.HighlightSpan(aw, dspSpans)) {
			minP := minP
			maxP := maxP
			minP.Y += float32((activityStateHeight - spanBorderWidth*2) / 2)

			eventsPath.MoveTo(minP)
			eventsPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			eventsPath.LineTo(maxP)
			eventsPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			eventsPath.Close()
		}

		if aw.tl.Activity.ShowTooltips < showTooltipsNone && aw.hoveredActivity && aw.pointerAt.X >= startPx && aw.pointerAt.X < endPx {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(aw.pointerAt.Round()).Push(gtx.Ops)
			SpanTooltip{dspSpans, aw.tl.Theme.Shaper}.Layout(gtx)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
		}

		first = false
	}

	if aw.tl.unchanged() {
		for _, prevSpans := range aw.tl.prevFrame.dspSpans[aw] {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := aw.tl.prevFrame.dspSpans[aw][:0]
		it := renderedSpansIterator{
			tl:    aw.tl,
			spans: aw.tl.visibleSpans(aw.AllSpans),
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
		aw.tl.prevFrame.dspSpans[aw] = allDspSpans
	}

	// Outlines are not grouped with other spans of the same color because they have to be drawn before spans.
	paint.FillShape(gtx.Ops, colors[colorSpanOutline], clip.Outline{Path: outlinesPath.End()}.Op())

	for cIdx := range paths {
		p := &paths[cIdx]
		paint.FillShape(gtx.Ops, colors[cIdx], clip.Outline{Path: p.End()}.Op())
	}
	paint.FillShape(gtx.Ops, colors[colorSpanWithEvents], clip.Outline{Path: eventsPath.End()}.Op())

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
}

func (tl *Timeline) visibleActivities(gtx layout.Context) []*ActivityWidget {
	activityHeight := tl.activityHeight(gtx)
	activityGap := gtx.Dp(activityGapDp)

	start := -1
	end := -1
	// OPT(dh): at least use binary search to find the range of activities we need to draw
	// OPT(dh): we can probably compute the indices directly
	for i := range tl.Activities {
		y := (activityHeight+activityGap)*int(i) - tl.Y
		// Don't draw activities that would be fully hidden, but do draw partially hidden ones
		if y < -activityHeight {
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
		// No visible activities
		return nil
	}

	if end == -1 {
		end = len(tl.Activities)
	}

	return tl.Activities[start:end]
}

func (tl *Timeline) layoutActivities(gtx layout.Context) (layout.Dimensions, []*ActivityWidget) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	activityHeight := tl.activityHeight(gtx)
	activityGap := gtx.Dp(activityGapDp)

	// Draw a scrollbar, then clip to smaller area. We've already computed nsPerPx, so clipping the activity area will
	// not bring us out of alignment with the axis.
	{
		// TODO(dh): add another screen worth of activities so the user can scroll a bit further
		totalHeight := float32((len(tl.Activities) + 1) * (activityHeight + activityGap))
		fraction := float32(gtx.Constraints.Max.Y) / totalHeight
		offset := float32(tl.Y) / totalHeight
		sb := material.Scrollbar(tl.Theme, &tl.Scrollbar)
		stack := op.Offset(image.Pt(gtx.Constraints.Max.X-gtx.Dp(sb.Width()), 0)).Push(gtx.Ops)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
		stack.Pop()

		gtx.Constraints.Max.X -= gtx.Dp(sb.Width())
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	}

	// OPT(dh): at least use binary search to find the range of activities we need to draw
	start := -1
	end := -1
	for i, gw := range tl.Activities {
		y := (activityHeight+activityGap)*int(i) - tl.Y
		// Don't draw activities that would be fully hidden, but do draw partially hidden ones
		if y < -activityHeight {
			continue
		}
		if y > gtx.Constraints.Max.Y {
			break
		}
		end = i
		if start == -1 {
			start = i
		}

		stack := op.Offset(image.Pt(0, y)).Push(gtx.Ops)
		topBorder := i > 0 && tl.Activities[i-1].hovered
		gw.Layout(gtx, tl.Activity.DisplayAllLabels, tl.Activity.Compact, topBorder)
		stack.Pop()
	}

	var out []*ActivityWidget
	if start != -1 {
		out = tl.Activities[start : end+1]
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
	var fnName string
	line1 := "Goroutine %[1]d\n\n"
	if tt.G.Function != nil {
		fnName = tt.G.Function.Fn
		line1 = "Goroutine %[1]d: %[2]s\n\n"
	}
	l := fmt.Sprintf(line1+
		"Appeared at: %[3]s\n"+
		"Disappeared at: %[4]s\n"+
		"Lifetime: %[5]s\n"+
		"Time in blocked states: %[6]s (%.2[7]f%%)\n"+
		"Time in inactive states: %[8]s (%.2[9]f%%)\n"+
		"Time in GC assist: %[10]s (%.2[11]f%%)\n"+
		"Time in running states: %[12]s (%.2[13]f%%)",
		tt.G.ID, fnName,
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
			if l := s.Event.Link; l != nil {
				label += fmt.Sprintf("\nSwept %d bytes, reclaimed %d bytes", l.Args[0], l.Args[1])
			}
		case stateRunningG:
			label += fmt.Sprintf("running goroutine %d", s.Event.G)
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
	var padding = gtx.Dp(tooltipPaddingDp)
	var tooltipBorderWidth = gtx.Dp(tooltipBorderWidthDp)

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

type Processor struct {
	ID    int
	Spans []Span
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
	// OPT(dh): cache this. especially because it gets called a lot by the goroutine selector window.
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
	Event *trace.Event
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

type Trace struct {
	Gs  []*Goroutine
	Ps  []*Processor
	GC  []Span
	STW []Span
}

func loadTrace(path string, ch chan Command) (*Trace, error) {
	var gs []*Goroutine
	var ps []*Processor
	var gc []Span
	var stw []Span

	r, err := os.Open(path)
	if err != nil {
		return nil, err
	}

	res, err := trace.Parse(bufio.NewReader(r), "")
	if err != nil {
		return nil, err
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
	psByID := map[int]*Processor{}
	getP := func(pid int) *Processor {
		p, ok := psByID[pid]
		if ok {
			return p
		}
		p = &Processor{ID: pid}
		psByID[pid] = p
		return p
	}

	lastSyscall := map[uint64][]*trace.Frame{}
	inMarkAssist := map[uint64]struct{}{}

	for i, ev := range res.Events {
		if i%10000 == 0 {
			select {
			case ch <- Command{"setProgress", float32(i) / float32(len(res.Events))}:
			default:
				// Don't let the rendering loop slow down parsing. Especially when vsync is enabled we'll only get to
				// read commands every blanking interval.
			}
		}
		var gid uint64
		var state schedulingState
		var reason string
		var pState int

		const (
			pNone = iota
			pRunG
			pStopG
		)

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
			pState = pRunG

			if _, ok := inMarkAssist[gid]; ok {
				state = stateGCMarkAssist
			} else {
				state = stateActive
			}
		case trace.EvGoStartLabel:
			// ev.G starts running
			// TODO(dh): make use of the label
			gid = ev.G
			pState = pRunG
			state = stateActive
		case trace.EvGoStop:
			// ev.G is stopping
			gid = ev.G
			pState = pStopG
			state = stateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			gid = ev.G
			pState = pStopG
			state = stateDone
		case trace.EvGoSched:
			// ev.G calls Gosched
			gid = ev.G
			pState = pStopG
			state = stateInactive
			reason = "called runtime.Gosched"
		case trace.EvGoSleep:
			// ev.G calls Sleep
			gid = ev.G
			pState = pStopG
			state = stateInactive
			reason = "called time.Sleep"
		case trace.EvGoPreempt:
			// ev.G got preempted
			gid = ev.G
			pState = pStopG
			state = stateInactive
			reason = "got preempted"
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoBlockGC, trace.EvGoBlock:
			// ev.G is blocking
			gid = ev.G
			pState = pStopG
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
			pState = pStopG
			state = stateBlockedSyscall
		case trace.EvGoInSyscall:
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoSysExit:
			gid = ev.G
			state = stateReady
		case trace.EvProcStart, trace.EvProcStop:
			// TODO(dh): should we implement a per-M timeline that shows which procs are running on which OS threads?
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

		case trace.EvGCStart:
			gc = append(gc, Span{Start: time.Duration(ev.Ts), State: stateActive, Event: ev, Stack: ev.Stk})
			continue

		case trace.EvGCSTWStart:
			stw = append(stw, Span{Start: time.Duration(ev.Ts), State: stateActive, Event: ev, Stack: ev.Stk})
			continue

		case trace.EvGCDone:
			// XXX verify that index isn't out of bounds
			gc[len(gc)-1].End = time.Duration(ev.Ts)
			continue

		case trace.EvGCSTWDone:
			// Even though STW happens as part of GC, we can see EvGCSTWDone after EvGCDone.
			// XXX verify that index isn't out of bounds
			stw[len(stw)-1].End = time.Duration(ev.Ts)
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
			return nil, fmt.Errorf("unsupported trace event %d", ev.Type)
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
						return nil, fmt.Errorf("illegal state transition %d -> %d for goroutine %d, offset %d", prevState, state, gid, ev.Off)
					}
				}
			}
		}

		s := Span{Start: time.Duration(ev.Ts), State: state, Event: ev, Reason: reason, Stack: ev.Stk}
		if ev.Type == trace.EvGoSysBlock {
			s.Stack = lastSyscall[ev.G]
		}
		s = applyPatterns(s)

		// move s.At out of the runtime
		for s.At+1 < len(s.Stack) && strings.HasPrefix(s.Stack[s.At].Fn, "runtime.") {
			s.At++
		}

		getG(gid).Spans = append(getG(gid).Spans, s)

		switch pState {
		case pRunG:
			p := getP(ev.P)
			p.Spans = append(p.Spans, Span{Start: time.Duration(ev.Ts), State: stateRunningG, Event: ev})
		case pStopG:
			// XXX guard against malformed traces
			p := getP(ev.P)
			p.Spans[len(p.Spans)-1].End = time.Duration(ev.Ts)
		}
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

	for _, p := range psByID {
		ps = append(ps, p)
	}

	sort.Slice(ps, func(i, j int) bool {
		return ps[i].ID < ps[j].ID
	})

	return &Trace{Gs: gs, Ps: ps, GC: gc, STW: stw}, nil
}

type Command struct {
	// TODO(dh): use an enum
	Command string
	Data    any
}

type Application struct {
	win      *app.Window
	theme    *material.Theme
	commands chan Command
	tl       Timeline
	gs       []*Goroutine
	ps       []*Processor
}

func main() {
	a := &Application{
		commands: make(chan Command, 16),
	}
	go func() {
		a.commands <- Command{"setState", "loadingTrace"}
		t, err := loadTrace(os.Args[1], a.commands)
		if err != nil {
			a.commands <- Command{"error", fmt.Errorf("couldn't load trace: %w", err)}
			return
		}
		a.commands <- Command{"loadTrace", t}
	}()
	go func() {
		a.win = app.NewWindow(app.Title("gotraceui"))
		if cpuprofiling {
			f, _ := os.Create("cpu.pprof")
			pprof.StartCPUProfile(f)
		}
		err := a.run()
		if cpuprofiling {
			pprof.StopCPUProfile()
		}
		if memprofiling {
			f, _ := os.Create("mem.pprof")
			pprof.WriteHeapProfile(f)
		}
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

	colorStateBlocked:                    toColor(0xBA4141FF),
	colorStateBlockedWaitingForTraceData: toColor(0xBA4141FF),
	colorStateBlockedHappensBefore:       toColor(0xBB6363FF),
	colorStateBlockedNet:                 toColor(0xBB5D5DFF),
	colorStateBlockedGC:                  toColor(0xBB554FFF),
	colorStateBlockedSyscall:             toColor(0xBA4F41FF),
	colorStateGCMarkAssist:               toColor(0x9C6FD6FF),
	colorStateGCSweep:                    toColor(0x9C6FD6FF),

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

	colorActivityLabel:  toColor(0x888888FF),
	colorActivityBorder: toColor(0xDDDDDDFF),

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
	colorStateBlockedWaitingForTraceData

	colorStateReady
	colorStateStuck
	colorStateMerged

	colorStateLast

	colorBackground
	colorZoomSelection
	colorCursor
	colorTick
	colorTickLabel
	colorTooltipText
	colorTooltipBackground
	colorTooltipBorder

	colorActivityLabel
	colorActivityBorder

	colorSpanWithEvents
	colorSpanOutline
)

type schedulingState int

const (
	stateNone schedulingState = iota

	// Goroutine states
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

	// Processor states
	stateRunningG
)

var stateColors = [...]colorIndex{
	stateInactive:                   colorStateInactive,
	stateActive:                     colorStateActive,
	stateBlocked:                    colorStateBlocked,
	stateBlockedWaitingForTraceData: colorStateBlockedWaitingForTraceData,
	stateBlockedSend:                colorStateBlockedHappensBefore,
	stateBlockedRecv:                colorStateBlockedHappensBefore,
	stateBlockedSelect:              colorStateBlockedHappensBefore,
	stateBlockedSync:                colorStateBlockedHappensBefore,
	stateBlockedCond:                colorStateBlockedHappensBefore,
	stateBlockedNet:                 colorStateBlockedNet,
	stateBlockedGC:                  colorStateBlockedGC,
	stateBlockedSyscall:             colorStateBlockedSyscall,
	stateStuck:                      colorStateStuck,
	stateReady:                      colorStateReady,
	stateCreated:                    colorStateReady,
	stateGCMarkAssist:               colorStateGCMarkAssist,
	stateGCSweep:                    colorStateGCSweep,
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

func (a *Application) loadTrace(t *Trace) {
	var end time.Duration
	for _, g := range t.Gs {
		if len(g.Spans) > 0 {
			d := g.Spans[len(g.Spans)-1].End
			if d > end {
				end = d
			}
		}
	}
	for _, p := range t.Ps {
		if len(p.Spans) > 0 {
			d := p.Spans[len(p.Spans)-1].End
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

	a.tl = Timeline{
		Start: start,
		End:   end,
		Theme: a.theme,
	}
	a.tl.Axis = Axis{tl: &a.tl}
	a.tl.Activities = make([]*ActivityWidget, 2, len(t.Gs)+len(t.Ps)+2)
	a.tl.Activities[0] = NewGCWidget(&a.tl, t.GC)
	a.tl.Activities[1] = NewSTWWidget(&a.tl, t.STW)
	for _, p := range t.Ps {
		a.tl.Activities = append(a.tl.Activities, NewProcessorWidget(&a.tl, p))
	}
	for _, g := range t.Gs {
		a.tl.Activities = append(a.tl.Activities, NewGoroutineWidget(&a.tl, g))
	}
	a.tl.prevFrame.dspSpans = map[any][]struct {
		dspSpans []Span
		startPx  float32
		endPx    float32
	}{}

	a.gs = t.Gs
	a.ps = t.Ps
}

func (a *Application) run() error {
	a.theme = material.NewTheme(gofont.Collection())
	a.tl.Theme = a.theme
	a.tl.Axis.tl = &a.tl

	profileTag := new(int)
	var ops op.Ops

	var ww *ListWindow[*Goroutine]
	var shortcuts int

	// TODO(dh): use enum for state
	state := "empty"
	var progress float32
	var err error
	for {
		select {
		case cmd := <-a.commands:
			switch cmd.Command {
			case "setState":
				state = cmd.Data.(string)
				progress = 0.0
				a.win.Invalidate()
			case "setProgress":
				progress = cmd.Data.(float32)
				a.win.Invalidate()
			case "loadTrace":
				a.loadTrace(cmd.Data.(*Trace))
				state = "main"
				progress = 0.0
				a.win.Invalidate()
				ww = nil
			case "error":
				state = "error"
				err = cmd.Data.(error)
				progress = 0.0
			default:
				panic(fmt.Sprintf("unknown command %s", cmd.Command))
			}

		case e := <-a.win.Events():
			switch ev := e.(type) {
			case system.DestroyEvent:
				return ev.Err
			case system.FrameEvent:
				gtx := layout.NewContext(&ops, ev)
				gtx.Constraints.Min = image.Point{}

				clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops)
				// Fill background
				paint.Fill(gtx.Ops, colors[colorBackground])

				switch state {
				case "empty":

				case "error":
					paint.ColorOp{Color: toColor(0x000000FF)}.Add(gtx.Ops)
					m := op.Record(gtx.Ops)
					dims := widget.Label{}.Layout(gtx, a.theme.Shaper, text.Font{}, 14, fmt.Sprintf("Error: %s", err))
					call := m.Stop()
					op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
					call.Add(gtx.Ops)

				case "loadingTrace":
					paint.ColorOp{Color: toColor(0x000000FF)}.Add(gtx.Ops)
					m := op.Record(gtx.Ops)
					dims := widget.Label{}.Layout(gtx, a.theme.Shaper, text.Font{}, 14, "Loading trace...")
					op.Offset(image.Pt(0, dims.Size.Y)).Add(gtx.Ops)
					Constrain(gtx, clip.Rect{Max: image.Pt(dims.Size.X, 5)}, material.ProgressBar(a.theme, progress).Layout)
					call := m.Stop()
					op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
					call.Add(gtx.Ops)

				case "main":
					for _, ev := range gtx.Events(&shortcuts) {
						switch ev := ev.(type) {
						case key.Event:
							if ev.State == key.Press && ev.Name == "G" && ww == nil {
								ww = NewListWindow[*Goroutine](a.theme)
								ww.SetItems(a.gs)
								ww.Filter = func(item *Goroutine, f string) bool {
									// XXX implement a much better filtering function that can do case-insensitive fuzzy search,
									// and allows matching goroutines by ID.
									if item.Function == nil {
										return f == ""
									}
									return strings.Contains(item.Function.Fn, f)
								}
							}
						}
					}

					key.InputOp{Tag: &shortcuts, Keys: "G"}.Add(gtx.Ops)

					if ww != nil {
						if item, ok := ww.Confirmed(); ok {
							a.tl.scrollToGoroutine(gtx, item)
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

					a.tl.Layout(gtx)
					a.tl.prevFrame.Start = a.tl.Start
					a.tl.prevFrame.End = a.tl.End
					a.tl.prevFrame.nsPerPx = a.tl.nsPerPx
					a.tl.prevFrame.Y = a.tl.Y
					a.tl.prevFrame.compact = a.tl.Activity.Compact
					a.tl.prevFrame.highlightSpansWithEvents = a.tl.Activity.HighlightSpansWithEvents
					a.tl.prevFrame.hoveredSpans = a.tl.Activity.HoveredSpans
				}

				if cpuprofiling {
					op.InvalidateOp{}.Add(&ops)
				}
				ev.Frame(&ops)
			}
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

/*
   Goroutine window, things to display:

   - ID, function name
   - stack of where it was created
   - Link to span that created it
   - First, last timestamp, duration
   - Per-state statistics (how long blocked, waiting, etc, number of state transitions)
   - List of all spans
   - List of all events of all spans
     - Syscalls
     - Outgoing unblocks
     - Incoming unblocks
   - List of other goroutines of the same function
   - Link to function window
   - List of procs it ran on
   - List of user regions
   - How much memory we sweeped/reclaimed
   - Maybe something about MMU?
*/

func Constrain(gtx layout.Context, c clip.Rect, w layout.Widget) layout.Dimensions {
	defer c.Push(gtx.Ops).Pop()
	gtx.Constraints.Max.X = c.Max.X - c.Min.X
	gtx.Constraints.Max.Y = c.Max.Y - c.Min.Y
	return w(gtx)
}

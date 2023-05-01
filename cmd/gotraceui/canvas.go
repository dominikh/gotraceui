package main

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"math"
	rtrace "runtime/trace"
	"sort"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/unit"
	"gioui.org/x/component"
	"golang.org/x/exp/slices"
)

type showTooltips uint8

const (
	showTooltipsBoth = iota
	showTooltipsSpans
	showTooltipsNone
)

type showGCOverlays uint8

const (
	showGCOverlaysNone = iota
	showGCOverlaysSTW
	showGCOverlaysBoth
)

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistanceDp      unit.Dp = 20
	tickHeightDp           unit.Dp = 12
	tickWidthDp            unit.Dp = 1
	minTickLabelDistanceDp unit.Dp = 8

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 4

	spanBorderWidthDp            unit.Dp = 1
	spanHighlightedBorderWidthDp unit.Dp = 2
)

const animateLength = 250 * time.Millisecond

// TODO(dh): is there any point in making this configurable?
const maxLocationHistoryEntries = 1024

type LocationHistoryEntry struct {
	start   trace.Timestamp
	nsPerPx float64
	y       int
}

type Canvas struct {
	trace *Trace

	debugWindow *DebugWindow

	clickedGoroutineTimelines []*ptrace.Goroutine
	clickedSpans              []struct {
		Spans     ptrace.Spans
		AllEvents []ptrace.EventID
		Timeline  *Timeline
		Track     *Track
	}

	// The start of the timeline
	start   trace.Timestamp
	nsPerPx float64

	// The width of the canvas, in pixels, updated on each frame
	width int

	// Imagine we're drawing all timelines onto an infinitely long canvas. Canvas.y specifies the y of that infinite
	// canvas that the timeline section's y == 0 is displaying.
	y            int
	cachedHeight int

	// Scratch space used by ActivityWidgetTrack.Layout
	trackSpanLabels []string

	animateTo struct {
		animating bool

		initialStart   trace.Timestamp
		initialNsPerPx float64
		initialY       int

		targetStart   trace.Timestamp
		targetNsPerPx float64
		targetY       int

		startedAt time.Time
	}

	locationHistory []LocationHistoryEntry
	// All timelines. Index 0 and 1 are the GC and STW timelines, followed by processors and goroutines.
	timelines []*Timeline
	scrollbar widget.Scrollbar
	axis      Axis

	memoryGraph Plot

	// State for dragging the canvas
	drag struct {
		drag    gesture.Drag
		ready   bool
		clickAt f32.Point
		active  bool
		start   trace.Timestamp
		end     trace.Timestamp
		startY  int
	}

	// State for zooming to a selection
	zoomSelection struct {
		ready   bool
		clickAt f32.Point
		active  bool
	}

	// We have multiple sources of the pointer position, which are valid during different times: Canvas.hover and
	// Canvas.drag.drag – when we're dragging, Canvas.drag.drag grabs pointer input and the hover won't update anymore.
	pointerAt f32.Point
	hover     gesture.Hover

	timeline struct {
		filter             Filter
		automaticFilter    Filter
		displayAllLabels   bool
		compact            bool
		displayStackTracks bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays showGCOverlays

		hoveredTimeline *Timeline
		hoveredSpans    ptrace.Spans
		hover           gesture.Hover
	}

	resizeMemoryTimelines component.Resize

	// prevFrame records the canvas's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the canvas hasn't changed.
	prevFrame struct {
		start              trace.Timestamp
		end                trace.Timestamp
		y                  int
		nsPerPx            float64
		compact            bool
		displayStackTracks bool
		displayedTls       []*Timeline
		hoveredTimeline    *Timeline
		hoveredSpans       ptrace.Spans
		width              int
		filter             Filter
		automaticFilter    Filter
	}

	// timelineEnds[i] describes the absolute Y pixel offset where timeline i ends. It is computed by
	// Canvas.computeTimelinePositions
	timelineEnds []int

	timelineWidgetsCache Cache[TimelineWidget]
	trackWidgetsCache    Cache[TrackWidget]
}

func NewCanvasInto(cv *Canvas, dwin *DebugWindow, t *Trace) {
	*cv = Canvas{}

	cv.resizeMemoryTimelines.Axis = layout.Vertical
	cv.resizeMemoryTimelines.Ratio = 0.1
	cv.timeline.displayAllLabels = true
	cv.axis = Axis{cv: cv, anchor: AxisAnchorCenter}
	cv.trace = t
	cv.debugWindow = dwin

	cv.timelines = make([]*Timeline, 2, len(t.Goroutines)+len(t.Processors)+len(t.Machines)+2)
	cv.timelines[0] = NewGCTimeline(cv, t, t.GC)
	cv.timelines[1] = NewSTWTimeline(cv, t, t.STW)

	cv.timeline.hoveredSpans = NoSpan{}
}

func (cv *Canvas) End() trace.Timestamp {
	return cv.start + trace.Timestamp(float64(cv.width)*cv.nsPerPx)
}

func (cv *Canvas) computeTimelinePositions(gtx layout.Context) {
	if len(cv.timelineEnds) == len(cv.timelines) &&
		cv.timeline.compact == cv.prevFrame.compact &&
		cv.timeline.displayStackTracks == cv.prevFrame.displayStackTracks {
		return
	}

	cv.timelineEnds = slices.Grow(cv.timelineEnds[:0], len(cv.timelines))[:len(cv.timelines)]
	accEnds := 0
	for i, tl := range cv.timelines {
		accEnds += tl.Height(gtx, cv)
		cv.timelineEnds[i] = accEnds
	}
}

func (cv *Canvas) rememberLocation() {
	e := LocationHistoryEntry{
		start:   cv.start,
		nsPerPx: cv.nsPerPx,
		y:       cv.y,
	}
	if o, ok := cv.peekLocationHistory(); ok && o == e {
		// don't record duplicate locations
		return
	}
	if len(cv.locationHistory) == maxLocationHistoryEntries {
		copy(cv.locationHistory, cv.locationHistory[1:])
		cv.locationHistory[len(cv.locationHistory)-1] = e
	} else {
		cv.locationHistory = append(cv.locationHistory, e)
	}
}

func (cv *Canvas) navigateToChecks(start trace.Timestamp, nsPerPx float64, y int) bool {
	if start == cv.start && nsPerPx == cv.nsPerPx && y == cv.y {
		// We're already there, do nothing. In particular, don't push to the location history.
		return false
	}

	if nsPerPx == 0 {
		return false
	}

	return true
}

func (cv *Canvas) cancelNavigation() {
	cv.animateTo.animating = false
}

// navigateTo modifes the canvas's start, end and y values, recording the previous location in the undo stack.
// navigateTo rejects invalid operations, like setting start = end.
func (cv *Canvas) navigateTo(gtx layout.Context, start trace.Timestamp, nsPerPx float64, y int) {
	if !cv.navigateToChecks(start, nsPerPx, y) {
		return
	}

	cv.rememberLocation()
	cv.navigateToImpl(gtx, start, nsPerPx, y)
}

func (cv *Canvas) navigateToStartAndEnd(gtx layout.Context, start, end trace.Timestamp, y int) {
	nsPerPx := float64(end-start) / float64(cv.width)
	cv.navigateTo(gtx, start, nsPerPx, y)
}

func (cv *Canvas) navigateToNoHistory(gtx layout.Context, start trace.Timestamp, nsPerPx float64, y int) {
	if !cv.navigateToChecks(start, nsPerPx, y) {
		return
	}

	cv.navigateToImpl(gtx, start, nsPerPx, y)
}

func (cv *Canvas) navigateToImpl(gtx layout.Context, start trace.Timestamp, nsPerPx float64, y int) {
	cv.animateTo.animating = true

	cv.animateTo.initialStart = cv.start
	cv.animateTo.initialNsPerPx = cv.nsPerPx
	cv.animateTo.initialY = cv.y

	cv.animateTo.targetStart = start
	cv.animateTo.targetNsPerPx = nsPerPx
	cv.animateTo.targetY = y

	cv.animateTo.startedAt = gtx.Now
	op.InvalidateOp{}.Add(gtx.Ops)
}

func (cv *Canvas) peekLocationHistory() (LocationHistoryEntry, bool) {
	if len(cv.locationHistory) == 0 {
		return LocationHistoryEntry{}, false
	}
	return cv.locationHistory[len(cv.locationHistory)-1], true
}

func (cv *Canvas) popLocationHistory() (LocationHistoryEntry, bool) {
	// XXX support redo

	if len(cv.locationHistory) == 0 {
		return LocationHistoryEntry{}, false
	}
	n := len(cv.locationHistory) - 1
	e := cv.locationHistory[n]
	cv.locationHistory = cv.locationHistory[:n]
	return e, true
}

func (cv *Canvas) unchanged() bool {
	if disableCaching {
		return false
	}

	return cv.prevFrame.start == cv.start &&
		cv.prevFrame.nsPerPx == cv.nsPerPx &&
		cv.prevFrame.width == cv.width &&
		cv.prevFrame.y == cv.y &&
		cv.prevFrame.compact == cv.timeline.compact &&
		cv.prevFrame.displayStackTracks == cv.timeline.displayStackTracks &&
		cv.prevFrame.filter == cv.timeline.filter &&
		cv.prevFrame.automaticFilter == cv.timeline.automaticFilter
}

func (cv *Canvas) startZoomSelection(pos f32.Point) {
	cv.zoomSelection.active = true
	cv.zoomSelection.clickAt = pos
}

func (cv *Canvas) abortZoomSelection() {
	cv.zoomSelection.active = false
}

func (cv *Canvas) endZoomSelection(win *theme.Window, gtx layout.Context, pos f32.Point) {
	cv.zoomSelection.active = false
	one := cv.zoomSelection.clickAt.X
	two := pos.X

	startPx := min(one, two)
	endPx := max(one, two)

	if startPx < 0 {
		startPx = 0
	}
	if limit := float32(cv.VisibleWidth(win, gtx)); endPx > limit {
		endPx = limit
	}

	start := cv.pxToTs(startPx)
	end := cv.pxToTs(endPx)
	if start == end {
		// Cannot zoom to a zero width area
		return
	}

	cv.navigateToStartAndEnd(gtx, start, end, cv.y)
}

func (cv *Canvas) startDrag(pos f32.Point) {
	cv.cancelNavigation()
	cv.rememberLocation()

	cv.drag.clickAt = pos
	cv.drag.active = true
	cv.drag.start = cv.start
	cv.drag.startY = cv.y
}

func (cv *Canvas) endDrag() {
	cv.drag.active = false
}

func (cv *Canvas) dragTo(gtx layout.Context, pos f32.Point) {
	td := time.Duration(math.Round(cv.nsPerPx * float64(cv.drag.clickAt.X-pos.X)))
	cv.start = cv.drag.start + trace.Timestamp(td)

	yd := int(round32(cv.drag.clickAt.Y - pos.Y))
	cv.y = cv.drag.startY + yd
	if cv.y < 0 {
		cv.y = 0
	}
	// XXX don't allow dragging cv.Y beyond the end
}

func (cv *Canvas) zoom(gtx layout.Context, ticks float32, at f32.Point) {
	// TODO(dh): implement location history for zooming. We shouldn't record one entry per call to zoom, and instead
	// only record on calls that weren't immediately preceeded by other calls to zoom.

	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		ds := trace.Timestamp(cv.nsPerPx * 100 * ratio)
		de := trace.Timestamp(cv.nsPerPx * 100 * (1 - ratio))

		start := cv.start + ds
		end := cv.End() - de
		cv.start = start
		cv.nsPerPx = float64(end-start) / float64(cv.width)
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		ds := trace.Timestamp(cv.nsPerPx * 100 * ratio)
		de := trace.Timestamp(cv.nsPerPx * 100 * (1 - ratio))

		// Make sure the user can always zoom out
		if ds < 1 {
			ds = 1
		}
		if de < 1 {
			de = 1
		}

		start := cv.start - ds
		end := cv.End() + de

		// Limit canvas to roughly one day. There's no reason to zoom out this far, and zooming out further will lead
		// to edge cases and eventually overflow.
		if time.Duration(end-start) < 24*time.Hour {
			cv.start = start
			cv.nsPerPx = float64(end-start) / float64(cv.width)
		}
	}
}

func (cv *Canvas) visibleSpans(spans ptrace.Spans) ptrace.Spans {
	// Visible spans have to end after cv.Start and begin before cv.End
	start := sort.Search((spans.Len()), func(i int) bool {
		s := spans.At(i)
		return s.End > cv.start
	})
	if start == (spans.Len()) {
		return NoSpan{}
	}
	end := sort.Search((spans.Len()), func(i int) bool {
		s := spans.At(i)
		return s.Start >= cv.End()
	})

	return spans.Slice(start, end)
}

//gcassert:inline
func (cv *Canvas) tsToPx(t trace.Timestamp) float32 {
	return float32(float64(t-cv.start) / float64(cv.nsPerPx))
}

//gcassert:inline
func (cv *Canvas) pxToTs(px float32) trace.Timestamp {
	return trace.Timestamp(math.Round(float64(px)*float64(cv.nsPerPx) + float64(cv.start)))
}

func (cv *Canvas) ZoomToFitCurrentView(gtx layout.Context) {
	var first, last trace.Timestamp = -1, -1
	start, end := cv.visibleTimelines(gtx)
	for _, tl := range cv.timelines[start:end] {
		for _, track := range tl.tracks {
			if track.spans.Len() == 0 || (track.kind == TrackKindStack && !cv.timeline.displayStackTracks) {
				continue
			}

			if t := track.spans.At(0).Start; t < first || first == -1 {
				first = t
			}
			if t := LastSpan(track.spans).End; t > last {
				last = t
			}
		}
	}
	if first != -1 && last == -1 {
		panic("unreachable")
	}

	cv.navigateToStartAndEnd(gtx, first, last, cv.y)
}

func (cv *Canvas) timelineY(gtx layout.Context, act any) int {
	// OPT(dh): don't be O(n)
	off := 0
	for _, tl := range cv.timelines {
		if act == tl.item {
			// TODO(dh): show goroutine at center of window, not the top
			return off
		}
		off += tl.Height(gtx, cv)
	}
	panic("unreachable")
}

func (cv *Canvas) scrollToTimeline(gtx layout.Context, act any) {
	off := cv.timelineY(gtx, act)
	cv.navigateTo(gtx, cv.start, cv.nsPerPx, off)
}

// The width in pixels of the visible portion of the canvas, i.e. the part that isn't occupied by the scrollbar.
func (cv *Canvas) VisibleWidth(win *theme.Window, gtx layout.Context) int {
	sbWidth := gtx.Dp(theme.Scrollbar(win.Theme, &cv.scrollbar).Width())
	return gtx.Constraints.Max.X - sbWidth
}

func easeOutQuart(progress float64) float64 {
	return 1 - math.Pow(1-progress, 4)
}

func easeInQuart(progress float64) float64 {
	return progress * progress * progress * progress
}

// height returns the sum of the heights of all visible timelines.
func (cv *Canvas) height(gtx layout.Context) int {
	if cv.prevFrame.compact == cv.timeline.compact &&
		cv.prevFrame.displayStackTracks == cv.timeline.displayStackTracks &&
		cv.cachedHeight != 0 {
		return cv.cachedHeight
	}

	var total int
	for _, tl := range cv.timelines {
		total += tl.Height(gtx, cv)
	}
	cv.cachedHeight = total
	return total
}

func (cv *Canvas) ToggleStackTracks() {
	cv.timeline.displayStackTracks = !cv.timeline.displayStackTracks
}

func (cv *Canvas) UndoNavigation(gtx layout.Context) {
	if e, ok := cv.popLocationHistory(); ok {
		cv.navigateToNoHistory(gtx, e.start, e.nsPerPx, e.y)
	}
}

func (cv *Canvas) ScrollToTop(gtx layout.Context) {
	cv.navigateTo(gtx, cv.start, cv.nsPerPx, 0)
}

func (cv *Canvas) JumpToBeginning(gtx layout.Context) {
	cv.navigateTo(gtx, 0, cv.nsPerPx, cv.y)
}

func (cv *Canvas) ToggleCompactDisplay() {
	cv.timeline.compact = !cv.timeline.compact
}

func (cv *Canvas) ToggleTimelineLabels() {
	cv.timeline.displayAllLabels = !cv.timeline.displayAllLabels
}

func (cv *Canvas) scroll(gtx layout.Context, dx, dy float32) {
	// TODO(dh): implement location history for scrolling. We shouldn't record one entry per call to scroll, and instead
	// only record on calls that weren't immediately preceeded by other calls to scroll.
	cv.y += int(round32(dy))
	if cv.y < 0 {
		cv.y = 0
	}
	// XXX don't allow dragging cv.y beyond end

	cv.start += trace.Timestamp(math.Round(float64(dx) * cv.nsPerPx))
}

func (cv *Canvas) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Canvas.Layout").End()

	// Compute the width. This has to make assumptions about the width of the timelines, because we need it
	// long before we get to laying out the timelines. Practically, the max width of timelines is only restricted by
	// the scrollbar, which has a fixed width.
	cv.width = cv.VisibleWidth(win, gtx)
	gtx.Constraints.Min = image.Point{}
	if gtx.Constraints.Max.X < 50 || gtx.Constraints.Max.Y < 50 {
		// Crude guard against having too litle space to render anything useful.
		return layout.Dimensions{Size: gtx.Constraints.Max}
	}

	if cv.nsPerPx == 0 {
		end := cv.trace.Events[len(cv.trace.Events)-1].Ts
		slack := float64(end) * 0.05
		cv.nsPerPx = (float64(end) + 2*slack) / float64(cv.width)
	}

	cv.timeline.hover.Update(gtx.Queue)
	cv.hover.Update(gtx.Queue)

	if cv.hover.Hovered() {
		cv.pointerAt = cv.hover.Pointer()
	}

	if cv.animateTo.animating {
		// XXX animation really makes it obvious that our span merging algorithm is unstable

		dt := gtx.Now.Sub(cv.animateTo.startedAt)
		if dt >= animateLength {
			cv.start = cv.animateTo.targetStart
			cv.nsPerPx = cv.animateTo.targetNsPerPx
			cv.y = cv.animateTo.targetY

			cv.debugWindow.animationProgress.addValue(gtx.Now, 1)
			cv.debugWindow.animationRatio.addValue(gtx.Now, 1)
			cv.animateTo.animating = false
		} else {
			timeRatio := float64(dt) / float64(animateLength)
			var r float64

			{
				initialStart := float64(cv.animateTo.initialStart)
				initialNsPerPx := float64(cv.animateTo.initialNsPerPx)
				targetStart := float64(cv.animateTo.targetStart)
				targetNsPerPx := float64(cv.animateTo.targetNsPerPx)

				initialWidth := initialNsPerPx*float64(cv.VisibleWidth(win, gtx)) - initialStart
				targetWidth := targetNsPerPx*float64(cv.VisibleWidth(win, gtx)) - targetStart

				var ease func(float64) float64
				if targetWidth <= initialWidth {
					// Zooming in (or not zooming at all)
					ease = easeOutQuart
				} else {
					// Zooming out
					ease = easeInQuart
				}
				r = ease(timeRatio)

				cv.debugWindow.animationProgress.addValue(gtx.Now, timeRatio)
				cv.debugWindow.animationRatio.addValue(gtx.Now, r)

				curStart := initialStart + (targetStart-initialStart)*r
				curNsPerPx := initialNsPerPx + (targetNsPerPx-initialNsPerPx)*r

				cv.start = trace.Timestamp(curStart)
				cv.nsPerPx = curNsPerPx
			}

			// XXX this looks bad when we panned in both X and Y, because the two animations don't run at the same speed
			// if the distances are different.

			{
				initialY := float64(cv.animateTo.initialY)
				targetY := float64(cv.animateTo.targetY)

				cv.y = int(initialY + (targetY-initialY)*r)
			}

			op.InvalidateOp{}.Add(gtx.Ops)
		}
	}

	for _, ev := range gtx.Events(cv) {
		switch ev := ev.(type) {
		case key.Event:
			if ev.State == key.Press {
				switch ev.Name {
				case key.NameHome:
					switch {
					case ev.Modifiers == key.ModShortcut:
						cv.ZoomToFitCurrentView(gtx)
					case ev.Modifiers == key.ModShift:
						cv.JumpToBeginning(gtx)
					case ev.Modifiers == 0:
						cv.ScrollToTop(gtx)
					}

				case "S":
					cv.ToggleStackTracks()
					if h := cv.timeline.hoveredTimeline; h != nil {
						cv.cancelNavigation()
						y := cv.timelineY(gtx, h.item)
						offset := h.hover.Pointer().Y
						if !cv.timeline.displayStackTracks {
							// We're going from stacks to no stacks. This shrinks the timeline and the cursor might end
							// up on the next timeline. Prevent that.
							if h.hover.Pointer().Y > float32(h.Height(gtx, cv)) {
								offset -= h.hover.Pointer().Y - float32(h.Height(gtx, cv))
							}
						}
						cv.y = y - (int(cv.timeline.hover.Pointer().Y) - int(offset))
					}

				case "Z":
					if ev.Modifiers.Contain(key.ModShortcut) {
						cv.UndoNavigation(gtx)
					}

				case "X":
					cv.ToggleTimelineLabels()

				case "C":
					cv.ToggleCompactDisplay()
					if h := cv.timeline.hoveredTimeline; h != nil {
						cv.cancelNavigation()
						y := cv.timelineY(gtx, h.item)

						offset := h.hover.Pointer().Y
						if cv.timeline.compact {
							// We're going from expanded to compact. This reduces the offset from the top of the
							// timeline to the first track to zero.
							offset -= float32(gtx.Dp(timelineLabelHeightDp))
							if h.hover.Pointer().Y < float32(gtx.Dp(timelineLabelHeightDp)) {
								// The cursor is above the first track; adjust the offset so that the cursor lands on the first track
								offset += float32(gtx.Dp(timelineLabelHeightDp)) - h.hover.Pointer().Y
							}
						}

						cv.y = y - (int(cv.timeline.hover.Pointer().Y) - int(offset))
					}

				case "T":
					cv.timeline.showTooltips = (cv.timeline.showTooltips + 1) % (showTooltipsNone + 1)
					var s string
					switch cv.timeline.showTooltips {
					case showTooltipsBoth:
						s = "Showing all tooltips"
					case showTooltipsSpans:
						s = "Showing span tooltips only"
					case showTooltipsNone:
						s = "Showing no tooltips"
					}
					win.ShowNotification(gtx, s)

				case "O":
					cv.timeline.showGCOverlays = (cv.timeline.showGCOverlays + 1) % (showGCOverlaysBoth + 1)
					var s string
					switch cv.timeline.showGCOverlays {
					case showGCOverlaysBoth:
						s = "Showing STW and GC overlays"
					case showGCOverlaysSTW:
						s = "Showing STW overlays"
					case showGCOverlaysNone:
						s = "Showing no overlays"
					}
					win.ShowNotification(gtx, s)

				}
			}
		case pointer.Event:
			switch ev.Type {
			case pointer.Scroll:
				// XXX deal with Gio's asinine "scroll focused area into view" behavior when shrinking windows
				cv.abortZoomSelection()
				if ev.Modifiers == 0 {
					cv.scroll(gtx, ev.Scroll.X, ev.Scroll.Y)
				} else if ev.Modifiers == key.ModShortcut {
					cv.zoom(gtx, ev.Scroll.Y, ev.Position)
				}
			}
		}
	}

	for _, ev := range cv.drag.drag.Events(gtx.Metric, gtx, gesture.Both) {
		switch ev.Type {
		case pointer.Press:
			if ev.Modifiers == 0 {
				cv.drag.ready = true
			} else if ev.Modifiers == key.ModShortcut {
				cv.zoomSelection.ready = true
			}
		case pointer.Drag:
			cv.pointerAt = ev.Position
			if cv.drag.ready && !cv.drag.active {
				cv.startDrag(ev.Position)
			} else if cv.zoomSelection.ready && !cv.zoomSelection.active {
				cv.startZoomSelection(ev.Position)
			}
			if cv.drag.active {
				cv.dragTo(gtx, ev.Position)
			}
		case pointer.Release, pointer.Cancel:
			cv.drag.ready = false
			cv.zoomSelection.ready = false
			if cv.drag.active {
				cv.endDrag()
			}
			if cv.zoomSelection.active {
				cv.endZoomSelection(win, gtx, ev.Position)
			}
		}
	}

	for _, tl := range cv.prevFrame.displayedTls {
		if spans := tl.NavigatedSpans(); spans.Len() > 0 {
			start := spans.At(0).Start
			end := LastSpan(spans).End
			cv.navigateToStartAndEnd(gtx, start, end, cv.y)
			break
		}
	}

	cv.computeTimelinePositions(gtx)

	func(gtx layout.Context) {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		cv.hover.Add(gtx.Ops)

		cv.clickedGoroutineTimelines = cv.clickedGoroutineTimelines[:0]

		if d := cv.scrollbar.ScrollDistance(); d != 0 {
			// TODO(dh): because scroll amounts are relative even when the user clicks on a specific spot on the
			// scrollbar, and because we've already executed this frame's navigation animation step, applying the
			// delta to cv.y can leave it in a different position than where the user clicked.
			//
			// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further

			cv.cancelNavigation()

			totalHeight := cv.height(gtx)
			cv.y += int(round32(d * float32(totalHeight)))
			if cv.y < 0 {
				cv.y = 0
			}
		}

		// Set up event handlers
		pointer.InputOp{
			Tag:          cv,
			ScrollBounds: image.Rectangle{Min: image.Pt(-100, -100), Max: image.Pt(100, 100)},
			Types:        pointer.Move | pointer.Scroll,
		}.Add(gtx.Ops)
		if cv.drag.active {
			pointer.CursorAllScroll.Add(gtx.Ops)
		}
		key.InputOp{Tag: cv, Keys: "Short-Z|C|S|O|T|X|(Shift)-(Short)-" + key.NameHome}.Add(gtx.Ops)

		drawRegionOverlays := func(spans ptrace.Spans, c color.NRGBA, height int) {
			var p clip.Path
			p.Begin(gtx.Ops)
			visible := cv.visibleSpans(spans)
			for i := 0; i < visible.Len(); i++ {
				s := visible.At(i)
				start := s.Start
				end := s.End

				if start < cv.start {
					start = cv.start
				}
				if end > cv.End() {
					end = cv.End()
				}

				xMin := cv.tsToPx(start)
				xMax := cv.tsToPx(end)
				if xMax-xMin < 1 {
					continue
				}
				clip.FRect{
					// TODO(dh): should the overlay start at the top of the screen, or after the axis?
					Min: f32.Pt(xMin, 0),
					Max: f32.Pt(xMax, float32(height)),
				}.IntoPath(&p)
			}
			paint.FillShape(gtx.Ops, c, clip.Outline{Path: p.End()}.Op())
		}

		// Draw axis, memory graph, timelines, and scrollbar
		layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
			// Axis
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				// Note that even though the axis is wider than the timelines (because timelines have a scrollbar), the
				// mapping of timestamp to pixel position is still correct, because it gets computed earlier, by using
				// Canvas.VisibleWidth.

				// Draw STW and GC regions
				// TODO(dh): make this be optional
				tickHeight := gtx.Dp(tickHeightDp)
				drawRegionOverlays((cv.trace.GC), colors[colorStateGC], tickHeight)
				drawRegionOverlays((cv.trace.STW), colors[colorStateBlocked], tickHeight)

				dims := cv.axis.Layout(win, gtx)

				return dims
			}),

			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				return theme.Resize(win.Theme, &cv.resizeMemoryTimelines).Layout(win, gtx,
					// Memory graph
					func(win *theme.Window, gtx layout.Context) layout.Dimensions {
						defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
						cv.drag.drag.Add(gtx.Ops)

						dims := cv.memoryGraph.Layout(win, gtx, cv)
						return dims
					},

					// Timelines and scrollbar
					func(win *theme.Window, gtx layout.Context) layout.Dimensions {
						return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
							// Timelines
							layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
								defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

								if width := gtx.Constraints.Max.X; width != cv.width {
									panic(fmt.Sprintf("computed timelines width differs from actual width: %d != %d", cv.width, width))
								}

								cv.drag.drag.Add(gtx.Ops)

								cv.timeline.hover.Add(gtx.Ops)
								dims, tws := cv.layoutTimelines(win, gtx)
								cv.prevFrame.displayedTls = tws
								return dims
							}),

							// Scrollbar
							layout.Rigid(func(gtx layout.Context) layout.Dimensions {
								totalHeight := cv.height(gtx)
								if len(cv.timelines) > 0 {
									// Allow scrolling past the last goroutine
									totalHeight += cv.timelines[len(cv.timelines)-1].Height(gtx, cv)
								}

								fraction := float32(gtx.Constraints.Max.Y) / float32(totalHeight)
								offset := float32(cv.y) / float32(totalHeight)
								sb := theme.Scrollbar(win.Theme, &cv.scrollbar)
								return sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
							}),
						)
					},
				)
			}),
		)

		// Draw zoom selection
		if cv.zoomSelection.active {
			one := cv.zoomSelection.clickAt.X
			two := cv.pointerAt.X
			rect := clip.FRect{
				Min: f32.Pt(min(one, two), 0),
				Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
			}
			paint.FillShape(gtx.Ops, win.Theme.Palette.PrimarySelection, rect.Op(gtx.Ops))
		}

		// Draw STW and GC overlays
		if cv.timeline.showGCOverlays >= showGCOverlaysBoth {
			c := colors[colorStateGC]
			c.A = 0x33
			drawRegionOverlays((cv.trace.GC), c, gtx.Constraints.Max.Y)
		}
		if cv.timeline.showGCOverlays >= showGCOverlaysSTW {
			c := colors[colorStateSTW]
			c.A = 0x33
			drawRegionOverlays((cv.trace.STW), c, gtx.Constraints.Max.Y)
		}

		// Draw cursor
		rect := clip.Rect{
			Min: image.Pt(int(round32(cv.pointerAt.X)), 0),
			Max: image.Pt(int(round32(cv.pointerAt.X+1)), gtx.Constraints.Max.Y),
		}
		paint.FillShape(gtx.Ops, win.Theme.Palette.Foreground, rect.Op())

	}(gtx)

	cv.prevFrame.start = cv.start
	cv.prevFrame.nsPerPx = cv.nsPerPx
	cv.prevFrame.width = cv.width
	cv.prevFrame.y = cv.y
	cv.prevFrame.compact = cv.timeline.compact
	cv.prevFrame.displayStackTracks = cv.timeline.displayStackTracks
	cv.prevFrame.hoveredSpans = cv.timeline.hoveredSpans
	cv.prevFrame.hoveredTimeline = cv.timeline.hoveredTimeline
	cv.prevFrame.filter = cv.timeline.filter
	cv.prevFrame.automaticFilter = cv.timeline.automaticFilter

	cv.clickedSpans = cv.clickedSpans[:0]
	cv.timeline.hoveredSpans = NoSpan{}
	cv.timeline.hoveredTimeline = nil
	cv.timeline.automaticFilter = Filter{Mode: FilterModeAnd}
	for _, tl := range cv.prevFrame.displayedTls {
		if clicked := tl.ClickedSpans(); clicked.Spans.Len() > 0 {
			var allEvents []ptrace.EventID
			switch item := tl.item.(type) {
			case *ptrace.Goroutine:
				allEvents = item.Events
			default:
				// TODO(dh): give all relevant types a method that we can check for, instead of having to hard-code
				// a list of types here.
			}
			cv.clickedSpans = append(cv.clickedSpans, struct {
				Spans     ptrace.Spans
				AllEvents []ptrace.EventID
				Timeline  *Timeline
				Track     *Track
			}{clicked.Spans, allEvents, tl, clicked.Track})
		}
		if tl.Hovered() {
			cv.timeline.hoveredTimeline = tl
			spans := tl.HoveredSpans()
			if spans.Len() > 0 {
				cv.timeline.hoveredSpans = spans
			}

			switch hitem := tl.item.(type) {
			case *ptrace.Goroutine:
				if spans.Len() == 0 {
					// A goroutine timeline is hovered, but no spans within are.
					cv.timeline.automaticFilter.Processor.Goroutine = hitem.ID
				} else {
					// Highlight processor spans for the same goroutine if they overlap with the highlighted span.
					cv.timeline.automaticFilter.Processor.Goroutine = hitem.ID
					cv.timeline.automaticFilter.Processor.StartAfter = cv.timeline.hoveredSpans.At(0).Start
					cv.timeline.automaticFilter.Processor.EndBefore = LastSpan(cv.timeline.hoveredSpans).End
				}

			case *ptrace.Processor:
				if spans.Len() == 1 {
					cv.timeline.automaticFilter.Processor.Goroutine = cv.trace.Event(spans.At(0).Event).G
				}

				cv.timeline.automaticFilter.Machine.Processor = hitem.ID

			case *ptrace.Machine:
				if spans.Len() == 1 {
					o := spans.At(0)
					if o.State == ptrace.StateRunningG {
						cv.timeline.automaticFilter.Processor.Goroutine = cv.trace.Event(o.Event).G
					}

					if o.State == ptrace.StateRunningP {
						cv.timeline.automaticFilter.Machine.Processor = cv.trace.Event(o.Event).P
					}
				}
			}
			break
		}
	}

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (cv *Canvas) visibleTimelines(gtx layout.Context) (start, end int) {
	// start at first timeline that ends within or after the visible range
	// end at first timeline that starts after the visible range
	start = sort.Search(len(cv.timelines), func(i int) bool {
		return cv.timelineEnds[i]-cv.y >= 0
	})
	end = sort.Search(len(cv.timelines), func(i int) bool {
		start := 0
		if i != 0 {
			start = cv.timelineEnds[i-1]
		}
		return start-cv.y >= gtx.Constraints.Max.Y
	})
	return start, end
}

func (cv *Canvas) layoutTimelines(win *theme.Window, gtx layout.Context) (layout.Dimensions, []*Timeline) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	for _, tl := range cv.prevFrame.displayedTls {
		tl.displayed = false
	}

	start, end := cv.visibleTimelines(gtx)

	y := -cv.y
	if start < len(cv.timelines) && start > 0 {
		y = cv.timelineEnds[start-1] - cv.y
	}

	for i := start; i < end; i++ {
		tl := cv.timelines[i]
		stack := op.Offset(image.Pt(0, y)).Push(gtx.Ops)
		topBorder := i > 0 && cv.timelines[i-1].Hovered()
		if tl.TimelineWidget == nil {
			tl.TimelineWidget = cv.timelineWidgetsCache.Get()
			*tl.TimelineWidget = TimelineWidget{cv: cv}
		}
		tl.Layout(win, gtx, cv, cv.timeline.displayAllLabels, cv.timeline.compact, topBorder, &cv.trackSpanLabels)
		stack.Pop()

		y += tl.Height(gtx, cv)

		if tl.LabelClicked() {
			if g, ok := tl.item.(*ptrace.Goroutine); ok {
				cv.clickedGoroutineTimelines = append(cv.clickedGoroutineTimelines, g)
			}
		}
	}

	for _, tl := range cv.prevFrame.displayedTls {
		if !tl.displayed {
			// The timeline was displayed last frame but wasn't this frame -> notify it that it is no longer visible so
			// that it can release temporary resources.
			tl.notifyHidden(cv)
		}
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}, cv.timelines[start:end]
}

// setPointerPosition updates the canvas's pointer position. This is used by Axis to keep the canvas updated while the
// axis is being dragged.
func (cv *Canvas) setPointerPosition(pos f32.Point) {
	cv.pointerAt = pos
}

type AxisAnchor uint8

const (
	AxisAnchorNone AxisAnchor = iota
	AxisAnchorStart
	AxisAnchorCenter
	AxisAnchorEnd
)

type Axis struct {
	cv       *Canvas
	click    gesture.Click
	drag     gesture.Drag
	ticksOps reusableOps

	// the location of the origin in pixels
	position float32
	// where the position of the origin is anchored to
	anchor AxisAnchor

	prevFrame struct {
		ops    reusableOps
		call   op.CallOp
		dims   layout.Dimensions
		origin trace.Timestamp
	}
}

func (axis *Axis) tickInterval(gtx layout.Context) (time.Duration, bool) {
	if axis.cv.nsPerPx == 0 {
		return 0, false
	}
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	minTickDistance := gtx.Dp(minTickDistanceDp)
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(math.Round(float64(t) / axis.cv.nsPerPx))
		if tickDistance >= minTickDistance {
			return t, true
		}
	}
	panic("unreachable")
}

func (axis *Axis) Layout(win *theme.Window, gtx layout.Context) (dims layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "main.Axis.Layout").End()
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	axis.click.Add(gtx.Ops)
	axis.drag.Add(gtx.Ops)

	for _, ev := range axis.click.Events(gtx.Queue) {
		if ev.Type == gesture.TypePress && ev.Button == pointer.ButtonSecondary {
			win.SetContextMenu(
				[]*theme.MenuItem{
					{
						Label:    PlainLabel("Move origin to the left"),
						Disabled: func() bool { return axis.anchor == AxisAnchorStart },
						Do:       func(gtx layout.Context) { axis.anchor = AxisAnchorStart },
					},
					{
						Label:    PlainLabel("Move origin to the center"),
						Disabled: func() bool { return axis.anchor == AxisAnchorCenter },
						Do:       func(gtx layout.Context) { axis.anchor = AxisAnchorCenter },
					},
					{
						Label:    PlainLabel("Move origin to the right"),
						Disabled: func() bool { return axis.anchor == AxisAnchorEnd },
						Do:       func(gtx layout.Context) { axis.anchor = AxisAnchorEnd },
					},
				},
			)
		}
	}

	for _, ev := range axis.drag.Events(gtx.Metric, gtx.Queue, gesture.Horizontal) {
		if ev.Type == pointer.Press || ev.Type == pointer.Drag {
			// We've grabbed the input, which makes us responsible for updating the canvas's cursor.
			axis.cv.setPointerPosition(ev.Position)
			width := axis.cv.VisibleWidth(win, gtx)
			axis.position = ev.Position.X
			axis.anchor = AxisAnchorNone
			if axis.position < 0 {
				axis.position = 0
			} else if axis.position > float32(width) {
				axis.position = float32(width)
			}
		}
	}

	tickWidth := float32(gtx.Dp(tickWidthDp))
	tickHeight := float32(gtx.Dp(tickHeightDp))
	minTickLabelDistance := float32(gtx.Dp(minTickLabelDistanceDp))

	tickInterval, ok := axis.tickInterval(gtx)
	if !ok {
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight))}
	}

	min := axis.cv.start
	max := axis.cv.End()
	var origin trace.Timestamp
	switch axis.anchor {
	case AxisAnchorNone:
		origin = axis.cv.pxToTs(axis.position)
		if origin < min {
			origin = min
		}
		if origin > max {
			origin = max
		}
	case AxisAnchorStart:
		origin = min
	case AxisAnchorCenter:
		origin = (min + max) / 2
	case AxisAnchorEnd:
		origin = max
	}

	if axis.cv.unchanged() && axis.prevFrame.origin == origin {
		axis.prevFrame.call.Add(gtx.Ops)
		debugCaching(gtx)
		return axis.prevFrame.dims
	}

	origOps := gtx.Ops
	gtx.Ops = axis.prevFrame.ops.get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		axis.prevFrame.call = call
		axis.prevFrame.dims = dims
		axis.prevFrame.origin = origin
	}()

	var ticksPath clip.Path
	ticksPath.Begin(axis.ticksOps.get())

	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	var originLabelExtents image.Rectangle
	var prevLabelEnd float32

	drawTick := func(t trace.Timestamp, label string, forward bool) {
		add := func(a, b trace.Timestamp) trace.Timestamp {
			if forward {
				return a + b
			} else {
				return a - b
			}
		}
		addf := func(a, b float32) float32 {
			if forward {
				return a + b
			} else {
				return a - b
			}
		}
		// a - b
		subf := func(a, b float32) float32 {
			if forward {
				return a - b
			} else {
				return a + b
			}
		}
		// a > b
		gtrf := func(a, b float32) bool {
			if forward {
				return a > b
			} else {
				return a < b
			}
		}

		start := axis.cv.tsToPx(t) - tickWidth/2
		end := axis.cv.tsToPx(t) + tickWidth/2
		rect := clip.FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := axis.cv.tsToPx(add(t, trace.Timestamp(tickInterval/10)*trace.Timestamp(j))) - tickWidth/2
			smallEnd := axis.cv.tsToPx(add(t, trace.Timestamp(tickInterval/10)*trace.Timestamp(j))) + tickWidth/2
			smallTickHeight := tickHeight / 3
			if j == 5 {
				smallTickHeight = tickHeight / 2
			}
			rect := clip.FRect{
				Min: f32.Pt(smallStart, 0),
				Max: f32.Pt(smallEnd, smallTickHeight),
			}
			rect.IntoPath(&ticksPath)
		}

		rec := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			return widget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, font.Font{}, win.Theme.TextSize, label)
		})
		// TODO separate value and unit symbol with a space

		if gtrf(subf(start, float32(rec.Dimensions.Size.X/2)), addf(prevLabelEnd, minTickLabelDistance)) || prevLabelEnd == 0 {
			// XXX don't draw label if it's partially off screen. this seems to happen for the left labels, which is
			// bad, because it might cut off the sign.
			prevLabelEnd = addf(start, float32(rec.Dimensions.Size.X/2))
			if start+float32(rec.Dimensions.Size.X/2) <= float32(gtx.Constraints.Max.X) {
				stack := op.Offset(image.Pt(int(round32(start-float32(rec.Dimensions.Size.X/2))), int(tickHeight))).Push(gtx.Ops)
				rec.Layout(win, gtx)
				stack.Pop()
			}
		}
	}

	// drawOrigin draws the origin tick and label, as well as minor ticks on both sides.
	drawOrigin := func(t trace.Timestamp) {
		start := axis.cv.tsToPx(t) - tickWidth/2
		end := axis.cv.tsToPx(t) + tickWidth/2
		rect := clip.FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := -9; j <= 9; j++ {
			smallStart := axis.cv.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) - tickWidth/2
			smallEnd := axis.cv.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) + tickWidth/2
			smallTickHeight := tickHeight / 3
			if j == 5 || j == -5 {
				smallTickHeight = tickHeight / 2
			}
			rect := clip.FRect{
				Min: f32.Pt(smallStart, 0),
				Max: f32.Pt(smallEnd, smallTickHeight),
			}
			rect.IntoPath(&ticksPath)
		}

		rec := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			f := font.Font{Weight: font.Bold}
			label := formatTimestamp(t)
			return widget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, label)
		})
		// TODO separate value and unit symbol with a space
		labelStart := image.Pt(int(round32(start-float32(rec.Dimensions.Size.X/2))), int(tickHeight))
		if labelStart.X < 0 {
			labelStart.X = 0
		} else if labelStart.X+rec.Dimensions.Size.X > gtx.Constraints.Max.X {
			labelStart.X = gtx.Constraints.Max.X - rec.Dimensions.Size.X
		}
		stack := op.Offset(labelStart).Push(gtx.Ops)
		rec.Layout(win, gtx)
		stack.Pop()

		originLabelExtents = image.Rectangle{
			Min: image.Pt(labelStart.X, 0),
			Max: image.Pt(labelStart.X+rec.Dimensions.Size.X, rec.Dimensions.Size.Y),
		}
	}

	drawOrigin(origin)

	prevLabelEnd = float32(originLabelExtents.Max.X)
	for t := origin + trace.Timestamp(tickInterval); t < max; t += trace.Timestamp(tickInterval) {
		label := fmt.Sprintf("+%s", time.Duration(t-origin))
		drawTick(t, label, true)
	}

	prevLabelEnd = float32(originLabelExtents.Min.X)
	for t := origin - trace.Timestamp(tickInterval); t >= min; t -= trace.Timestamp(tickInterval) {
		label := fmt.Sprintf("-%s", time.Duration(origin-t))
		drawTick(t, label, false)
	}

	paint.FillShape(gtx.Ops, win.Theme.Palette.Foreground, clip.Outline{Path: ticksPath.End()}.Op())

	labelHeight := originLabelExtents.Max.Y
	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight+0.5)+labelHeight)}
}

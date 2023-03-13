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

	"golang.org/x/exp/slices"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	mywidget "honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/gesture"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
	"gioui.org/x/component"
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
	start trace.Timestamp
	end   trace.Timestamp
	y     int
}

type Canvas struct {
	trace *Trace

	debugWindow *DebugWindow

	clickedGoroutineTimelines []*ptrace.Goroutine

	// The region of the canvas that we're displaying, measured in nanoseconds
	start trace.Timestamp
	end   trace.Timestamp
	// Imagine we're drawing all timelines onto an infinitely long canvas. Canvas.y specifies the y of that infinite
	// canvas that the timeline section's y == 0 is displaying.
	y            int
	cachedHeight int

	// Scratch space used by ActivityWidgetTrack.Layout
	trackSpanLabels []string

	animateTo struct {
		animating bool

		initialStart trace.Timestamp
		initialEnd   trace.Timestamp
		initialY     int

		targetStart trace.Timestamp
		targetEnd   trace.Timestamp
		targetY     int

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

	// Frame-local state set by Layout and read by various helpers
	nsPerPx float32

	pointerAt f32.Point

	timeline struct {
		displayAllLabels   bool
		compact            bool
		displayStackTracks bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays showGCOverlays

		hoveredTimeline *Timeline
		hoveredSpans    SpanSelector
		pointerAt       f32.Point
	}

	resizeMemoryTimelines component.Resize

	spanModal *SpanModal

	// prevFrame records the canvas's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the canvas hasn't changed.
	prevFrame struct {
		start              trace.Timestamp
		end                trace.Timestamp
		y                  int
		nsPerPx            float32
		compact            bool
		displayStackTracks bool
		displayedTls       []*Timeline
		hoveredTimeline    *Timeline
		hoveredSpans       SpanSelector
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
	cv.axis = Axis{cv: cv, origin: 0.5}
	cv.trace = t
	cv.debugWindow = dwin

	cv.timelines = make([]*Timeline, 2, len(t.Goroutines)+len(t.Processors)+len(t.Machines)+2)
	cv.timelines[0] = NewGCTimeline(cv, t, t.GC)
	cv.timelines[1] = NewSTWTimeline(cv, t, t.STW)
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
		start: cv.start,
		end:   cv.end,
		y:     cv.y,
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

func (cv *Canvas) navigateToChecks(start, end trace.Timestamp, y int) bool {
	if start == cv.start && end == cv.end && y == cv.y {
		// We're already there, do nothing. In particular, don't push to the location history.
		return false
	}

	if start == end {
		// Cannot zoom to a zero width area
		return false
	}

	return true
}

func (cv *Canvas) cancelNavigation() {
	cv.animateTo.animating = false
}

// navigateTo modifes the canvas's start, end and y values, recording the previous location in the undo stack.
// navigateTo rejects invalid operations, like setting start = end.
func (cv *Canvas) navigateTo(gtx layout.Context, start, end trace.Timestamp, y int) {
	if !cv.navigateToChecks(start, end, y) {
		return
	}

	cv.rememberLocation()
	cv.navigateToImpl(gtx, start, end, y)
}

func (cv *Canvas) navigateToNoHistory(gtx layout.Context, start, end trace.Timestamp, y int) {
	if !cv.navigateToChecks(start, end, y) {
		return
	}

	cv.navigateToImpl(gtx, start, end, y)
}

func (cv *Canvas) navigateToImpl(gtx layout.Context, start, end trace.Timestamp, y int) {
	cv.animateTo.animating = true

	cv.animateTo.initialStart = cv.start
	cv.animateTo.initialEnd = cv.end
	cv.animateTo.initialY = cv.y

	cv.animateTo.targetStart = start
	cv.animateTo.targetEnd = end
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
		cv.prevFrame.end == cv.end &&
		cv.prevFrame.nsPerPx == cv.nsPerPx &&
		cv.prevFrame.y == cv.y &&
		cv.prevFrame.compact == cv.timeline.compact &&
		cv.prevFrame.displayStackTracks == cv.timeline.displayStackTracks
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

	cv.navigateTo(gtx, start, end, cv.y)
}

func (cv *Canvas) startDrag(pos f32.Point) {
	cv.cancelNavigation()
	cv.rememberLocation()

	cv.drag.clickAt = pos
	cv.drag.active = true
	cv.drag.start = cv.start
	cv.drag.end = cv.end
	cv.drag.startY = cv.y
}

func (cv *Canvas) endDrag() {
	cv.drag.active = false
}

func (cv *Canvas) dragTo(gtx layout.Context, pos f32.Point) {
	td := time.Duration(round32(cv.nsPerPx * (cv.drag.clickAt.X - pos.X)))
	cv.start = cv.drag.start + trace.Timestamp(td)
	cv.end = cv.drag.end + trace.Timestamp(td)

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

	// FIXME(dh): repeatedly zooming in and out doesn't cancel each other out. Fix that.
	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := at.X / float32(gtx.Constraints.Max.X)
		ds := time.Duration(cv.nsPerPx * 100 * ratio)
		de := time.Duration(cv.nsPerPx * 100 * (1 - ratio))
		cv.start += trace.Timestamp(ds)
		cv.end -= trace.Timestamp(de)
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := at.X / float32(gtx.Constraints.Max.X)
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
		end := cv.end + de

		// Limit canvas to roughly one day. There's rno reason to zoom out this far, and zooming out further will lead
		// to edge cases and eventually overflow.
		if time.Duration(end-start) < 24*time.Hour {
			cv.start = start
			cv.end = end
		}
	}

	if cv.start > cv.end {
		cv.start = cv.end - 1
	}
}

func (cv *Canvas) visibleSpans(spanSel SpanSelector) SpanSelector {
	// Visible spans have to end after cv.Start and begin before cv.End
	spans := spanSel.Spans()
	start := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.End > cv.start
	})
	if start == len(spans) {
		return NoSpan{}
	}
	end := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.Start >= cv.end
	})

	return spanSel.Slice(start, end)
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
			if track.spans.Size() == 0 || (track.kind == TrackKindStack && !cv.timeline.displayStackTracks) {
				continue
			}

			if t := track.spans.At(0).Start; t < first || first == -1 {
				first = t
			}
			if t := track.spans.At(track.spans.Size() - 1).End; t > last {
				last = t
			}
		}
	}
	if first != -1 && last == -1 {
		panic("unreachable")
	}

	cv.navigateTo(gtx, first, last, cv.y)
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
	cv.navigateTo(gtx, cv.start, cv.end, off)
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
		cv.navigateToNoHistory(gtx, e.start, e.end, e.y)
	}
}

func (cv *Canvas) ScrollToTop(gtx layout.Context) {
	cv.navigateTo(gtx, cv.start, cv.end, 0)
}

func (cv *Canvas) JumpToBeginning(gtx layout.Context) {
	d := cv.end - cv.start
	cv.navigateTo(gtx, 0, d, cv.y)
}

func (cv *Canvas) ToggleCompactDisplay() {
	// FIXME(dh): adjust cv.Y so that the top visible goroutine stays the same
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

	cv.start += trace.Timestamp(round32(dx * cv.nsPerPx))
	cv.end += trace.Timestamp(round32(dx * cv.nsPerPx))
}

func (cv *Canvas) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Canvas.Layout").End()
	gtx.Constraints.Min = image.Point{}

	if gtx.Constraints.Max.X < 50 || gtx.Constraints.Max.Y < 50 {
		// Crude guard against having too litle space to render anything useful.
		return layout.Dimensions{Size: gtx.Constraints.Max}
	}

	if cv.animateTo.animating {
		// XXX animation really makes it obvious that our span merging algorithm is unstable

		dt := gtx.Now.Sub(cv.animateTo.startedAt)
		if dt >= animateLength {
			cv.start = cv.animateTo.targetStart
			cv.end = cv.animateTo.targetEnd
			cv.y = cv.animateTo.targetY

			cv.debugWindow.animationProgress.addValue(gtx.Now, 1)
			cv.debugWindow.animationRatio.addValue(gtx.Now, 1)
			cv.animateTo.animating = false
		} else {
			timeRatio := float64(dt) / float64(animateLength)
			var r float64

			{
				initialStart := float64(cv.animateTo.initialStart)
				initialEnd := float64(cv.animateTo.initialEnd)
				targetStart := float64(cv.animateTo.targetStart)
				targetEnd := float64(cv.animateTo.targetEnd)

				initialWidth := initialEnd - initialStart
				targetWidth := targetEnd - targetStart

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
				curEnd := initialEnd + (targetEnd-initialEnd)*r

				cv.start = trace.Timestamp(curStart)
				cv.end = trace.Timestamp(curEnd)
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
						if _, ok := h.item.(ptrace.Spans); ok {
							// FIXME(dh): these are the GC and STW timelines at the top. We don't have to do anything
							// for them because there can't be stack tracks above them. We _mustn't_ do anything because
							// timelineY will crash upon seeing ptrace.Spans.
						} else {
							cv.cancelNavigation()
							y := cv.timelineY(gtx, h.item)
							cv.y = y - (int(cv.timeline.pointerAt.Y) - int(h.pointerAt.Y))
						}
					}

				case "Z":
					if ev.Modifiers.Contain(key.ModShortcut) {
						cv.UndoNavigation(gtx)
					}

				case "X":
					cv.ToggleTimelineLabels()

				case "C":
					cv.ToggleCompactDisplay()

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
			cv.pointerAt = ev.Position

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

	for _, e := range gtx.Events(&cv.timeline.pointerAt) {
		ev := e.(pointer.Event)
		cv.timeline.pointerAt = ev.Position
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

	cv.computeTimelinePositions(gtx)

	func(gtx layout.Context) {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

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

		cv.timeline.hoveredSpans = NoSpan{}
		cv.timeline.hoveredTimeline = nil
		for _, tl := range cv.prevFrame.displayedTls {
			if spanSel := tl.NavigatedSpans(); spanSel.Size() > 0 {
				start := spanSel.At(0).Start
				end := spanSel.At(spanSel.Size() - 1).End
				cv.navigateTo(gtx, start, end, cv.y)
				break
			}
			if spanSel := tl.ClickedSpans(); spanSel.Size() == 1 {
				// XXX defer
				var allEvents []ptrace.EventID
				switch item := tl.item.(type) {
				case *ptrace.Goroutine:
					allEvents = item.Events
				default:
					// TODO(dh): give all relevant types a method that we can check for, instead of having to hard-code
					// a list of types here.
				}
				sm := &SpanModal{
					Span:      spanSel.At(0),
					AllEvents: allEvents,
					Trace:     cv.trace,
				}
				cv.spanModal = sm
				win.SetPopup(sm.Layout)
				break
			}
		}
		for _, tl := range cv.prevFrame.displayedTls {
			if tl.Hovered() {
				cv.timeline.hoveredTimeline = tl
				if spanSel := tl.HoveredSpans(); spanSel.Size() > 0 {
					cv.timeline.hoveredSpans = spanSel
				}
				break
			}
		}

		// Compute nsPerPx. This has to make assumptions about the width of the timelines, because we need nsPerPx set
		// long before we get to laying out the timelines. Practically, the max width of timelines is only restricted by
		// the scrollbar, which has a fixed width.
		estimatedTimelinesWidth := cv.VisibleWidth(win, gtx)
		cv.nsPerPx = float32(cv.end-cv.start) / float32(estimatedTimelinesWidth)
		cv.debugWindow.cvPxPerNs.addValue(gtx.Now, 1.0/float64(cv.nsPerPx))

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
		key.FocusOp{Tag: cv}.Add(gtx.Ops)

		drawRegionOverlays := func(spanSel SpanSelector, c color.NRGBA, height int) {
			var p clip.Path
			p.Begin(gtx.Ops)
			for _, s := range cv.visibleSpans(spanSel).Spans() {
				start := s.Start
				end := s.End

				if start < cv.start {
					start = cv.start
				}
				if end > cv.end {
					end = cv.end
				}

				xMin := cv.tsToPx(start)
				xMax := cv.tsToPx(end)
				if xMax-xMin < 1 {
					continue
				}
				FRect{
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
				drawRegionOverlays(SliceToSpanSelector(cv.trace.GC), colors[colorStateGC], tickHeight)
				drawRegionOverlays(SliceToSpanSelector(cv.trace.STW), colors[colorStateBlocked], tickHeight)

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

								if width := gtx.Constraints.Max.X; width != estimatedTimelinesWidth {
									panic(fmt.Sprintf("estimated timelines width differs from actual width: %d != %d", estimatedTimelinesWidth, width))
								}

								cv.drag.drag.Add(gtx.Ops)

								pointer.InputOp{Tag: &cv.timeline.pointerAt, Types: pointer.Move | pointer.Drag}.Add(gtx.Ops)
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
			rect := FRect{
				Min: f32.Pt(min(one, two), 0),
				Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
			}
			paint.FillShape(gtx.Ops, win.Theme.Palette.PrimarySelection, rect.Op(gtx.Ops))
		}

		// Draw STW and GC overlays
		if cv.timeline.showGCOverlays >= showGCOverlaysBoth {
			c := colors[colorStateGC]
			c.A = 0xCC
			drawRegionOverlays(SliceToSpanSelector(cv.trace.GC), c, gtx.Constraints.Max.Y)
		}
		if cv.timeline.showGCOverlays >= showGCOverlaysSTW {
			c := colors[colorStateSTW]
			c.A = 0xCC
			drawRegionOverlays(SliceToSpanSelector(cv.trace.STW), c, gtx.Constraints.Max.Y)
		}

		// Draw cursor
		rect := clip.Rect{
			Min: image.Pt(int(round32(cv.pointerAt.X)), 0),
			Max: image.Pt(int(round32(cv.pointerAt.X+1)), gtx.Constraints.Max.Y),
		}
		paint.FillShape(gtx.Ops, win.Theme.Palette.Foreground, rect.Op())

	}(gtx)

	cv.prevFrame.start = cv.start
	cv.prevFrame.end = cv.end
	cv.prevFrame.nsPerPx = cv.nsPerPx
	cv.prevFrame.y = cv.y
	cv.prevFrame.compact = cv.timeline.compact
	cv.prevFrame.displayStackTracks = cv.timeline.displayStackTracks
	cv.prevFrame.hoveredSpans = cv.timeline.hoveredSpans
	cv.prevFrame.hoveredTimeline = cv.timeline.hoveredTimeline

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

type Axis struct {
	cv *Canvas

	ticksOps reusableOps

	// the location of the origin, expressed as a ratio of the axis width in [0, 1]
	origin float32

	prevFrame struct {
		ops    reusableOps
		call   op.CallOp
		dims   layout.Dimensions
		origin float32
	}
}

func (axis *Axis) tickInterval(gtx layout.Context) (time.Duration, bool) {
	if axis.cv.nsPerPx == 0 {
		return 0, false
	}
	// Note that an analytical solution exists for this, but computing it is slower than the loop.
	minTickDistance := gtx.Dp(minTickDistanceDp)
	for t := time.Duration(1); true; t *= 10 {
		tickDistance := int(round32(float32(t) / axis.cv.nsPerPx))
		if tickDistance >= minTickDistance {
			return t, true
		}
	}
	panic("unreachable")
}

func (axis *Axis) Layout(win *theme.Window, gtx layout.Context) (dims layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "main.Axis.Layout").End()
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	for _, e := range gtx.Events(axis) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Press, pointer.Drag:
			switch ev.Buttons {
			case pointer.ButtonPrimary:
				// We've grabbed the input, which makes us responsible for updating the canvas's cursor.
				axis.cv.setPointerPosition(ev.Position)
				axis.origin = ev.Position.X / float32(gtx.Constraints.Max.X)
				if axis.origin < 0 {
					axis.origin = 0
				} else if axis.origin > 1 {
					axis.origin = 1
				}
			case pointer.ButtonSecondary:
				win.SetContextMenu(
					[]*theme.MenuItem{
						{
							Label:    PlainLabel("Move origin to the left"),
							Disabled: func() bool { return axis.origin == 0 },
							Do:       func(gtx layout.Context) { axis.origin = 0 },
						},
						{
							Label:    PlainLabel("Move origin to the center"),
							Disabled: func() bool { return axis.origin == 0.5 },
							Do:       func(gtx layout.Context) { axis.origin = 0.5 },
						},
						{
							Label:    PlainLabel("Move origin to the right"),
							Disabled: func() bool { return axis.origin == 1 },
							Do:       func(gtx layout.Context) { axis.origin = 1 },
						},
					},
				)
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
	max := axis.cv.end
	origin := min + trace.Timestamp(round32(float32(max-min)*axis.origin))

	if axis.cv.unchanged() && axis.prevFrame.origin == axis.origin {
		axis.prevFrame.call.Add(gtx.Ops)
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
		axis.prevFrame.origin = axis.origin
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
		rect := FRect{
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
			rect := FRect{
				Min: f32.Pt(smallStart, 0),
				Max: f32.Pt(smallEnd, smallTickHeight),
			}
			rect.IntoPath(&ticksPath)
		}

		macro := op.Record(gtx.Ops)
		// TODO separate value and unit symbol with a space
		dims := mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, label)
		call := macro.Stop()

		if gtrf(subf(start, float32(dims.Size.X/2)), addf(prevLabelEnd, minTickLabelDistance)) || prevLabelEnd == 0 {
			// XXX don't draw label if it's partially off screen. this seems to happen for the left labels, which is
			// bad, because it might cut off the sign.
			prevLabelEnd = addf(start, float32(dims.Size.X/2))
			if start+float32(dims.Size.X/2) <= float32(gtx.Constraints.Max.X) {
				stack := op.Offset(image.Pt(int(round32(start-float32(dims.Size.X/2))), int(tickHeight))).Push(gtx.Ops)
				call.Add(gtx.Ops)
				stack.Pop()
			}
		}
	}

	// drawOrigin draws the origin tick and label, as well as minor ticks on both sides.
	drawOrigin := func(t trace.Timestamp) {
		start := axis.cv.tsToPx(t) - tickWidth/2
		end := axis.cv.tsToPx(t) + tickWidth/2
		rect := FRect{
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
			rect := FRect{
				Min: f32.Pt(smallStart, 0),
				Max: f32.Pt(smallEnd, smallTickHeight),
			}
			rect.IntoPath(&ticksPath)
		}

		macro := op.Record(gtx.Ops)
		// TODO separate value and unit symbol with a space
		f := text.Font{Weight: text.Bold}
		label := formatTimestamp(t)
		dims := mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, label)
		call := macro.Stop()

		labelStart := image.Pt(int(round32(start-float32(dims.Size.X/2))), int(tickHeight))
		if labelStart.X < 0 {
			labelStart.X = 0
		} else if labelStart.X+dims.Size.X > gtx.Constraints.Max.X {
			labelStart.X = gtx.Constraints.Max.X - dims.Size.X
		}
		stack := op.Offset(labelStart).Push(gtx.Ops)
		call.Add(gtx.Ops)
		stack.Pop()

		originLabelExtents = image.Rectangle{
			Min: image.Pt(labelStart.X, 0),
			Max: image.Pt(labelStart.X+dims.Size.X, dims.Size.Y),
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

	pointer.InputOp{Tag: axis, Grab: true, Types: pointer.Press | pointer.Drag}.Add(gtx.Ops)

	labelHeight := originLabelExtents.Max.Y
	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight+0.5)+labelHeight)}
}

type SpanModal struct {
	Trace     *Trace
	AllEvents []ptrace.EventID
	Span      ptrace.Span
	Events    *Events
}

func (sm *SpanModal) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	return theme.Dialog(win.Theme, "Span").Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Max.X = gtx.Constraints.Constrain(image.Pt(1000, 0)).X
		gtx.Constraints.Max = gtx.Constraints.Constrain(image.Pt(gtx.Constraints.Max.X, 300))

		if sm.Events == nil {
			sm.Events = &Events{
				Trace:  sm.Trace,
				Events: sm.Span.Events(sm.AllEvents, sm.Trace.Trace),
			}

			sm.Events.Filter.ShowGoCreate.Value = true
			sm.Events.Filter.ShowGoUnblock.Value = true
			sm.Events.Filter.ShowGoSysCall.Value = true
			sm.Events.Filter.ShowUserLog.Value = true
			sm.Events.UpdateFilter()
		}

		return sm.Events.Layout(win, gtx)
	})
}

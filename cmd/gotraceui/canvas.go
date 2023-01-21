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
	timelines []*TimelineWidget
	scrollbar widget.Scrollbar
	axis      Axis

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

	timeline struct {
		displayAllLabels   bool
		compact            bool
		displayStackTracks bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays showGCOverlays

		hoveredTimeline *TimelineWidget
		hoveredSpans    SpanSelector
		cursorPos       f32.Point
	}

	contextMenu []*theme.MenuItem

	// prevFrame records the canvas's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the canvas hasn't changed.
	prevFrame struct {
		start              trace.Timestamp
		end                trace.Timestamp
		y                  int
		nsPerPx            float32
		compact            bool
		displayStackTracks bool
		displayedTws       []*TimelineWidget
		hoveredTimeline    *TimelineWidget
		hoveredSpans       SpanSelector
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
	return float32(t-cv.start) / cv.nsPerPx
}

//gcassert:inline
func (cv *Canvas) pxToTs(px float32) trace.Timestamp {
	return trace.Timestamp(round32(px*cv.nsPerPx + float32(cv.start)))
}

func (cv *Canvas) ZoomToFitCurrentView(gtx layout.Context) {
	var first, last trace.Timestamp = -1, -1
	for _, tw := range cv.visibleTimelines(gtx) {
		for _, track := range tw.tracks {
			if track.spans.Size() == 0 || (track.kind == TimelineWidgetTrackStack && !cv.timeline.displayStackTracks) {
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
	for _, tw := range cv.timelines {
		if act == tw.item {
			// TODO(dh): show goroutine at center of window, not the top
			return off
		}
		off += tw.Height(gtx)
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
	for _, tw := range cv.timelines {
		total += tw.Height(gtx)
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
			cv.timeline.cursorPos = ev.Position

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
			cv.timeline.cursorPos = ev.Position
			if cv.drag.ready && !cv.drag.active {
				cv.startDrag(ev.Position)
			} else if cv.zoomSelection.ready && !cv.zoomSelection.active {
				cv.startZoomSelection(ev.Position)
			}
			if cv.drag.active {
				cv.dragTo(gtx, ev.Position)
			}
		case pointer.Release:
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

	var axisHeight int

	// Draw axis and timelines
	func(gtx layout.Context) {
		gtx.Constraints.Max.X = cv.VisibleWidth(win, gtx)
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
		for _, tw := range cv.prevFrame.displayedTws {
			if spanSel := tw.NavigatedSpans(); spanSel.Size() > 0 {
				start := spanSel.At(0).Start
				end := spanSel.At(spanSel.Size() - 1).End
				cv.navigateTo(gtx, start, end, cv.y)
				break
			}
		}
		for _, tw := range cv.prevFrame.displayedTws {
			if tw.hovered {
				cv.timeline.hoveredTimeline = tw
				if spanSel := tw.HoveredSpans(); spanSel.Size() > 0 {
					cv.timeline.hoveredSpans = spanSel
				}
				break
			}
		}

		for _, item := range cv.contextMenu {
			if item.Clicked() {
				item.Do(gtx)
				win.CloseContextMenu()
			}
		}

		cv.nsPerPx = float32(cv.end-cv.start) / float32(gtx.Constraints.Max.X)
		cv.debugWindow.cvPxPerNs.addValue(gtx.Now, 1.0/float64(cv.nsPerPx))

		if debug {
			if cv.end < cv.start {
				panic("XXX")
			}
		}

		// Set up event handlers
		cv.drag.drag.Add(gtx.Ops)
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
				rect := FRect{
					// TODO(dh): should the overlay start at the top of the screen, or after the axis?
					Min: f32.Pt(xMin, 0),
					Max: f32.Pt(xMax, float32(height)),
				}
				paint.FillShape(gtx.Ops, c, rect.Op(gtx.Ops))
			}
		}

		// Draw axis and timelines
		layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				// Draw STW and GC regions
				// TODO(dh): make this be optional
				tickHeight := gtx.Dp(tickHeightDp)
				drawRegionOverlays(SliceToSpanSelector(cv.trace.GC), colors[colorStateGC], tickHeight)
				drawRegionOverlays(SliceToSpanSelector(cv.trace.STW), colors[colorStateBlocked], tickHeight)

				dims := cv.axis.Layout(win, gtx)
				axisHeight = dims.Size.Y

				return dims
			}),
			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				dims, tws := cv.layoutTimelines(win, gtx)
				cv.prevFrame.displayedTws = tws
				return dims
			}))

		// Draw zoom selection
		if cv.zoomSelection.active {
			one := cv.zoomSelection.clickAt.X
			two := cv.timeline.cursorPos.X
			rect := FRect{
				Min: f32.Pt(min(one, two), 0),
				Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
			}
			paint.FillShape(gtx.Ops, colors[colorZoomSelection], rect.Op(gtx.Ops))
		}

		// Draw STW and GC overlays
		if cv.timeline.showGCOverlays >= showGCOverlaysBoth {
			drawRegionOverlays(SliceToSpanSelector(cv.trace.GC), rgba(0x9C6FD633), gtx.Constraints.Max.Y)
		}
		if cv.timeline.showGCOverlays >= showGCOverlaysSTW {
			drawRegionOverlays(SliceToSpanSelector(cv.trace.STW), rgba(0xBA414133), gtx.Constraints.Max.Y)
		}

		// Draw cursor
		rect := clip.Rect{
			Min: image.Pt(int(round32(cv.timeline.cursorPos.X)), 0),
			Max: image.Pt(int(round32(cv.timeline.cursorPos.X+1)), gtx.Constraints.Max.Y),
		}
		paint.FillShape(gtx.Ops, colors[colorCursor], rect.Op())

		cv.prevFrame.start = cv.start
		cv.prevFrame.end = cv.end
		cv.prevFrame.nsPerPx = cv.nsPerPx
		cv.prevFrame.y = cv.y
		cv.prevFrame.compact = cv.timeline.compact
		cv.prevFrame.displayStackTracks = cv.timeline.displayStackTracks
		cv.prevFrame.hoveredSpans = cv.timeline.hoveredSpans
		cv.prevFrame.hoveredTimeline = cv.timeline.hoveredTimeline
	}(gtx)

	// Draw scrollbar
	func(gtx layout.Context) {
		defer op.Offset(image.Pt(cv.VisibleWidth(win, gtx), axisHeight)).Push(gtx.Ops).Pop()
		gtx.Constraints.Max.Y -= axisHeight

		totalHeight := cv.height(gtx)
		if len(cv.timelines) > 0 {
			// Allow scrolling past the last goroutine
			totalHeight += cv.timelines[len(cv.timelines)-1].Height(gtx)
		}

		fraction := float32(gtx.Constraints.Max.Y) / float32(totalHeight)
		offset := float32(cv.y) / float32(totalHeight)
		sb := theme.Scrollbar(win.Theme, &cv.scrollbar)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
	}(gtx)

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (cv *Canvas) visibleTimelines(gtx layout.Context) []*TimelineWidget {
	start := -1
	end := -1
	// OPT(dh): at least use binary search to find the range of timelines we need to draw. now that timeline heights
	// aren't constant anymore, however, this is more complicated.
	y := -cv.y
	for i, tw := range cv.timelines {
		// Don't draw timelines that would be fully hidden, but do draw partially hidden ones
		twHeight := tw.Height(gtx)
		if y < -twHeight {
			y += twHeight
			continue
		}
		if start == -1 {
			start = i
		}
		if y > gtx.Constraints.Max.Y {
			end = i
			break
		}
		y += twHeight
	}

	if start == -1 {
		// No visible timelines
		return nil
	}

	if end == -1 {
		end = len(cv.timelines)
	}

	return cv.timelines[start:end]
}

func (cv *Canvas) layoutTimelines(win *theme.Window, gtx layout.Context) (layout.Dimensions, []*TimelineWidget) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	// OPT(dh): at least use binary search to find the range of timelines we need to draw. now that timeline heights
	// aren't constant anymore, however, this is more complicated.
	start := -1
	end := -1
	y := -cv.y

	for _, tw := range cv.prevFrame.displayedTws {
		tw.displayed = false
	}
	for i, tw := range cv.timelines {
		// Don't draw timelines that would be fully hidden, but do draw partially hidden ones
		twHeight := tw.Height(gtx)
		if y < -twHeight {
			y += twHeight
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
		topBorder := i > 0 && cv.timelines[i-1].hovered
		tw.Layout(win, gtx, cv.timeline.displayAllLabels, cv.timeline.compact, topBorder, &cv.trackSpanLabels)
		stack.Pop()

		y += twHeight

		if tw.LabelClicked() {
			if g, ok := tw.item.(*ptrace.Goroutine); ok {
				cv.clickedGoroutineTimelines = append(cv.clickedGoroutineTimelines, g)
			}
		}
	}
	for _, tw := range cv.prevFrame.displayedTws {
		if !tw.displayed {
			// The timeline was displayed last frame but wasn't this frame -> notify it that it is no longer visible so
			// that it can release temporary resources.
			tw.notifyHidden()
		}
	}

	var out []*TimelineWidget
	if start != -1 {
		out = cv.timelines[start : end+1]
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}, out
}

type Axis struct {
	theme *theme.Theme
	cv    *Canvas

	ticksOps reusableOps

	prevFrame struct {
		ops    reusableOps
		call   op.CallOp
		labels []string
		dims   layout.Dimensions
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
	if axis.cv.unchanged() {
		axis.prevFrame.call.Add(gtx.Ops)
		return axis.prevFrame.dims
	} else if axis.cv.prevFrame.nsPerPx == axis.cv.nsPerPx {
		// Panning only changes the first label
		labels = axis.prevFrame.labels
		labels[0] = formatTimestamp(axis.cv.start)
	} else {
		for t := axis.cv.start; t < axis.cv.end; t += trace.Timestamp(tickInterval) {
			if t == axis.cv.start {
				labels = append(labels, formatTimestamp(t))
			} else {
				// TODO separate value and unit symbol with a space
				labels = append(labels, fmt.Sprintf("+%s", time.Duration(t-axis.cv.start)))
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
	for t := axis.cv.start; t < axis.cv.end; t += trace.Timestamp(tickInterval) {
		start := axis.cv.tsToPx(t) - tickWidth/2
		end := axis.cv.tsToPx(t) + tickWidth/2
		rect := FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := axis.cv.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) - tickWidth/2
			smallEnd := axis.cv.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) + tickWidth/2
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

		if t == axis.cv.start {
			label := labels[i]
			stack := op.Offset(image.Pt(0, int(tickHeight))).Push(gtx.Ops)
			dims := mywidget.TextLine{Color: colors[colorTickLabel]}.Layout(gtx, axis.theme.Shaper, text.Font{}, axis.theme.TextSize, label)
			if dims.Size.Y > labelHeight {
				labelHeight = dims.Size.Y
			}
			prevLabelEnd = float32(dims.Size.X)
			stack.Pop()
		} else {
			macro := op.Record(gtx.Ops)
			// TODO separate value and unit symbol with a space
			label := labels[i]
			dims := mywidget.TextLine{Color: colors[colorTickLabel]}.Layout(gtx, axis.theme.Shaper, text.Font{}, axis.theme.TextSize, label)
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

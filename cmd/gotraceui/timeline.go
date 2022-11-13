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
	mywidget "honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
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

	spanBorderWidthDp unit.Dp = 1
)

const animateLength = 250 * time.Millisecond

// TODO(dh): is there any point in making this configurable?
const maxLocationHistoryEntries = 1024

type LocationHistoryEntry struct {
	start trace.Timestamp
	end   trace.Timestamp
	y     int
}

type Timeline struct {
	trace *Trace

	debugWindow *DebugWindow

	clickedGoroutineActivities []*Goroutine

	// The region of the timeline that we're displaying, measured in nanoseconds
	start trace.Timestamp
	end   trace.Timestamp
	// Imagine we're drawing all activities onto an infinitely long canvas. Timeline.y specifies the y of that infinite
	// canvas that the activity section's y == 0 is displaying.
	y            int
	cachedHeight int

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
	// All activities. Index 0 and 1 are the GC and STW timelines, followed by processors and goroutines.
	activities []*ActivityWidget
	scrollbar  widget.Scrollbar
	axis       Axis

	// State for dragging the timeline
	drag struct {
		clickAt f32.Point
		active  bool
		start   trace.Timestamp
		end     trace.Timestamp
		startY  int
	}

	// State for zooming to a selection
	zoomSelection struct {
		active  bool
		clickAt f32.Point
	}

	// Frame-local state set by Layout and read by various helpers
	nsPerPx float32

	activity struct {
		displayAllLabels    bool
		compact             bool
		displaySampleTracks bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays showGCOverlays

		hoveredSpans MergedSpans
		cursorPos    f32.Point
	}

	contextMenu struct {
		spans                       MergedSpans
		zoom                        theme.MenuItem
		scrollToGoroutine           theme.MenuItem
		scrollToProcessor           theme.MenuItem
		scrollToUnblockingGoroutine theme.MenuItem
	}

	// prevFrame records the timeline's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the timeline hasn't changed.
	prevFrame struct {
		start               trace.Timestamp
		end                 trace.Timestamp
		y                   int
		nsPerPx             float32
		compact             bool
		displaySampleTracks bool
		displayedAws        []*ActivityWidget
		hoveredSpans        MergedSpans
	}
}

func NewTimeline(th *theme.Theme) Timeline {
	tl := Timeline{}
	tl.contextMenu.zoom = theme.MenuItem{Shortcut: "Ctrl+MMB", Label: PlainLabel("Zoom")}
	tl.contextMenu.scrollToGoroutine = theme.MenuItem{Label: PlainLabel("Scroll to goroutine")}
	tl.contextMenu.scrollToProcessor = theme.MenuItem{Label: PlainLabel("Scroll to processor")}
	tl.contextMenu.scrollToUnblockingGoroutine = theme.MenuItem{Label: PlainLabel("Scroll to unblocking goroutine")}
	return tl
}

func (tl *Timeline) rememberLocation() {
	e := LocationHistoryEntry{
		start: tl.start,
		end:   tl.end,
		y:     tl.y,
	}
	if o, ok := tl.peekLocationHistory(); ok && o == e {
		// don't record duplicate locations
		return
	}
	if len(tl.locationHistory) == maxLocationHistoryEntries {
		copy(tl.locationHistory, tl.locationHistory[1:])
		tl.locationHistory[len(tl.locationHistory)-1] = e
	} else {
		tl.locationHistory = append(tl.locationHistory, e)
	}
}

func (tl *Timeline) navigateToChecks(start, end trace.Timestamp, y int) bool {
	if start == tl.start && end == tl.end && y == tl.y {
		// We're already there, do nothing. In particular, don't push to the location history.
		return false
	}

	if start == end {
		// Cannot zoom to a zero width area
		return false
	}

	return true
}

func (tl *Timeline) cancelNavigation() {
	tl.animateTo.animating = false
}

// navigateTo modifes the timeline's start, end and y values, recording the previous location in the undo stack.
// navigateTo rejects invalid operations, like setting start = end.
func (tl *Timeline) navigateTo(gtx layout.Context, start, end trace.Timestamp, y int) {
	if !tl.navigateToChecks(start, end, y) {
		return
	}

	tl.rememberLocation()
	tl.navigateToImpl(gtx, start, end, y)
}

func (tl *Timeline) navigateToNoHistory(gtx layout.Context, start, end trace.Timestamp, y int) {
	if !tl.navigateToChecks(start, end, y) {
		return
	}

	tl.navigateToImpl(gtx, start, end, y)
}

func (tl *Timeline) navigateToImpl(gtx layout.Context, start, end trace.Timestamp, y int) {
	tl.animateTo.animating = true

	tl.animateTo.initialStart = tl.start
	tl.animateTo.initialEnd = tl.end
	tl.animateTo.initialY = tl.y

	tl.animateTo.targetStart = start
	tl.animateTo.targetEnd = end
	tl.animateTo.targetY = y

	tl.animateTo.startedAt = gtx.Now
	op.InvalidateOp{}.Add(gtx.Ops)
}

func (tl *Timeline) peekLocationHistory() (LocationHistoryEntry, bool) {
	if len(tl.locationHistory) == 0 {
		return LocationHistoryEntry{}, false
	}
	return tl.locationHistory[len(tl.locationHistory)-1], true
}

func (tl *Timeline) popLocationHistory() (LocationHistoryEntry, bool) {
	// XXX support redo

	if len(tl.locationHistory) == 0 {
		return LocationHistoryEntry{}, false
	}
	n := len(tl.locationHistory) - 1
	e := tl.locationHistory[n]
	tl.locationHistory = tl.locationHistory[:n]
	return e, true
}

func (tl *Timeline) unchanged() bool {
	if disableCaching {
		return false
	}

	return tl.prevFrame.start == tl.start &&
		tl.prevFrame.end == tl.end &&
		tl.prevFrame.nsPerPx == tl.nsPerPx &&
		tl.prevFrame.y == tl.y &&
		tl.prevFrame.compact == tl.activity.compact &&
		tl.prevFrame.displaySampleTracks == tl.activity.displaySampleTracks
}

func (tl *Timeline) startZoomSelection(pos f32.Point) {
	tl.zoomSelection.active = true
	tl.zoomSelection.clickAt = pos
}

func (tl *Timeline) abortZoomSelection() {
	tl.zoomSelection.active = false
}

func (tl *Timeline) endZoomSelection(win *theme.Window, gtx layout.Context, pos f32.Point) {
	tl.zoomSelection.active = false
	one := tl.zoomSelection.clickAt.X
	two := pos.X

	startPx := min(one, two)
	endPx := max(one, two)

	if startPx < 0 {
		startPx = 0
	}
	if limit := float32(tl.VisibleWidth(win, gtx)); endPx > limit {
		endPx = limit
	}

	start := tl.pxToTs(startPx)
	end := tl.pxToTs(endPx)
	if start == end {
		// Cannot zoom to a zero width area
		return
	}

	tl.navigateTo(gtx, start, end, tl.y)
}

func (tl *Timeline) startDrag(pos f32.Point) {
	tl.cancelNavigation()
	tl.rememberLocation()

	tl.drag.clickAt = pos
	tl.drag.active = true
	tl.drag.start = tl.start
	tl.drag.end = tl.end
	tl.drag.startY = tl.y
}

func (tl *Timeline) endDrag() {
	tl.drag.active = false
}

func (tl *Timeline) dragTo(gtx layout.Context, pos f32.Point) {
	td := time.Duration(round32(tl.nsPerPx * (tl.drag.clickAt.X - pos.X)))
	tl.start = tl.drag.start + trace.Timestamp(td)
	tl.end = tl.drag.end + trace.Timestamp(td)

	yd := int(round32(tl.drag.clickAt.Y - pos.Y))
	tl.y = tl.drag.startY + yd
	if tl.y < 0 {
		tl.y = 0
	}
	// XXX don't allow dragging tl.Y beyond the end
}

func (tl *Timeline) zoom(gtx layout.Context, ticks float32, at f32.Point) {
	// TODO(dh): implement location history for zooming. We shouldn't record one entry per call to zoom, and instead
	// only record on calls that weren't immediately preceeded by other calls to zoom.

	// FIXME(dh): repeatedly zooming in and out doesn't cancel each other out. Fix that.
	if ticks < 0 {
		// Scrolling up, into the screen, zooming in
		ratio := at.X / float32(gtx.Constraints.Max.X)
		ds := time.Duration(tl.nsPerPx * 100 * ratio)
		de := time.Duration(tl.nsPerPx * 100 * (1 - ratio))
		tl.start += trace.Timestamp(ds)
		tl.end -= trace.Timestamp(de)
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := at.X / float32(gtx.Constraints.Max.X)
		ds := trace.Timestamp(tl.nsPerPx * 100 * ratio)
		de := trace.Timestamp(tl.nsPerPx * 100 * (1 - ratio))

		// Make sure the user can always zoom out
		if ds < 1 {
			ds = 1
		}
		if de < 1 {
			de = 1
		}

		start := tl.start - ds
		end := tl.end + de

		// Limit timeline to roughly one day. There's rno reason to zoom out this far, and zooming out further will lead
		// to edge cases and eventually overflow.
		if time.Duration(end-start) < 24*time.Hour {
			tl.start = start
			tl.end = end
		}
	}

	if tl.start > tl.end {
		tl.start = tl.end - 1
	}
}

func (tl *Timeline) visibleSpans(spans Spans) Spans {
	// Visible spans have to end after tl.Start and begin before tl.End
	start := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.end > tl.start
	})
	if start == len(spans) {
		return nil
	}
	end := sort.Search(len(spans), func(i int) bool {
		s := spans[i]
		return s.start >= tl.end
	})

	return spans[start:end]
}

//gcassert:inline
func (tl *Timeline) tsToPx(t trace.Timestamp) float32 {
	return float32(t-tl.start) / tl.nsPerPx
}

//gcassert:inline
func (tl *Timeline) pxToTs(px float32) trace.Timestamp {
	return trace.Timestamp(round32(px*tl.nsPerPx + float32(tl.start)))
}

func (tl *Timeline) ZoomToFitCurrentView(gtx layout.Context) {
	tr := tl.trace
	var first, last trace.Timestamp = -1, -1
	for _, aw := range tl.visibleActivities(gtx) {
		for _, track := range aw.tracks {
			if len(track.spans) == 0 || (track.kind == ActivityWidgetTrackSampled && !tl.activity.displaySampleTracks) {
				continue
			}

			if t := track.spans.Start(tr); t < first || first == -1 {
				first = t
			}
			if t := track.spans.End(); t > last {
				last = t
			}
		}
	}
	if first != -1 && last == -1 {
		panic("unreachable")
	}

	tl.navigateTo(gtx, first, last, tl.y)
}

func (tl *Timeline) activityY(gtx layout.Context, act any) int {
	// OPT(dh): don't be O(n)
	off := 0
	for _, aw := range tl.activities {
		if act == aw.item {
			// TODO(dh): show goroutine at center of window, not the top
			return off
		}
		off += aw.Height(gtx) + gtx.Dp(activityGapDp)
	}
	panic("unreachable")
}

func (tl *Timeline) scrollToActivity(gtx layout.Context, act any) {
	off := tl.activityY(gtx, act)
	tl.navigateTo(gtx, tl.start, tl.end, off)
}

// The width in pixels of the visible portion of the timeline, i.e. the part that isn't occupied by the scrollbar.
func (tl *Timeline) VisibleWidth(win *theme.Window, gtx layout.Context) int {
	sbWidth := gtx.Dp(theme.Scrollbar(win.Theme, &tl.scrollbar).Width())
	return gtx.Constraints.Max.X - sbWidth
}

func easeOutQuart(progress float64) float64 {
	return 1 - math.Pow(1-progress, 4)
}

func easeInQuart(progress float64) float64 {
	return progress * progress * progress * progress
}

// height returns the sum of the heights of all visible activities.
func (tl *Timeline) height(gtx layout.Context) int {
	if tl.prevFrame.compact == tl.activity.compact &&
		tl.prevFrame.displaySampleTracks == tl.activity.displaySampleTracks &&
		tl.cachedHeight != 0 {
		return tl.cachedHeight
	}

	var total int
	activityGap := gtx.Dp(activityGapDp)
	for _, aw := range tl.activities {
		total += aw.Height(gtx)
		total += activityGap
	}
	tl.cachedHeight = total
	return total
}

func (tl *Timeline) ToggleSampleTracks() {
	tl.activity.displaySampleTracks = !tl.activity.displaySampleTracks
}

func (tl *Timeline) UndoNavigation(gtx layout.Context) {
	if e, ok := tl.popLocationHistory(); ok {
		tl.navigateToNoHistory(gtx, e.start, e.end, e.y)
	}
}

func (tl *Timeline) ScrollToTop(gtx layout.Context) {
	tl.navigateTo(gtx, tl.start, tl.end, 0)
}

func (tl *Timeline) JumpToBeginning(gtx layout.Context) {
	d := tl.end - tl.start
	tl.navigateTo(gtx, 0, d, tl.y)
}

func (tl *Timeline) ToggleCompactDisplay() {
	// FIXME(dh): adjust tl.Y so that the top visible goroutine stays the same
	tl.activity.compact = !tl.activity.compact
}

func (tl *Timeline) ToggleActivityLabels() {
	tl.activity.displayAllLabels = !tl.activity.displayAllLabels
}

func (tl *Timeline) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Timeline.Layout").End()
	tr := tl.trace

	if tl.animateTo.animating {
		// XXX animation really makes it obvious that our span merging algorithm is unstable

		dt := gtx.Now.Sub(tl.animateTo.startedAt)
		if dt >= animateLength {
			tl.start = tl.animateTo.targetStart
			tl.end = tl.animateTo.targetEnd
			tl.y = tl.animateTo.targetY

			tl.debugWindow.animationProgress.addValue(gtx.Now, 1)
			tl.debugWindow.animationRatio.addValue(gtx.Now, 1)
			tl.animateTo.animating = false
		} else {
			timeRatio := float64(dt) / float64(animateLength)
			var r float64

			{
				initialStart := float64(tl.animateTo.initialStart)
				initialEnd := float64(tl.animateTo.initialEnd)
				targetStart := float64(tl.animateTo.targetStart)
				targetEnd := float64(tl.animateTo.targetEnd)

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

				tl.debugWindow.animationProgress.addValue(gtx.Now, timeRatio)
				tl.debugWindow.animationRatio.addValue(gtx.Now, r)

				curStart := initialStart + (targetStart-initialStart)*r
				curEnd := initialEnd + (targetEnd-initialEnd)*r

				tl.start = trace.Timestamp(curStart)
				tl.end = trace.Timestamp(curEnd)
			}

			// XXX this looks bad when we panned in both X and Y, because the two animations don't run at the same speed
			// if the distances are different.

			{
				initialY := float64(tl.animateTo.initialY)
				targetY := float64(tl.animateTo.targetY)

				tl.y = int(initialY + (targetY-initialY)*r)
			}

			op.InvalidateOp{}.Add(gtx.Ops)
		}
	}

	for _, ev := range gtx.Events(tl) {
		switch ev := ev.(type) {
		case key.Event:
			if ev.State == key.Press {
				switch ev.Name {
				case key.NameHome:
					switch {
					case ev.Modifiers.Contain(key.ModCtrl):
						tl.ZoomToFitCurrentView(gtx)
					case ev.Modifiers.Contain(key.ModShift):
						tl.JumpToBeginning(gtx)
					case ev.Modifiers == 0:
						tl.ScrollToTop(gtx)
					}

				case "S":
					tl.ToggleSampleTracks()

				case "Z":
					if ev.Modifiers.Contain(key.ModCtrl) {
						tl.UndoNavigation(gtx)
					}

				case "X":
					tl.ToggleActivityLabels()

				case "C":
					tl.ToggleCompactDisplay()

				case "T":
					tl.activity.showTooltips = (tl.activity.showTooltips + 1) % (showTooltipsNone + 1)
					var s string
					switch tl.activity.showTooltips {
					case showTooltipsBoth:
						s = "Showing all tooltips"
					case showTooltipsSpans:
						s = "Showing span tooltips only"
					case showTooltipsNone:
						s = "Showing no tooltips"
					}
					win.ShowNotification(gtx, s)

				case "O":
					tl.activity.showGCOverlays = (tl.activity.showGCOverlays + 1) % (showGCOverlaysBoth + 1)
					var s string
					switch tl.activity.showGCOverlays {
					case showGCOverlaysBoth:
						s = "Showing STW and GC overlays"
					case showGCOverlaysSTW:
						s = "Showing STW overlays"
					case showGCOverlaysNone:
						s = "Showing no overlays"
					}
					win.ShowNotification(gtx, s)

				case "Space":
					if !tl.drag.active {
						tl.startDrag(tl.activity.cursorPos)
					}

				}
			} else if ev.State == key.Release && ev.Name == "Space" {
				tl.endDrag()
			}
		case pointer.Event:
			tl.activity.cursorPos = ev.Position

			switch ev.Type {
			case pointer.Press:
				if ev.Buttons.Contain(pointer.ButtonTertiary) {
					if ev.Modifiers.Contain(key.ModShift) {
						tl.startZoomSelection(ev.Position)
					} else if ev.Modifiers == 0 {
						tl.startDrag(ev.Position)
					}
				}

			case pointer.Scroll:
				// XXX deal with Gio's asinine "scroll focused area into view" behavior when shrinking windows
				tl.abortZoomSelection()
				if ev.Modifiers.Contain(key.ModCtrl) {
					tl.zoom(gtx, ev.Scroll.Y, ev.Position)
				}

			case pointer.Drag:
				if tl.drag.active {
					tl.dragTo(gtx, ev.Position)
				}

			case pointer.Move:
				if tl.drag.active {
					tl.dragTo(gtx, ev.Position)
				}

			case pointer.Release:
				// For pointer.Release, ev.Buttons contains the buttons still being pressed, not the ones that have been
				// released.
				if !ev.Buttons.Contain(pointer.ButtonTertiary) {
					if tl.drag.active {
						tl.endDrag()
					} else if tl.zoomSelection.active {
						tl.endZoomSelection(win, gtx, ev.Position)
					}
				}
			}
		}
	}

	var axisHeight int

	// Draw axis and activities
	func(gtx layout.Context) {
		gtx.Constraints.Max.X = tl.VisibleWidth(win, gtx)
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		tl.clickedGoroutineActivities = tl.clickedGoroutineActivities[:0]

		if d := tl.scrollbar.ScrollDistance(); d != 0 {
			// TODO(dh): because scroll amounts are relative even when the user clicks on a specific spot on the
			// scrollbar, and because we've already executed this frame's navigation animation step, applying the
			// delta to tl.y can leave it in a different position than where the user clicked.
			//
			// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further

			tl.cancelNavigation()

			totalHeight := tl.height(gtx)
			tl.y += int(round32(d * float32(totalHeight)))
			if tl.y < 0 {
				tl.y = 0
			}
		}

		tl.activity.hoveredSpans = nil
		for _, aw := range tl.prevFrame.displayedAws {
			if spans := aw.NavigatedSpans(); len(spans) > 0 {
				start := spans.Start(tr)
				end := spans.End()
				tl.navigateTo(gtx, start, end, tl.y)
				break
			}
		}
		for _, aw := range tl.prevFrame.displayedAws {
			if spans := aw.HoveredSpans(); len(spans) > 0 {
				tl.activity.hoveredSpans = spans
				break
			}
		}

		if tl.contextMenu.zoom.Clicked() {
			start := tl.contextMenu.spans.Start(tr)
			end := tl.contextMenu.spans.End()
			tl.navigateTo(gtx, start, end, tl.y)
			win.CloseContextMenu()
		}

		if tl.contextMenu.scrollToGoroutine.Clicked() {
			tl.scrollToActivity(gtx, tr.getG(tr.Event((tl.contextMenu.spans[0].event())).G))
			win.CloseContextMenu()
		}
		if tl.contextMenu.scrollToUnblockingGoroutine.Clicked() {
			gid, _ := unblockedByGoroutine(tr, &tl.contextMenu.spans[0])
			tl.scrollToActivity(gtx, tr.getG(gid))
			win.CloseContextMenu()
		}
		if tl.contextMenu.scrollToProcessor.Clicked() {
			tl.scrollToActivity(gtx, tr.getP(tr.Event((tl.contextMenu.spans[0].event())).P))
			win.CloseContextMenu()
		}

		tl.nsPerPx = float32(tl.end-tl.start) / float32(gtx.Constraints.Max.X)
		tl.debugWindow.tlPxPerNs.addValue(gtx.Now, 1.0/float64(tl.nsPerPx))

		if debug {
			if tl.end < tl.start {
				panic("XXX")
			}
		}

		// Set up event handlers
		pointer.InputOp{
			Tag:          tl,
			Types:        pointer.Scroll | pointer.Drag | pointer.Press | pointer.Release | pointer.Move,
			ScrollBounds: image.Rectangle{Min: image.Pt(-1, -1), Max: image.Pt(1, 1)},
			Grab:         tl.drag.active,
		}.Add(gtx.Ops)
		if tl.drag.active {
			pointer.CursorAllScroll.Add(gtx.Ops)
		}
		key.InputOp{Tag: tl, Keys: "Space|Ctrl-Z|C|S|O|T|X|(Shift)-(Ctrl)-" + key.NameHome}.Add(gtx.Ops)
		key.FocusOp{Tag: tl}.Add(gtx.Ops)

		drawRegionOverlays := func(spans Spans, c color.NRGBA, height int) {
			for _, s := range tl.visibleSpans(spans) {
				start := s.start
				end := s.end

				if start < tl.start {
					start = tl.start
				}
				if end > tl.end {
					end = tl.end
				}

				xMin := tl.tsToPx(start)
				xMax := tl.tsToPx(end)
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

		// Draw axis and activities
		layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx, layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			// Draw STW and GC regions
			// TODO(dh): make this be optional
			tickHeight := gtx.Dp(tickHeightDp)
			drawRegionOverlays(tl.trace.gc, colors[colorStateGC], tickHeight)
			drawRegionOverlays(tl.trace.stw, colors[colorStateBlocked], tickHeight)

			dims := tl.axis.Layout(win, gtx)
			axisHeight = dims.Size.Y

			return dims
		}), layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			dims, aws := tl.layoutActivities(win, gtx)
			tl.prevFrame.displayedAws = aws
			return dims
		}))

		// Draw zoom selection
		if tl.zoomSelection.active {
			one := tl.zoomSelection.clickAt.X
			two := tl.activity.cursorPos.X
			rect := FRect{
				Min: f32.Pt(min(one, two), 0),
				Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
			}
			paint.FillShape(gtx.Ops, colors[colorZoomSelection], rect.Op(gtx.Ops))
		}

		// Draw STW and GC overlays
		if tl.activity.showGCOverlays >= showGCOverlaysBoth {
			drawRegionOverlays(tl.trace.gc, rgba(0x9C6FD633), gtx.Constraints.Max.Y)
		}
		if tl.activity.showGCOverlays >= showGCOverlaysSTW {
			drawRegionOverlays(tl.trace.stw, rgba(0xBA414133), gtx.Constraints.Max.Y)
		}

		// Draw cursor
		rect := clip.Rect{
			Min: image.Pt(int(round32(tl.activity.cursorPos.X)), 0),
			Max: image.Pt(int(round32(tl.activity.cursorPos.X+1)), gtx.Constraints.Max.Y),
		}
		paint.FillShape(gtx.Ops, colors[colorCursor], rect.Op())

		tl.prevFrame.start = tl.start
		tl.prevFrame.end = tl.end
		tl.prevFrame.nsPerPx = tl.nsPerPx
		tl.prevFrame.y = tl.y
		tl.prevFrame.compact = tl.activity.compact
		tl.prevFrame.displaySampleTracks = tl.activity.displaySampleTracks
		tl.prevFrame.hoveredSpans = tl.activity.hoveredSpans
	}(gtx)

	// Draw scrollbar
	func(gtx layout.Context) {
		defer op.Offset(image.Pt(tl.VisibleWidth(win, gtx), axisHeight)).Push(gtx.Ops).Pop()
		gtx.Constraints.Max.Y -= axisHeight

		activityGap := gtx.Dp(activityGapDp)

		totalHeight := tl.height(gtx)
		if len(tl.activities) > 0 {
			// Allow scrolling past the last goroutine
			totalHeight += tl.activities[len(tl.activities)-1].Height(gtx) + activityGap
		}

		fraction := float32(gtx.Constraints.Max.Y) / float32(totalHeight)
		offset := float32(tl.y) / float32(totalHeight)
		sb := theme.Scrollbar(win.Theme, &tl.scrollbar)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
	}(gtx)

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (tl *Timeline) visibleActivities(gtx layout.Context) []*ActivityWidget {
	activityGap := gtx.Dp(activityGapDp)

	start := -1
	end := -1
	// OPT(dh): at least use binary search to find the range of activities we need to draw. now that activity heights
	// aren't constant anymore, however, this is more complicated.
	y := -tl.y
	for i, aw := range tl.activities {
		// Don't draw activities that would be fully hidden, but do draw partially hidden ones
		awHeight := aw.Height(gtx)
		if y < -awHeight {
			y += activityGap + awHeight
			continue
		}
		if start == -1 {
			start = i
		}
		if y > gtx.Constraints.Max.Y {
			end = i
			break
		}
		y += activityGap + awHeight
	}

	if start == -1 {
		// No visible activities
		return nil
	}

	if end == -1 {
		end = len(tl.activities)
	}

	return tl.activities[start:end]
}

func (tl *Timeline) layoutActivities(win *theme.Window, gtx layout.Context) (layout.Dimensions, []*ActivityWidget) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	activityGap := gtx.Dp(activityGapDp)

	// OPT(dh): at least use binary search to find the range of activities we need to draw. now that activity heights
	// aren't constant anymore, however, this is more complicated.
	start := -1
	end := -1
	y := -tl.y

	for _, aw := range tl.prevFrame.displayedAws {
		aw.displayed = false
	}
	for i, aw := range tl.activities {
		if aw.LabelClicked() {
			if g, ok := aw.item.(*Goroutine); ok {
				tl.clickedGoroutineActivities = append(tl.clickedGoroutineActivities, g)
			}
		}
		// Don't draw activities that would be fully hidden, but do draw partially hidden ones
		awHeight := aw.Height(gtx)
		if y < -awHeight {
			y += activityGap + awHeight
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
		topBorder := i > 0 && tl.activities[i-1].hovered
		aw.Layout(win, gtx, tl.activity.displayAllLabels, tl.activity.compact, topBorder)
		stack.Pop()

		y += activityGap + awHeight
	}
	for _, aw := range tl.prevFrame.displayedAws {
		if !aw.displayed {
			// The activity was displayed last frame but wasn't this frame -> notify it that it is no longer visible so
			// that it can release temporary resources.
			aw.notifyHidden()
		}
	}

	var out []*ActivityWidget
	if start != -1 {
		out = tl.activities[start : end+1]
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}, out
}

type Axis struct {
	theme *theme.Theme
	tl    *Timeline

	ticksOps reusableOps

	prevFrame struct {
		ops    reusableOps
		call   op.CallOp
		labels []string
		dims   layout.Dimensions
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
	if axis.tl.unchanged() {
		axis.prevFrame.call.Add(gtx.Ops)
		return axis.prevFrame.dims
	} else if axis.tl.prevFrame.nsPerPx == axis.tl.nsPerPx {
		// Panning only changes the first label
		labels = axis.prevFrame.labels
		labels[0] = formatTimestamp(axis.tl.start)
	} else {
		for t := axis.tl.start; t < axis.tl.end; t += trace.Timestamp(tickInterval) {
			if t == axis.tl.start {
				labels = append(labels, formatTimestamp(t))
			} else {
				// TODO separate value and unit symbol with a space
				labels = append(labels, fmt.Sprintf("+%s", time.Duration(t-axis.tl.start)))
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
	for t := axis.tl.start; t < axis.tl.end; t += trace.Timestamp(tickInterval) {
		start := axis.tl.tsToPx(t) - tickWidth/2
		end := axis.tl.tsToPx(t) + tickWidth/2
		rect := FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := 1; j <= 9; j++ {
			smallStart := axis.tl.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) - tickWidth/2
			smallEnd := axis.tl.tsToPx(t+trace.Timestamp(tickInterval/10)*trace.Timestamp(j)) + tickWidth/2
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

		if t == axis.tl.start {
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

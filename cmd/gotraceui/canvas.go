package main

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"sort"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/container"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/mysync"
	"honnef.co/go/gotraceui/theme"
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
	exptrace "golang.org/x/exp/trace"
)

type showTooltips uint8

type normalizedY float64

func (cv *Canvas) denormalizeY(gtx layout.Context, y normalizedY) int {
	return int(math.Round(float64(cv.height(gtx)) * float64(y)))
}

func (cv *Canvas) normalizeY(gtx layout.Context, y int) normalizedY {
	return normalizedY(float64(y) / float64(cv.height(gtx)))
}

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

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 6

	spanBorderWidthDp unit.Dp = 1
)

const animateLength = 250 * time.Millisecond

// TODO(dh): is there any point in making this configurable?
const maxLocationHistoryEntries = 1024

type LocationHistoryEntry struct {
	start   exptrace.Time
	nsPerPx float64
	y       normalizedY
}

type canvasAnimation struct {
	start   exptrace.Time
	nsPerPx float64
	y       normalizedY
}

func (ca canvasAnimation) Lerp(end canvasAnimation, r float64) canvasAnimation {
	return canvasAnimation{
		start:   theme.Lerp(ca.start, end.start, r),
		nsPerPx: theme.Lerp(ca.nsPerPx, end.nsPerPx, r),
		y:       theme.Lerp(ca.y, end.y, r),
	}
}

type Canvas struct {
	// TODO(dh): remove trace field and make Canvas agnostic of Go traces
	trace *Trace

	debugWindow *DebugWindow

	clickedTimelines      []*Timeline
	rightClickedTimelines []*Timeline
	clickedSpans          []Items[ptrace.Span]

	// The start of the timeline
	start   exptrace.Time
	nsPerPx float64

	// The width of the canvas, in pixels, updated on each frame
	width int

	// Imagine we're drawing all timelines onto an infinitely long canvas. Canvas.y specifies the y of that infinite
	// canvas that the timeline section's y == 0 is displaying.
	y normalizedY

	// Scratch space used by ActivityWidgetTrack.Layout
	trackSpanLabels []string
	// Scratch space used for planning textures
	scratchTexs  []TextureStack
	scratchDones []chan struct{}

	indicateTimestamp container.Option[exptrace.Time]

	animate theme.Animation[canvasAnimation]

	locationHistory locationHistory
	// All timelines. Index 0 and 1 are the GC and STW timelines, followed by processors and goroutines.
	timelines      []*Timeline
	itemToTimeline map[any]*Timeline
	scrollbar      widget.Scrollbar
	axis           Axis

	memoryGraph    Plot
	goroutineGraph Plot

	// State for dragging the canvas
	drag struct {
		drag    gesture.Drag
		ready   bool
		clickAt f32.Point
		active  bool
		start   exptrace.Time
		end     exptrace.Time
		startY  normalizedY
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
		displayAllLabels   bool
		compact            bool
		displayStackTracks bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays showGCOverlays

		hoveredTimeline *Timeline
		hover           gesture.Hover
	}

	resizeMemoryTimelines component.Resize

	// prevFrame records the canvas's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the canvas hasn't changed.
	prevFrame struct {
		start              exptrace.Time
		y                  normalizedY
		metric             unit.Metric
		nsPerPx            float64
		compact            bool
		displayStackTracks bool
		displayedTls       []*Timeline
		hoveredTimeline    *Timeline
		width              int
		filter             Filter
	}

	cachedCanvasHeight struct {
		compact            bool
		displayStackTracks bool
		metric             unit.Metric
		height             int
	}

	// timelineEnds[i] describes the absolute Y pixel offset where timeline i ends. It is computed by
	// Canvas.computeTimelinePositions
	timelineEnds []int

	timelineWidgetsCache mem.AllocationCache[TimelineWidget]
	trackWidgetsCache    mem.AllocationCache[TrackWidget]

	textures TextureManager
}

func NewCanvasInto(cv *Canvas, dwin *DebugWindow, t *Trace) {
	*cv = Canvas{
		resizeMemoryTimelines: component.Resize{
			Axis:  layout.Vertical,
			Ratio: 0.2,
		},
		axis:           Axis{cv: cv, anchor: AxisAnchorCenter},
		trace:          t,
		debugWindow:    dwin,
		itemToTimeline: make(map[any]*Timeline),
		timelines:      make([]*Timeline, 0, len(t.Goroutines)+len(t.Processors)+len(t.Machines)+2),
		textures: TextureManager{
			rgbas:         mysync.NewMutex(&container.RBTree[comparableTimeDuration, *texture]{AllowDuplicates: true}),
			realizedRGBAs: mysync.NewMutex(container.Set[*texture]{}),
		},
		locationHistory: locationHistory{
			cursor: -1,
		},
	}
	cv.timeline.displayAllLabels = true

	if len(t.GC) != 0 {
		cv.timelines = append(cv.timelines, NewGCTimeline(cv, t, t.GC))
	}
	if len(t.STW) != 0 {
		cv.timelines = append(cv.timelines, NewSTWTimeline(cv, t, t.STW))
	}
}

func (cv *Canvas) End() exptrace.Time {
	return cv.start + exptrace.Time(float64(cv.width)*cv.nsPerPx)
}

func (cv *Canvas) computeTimelinePositions(gtx layout.Context) {
	if len(cv.timelineEnds) == len(cv.timelines) &&
		cv.timeline.compact == cv.prevFrame.compact &&
		cv.timeline.displayStackTracks == cv.prevFrame.displayStackTracks &&
		gtx.Metric == cv.prevFrame.metric {
		return
	}

	cv.timelineEnds = mem.GrowLen(cv.timelineEnds[:0], len(cv.timelines))
	accEnds := 0
	for i, tl := range cv.timelines {
		accEnds += tl.Height(gtx, cv)
		cv.timelineEnds[i] = accEnds
	}
}

// locationHistory is a stack of locations. Popping decrements a cursor, which can be undone until a new
// element gets pushed. The element under the cursor is the current location.
type locationHistory struct {
	items  []LocationHistoryEntry
	cursor int
}

func (h *locationHistory) undo() (LocationHistoryEntry, bool) {
	if h.cursor > 0 {
		h.cursor--
		return h.items[h.cursor], true
	}
	return LocationHistoryEntry{}, false
}

func (h *locationHistory) redo() (LocationHistoryEntry, bool) {
	if h.cursor < len(h.items)-1 {
		h.cursor++
		return h.items[h.cursor], true
	}
	return LocationHistoryEntry{}, false
}

func (h *locationHistory) push(loc LocationHistoryEntry) {
	h.items = h.items[:h.cursor+1]

	if len(h.items) > 0 && h.items[h.cursor] == loc {
		// don't record duplicate locations
		return
	}
	if len(h.items) == maxLocationHistoryEntries {
		copy(h.items, h.items[1:])
		h.items[len(h.items)-1] = loc
	} else {
		h.items = append(h.items, loc)
		h.cursor++
	}
}

func (cv *Canvas) rememberLocation() {
	e := LocationHistoryEntry{
		start:   cv.start,
		nsPerPx: cv.nsPerPx,
		y:       cv.y,
	}
	cv.locationHistory.push(e)
}

func (cv *Canvas) navigateToChecks(start exptrace.Time, nsPerPx float64, y normalizedY) bool {
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
	cv.animate.Cancel()
}

// navigateTo modifes the canvas's start, end and y values, recording the new location in the undo stack.
// navigateTo rejects invalid operations, like setting start = end.
func (cv *Canvas) navigateTo(gtx layout.Context, start exptrace.Time, nsPerPx float64, y normalizedY) {
	if !cv.navigateToChecks(start, nsPerPx, y) {
		return
	}

	cv.navigateToImpl(gtx, start, nsPerPx, y)
	cv.locationHistory.push(LocationHistoryEntry{
		start:   start,
		nsPerPx: nsPerPx,
		y:       y,
	})
}

func (cv *Canvas) navigateToStartAndEnd(gtx layout.Context, start, end exptrace.Time, y normalizedY) {
	nsPerPx := float64(end-start) / float64(cv.width)
	cv.navigateTo(gtx, start, nsPerPx, y)
}

func (cv *Canvas) navigateToNoHistory(gtx layout.Context, start exptrace.Time, nsPerPx float64, y normalizedY) {
	if !cv.navigateToChecks(start, nsPerPx, y) {
		return
	}

	cv.navigateToImpl(gtx, start, nsPerPx, y)
}

func (cv *Canvas) navigateToImpl(gtx layout.Context, start exptrace.Time, nsPerPx float64, y normalizedY) {
	cv.animate.Start(gtx, canvasAnimation{cv.start, cv.nsPerPx, cv.y}, canvasAnimation{start, nsPerPx, y}, animateLength, nil)
}

func (cv *Canvas) unchanged(gtx layout.Context) bool {
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
		cv.prevFrame.metric == gtx.Metric
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

	cv.drag.clickAt = pos
	cv.drag.active = true
	cv.drag.start = cv.start
	cv.drag.startY = cv.y
}

func (cv *Canvas) endDrag() {
	cv.drag.active = false
	cv.rememberLocation()
}

func (cv *Canvas) dragTo(gtx layout.Context, pos f32.Point) {
	td := time.Duration(math.Round(cv.nsPerPx * float64(cv.drag.clickAt.X-pos.X)))
	cv.start = cv.drag.start + exptrace.Time(td)

	yd := int(round32(cv.drag.clickAt.Y - pos.Y))
	cv.y = cv.drag.startY + cv.normalizeY(gtx, yd)
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
		ds := exptrace.Time(cv.nsPerPx * 100 * ratio)
		de := exptrace.Time(cv.nsPerPx * 100 * (1 - ratio))

		start := cv.start + ds
		end := cv.End() - de
		if start == end {
			// nsPerPx must never be 0.
			end++
		}
		cv.start = start
		cv.nsPerPx = float64(end-start) / float64(cv.width)
	} else if ticks > 0 {
		// Scrolling down, out of the screen, zooming out
		ratio := float64(at.X) / float64(gtx.Constraints.Max.X)
		ds := exptrace.Time(cv.nsPerPx * 100 * ratio)
		de := exptrace.Time(cv.nsPerPx * 100 * (1 - ratio))

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

func (cv *Canvas) visibleSpans(spans Items[ptrace.Span]) Items[ptrace.Span] {
	// Visible spans have to end after cv.Start and begin before cv.End
	start := sort.Search(spans.Len(), func(i int) bool {
		return spans.AtPtr(i).End > cv.start
	})
	if start == spans.Len() {
		return NoItems[ptrace.Span]{}
	}
	end := sort.Search(spans.Len(), func(i int) bool {
		return spans.AtPtr(i).Start >= cv.End()
	})

	return spans.Slice(start, end)
}

//gcassert:inline
func (cv *Canvas) tsToPx(t exptrace.Time) float32 {
	return float32(float64(t-cv.start) / float64(cv.nsPerPx))
}

//gcassert:inline
func (cv *Canvas) pxToTs(px float32) exptrace.Time {
	return exptrace.Time(math.Round(float64(px)*float64(cv.nsPerPx) + float64(cv.start)))
}

func (cv *Canvas) ZoomToFitCurrentView(gtx layout.Context) {
	var first, last exptrace.Time = -1, -1
	start, end := cv.visibleTimelines(gtx)
	for _, tl := range cv.timelines[start:end] {
		for _, track := range tl.tracks {
			if track.kind == TrackKindStack && !cv.timeline.displayStackTracks {
				continue
			}

			if t := track.Start; t < first || first == -1 {
				first = t
			}
			if t := track.End; t > last {
				last = t
			}
		}
	}
	if first != -1 && last == -1 {
		panic("unreachable")
	}

	cv.navigateToStartAndEnd(gtx, first, last, cv.y)
}

func (cv *Canvas) timelineY(gtx layout.Context, dst *Timeline) normalizedY {
	// OPT(dh): don't be O(n)
	off := 0
	for _, tl := range cv.timelines {
		if tl == dst {
			// TODO(dh): show goroutine at center of window, not the top
			return cv.normalizeY(gtx, off)
		}
		off += tl.Height(gtx, cv)
	}
	panic("unreachable")
}

func (cv *Canvas) objectY(gtx layout.Context, act any) normalizedY {
	// OPT(dh): don't be O(n)
	off := 0
	for _, tl := range cv.timelines {
		if act == tl.item {
			// TODO(dh): show goroutine at center of window, not the top
			return cv.normalizeY(gtx, off)
		}
		off += tl.Height(gtx, cv)
	}
	panic("unreachable")
}

func (cv *Canvas) scrollToTimeline(gtx layout.Context, tl *Timeline) {
	off := cv.timelineY(gtx, tl)
	cv.navigateTo(gtx, cv.start, cv.nsPerPx, off)
}

func (cv *Canvas) scrollToObject(gtx layout.Context, act any) {
	off := cv.objectY(gtx, act)
	cv.navigateTo(gtx, cv.start, cv.nsPerPx, off)
}

// The width in pixels of the visible portion of the canvas, i.e. the part that isn't occupied by the scrollbar.
func (cv *Canvas) VisibleWidth(win *theme.Window, gtx layout.Context) int {
	sbWidth := gtx.Dp(theme.Scrollbar(win.Theme, &cv.scrollbar).Width())
	return gtx.Constraints.Max.X - sbWidth
}

// height returns the sum of the heights of all visible timelines.
func (cv *Canvas) height(gtx layout.Context) int {
	cch := &cv.cachedCanvasHeight
	if cch.compact == cv.timeline.compact &&
		cch.displayStackTracks == cv.timeline.displayStackTracks &&
		cch.metric == gtx.Metric &&
		cch.height != 0 {
		return cch.height
	}

	// OPT(dh): reuse slice
	totals, _ := mysync.Map(cv.timelines, 0, nil, func(subitems []*Timeline) (int, error) {
		total := 0
		for _, tl := range subitems {
			total += tl.Height(gtx, cv)
		}
		return total, nil
	})
	var total int
	for _, t := range totals {
		total += t
	}

	cch.compact = cv.timeline.compact
	cch.displayStackTracks = cv.timeline.displayStackTracks
	cch.metric = gtx.Metric
	cch.height = total
	return total
}

func (cv *Canvas) ToggleStackTracks() {
	cv.timeline.displayStackTracks = !cv.timeline.displayStackTracks
}

func (cv *Canvas) UndoNavigation(gtx layout.Context) {
	if e, ok := cv.locationHistory.undo(); ok {
		cv.navigateToNoHistory(gtx, e.start, e.nsPerPx, e.y)
	}
}

func (cv *Canvas) RedoNavigation(gtx layout.Context) {
	if e, ok := cv.locationHistory.redo(); ok {
		cv.navigateToNoHistory(gtx, e.start, e.nsPerPx, e.y)
	}
}

func (cv *Canvas) ScrollToTop(gtx layout.Context) {
	cv.navigateTo(gtx, cv.start, cv.nsPerPx, 0)
}

func (cv *Canvas) JumpToBeginning(gtx layout.Context) {
	cv.navigateTo(gtx, -cv.trace.TimeOffset, cv.nsPerPx, cv.y)
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
	cv.y += cv.normalizeY(gtx, int(round32(dy)))
	if cv.y < 0 {
		cv.y = 0
	}
	// XXX don't allow dragging cv.y beyond end

	cv.start += exptrace.Time(math.Round(float64(dx) * cv.nsPerPx))
}

func (cv *Canvas) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Canvas.Layout").End()

	if win.Frame%compactInterval == 0 {
		cv.textures.Compact()
	}

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
		end := cv.trace.End()
		slack := exptrace.Time(float64(end - -cv.trace.TimeOffset) * 0.05)
		cv.start = -cv.trace.TimeOffset - slack
		cv.nsPerPx = float64((end - -cv.trace.TimeOffset)+2*slack) / float64(cv.width)
		cv.rememberLocation()
	}

	cv.timeline.hover.Update(gtx.Queue)

	if cv.hover.Update(gtx.Queue) {
		cv.pointerAt = cv.hover.Pointer()
	}

	if !cv.animate.Done() {
		initialStart := float64(cv.animate.StartValue.start)
		initialNsPerPx := float64(cv.animate.StartValue.nsPerPx)
		targetStart := float64(cv.animate.EndValue.start)
		targetNsPerPx := float64(cv.animate.EndValue.nsPerPx)

		initialWidth := initialNsPerPx*float64(cv.VisibleWidth(win, gtx)) - initialStart
		targetWidth := targetNsPerPx*float64(cv.VisibleWidth(win, gtx)) - targetStart

		var ease theme.EasingFunction
		if targetWidth <= initialWidth {
			// Zooming in (or not zooming at all)
			ease = theme.EaseOut(4)
		} else {
			// Zooming out
			ease = theme.EaseIn(4)
		}

		cv.animate.Ease = ease

		v := cv.animate.Value(gtx)
		cv.start = v.start
		cv.nsPerPx = v.nsPerPx
		cv.y = v.y
	}

	win.AddShortcut(theme.Shortcut{Name: key.NameHome})
	win.AddShortcut(theme.Shortcut{Name: key.NameHome, Modifiers: key.ModShortcut})
	win.AddShortcut(theme.Shortcut{Name: key.NameHome, Modifiers: key.ModShift})
	win.AddShortcut(theme.Shortcut{Name: "S"})
	win.AddShortcut(theme.Shortcut{Name: "Z", Modifiers: key.ModShortcut})
	win.AddShortcut(theme.Shortcut{Name: "Y", Modifiers: key.ModShortcut})
	win.AddShortcut(theme.Shortcut{Name: "X"})
	win.AddShortcut(theme.Shortcut{Name: "C"})
	win.AddShortcut(theme.Shortcut{Name: "T"})
	win.AddShortcut(theme.Shortcut{Name: "O"})

	for _, s := range win.PressedShortcuts() {
		switch s {
		case theme.Shortcut{Name: key.NameHome, Modifiers: 0}:
			cv.ScrollToTop(gtx)

		case theme.Shortcut{Name: key.NameHome, Modifiers: key.ModShortcut}:
			cv.ZoomToFitCurrentView(gtx)

		case theme.Shortcut{Name: key.NameHome, Modifiers: key.ModShift}:
			cv.JumpToBeginning(gtx)

		case theme.Shortcut{Name: "S"}:
			cv.ToggleStackTracks()
			if h := cv.timeline.hoveredTimeline; h != nil {
				cv.cancelNavigation()
				y := cv.timelineY(gtx, h)
				offset := h.widget.hover.Pointer().Y
				if !cv.timeline.displayStackTracks {
					// We're going from stacks to no stacks. This shrinks the timeline and the cursor might end
					// up on the next timeline. Prevent that.
					if h.widget.hover.Pointer().Y > float32(h.Height(gtx, cv)) {
						offset -= h.widget.hover.Pointer().Y - float32(h.Height(gtx, cv))
					}
				}
				cv.y = y - cv.normalizeY(gtx, int(cv.timeline.hover.Pointer().Y)-int(offset))
			}

		case theme.Shortcut{Name: "Z", Modifiers: key.ModShortcut}:
			cv.UndoNavigation(gtx)

		case theme.Shortcut{Name: "Y", Modifiers: key.ModShortcut}:
			cv.RedoNavigation(gtx)

		case theme.Shortcut{Name: "X"}:
			cv.ToggleTimelineLabels()

		case theme.Shortcut{Name: "C"}:
			cv.ToggleCompactDisplay()
			if h := cv.timeline.hoveredTimeline; h != nil {
				cv.cancelNavigation()
				y := cv.timelineY(gtx, h)

				offset := h.widget.hover.Pointer().Y
				if cv.timeline.compact {
					// We're going from expanded to compact. This reduces the offset from the top of the
					// timeline to the first track to zero.
					offset -= float32(gtx.Dp(timelineLabelHeightDp))
					if h.widget.hover.Pointer().Y < float32(gtx.Dp(timelineLabelHeightDp)) {
						// The cursor is above the first track; adjust the offset so that the cursor lands on the first track
						offset += float32(gtx.Dp(timelineLabelHeightDp)) - h.widget.hover.Pointer().Y
					}
				}

				cv.y = y - cv.normalizeY(gtx, int(cv.timeline.hover.Pointer().Y)-int(offset))
			}

		case theme.Shortcut{Name: "T"}:
			cv.timeline.showTooltips = (cv.timeline.showTooltips + 1) % (showTooltipsNone + 1)
			showTooltipSettingNotification(win, gtx, cv.timeline.showTooltips)

		case theme.Shortcut{Name: "O"}:
			cv.timeline.showGCOverlays = (cv.timeline.showGCOverlays + 1) % (showGCOverlaysBoth + 1)
			showGCOverlaySettingNotification(win, gtx, cv.timeline.showGCOverlays)
		}
	}

	for _, ev := range gtx.Events(cv) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Kind {
			case pointer.Scroll:
				// XXX deal with Gio's asinine "scroll focused area into view" behavior when shrinking windows
				cv.abortZoomSelection()
				switch ev.Modifiers {
				case key.ModShortcut:
					cv.zoom(gtx, ev.Scroll.Y, ev.Position)
				case key.ModShift:
					// Gio swaps X and Y when Shift is pressed, matching the behavior we desire.
					fallthrough
				default:
					cv.scroll(gtx, ev.Scroll.X, ev.Scroll.Y)
				}
			}
		}
	}

	for _, ev := range cv.drag.drag.Update(gtx.Metric, gtx, gesture.Both) {
		switch ev.Kind {
		case pointer.Press:
			switch ev.Modifiers {
			case 0:
				cv.drag.ready = true
			case key.ModShortcut:
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
		cv.hover.Add(gtx.Ops)

		cv.clickedTimelines = cv.clickedTimelines[:0]
		cv.rightClickedTimelines = cv.rightClickedTimelines[:0]

		if d := cv.scrollbar.ScrollDistance(); d != 0 {
			// TODO(dh): because scroll amounts are relative even when the user clicks on a specific spot on the
			// scrollbar, and because we've already executed this frame's navigation animation step, applying the
			// delta to cv.y can leave it in a different position than where the user clicked.
			//
			// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further

			cv.cancelNavigation()

			cv.y += normalizedY(d)
			if cv.y < 0 {
				cv.y = 0
			}
		}

		// Set up event handlers
		pointer.InputOp{
			Tag:          cv,
			ScrollBounds: image.Rectangle{Min: image.Pt(-100, -100), Max: image.Pt(100, 100)},
			Kinds:        pointer.Move | pointer.Scroll,
		}.Add(gtx.Ops)
		if cv.drag.active {
			pointer.CursorAllScroll.Add(gtx.Ops)
		}

		drawRegionOverlays := func(spans Items[ptrace.Span], c color.Oklch, height int) {
			var p clip.Path
			p.Begin(gtx.Ops)
			visible := cv.visibleSpans(spans)
			for i := 0; i < visible.Len(); i++ {
				s := visible.AtPtr(i)
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
			theme.FillShape(win, gtx.Ops, c, clip.Outline{Path: p.End()}.Op())
		}

		// Draw axis, memory graph, timelines, and scrollbar
		layout.Rigids(gtx, layout.Vertical,
			// Axis
			func(gtx layout.Context) layout.Dimensions {
				// Note that even though the axis is wider than the timelines (because timelines have a scrollbar), the
				// mapping of timestamp to pixel position is still correct, because it gets computed earlier, by using
				// Canvas.VisibleWidth.

				// Draw STW and GC regions
				// TODO(dh): make this be optional
				tickHeight := gtx.Dp(tickHeightDp)

				// TODO(dh): make this less brittle. relying on the fact that cv.timelines[0] and [1] are GC and STW
				// respectively is bad.
				sGC := SimpleItems[ptrace.Span, any]{
					items: cv.trace.GC,
					container: ItemContainer{
						Timeline: cv.timelines[0],
						Track:    cv.timelines[0].tracks[0],
					},
					subslice: true,
				}
				sSTW := SimpleItems[ptrace.Span, any]{
					items: cv.trace.STW,
					container: ItemContainer{
						Timeline: cv.timelines[1],
						Track:    cv.timelines[1].tracks[0],
					},
					subslice: true,
				}
				drawRegionOverlays(sGC, colors[colorStateGC], tickHeight)
				drawRegionOverlays(sSTW, colors[colorStateBlocked], tickHeight)

				dims := cv.axis.Layout(win, gtx)

				return dims
			},

			func(gtx layout.Context) layout.Dimensions {
				return theme.Resize(win.Theme, &cv.resizeMemoryTimelines).Layout(win, gtx,
					func(win *theme.Window, gtx layout.Context) layout.Dimensions {
						return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
							layout.Flexed(0.5, func(gtx layout.Context) layout.Dimensions {
								// Memory graph
								defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
								cv.drag.drag.Add(gtx.Ops)

								dims := cv.memoryGraph.Layout(win, gtx, cv)
								return dims
							}),
							layout.Flexed(0.5, func(gtx layout.Context) layout.Dimensions {
								// Goroutine graph
								defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
								cv.drag.drag.Add(gtx.Ops)

								dims := cv.goroutineGraph.Layout(win, gtx, cv)
								return dims
							}),
						)
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
								// OPT(dh): reuse slice
								texs := cv.scratchTexs[:0]
								texs = cv.planTimelines(win, gtx, texs)
								cv.scratchTexs = texs[:0]
								if time.Since(gtx.Now) <= 5*time.Millisecond {
									// Start computing textures in the background and wait up to 1ms for all textures to
									// finish. This avoids showing single frames of placeholders. We only do this if we
									// have enough time to spare to hit our rough goal of 144 fps.

									dones := cv.scratchDones
									if cap(dones) >= len(texs) {
										dones = dones[:len(texs)]
									} else {
										dones = make([]chan struct{}, len(texs))
										cv.scratchDones = dones
									}

									for i, tex := range texs {
										dones[i] = tex.Realize(&cv.textures, cv.trace)
									}

									notReady := false
									for _, ch := range dones {
										if !TryRecv(ch) {
											notReady = true
											break
										}
									}
									if notReady {
										timeout := time.NewTimer(time.Millisecond)
										defer timeout.Stop()
									doneLoop:
										for n := 0; n < len(dones); n++ {
											select {
											case <-dones[n]:
											case <-timeout.C:
												break doneLoop
											}
										}
									}
									clear(dones)
								}
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
								sb := theme.Scrollbar(win.Theme, &cv.scrollbar)
								return sb.Layout(win, gtx, layout.Vertical, float32(cv.y), float32(cv.y)+fraction)
							}),
						)
					},
				)
			},
		)

		// Draw zoom selection
		if cv.zoomSelection.active {
			one := cv.zoomSelection.clickAt.X
			two := cv.pointerAt.X
			rect := clip.FRect{
				Min: f32.Pt(min(one, two), 0),
				Max: f32.Pt(max(one, two), float32(gtx.Constraints.Max.Y)),
			}
			theme.FillShape(win, gtx.Ops, win.Theme.Palette.PrimarySelection, rect.Op(gtx.Ops))
		}

		// Draw STW and GC overlays
		if cv.timeline.showGCOverlays >= showGCOverlaysBoth {
			// TODO(dh): make this less brittle. relying on the fact that cv.timelines[0] and [1] are GC and STW
			// respectively is bad.
			sGC := SimpleItems[ptrace.Span, any]{
				items: cv.trace.GC,
				container: ItemContainer{
					Timeline: cv.timelines[0],
					Track:    cv.timelines[0].tracks[0],
				},
				subslice: true,
			}
			c := colors[colorStateGC]
			c.A = 0.2
			drawRegionOverlays(sGC, c, gtx.Constraints.Max.Y)
		}
		if cv.timeline.showGCOverlays >= showGCOverlaysSTW {
			// TODO(dh): make this less brittle. relying on the fact that cv.timelines[0] and [1] are GC and STW
			// respectively is bad.
			sSTW := SimpleItems[ptrace.Span, any]{
				items: cv.trace.STW,
				container: ItemContainer{
					Timeline: cv.timelines[1],
					Track:    cv.timelines[1].tracks[0],
				},
				subslice: true,
			}
			c := colors[colorStateSTW]
			c.A = 0.2
			drawRegionOverlays(sSTW, c, gtx.Constraints.Max.Y)
		}

		// Draw cursor
		rect := clip.Rect{
			Min: image.Pt(int(round32(cv.pointerAt.X)), 0),
			Max: image.Pt(int(round32(cv.pointerAt.X+1)), gtx.Constraints.Max.Y),
		}
		theme.FillShape(win, gtx.Ops, win.Theme.Palette.Foreground, rect.Op())

		// cv.indicateTimestamp is set when the user hovers over a timestamp link. We indicate the destination of the
		// link by drawing a cursor, or by highlighting the left or right edge of the canvas if the timestamp isn't
		// visible.
		if hts, ok := cv.indicateTimestamp.Get(); ok {
			px := cv.tsToPx(hts)
			if px < 0 {
				stack := clip.Rect{
					Min: image.Pt(int(round32(0)), 0),
					Max: image.Pt(int(round32(55)), gtx.Constraints.Max.Y),
				}.Push(gtx.Ops)
				c1 := win.Theme.Palette.NavigationLink
				c2 := c1
				c2.A = 0
				paint.LinearGradientOp{
					Stop1:  f32.Pt(-5, 0),
					Color1: win.ConvertColor(c1),
					Stop2:  f32.Pt(55, 0),
					Color2: win.ConvertColor(c2),
				}.Add(gtx.Ops)
				paint.PaintOp{}.Add(gtx.Ops)
				stack.Pop()
			} else if px >= float32(gtx.Constraints.Max.X) {
				stack := clip.Rect{
					Min: image.Pt(int(gtx.Constraints.Max.X-55), 0),
					Max: image.Pt(int(gtx.Constraints.Max.X), gtx.Constraints.Max.Y),
				}.Push(gtx.Ops)
				c1 := win.Theme.Palette.NavigationLink
				c2 := c1
				c2.A = 0
				paint.LinearGradientOp{
					Stop1:  f32.Pt(float32(gtx.Constraints.Max.X-55), 0),
					Color1: win.ConvertColor(c2),
					Stop2:  f32.Pt(float32(gtx.Constraints.Max.X+5), 0),
					Color2: win.ConvertColor(c1),
				}.Add(gtx.Ops)
				paint.PaintOp{}.Add(gtx.Ops)
				stack.Pop()
			} else {
				rect := clip.Rect{
					Min: image.Pt(int(round32(px)), 0),
					Max: image.Pt(int(round32(px+1)), gtx.Constraints.Max.Y),
				}
				theme.FillShape(win, gtx.Ops, win.Theme.Palette.NavigationLink, rect.Op())
			}
		}

	}(gtx)

	cv.prevFrame.start = cv.start
	cv.prevFrame.nsPerPx = cv.nsPerPx
	cv.prevFrame.width = cv.width
	cv.prevFrame.y = cv.y
	cv.prevFrame.compact = cv.timeline.compact
	cv.prevFrame.displayStackTracks = cv.timeline.displayStackTracks
	cv.prevFrame.hoveredTimeline = cv.timeline.hoveredTimeline
	cv.prevFrame.filter = cv.timeline.filter
	cv.prevFrame.metric = gtx.Metric

	cv.clickedSpans = cv.clickedSpans[:0]
	cv.timeline.hoveredTimeline = nil
	for _, tl := range cv.prevFrame.displayedTls {
		if clicked := tl.widget.ClickedSpans(); clicked.Len() > 0 {
			cv.clickedSpans = append(cv.clickedSpans, clicked)
		}
		if tl.widget.Hovered(gtx) {
			cv.timeline.hoveredTimeline = tl
			break
		}
	}

	for _, tl := range cv.prevFrame.displayedTls {
		if ts, ok := tl.widget.NavigatedTimeSpan().Get(); ok {
			cv.navigateToStartAndEnd(gtx, ts.Start, ts.End, cv.y)
			// FIXME(dh): canvas does event handling _after_ layout, so we need a second frame
			op.InvalidateOp{}.Add(gtx.Ops)
			break
		}
	}

	return layout.Dimensions{
		Size: gtx.Constraints.Max,
	}
}

func (cv *Canvas) visibleTimelines(gtx layout.Context) (start, end int) {
	cvy := cv.denormalizeY(gtx, cv.y)
	// start at first timeline that ends within or after the visible range
	// end at first timeline that starts after the visible range
	start = sort.Search(len(cv.timelines), func(i int) bool {
		return cv.timelineEnds[i]-cvy >= 0
	})
	end = sort.Search(len(cv.timelines), func(i int) bool {
		start := 0
		if i != 0 {
			start = cv.timelineEnds[i-1]
		}
		return start-cvy >= gtx.Constraints.Max.Y
	})
	return start, end
}

func (cv *Canvas) planTimelines(win *theme.Window, gtx layout.Context, texs []TextureStack) []TextureStack {
	start, end := cv.visibleTimelines(gtx)

	cvy := cv.denormalizeY(gtx, cv.y)
	y := -cvy
	if start < len(cv.timelines) && start > 0 {
		y = cv.timelineEnds[start-1] - cvy
	}

	for i := start; i < end; i++ {
		tl := cv.timelines[i]
		texs = tl.Plan(win, texs)
		y += tl.Height(gtx, cv)
	}
	return texs
}

func (cv *Canvas) layoutTimelines(win *theme.Window, gtx layout.Context) (layout.Dimensions, []*Timeline) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	for _, tl := range cv.prevFrame.displayedTls {
		tl.displayed = false
	}

	start, end := cv.visibleTimelines(gtx)

	cvy := cv.denormalizeY(gtx, cv.y)
	y := -cvy
	if start < len(cv.timelines) && start > 0 {
		y = cv.timelineEnds[start-1] - cvy
	}

	for i := start; i < end; i++ {
		tl := cv.timelines[i]
		stack := op.Offset(image.Pt(0, y)).Push(gtx.Ops)
		topBorder := i > 0 && cv.timelines[i-1].widget.Hovered(gtx)
		tl.Layout(win, gtx, cv, cv.timeline.displayAllLabels, cv.timeline.compact, topBorder, &cv.trackSpanLabels)
		stack.Pop()

		y += tl.Height(gtx, cv)

		if tl.widget.LabelClicked() {
			cv.clickedTimelines = append(cv.clickedTimelines, tl)
		}
		if tl.widget.LabelRightClicked() {
			cv.rightClickedTimelines = append(cv.rightClickedTimelines, tl)
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
	ticksOps mem.ReusableOps

	// the location of the origin in pixels
	position float32
	// where the position of the origin is anchored to
	anchor AxisAnchor

	prevFrame struct {
		ops    mem.ReusableOps
		call   op.CallOp
		dims   layout.Dimensions
		origin AdjustedTime
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

	for _, ev := range axis.click.Update(gtx.Queue) {
		if ev.Kind == gesture.KindPress && ev.Button == pointer.ButtonSecondary {
			win.SetContextMenu(
				[]*theme.MenuItem{
					{
						Label:    PlainLabel("Move origin to the left"),
						Disabled: func() bool { return axis.anchor == AxisAnchorStart },
						Action: func() theme.Action {
							return theme.ExecuteAction(func(gtx layout.Context) {
								axis.anchor = AxisAnchorStart
							})
						},
					},
					{
						Label:    PlainLabel("Move origin to the center"),
						Disabled: func() bool { return axis.anchor == AxisAnchorCenter },
						Action: func() theme.Action {
							return theme.ExecuteAction(func(gtx layout.Context) {
								axis.anchor = AxisAnchorCenter
							})
						},
					},
					{
						Label:    PlainLabel("Move origin to the right"),
						Disabled: func() bool { return axis.anchor == AxisAnchorEnd },
						Action: func() theme.Action {
							return theme.ExecuteAction(func(gtx layout.Context) {
								axis.anchor = AxisAnchorEnd
							})
						},
					},
				},
			)
		}
	}

	for _, ev := range axis.drag.Update(gtx.Metric, gtx.Queue, gesture.Horizontal) {
		if ev.Kind == pointer.Press || ev.Kind == pointer.Drag {
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

	min := axis.cv.trace.AdjustedTime(axis.cv.start)
	max := axis.cv.trace.AdjustedTime(axis.cv.End())

	var origin AdjustedTime
	switch axis.anchor {
	case AxisAnchorNone:
		origin = axis.cv.trace.AdjustedTime(axis.cv.pxToTs(axis.position))
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

	if axis.cv.unchanged(gtx) && axis.prevFrame.origin == origin {
		axis.prevFrame.call.Add(gtx.Ops)
		debugCaching(win, gtx)
		return axis.prevFrame.dims
	}

	origOps := gtx.Ops
	gtx.Ops = axis.prevFrame.ops.Get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		axis.prevFrame.call = call
		axis.prevFrame.dims = dims
		axis.prevFrame.origin = origin
	}()

	var ticksPath clip.Path
	ticksPath.Begin(axis.ticksOps.Get())

	// prevLabelEnd tracks where the previous tick label ended, so that we don't draw overlapping labels
	var originLabelExtents image.Rectangle
	var prevLabelEnd float32

	drawTick := func(at AdjustedTime, label string, forward bool) {
		t := axis.cv.trace.UnadjustedTime(at)
		add := func(a, b exptrace.Time) exptrace.Time {
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
			smallStart := axis.cv.tsToPx(add(t, exptrace.Time(tickInterval/10)*exptrace.Time(j))) - tickWidth/2
			smallEnd := axis.cv.tsToPx(add(t, exptrace.Time(tickInterval/10)*exptrace.Time(j))) + tickWidth/2
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

		rec := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			return theme.LineLabel(win.Theme, label).Layout(win, gtx)
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
	drawOrigin := func(at AdjustedTime) {
		t := axis.cv.trace.UnadjustedTime(at)
		start := axis.cv.tsToPx(t) - tickWidth/2
		end := axis.cv.tsToPx(t) + tickWidth/2
		rect := clip.FRect{
			Min: f32.Pt(start, 0),
			Max: f32.Pt(end, tickHeight),
		}
		rect.IntoPath(&ticksPath)

		for j := -9; j <= 9; j++ {
			smallStart := axis.cv.tsToPx(t+exptrace.Time(tickInterval/10)*exptrace.Time(j)) - tickWidth/2
			smallEnd := axis.cv.tsToPx(t+exptrace.Time(tickInterval/10)*exptrace.Time(j)) + tickWidth/2
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

		rec := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
			label := formatTimestamp(nil, at)
			ls := theme.LineLabel(win.Theme, label)
			ls.Font = font.Font{Weight: font.Bold}
			return ls.Layout(win, gtx)
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
	for t := origin + AdjustedTime(tickInterval); t < max; t += AdjustedTime(tickInterval) {
		label := fmt.Sprintf("+%s", time.Duration(t-origin))
		drawTick(t, label, true)
	}

	prevLabelEnd = float32(originLabelExtents.Min.X)
	for t := origin - AdjustedTime(tickInterval); t >= min; t -= AdjustedTime(tickInterval) {
		label := fmt.Sprintf("-%s", time.Duration(origin-t))
		drawTick(t, label, false)
	}

	theme.FillShape(win, gtx.Ops, win.Theme.Palette.Foreground, clip.Outline{Path: ticksPath.End()}.Op())

	labelHeight := originLabelExtents.Max.Y
	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, int(tickHeight+0.5)+labelHeight)}
}

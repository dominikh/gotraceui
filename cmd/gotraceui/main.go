package main

import (
	"flag"
	"fmt"
	"image"
	"image/color"
	"log"
	"math"
	"os"
	"runtime/pprof"
	"sort"
	"strconv"
	"strings"
	"time"

	mylayout "honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	mywidget "honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font/gofont"
	"gioui.org/gesture"
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
	"gioui.org/x/outlay"
	"gioui.org/x/poortext"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
	"golang.org/x/image/math/fixed"
	"golang.org/x/text/message"
)

// A note on Ps
//
// Not all events have a P. For example, when sysmon wakes up the scavenger, it doesn't have a P while unblocking
// goroutines.

/*
   GC notes:
   - The only use of p=1000004 is for GCStart
   - GCDone happens on other procs, probably whichever proc was running the background worker that determined that we're done
   - A similar thing applies to GCSTWStart and GCSTWDone
   - The second GCSTWDone can happen after GCDone

   GC can get started by normal user goroutines, when they allocate and detect that the GC trigger condition has been
   met. The lucky goroutine will be responsible for running the runtime code that stops the world, sets up write
   barriers and so on. It returns to the user's code once the first STW phase is over and the concurrent mark phase has
   started. Since multiple goroutines may allocate in parallel, multiple goroutines might detect the GC trigger and
   attempt to start GC. All but one will block trying to acquire the semaphore, then not start GC once they unblock.

   The GCStart and the first pair of STWStart + STWStop will be sent from that goroutine, but note that these events
   form a timeline separate from normal goroutine events. In particular, seeing STWStart on the goroutine doesn't mean
   that its previous GoStart has been superseded, and we'll not see another GoStart after we see STWStop.

   The second STW phase and GCDone are sent from another goroutine, probably the background mark worker that determined
   that we're done.
*/

// FIXME(dh): in ListWindow, when all items got filtered away and we change the filter so there are items again, no item
//   will be selected, and pressing enter will panic, trying to access index -1
// FIXME(dh): shift+arrow keys selects text in our instances of widget.Editor, but the selected text doesn't look selected
// TODO(dh): disable navigation keybindings such as Home when we're dragging
// TODO(dh): How should resizing the window affect the zoom level? When making the window wider, should it display more
//   time or should it display the same time, stretched to fill the new space? Tracy does the latter.
// TODO(dh): button to zoom so far into a span that it no longer gets merged. this has to work recursively, because if
//   the span splits into more merged spans, those should get unmerged, too.
// XXX how do we have a minimum inactive span of length 0?
// OPT(dh): optimize drawing merged spans with millions of spans
// OPT(dh): optimize highlighting hovered goroutine in per-processor view when there are merged spans with lots of children
// TODO(dh): allow jumping from span in per-goroutine view to corresponding goroutine span in per-processor view
// TODO(dh): support exporting an image of the entire trace, at a zoom level that shows all details
// TODO(dh): display parent goroutine in goroutine window
// TODO(dh): clicking on a goroutine in the per-P view should bring up the goroutine window
// TODO(dh): add a dialog with text fields for zooming to a specific time range
// TODO(dh): display different cursor when we're panning
// TODO(dh): display number of spans in goroutine tooltip
// OPT(dh): the goroutine span tooltip should cache the stats. for the bgsweep goroutine in the staticcheck-std trace,
//   rendering the tooltip alone takes ~16ms
// TODO(dh): add tooltip for processor timelines
// TODO(dh): implement popup windows that can be used to customize UI settings. e.g. instead of needing different
//   shortcuts for toggling labels, compact mode, tooltips etc, have one shortcut that opens a menu that allows toggling
//   these features. maybe even use a radial menu? (probably not.)
// TODO(dh): allow computing statistics for a selectable region of time
// TODO(dh): hovering over spans in the goroutine timelines highlights goroutines in the processor timelines. that's a
//   happy accident. however, it doesn't work reliably, because we just look at trace.Event.G for the matching, and for
//   some events, like unblocking, that's the wrong G.
// TODO(dh): use the GC-purple color in the GC and STW timelines
// TODO(dh): toggleable behavior for hovering spans in goroutine timelines. For example, hovering a blocked span could
//   highlight the span that unblocks it (or maybe when hovering the "runnable" span, but same idea). Hovering a running
//   span could highlight all the spans it unblocks.
// TODO(dh): the Event.Stk is meaningless for goroutines that already existed when tracing started, i.e. ones that get a
//   GoWaiting event. The GoCreate event will be caused by starting the trace, and the stack of the event will be that
//   leading up to starting the trace. It will in no way reflect the code that actually, historically, started the
//   goroutine. To avoid confusion, we should remove those stacks altogether.

// TODO(dh): make configurable. 0, 250ms and 500ms would make for good presets.
const animateLength = 250 * time.Millisecond

const (
	// TODO(dh): compute min tick distance based on font size
	minTickDistanceDp      unit.Dp = 20
	tickHeightDp           unit.Dp = 12
	tickWidthDp            unit.Dp = 1
	minTickLabelDistanceDp unit.Dp = 8

	// XXX the label height depends on the font used
	activityLabelHeightDp unit.Dp = 20
	activityTrackHeightDp unit.Dp = 16
	activityGapDp         unit.Dp = 5
	activityTrackGapDp    unit.Dp = 2

	minSpanWidthDp unit.Dp = spanBorderWidthDp*2 + 4

	spanBorderWidthDp unit.Dp = 1
)

// TODO(dh): is there any point in making this configurable?
const maxLocationHistoryEntries = 1024

var (
	cpuprofile       string
	memprofileLoad   string
	memprofileExit   string
	disableCaching   bool
	exitAfterLoading bool
	exitAfterParsing bool
)

var goFonts = gofont.Collection()

var spanStateLabels = [...][]string{
	stateGCDedicated:             {"GC (dedicated)", "D"},
	stateGCIdle:                  {"GC (idle)", "I"},
	stateBlockedCond:             {"sync.Cond"},
	stateBlockedGC:               {"GC assist wait", "W"},
	stateBlockedNet:              {"I/O"},
	stateBlockedRecv:             {"recv"},
	stateBlockedSelect:           {"select"},
	stateBlockedSend:             {"send"},
	stateBlockedSync:             {"sync"},
	stateBlockedSyncOnce:         {"sync.Once"},
	stateBlockedSyncTriggeringGC: {"triggering GC", "T"},
	stateBlockedSyscall:          {"syscall"},
	stateGCMarkAssist:            {"GC mark assist", "M"},
	stateGCSweep:                 {"GC sweep", "S"},
	stateStuck:                   {"stuck"},
	stateLast:                    nil,
}

type EventID uint64

// MergedSpans and Spans have the same functionality. The two different types are used to make APIs easier to read, to
// be able to tell apart functions that operate on multiple spans as if they were individual items and functions that
// treat them as one unit, because they get merged during rendering.

// MergedSpans represents a list of consecutive spans from a shared timeline, which were merged during display.
//
// OPT(dh): we could theoretically save 8 bytes by storing the start and end indices instead of a slice, as merged
// spans have to be consecutive. It would also prevent potential misuse of MergedSpans, e.g. by creating an entirely
// new slice, instead of slicing an existing one. However, a slice is easier to access and iterate over.
type MergedSpans []Span

type reason uint8

var reasonLabels = [256]string{
	reasonNewlyCreated: "newly created",
	reasonGosched:      "called runtime.Gosched",
	reasonTimeSleep:    "called time.Sleep",
	reasonPreempted:    "got preempted",
}

const (
	reasonNone reason = iota
	reasonNewlyCreated
	reasonGosched
	reasonTimeSleep
	reasonPreempted
)

var reasonByEventType = [256]reason{
	trace.EvGoCreate:  reasonNewlyCreated,
	trace.EvGoSched:   reasonGosched,
	trace.EvGoSleep:   reasonTimeSleep,
	trace.EvGoPreempt: reasonPreempted,
}

type reusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *reusableOps) get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
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

type LocationHistoryEntry struct {
	start trace.Timestamp
	end   trace.Timestamp
	y     int
}

type Timeline struct {
	theme *theme.Theme
	trace *Trace

	debugWindow *DebugWindow

	clickedGoroutineActivities []*Goroutine

	// The region of the timeline that we're displaying, measured in nanoseconds
	start trace.Timestamp
	end   trace.Timestamp
	// Imagine we're drawing all activities onto an infinitely long canvas. Timeline.y specifies the y of that infinite
	// canvas that the activity section's y == 0 is displaying.
	y int

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

	gs map[uint64]*Goroutine

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
		displayAllLabels bool
		compact          bool
		// Should tooltips be shown?
		showTooltips showTooltips
		// Should GC overlays be shown?
		showGCOverlays            showGCOverlays
		toggleSettingNotification Notification

		hoveredSpans MergedSpans
		cursorPos    f32.Point
	}

	// prevFrame records the timeline's state in the previous state. It allows reusing the computed displayed spans
	// between frames if the timeline hasn't changed.
	prevFrame struct {
		start        trace.Timestamp
		end          trace.Timestamp
		y            int
		nsPerPx      float32
		compact      bool
		displayedAws []*ActivityWidget
		hoveredSpans MergedSpans
	}
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

func (tl *Timeline) navigateToChecks(gtx layout.Context, start, end trace.Timestamp, y int) bool {
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
	if !tl.navigateToChecks(gtx, start, end, y) {
		return
	}

	tl.rememberLocation()
	tl.navigateToImpl(gtx, start, end, y)
}

func (tl *Timeline) navigateToNoHistory(gtx layout.Context, start, end trace.Timestamp, y int) {
	if !tl.navigateToChecks(gtx, start, end, y) {
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
		tl.prevFrame.compact == tl.activity.compact
}

func (tl *Timeline) startZoomSelection(pos f32.Point) {
	tl.zoomSelection.active = true
	tl.zoomSelection.clickAt = pos
}

func (tl *Timeline) abortZoomSelection() {
	tl.zoomSelection.active = false
}

func (tl *Timeline) endZoomSelection(gtx layout.Context, pos f32.Point) {
	tl.zoomSelection.active = false
	one := tl.zoomSelection.clickAt.X
	two := pos.X

	startPx := min(one, two)
	endPx := max(one, two)

	if startPx < 0 {
		startPx = 0
	}
	if limit := float32(tl.VisibleWidth(gtx)); endPx > limit {
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
	tr := tl.trace
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
		return tr.Event(s.event()).Ts >= tl.end
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

type renderedSpansIterator struct {
	offset  int
	tl      *Timeline
	spans   Spans
	prevEnd trace.Timestamp
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut MergedSpans, startPx, endPx float32, ok bool) {
	offset := it.offset
	spans := it.spans
	tr := it.tl.trace

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	nsPerPx := float32(it.tl.nsPerPx)
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * float64(nsPerPx)))
	startOffset := offset
	tlStart := it.tl.start

	s := &spans[offset]
	offset++

	start := tr.Event(s.event()).Ts
	end := s.end
	if it.prevEnd > start {
		// The previous span was extended and grew into this span. This shifts our start position to the right.
		start = it.prevEnd
	}

	if time.Duration(end-start) < minSpanWidthD {
		// Merge all tiny spans until we find a span or gap that's big enough to stand on its own. We do not stop
		// merging after we've reached the minimum size because that can lead to multiple merges being next to each
		// other. Not only does this look bad, it is also prone to tiny spans toggling between two merged spans, and
		// previously merged spans becoming visible again when zooming out.
		for ; offset < len(it.spans); offset++ {
			adjustedEnd := end
			if time.Duration(end-start) < minSpanWidthD {
				adjustedEnd = start + trace.Timestamp(minSpanWidthD)
			} else {
				// Our merged span is long enough now and won't need to be extended anymore. Break out of this loop and
				// go into a smaller loop that specializes on just collecting tiny spans, avoiding the comparisons
				// needed for extending.
				break
			}

			nextSpan := &spans[offset]
			// Assume that we stop at this span. Compute the final size and extension. Use that to see
			// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
			nextStart := tr.Event(nextSpan.event()).Ts
			nextEnd := nextSpan.end
			if adjustedEnd > nextStart {
				// The current span would have to grow into the next span, making it smaller
				nextStart = adjustedEnd
			}
			if time.Duration(nextEnd-nextStart) >= minSpanWidthD || time.Duration(nextStart-adjustedEnd) >= minSpanWidthD {
				// Don't merge spans or gaps that can stand on their own
				break
			}

			end = nextSpan.end
		}

		for ; offset < len(it.spans); offset++ {
			nextSpan := &spans[offset]
			// Assume that we stop at this span. Compute the final size. Use that to see
			// if the next span would be large enough to stand on its own. If so, actually do stop at this span.
			nextStart := tr.Event(nextSpan.event()).Ts
			nextEnd := nextSpan.end
			if time.Duration(nextEnd-nextStart) >= minSpanWidthD || time.Duration(nextStart-end) >= minSpanWidthD {
				// Don't merge spans or gaps that can stand on their own
				break
			}

			end = nextSpan.end
		}
	}

	if time.Duration(end-start) < minSpanWidthD {
		// We're still too small, so extend the span to its minimum size.
		end = start + trace.Timestamp(minSpanWidthD)
	}

	it.offset = offset
	it.prevEnd = end
	startPx = float32(start-tlStart) / nsPerPx
	endPx = float32(end-tlStart) / nsPerPx
	return MergedSpans(spans[startOffset:it.offset]), startPx, endPx, true
}

func (tl *Timeline) zoomToFitCurrentView(gtx layout.Context) {
	tr := tl.trace
	var first, last trace.Timestamp = -1, -1
	for _, aw := range tl.visibleActivities(gtx) {
		for _, track := range aw.tracks {
			if len(track.spans) == 0 {
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

func (tl *Timeline) scrollToGoroutine(gtx layout.Context, g *Goroutine) {
	// OPT(dh): don't be O(n)
	off := 0
	for _, aw := range tl.activities {
		if g == aw.item {
			// TODO(dh): show goroutine at center of window, not the top
			tl.navigateTo(gtx, tl.start, tl.end, off)
			return
		}
		off += aw.Height(gtx) + gtx.Dp(activityGapDp)
	}
	panic("unreachable")
}

// The width in pixels of the visible portion of the timeline, i.e. the part that isn't occupied by the scrollbar.
func (tl *Timeline) VisibleWidth(gtx layout.Context) int {
	sbWidth := gtx.Dp(theme.Scrollbar(tl.theme, &tl.scrollbar).Width())
	return gtx.Constraints.Max.X - sbWidth
}

func easeOutQuart(progress float64) float64 {
	return 1 - math.Pow(1-progress, 4)
}

func easeInQuart(progress float64) float64 {
	return progress * progress * progress * progress
}

func (tl *Timeline) Layout(gtx layout.Context) layout.Dimensions {
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
						tl.zoomToFitCurrentView(gtx)
					case ev.Modifiers.Contain(key.ModShift):
						d := tl.end - tl.start
						tl.navigateTo(gtx, 0, d, tl.y)
					case ev.Modifiers == 0:
						tl.navigateTo(gtx, tl.start, tl.end, 0)
					}

				case "Z":
					if ev.Modifiers.Contain(key.ModCtrl) {
						if e, ok := tl.popLocationHistory(); ok {
							tl.navigateToNoHistory(gtx, e.start, e.end, e.y)
						}
					}

				case "X":
					tl.activity.displayAllLabels = !tl.activity.displayAllLabels

				case "C":
					// FIXME(dh): adjust tl.Y so that the top visible goroutine stays the same
					tl.activity.compact = !tl.activity.compact

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
					tl.activity.toggleSettingNotification.Show(gtx, s)

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
					tl.activity.toggleSettingNotification.Show(gtx, s)

				}
			}
		case pointer.Event:
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
				tl.abortZoomSelection()
				tl.zoom(gtx, ev.Scroll.Y, ev.Position)

			case pointer.Drag:
				tl.activity.cursorPos = ev.Position
				if tl.drag.active {
					if tl.drag.active {
						tl.dragTo(gtx, ev.Position)
					}
				}

			case pointer.Release:
				// For pointer.Release, ev.Buttons contains the buttons still being pressed, not the ones that have been
				// released.
				if !ev.Buttons.Contain(pointer.ButtonTertiary) {
					if tl.drag.active {
						tl.endDrag()
					} else if tl.zoomSelection.active {
						tl.endZoomSelection(gtx, ev.Position)
					}
				}

			case pointer.Move:
				tl.activity.cursorPos = ev.Position
			}
		}
	}

	var axisHeight int

	// Draw axis and activities
	func(gtx layout.Context) {
		gtx.Constraints.Max.X = tl.VisibleWidth(gtx)
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		tl.clickedGoroutineActivities = tl.clickedGoroutineActivities[:0]

		{
			activityGap := gtx.Dp(activityGapDp)
			// TODO(dh): add another screen worth of goroutines so the user can scroll a bit further
			d := tl.scrollbar.ScrollDistance()
			if d != 0 {
				// TODO(dh): because scroll amounts are relative even when the user clicks on a specific spot on the
				// scrollbar, and because we've already executed this frame's navigation animation step, applying the
				// delta to tl.y can leave it in a different position than where the user clicked.
				tl.cancelNavigation()
			}
			var totalHeight int
			for _, aw := range tl.activities {
				// OPT(dh): cache this value
				totalHeight += activityGap + aw.Height(gtx)
			}
			tl.y += int(round32(d * float32(totalHeight)))
			if tl.y < 0 {
				tl.y = 0
			}
		}

		tl.activity.hoveredSpans = nil
		for _, aw := range tl.prevFrame.displayedAws {
			if spans := aw.clickedSpans; len(spans) > 0 {
				start := spans.Start(tr)
				end := spans.End()
				tl.navigateTo(gtx, start, end, tl.y)
				break
			}
		}
		for _, aw := range tl.prevFrame.displayedAws {
			if spans := aw.hoveredSpans; len(spans) > 0 {
				tl.activity.hoveredSpans = spans
				break
			}
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
			Tag: tl,
			Types: pointer.Scroll |
				pointer.Drag |
				pointer.Press |
				pointer.Release |
				pointer.Move,
			ScrollBounds: image.Rectangle{Min: image.Pt(-1, -1), Max: image.Pt(1, 1)},
			Grab:         tl.drag.active,
		}.Add(gtx.Ops)
		key.InputOp{Tag: tl, Keys: "Ctrl-Z|C|O|T|X|(Shift)-(Ctrl)-" + key.NameHome}.Add(gtx.Ops)
		key.FocusOp{Tag: tl}.Add(gtx.Ops)

		drawRegionOverlays := func(spans Spans, c color.NRGBA, height int) {
			for _, s := range tl.visibleSpans(spans) {
				start := tl.trace.Events[s.event()].Ts
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

		// Draw axis and goroutines
		layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx, layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			// Draw STW and GC regions
			// TODO(dh): make this be optional
			tickHeight := gtx.Dp(tickHeightDp)
			drawRegionOverlays(tl.trace.gc, colors[colorStateGC], tickHeight)
			drawRegionOverlays(tl.trace.stw, colors[colorStateBlocked], tickHeight)

			dims := tl.axis.Layout(gtx)
			axisHeight = dims.Size.Y

			return dims
		}), layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			dims, aws := tl.layoutActivities(gtx)
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

		tl.activity.toggleSettingNotification.Layout(gtx, tl.theme)

		tl.prevFrame.start = tl.start
		tl.prevFrame.end = tl.end
		tl.prevFrame.nsPerPx = tl.nsPerPx
		tl.prevFrame.y = tl.y
		tl.prevFrame.compact = tl.activity.compact
		tl.prevFrame.hoveredSpans = tl.activity.hoveredSpans
	}(gtx)

	// Draw scrollbar
	func(gtx layout.Context) {
		defer op.Offset(image.Pt(tl.VisibleWidth(gtx), axisHeight)).Push(gtx.Ops).Pop()
		gtx.Constraints.Max.Y -= axisHeight

		activityGap := gtx.Dp(activityGapDp)

		var totalHeight int
		for _, aw := range tl.activities {
			// OPT(dh): cache this value
			totalHeight += aw.Height(gtx) + activityGap
		}
		if len(tl.activities) > 0 {
			// Allow scrolling past the last goroutine
			totalHeight += tl.activities[len(tl.activities)-1].Height(gtx) + activityGap
		}

		fraction := float32(gtx.Constraints.Max.Y) / float32(totalHeight)
		offset := float32(tl.y) / float32(totalHeight)
		sb := theme.Scrollbar(tl.theme, &tl.scrollbar)
		sb.Layout(gtx, layout.Vertical, offset, offset+fraction)
	}(gtx)

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

type SpanTooltipState struct {
	spans             MergedSpans
	events            []EventID
	eventsUnderCursor []EventID
}

type ActivityWidgetTrack struct {
	spans  Spans
	events []EventID

	// XXX these callbacks should probably get a reference to the track, too
	highlightSpan func(aw *ActivityWidget, spans MergedSpans) bool
	spanLabel     func(aw *ActivityWidget, spans MergedSpans) []string
	spanColor     func(aw *ActivityWidget, spans MergedSpans) [2]colorIndex
	spanTooltip   func(gtx layout.Context, aw *ActivityWidget, state SpanTooltipState)

	// op lists get reused between frames to avoid generating garbage
	ops          [colorStateLast * 2]op.Ops
	outlinesOps  reusableOps
	highlightOps reusableOps
	eventsOps    reusableOps
	labelsOps    reusableOps

	pointerAt f32.Point
	hovered   bool

	// cached state
	prevFrame struct {
		dspSpans []struct {
			dspSpans       MergedSpans
			startPx, endPx float32
		}
	}
}

type ActivityWidget struct {
	// Inputs
	tracks          []ActivityWidgetTrack
	widgetTooltip   func(gtx layout.Context, aw *ActivityWidget)
	invalidateCache func(aw *ActivityWidget) bool
	theme           *theme.Theme
	tl              *Timeline
	item            any
	label           string

	labelClicks int

	pointerAt    f32.Point
	hovered      bool
	hoveredLabel bool

	clickedSpans MergedSpans
	hoveredSpans MergedSpans

	prevFrame struct {
		// State for reusing the previous frame's ops, to avoid redrawing from scratch if no relevant state has changed.
		hovered    bool
		forceLabel bool
		compact    bool
		topBorder  bool
		ops        reusableOps
		call       op.CallOp
	}
}

func (aw *ActivityWidget) LabelClicked() bool {
	if aw.labelClicks > 0 {
		aw.labelClicks--
		return true
	} else {
		return false
	}
}

func NewGCWidget(th *theme.Theme, tl *Timeline, trace *Trace, spans Spans) *ActivityWidget {
	return &ActivityWidget{
		tracks: []ActivityWidgetTrack{{spans: spans}},
		tl:     tl,
		item:   spans,
		label:  "GC",
		theme:  th,
	}
}

func NewSTWWidget(th *theme.Theme, tl *Timeline, trace *Trace, spans Spans) *ActivityWidget {
	return &ActivityWidget{
		tracks: []ActivityWidgetTrack{{spans: spans}},
		tl:     tl,
		item:   spans,
		label:  "STW",
		theme:  th,
	}
}

func defaultSpanColor(aw *ActivityWidget, spans MergedSpans) [2]colorIndex {
	if len(spans) == 1 {
		return [2]colorIndex{stateColors[spans[0].state], 0}
	} else {
		c := stateColors[spans[0].state]
		for _, s := range spans[1:] {
			cc := stateColors[s.state]
			if cc != c {
				return [2]colorIndex{colorStateMerged, 0}
			}
		}
		return [2]colorIndex{c, colorStateMerged}
	}
}

func goroutineSpanTooltip(gtx layout.Context, aw *ActivityWidget, state SpanTooltipState) {
	tr := aw.tl.trace
	var label string
	if debug {
		label += fmt.Sprintf("Event ID: %d\n", state.spans[0].event())
	}
	label += "State: "
	var at string
	if len(state.spans) == 1 {
		s := &state.spans[0]
		ev := tr.Event(s.event())
		if at == "" && ev.StkID > 0 {
			at = tr.PCs[tr.Stacks[ev.StkID][s.at]].Fn
		}
		switch state := s.state; state {
		case stateInactive:
			label += "inactive"
		case stateActive:
			label += "active"
		case stateGCDedicated:
			label += "GC (dedicated)"
		case stateGCIdle:
			label += "GC (idle)"
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
		case stateBlockedSyncOnce:
			label += "blocked on sync.Once"
		case stateBlockedSyncTriggeringGC:
			label += "blocked triggering GC"
		case stateBlockedCond:
			label += "blocked on condition variable"
		case stateBlockedNet:
			label += "blocked on polled I/O"
		case stateBlockedGC:
			label += "GC assist wait"
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
			if link := fromUint40(&ev.Link); link != -1 {
				l := tr.Events[link]
				label += local.Sprintf("\nSwept %d bytes, reclaimed %d bytes",
					l.Args[trace.ArgGCSweepDoneSwept], l.Args[trace.ArgGCSweepDoneReclaimed])
			}
		case stateRunningG:
			g := aw.tl.trace.getG(ev.G)
			label += local.Sprintf("running goroutine %d", ev.G)
			label = local.Sprintf("Goroutine %d: %s\n", ev.G, g.function) + label
		default:
			if debug {
				panic(fmt.Sprintf("unhandled state %d", state))
			}
		}

		tags := make([]string, 0, 4)
		if s.tags&spanTagRead != 0 {
			tags = append(tags, "read")
		}
		if s.tags&spanTagAccept != 0 {
			tags = append(tags, "accept")
		}
		if s.tags&spanTagDial != 0 {
			tags = append(tags, "dial")
		}
		if s.tags&spanTagNetwork != 0 {
			tags = append(tags, "network")
		}
		if s.tags&spanTagTCP != 0 {
			tags = append(tags, "TCP")
		}
		if s.tags&spanTagTLS != 0 {
			tags = append(tags, "TLS")
		}
		if s.tags&spanTagHTTP != 0 {
			tags = append(tags, "HTTP")
		}
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}
	} else {
		label += local.Sprintf("mixed (%d spans)", len(state.spans))
	}
	label += "\n"

	if len(state.spans) == 1 {
		if reason := reasonLabels[tr.Reason(&state.spans[0])]; reason != "" {
			label += "Reason: " + reason + "\n"
		}
	}

	if at != "" {
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("In: %s\n", at)
	}

	label += fmt.Sprintf("Duration: %s\n", state.spans.Duration(tr))

	if len(state.events) > 0 {
		label += local.Sprintf("Events in span: %d\n", len(state.events))
	}

	if len(state.eventsUnderCursor) > 0 {
		kind := tr.Event(state.eventsUnderCursor[0]).Type
		for _, ev := range state.eventsUnderCursor[1:] {
			if tr.Event(ev).Type != kind {
				kind = 255
				break
			}
		}
		if kind != 255 {
			var noun string
			switch kind {
			case trace.EvGoSysCall:
				noun = "syscalls"
				if len(state.eventsUnderCursor) == 1 {
					stk := tr.Stacks[tr.Event(state.eventsUnderCursor[0]).StkID]
					if len(stk) != 0 {
						frame := tr.PCs[stk[0]]
						noun += fmt.Sprintf(" (%s)", frame.Fn)
					}
				}
			case trace.EvGoCreate:
				noun = "goroutine creations"
			case trace.EvGoUnblock:
				noun = "goroutine unblocks"
			default:
				if debug {
					panic(fmt.Sprintf("unhandled kind %d", kind))
				}
			}
			label += local.Sprintf("Events under cursor: %d %s\n", len(state.eventsUnderCursor), noun)
		} else {
			label += local.Sprintf("Events under cursor: %d\n", len(state.eventsUnderCursor))
		}
	}

	if n := len(label) - 1; label[n] == '\n' {
		label = label[:n]
	}

	theme.Tooltip{Theme: aw.theme}.Layout(gtx, label)
}

func userRegionSpanTooltip(gtx layout.Context, aw *ActivityWidget, state SpanTooltipState) {
	tr := aw.tl.trace
	var label string
	if len(state.spans) == 1 {
		s := &state.spans[0]
		ev := tr.Event(s.event())
		if s.state != stateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.state))
		}
		label = local.Sprintf("User region: %s\nTask: %s\n",
			tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], tr.Task(ev.Args[trace.ArgUserRegionTaskID]).name)
	} else {
		label = local.Sprintf("mixed (%d spans)\n", len(state.spans))
	}
	label += fmt.Sprintf("Duration: %s", state.spans.Duration(tr))
	theme.Tooltip{Theme: aw.theme}.Layout(gtx, label)
}

func NewGoroutineWidget(th *theme.Theme, tl *Timeline, g *Goroutine) *ActivityWidget {
	var l string
	if g.function != "" {
		l = local.Sprintf("goroutine %d: %s", g.id, g.function)
	} else {
		l = local.Sprintf("goroutine %d", g.id)
	}

	aw := &ActivityWidget{
		tracks: []ActivityWidgetTrack{{
			spans:  g.spans,
			events: g.events,
			spanLabel: func(aw *ActivityWidget, spans MergedSpans) []string {
				if len(spans) != 1 {
					return nil
				}
				return spanStateLabels[spans[0].state]
			},
			spanTooltip: goroutineSpanTooltip,
		}},
		widgetTooltip: func(gtx layout.Context, aw *ActivityWidget) {
			GoroutineTooltip{g, th, tl.trace}.Layout(gtx)
		},
		theme: th,
		tl:    tl,
		item:  g,
		label: l,
	}

	for _, ug := range g.userRegions {
		aw.tracks = append(aw.tracks, ActivityWidgetTrack{
			spans: ug,
			spanLabel: func(aw *ActivityWidget, spans MergedSpans) []string {
				if len(spans) != 1 {
					return nil
				}
				// OPT(dh): avoid this allocation
				s := tl.trace.Strings[tl.trace.Events[spans[0].event()].Args[trace.ArgUserRegionTypeID]]
				return []string{s}
			},
			spanTooltip: userRegionSpanTooltip,
			spanColor: func(aw *ActivityWidget, spans MergedSpans) [2]colorIndex {
				if len(spans) == 1 {
					return [2]colorIndex{colorStateUserRegion, 0}
				} else {
					return [2]colorIndex{colorStateUserRegion, colorStateMerged}
				}
			},
		})
	}

	return aw
}

func (w *MainWindow) openGoroutineWindow(g *Goroutine) {
	_, ok := w.goroutineWindows[g.id]
	if ok {
		// XXX try to activate (bring to the front) the existing window
	} else {
		win := &GoroutineWindow{
			// Note that we cannot use a.theme, because text.Shaper isn't safe for concurrent use.
			theme: theme.NewTheme(gofont.Collection()),
			trace: w.trace,
			g:     g,
		}
		win.stats = NewGoroutineStats(g, w.trace)
		w.goroutineWindows[g.id] = win
		// XXX computing the label is duplicated with rendering the activity widget
		var l string
		if g.function != "" {
			l = local.Sprintf("goroutine %d: %s", g.id, g.function)
		} else {
			l = local.Sprintf("goroutine %d", g.id)
		}
		go func() {
			// XXX handle error?
			win.Run(app.NewWindow(app.Title(fmt.Sprintf("gotraceui - %s", l))))
			w.notifyGoroutineWindowClosed <- g.id
		}()
	}
}

func (w *MainWindow) openHeatmap() {
	win := &HeatmapWindow{
		theme: w.theme,
		trace: w.trace,
	}
	go func() {
		// XXX handle error?
		win.Run(app.NewWindow(app.Title("gotraceui - heatmap")))
	}()
}

func processorSpanTooltip(gtx layout.Context, aw *ActivityWidget, state SpanTooltipState) {
	tr := aw.tl.trace
	var label string
	if len(state.spans) == 1 {
		s := &state.spans[0]
		ev := tr.Event(s.event())
		if s.state != stateRunningG {
			panic(fmt.Sprintf("unexpected state %d", s.state))
		}
		g := aw.tl.trace.getG(ev.G)
		label = local.Sprintf("Goroutine %d: %s\n", ev.G, g.function)
	} else {
		label = local.Sprintf("mixed (%d spans)\n", len(state.spans))
	}
	label += fmt.Sprintf("Duration: %s", state.spans.Duration(tr))
	theme.Tooltip{Theme: aw.theme}.Layout(gtx, label)
}

func NewProcessorWidget(th *theme.Theme, tl *Timeline, p *Processor) *ActivityWidget {
	tr := tl.trace
	return &ActivityWidget{
		tracks: []ActivityWidgetTrack{{
			spans: p.spans,
			highlightSpan: func(aw *ActivityWidget, spans MergedSpans) bool {
				if len(tl.activity.hoveredSpans) != 1 {
					return false
				}
				// OPT(dh): don't be O(n)
				o := &tl.activity.hoveredSpans[0]
				for i := range spans {
					if tr.Event(spans[i].event()).G == tr.Event(o.event()).G {
						return true
					}
				}
				return false
			},
			spanLabel: func(aw *ActivityWidget, spans MergedSpans) []string {
				if len(spans) != 1 {
					return nil
				}
				// OPT(dh): cache the strings
				out := make([]string, 3)
				g := aw.tl.gs[tr.Event(spans[0].event()).G]
				if g.function != "" {
					out[0] = fmt.Sprintf("g%d: %s", g.id, g.function)
				} else {
					out[0] = fmt.Sprintf("g%d", g.id)
				}
				out[1] = fmt.Sprintf("g%d", g.id)
				out[2] = ""
				return out

			},
			spanColor: func(aw *ActivityWidget, spans MergedSpans) [2]colorIndex {
				do := func(aw *ActivityWidget, s Span) colorIndex {
					gid := aw.tl.trace.Events[s.event()].G
					g := aw.tl.trace.getG(gid)
					switch fn := g.function; fn {
					case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
						return colorStateGC
					default:
						// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
						return stateColors[s.state]
					}
				}

				if len(spans) == 1 {
					return [2]colorIndex{do(aw, spans[0]), 0}
				} else {
					c := do(aw, spans[0])
					for _, s := range spans[1:] {
						cc := do(aw, s)
						if cc != c {
							return [2]colorIndex{colorStateMerged, 0}
						}
					}
					return [2]colorIndex{c, colorStateMerged}
				}
			},
			spanTooltip: processorSpanTooltip,
		}},
		widgetTooltip: func(gtx layout.Context, aw *ActivityWidget) {},
		invalidateCache: func(aw *ActivityWidget) bool {
			if len(tl.prevFrame.hoveredSpans) == 0 && len(tl.activity.hoveredSpans) == 0 {
				// Nothing hovered in either frame.
				return false
			}

			if len(tl.prevFrame.hoveredSpans) > 1 && len(tl.activity.hoveredSpans) > 1 {
				// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
				// frames, then nothing changes for rendering.
				return false
			}

			if len(tl.prevFrame.hoveredSpans) != len(tl.activity.hoveredSpans) {
				// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
				// previously highlighted.
				//
				// The number of hovered spans changed, and at least in one frame the number was 1.
				return true
			}

			// If we got to this point, then both slices have exactly one element.
			if tr.Event(tl.prevFrame.hoveredSpans[0].event()).G != tr.Event(tl.activity.hoveredSpans[0].event()).G {
				return true
			}

			return false
		},
		tl:    tl,
		item:  p,
		label: local.Sprintf("Processor %d", p.id),
		theme: th,
	}
}

func (aw *ActivityWidget) Height(gtx layout.Context) int {
	if aw.tl.activity.compact {
		return (gtx.Dp(activityTrackHeightDp) + gtx.Dp(activityTrackGapDp)) * len(aw.tracks)
	} else {
		return (gtx.Dp(activityTrackHeightDp)+gtx.Dp(activityTrackGapDp))*len(aw.tracks) + gtx.Dp(activityLabelHeightDp)
	}
}

func (aw *ActivityWidget) Layout(gtx layout.Context, forceLabel bool, compact bool, topBorder bool) layout.Dimensions {
	// TODO(dh): we could replace all uses of activityHeight by using normal Gio widget patterns: lay out all the
	// tracks, sum their heights and the gaps we apply. We'd use a macro to get the total size and then set up the clip
	// and pointer input. When we reuse the previous frame and need to return dimensions, we should use a stored value
	// of the height. Making the change isn't currently worth it, though, because we still need ActivityWidget.Height to
	// exist so we can compute the scrollbar, and so we can jump to goroutines, which needs to compute an offset. In
	// both cases we don't want to lay out every widget to figure out its size.
	activityHeight := aw.Height(gtx)
	activityTrackGap := gtx.Dp(activityTrackGapDp)
	activityLabelHeight := gtx.Dp(activityLabelHeightDp)

	aw.clickedSpans = nil
	aw.hoveredSpans = nil

	var widgetClicked bool

	for _, e := range gtx.Events(aw) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			aw.hovered = true
			aw.pointerAt = ev.Position
		case pointer.Drag:
			aw.pointerAt = ev.Position
		case pointer.Leave, pointer.Cancel:
			aw.hovered = false
		case pointer.Press:
			// If any part of the widget has been clicked then don't reuse the cached macro; we have to figure out what
			// was clicked and react to it.
			//
			// OPT(dh): if it was the label that was clicked, then we can reuse the macro

			widgetClicked = true
		}
	}

	aw.labelClicks = 0
	for _, ev := range gtx.Events(&aw.label) {
		switch ev := ev.(type) {
		case pointer.Event:
			switch ev.Type {
			case pointer.Enter, pointer.Move:
				aw.hoveredLabel = true
			case pointer.Leave, pointer.Cancel:
				aw.hoveredLabel = false
			case pointer.Press:
				if ev.Buttons.Contain(pointer.ButtonPrimary) && ev.Modifiers == 0 {
					aw.labelClicks++
				}

				if ev.Buttons.Contain(pointer.ButtonTertiary) && ev.Modifiers.Contain(key.ModCtrl) {
					// XXX this assumes that the first track is the widest one. This is currently true, but a brittle
					// assumption to make.
					aw.clickedSpans = MergedSpans(aw.tracks[0].spans)
				}
			}
		}
	}

	if !widgetClicked &&
		aw.tl.unchanged() &&
		!aw.hovered &&
		!aw.prevFrame.hovered &&
		forceLabel == aw.prevFrame.forceLabel &&
		compact == aw.prevFrame.compact &&
		(aw.invalidateCache == nil || !aw.invalidateCache(aw)) &&
		topBorder == aw.prevFrame.topBorder {

		// OPT(dh): instead of avoiding cached ops completely when the activity is hovered, draw the tooltip
		// separately.
		aw.prevFrame.call.Add(gtx.Ops)
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
	}

	aw.prevFrame.hovered = aw.hovered
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
	pointer.InputOp{Tag: aw, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)

	if !compact {
		if aw.hovered || forceLabel || topBorder {
			// Draw border at top of the activity
			paint.FillShape(gtx.Ops, colors[colorActivityBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if aw.hovered || forceLabel {
			labelGtx := gtx
			labelGtx.Constraints.Min = image.Point{}
			labelDims := mywidget.TextLine{Color: colors[colorActivityLabel]}.Layout(labelGtx, aw.theme.Shaper, text.Font{}, aw.theme.TextSize, aw.label)

			stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
			pointer.InputOp{Tag: &aw.label, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Cancel | pointer.Move}.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
			stack.Pop()
		}

		if aw.widgetTooltip != nil && aw.tl.activity.showTooltips == showTooltipsBoth && aw.hoveredLabel {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(aw.pointerAt.Round()).Push(gtx.Ops)
			aw.widgetTooltip(gtx, aw)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
		}

		defer op.Offset(image.Pt(0, activityLabelHeight)).Push(gtx.Ops).Pop()
	}

	stack := op.TransformOp{}.Push(gtx.Ops)
	for i := range aw.tracks {
		dims := aw.tracks[i].Layout(gtx, aw)
		op.Offset(image.Pt(0, dims.Size.Y+activityTrackGap)).Add(gtx.Ops)
	}
	stack.Pop()

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
}

func (track *ActivityWidgetTrack) Layout(gtx layout.Context, aw *ActivityWidget) layout.Dimensions {
	tr := aw.tl.trace
	trackHeight := gtx.Dp(activityTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	trackClicked := false
	for _, e := range gtx.Events(track) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			track.hovered = true
			track.pointerAt = ev.Position
		case pointer.Drag:
			track.pointerAt = ev.Position
		case pointer.Leave, pointer.Cancel:
			track.hovered = false
		case pointer.Press:
			if ev.Buttons.Contain(pointer.ButtonTertiary) && ev.Modifiers.Contain(key.ModCtrl) {
				trackClicked = true
			}
		}
	}

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, trackHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: track, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)

	// Draw activity lifetimes
	//
	// We batch draw operations by color to avoid making thousands of draw calls. See
	// https://lists.sr.ht/~eliasnaur/gio/%3C871qvbdx5r.fsf%40honnef.co%3E#%3C87v8smctsd.fsf@honnef.co%3E
	//
	for i := range track.ops {
		track.ops[i].Reset()
	}
	// one path per single-color span and one path per merged+color gradient
	//
	//gcassert:noescape
	paths := [colorStateLast * 2]clip.Path{}

	var outlinesPath clip.Path
	var highlightPath clip.Path
	var eventsPath clip.Path
	outlinesPath.Begin(track.outlinesOps.get())
	highlightPath.Begin(track.highlightOps.get())
	eventsPath.Begin(track.eventsOps.get())
	labelsOps := track.labelsOps.get()
	labelsMacro := op.Record(labelsOps)

	for i := range paths {
		paths[i].Begin(&track.ops[i])
	}

	first := true
	var prevEndPx float32
	doSpans := func(dspSpans MergedSpans, startPx, endPx float32) {
		if track.hovered && track.pointerAt.X >= startPx && track.pointerAt.X < endPx {
			if trackClicked {
				aw.clickedSpans = dspSpans
			}
			aw.hoveredSpans = dspSpans
		}

		var cs [2]colorIndex
		if track.spanColor != nil {
			cs = track.spanColor(aw, dspSpans)
		} else {
			cs = defaultSpanColor(aw, dspSpans)
		}

		if cs[1] != 0 && cs[1] != colorStateMerged {
			panic(fmt.Sprintf("two-color spans are only supported with color₁ == colorStateMerged, got %v", cs))
		}

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt((max(startPx, 0)), 0)
		maxP = f32.Pt((min(endPx, float32(gtx.Constraints.Max.X))), float32(trackHeight))

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

		pathID := cs[0]
		if cs[1] != 0 {
			pathID += colorStateLast
		}
		p := &paths[pathID]
		p.MoveTo(minP)
		p.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
		p.LineTo(maxP)
		p.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
		p.Close()

		var spanTooltipState SpanTooltipState
		if aw.tl.activity.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= startPx && track.pointerAt.X < endPx {
			events := dspSpans.Events(track.events, tr)

			spanTooltipState = SpanTooltipState{
				spans:  dspSpans,
				events: events,
			}
		}

		dotRadiusX := float32(gtx.Dp(4))
		dotRadiusY := float32(gtx.Dp(3))
		if maxP.X-minP.X > dotRadiusX*2 && len(dspSpans) == 1 {
			// We only display event dots in unmerged spans because merged spans can split into smaller spans when we
			// zoom in, causing dots to disappear and reappearappear and disappear.
			events := dspSpans[0].Events(track.events, tr)

			dotGap := float32(gtx.Dp(4))
			centerY := float32(trackHeight) / 2

			for i := 0; i < len(events); i++ {
				ev := events[i]
				px := aw.tl.tsToPx(tr.Event(ev).Ts)

				if px+dotRadiusX < minP.X {
					continue
				}
				if px-dotRadiusX > maxP.X {
					break
				}

				start := px
				end := px
				oldi := i
				for i = i + 1; i < len(events); i++ {
					ev := events[i]
					px := aw.tl.tsToPx(tr.Event(ev).Ts)
					if px < end+dotRadiusX*2+dotGap {
						end = px
					} else {
						break
					}
				}
				i--

				if minP.X != 0 && start-dotRadiusX < minP.X {
					start = minP.X + dotRadiusX
				}
				if maxP.X != float32(gtx.Constraints.Max.X) && end+dotRadiusX > maxP.X {
					end = maxP.X - dotRadiusX
				}

				minX := start - dotRadiusX
				minY := centerY - dotRadiusY
				maxX := end + dotRadiusX
				maxY := centerY + dotRadiusY

				eventsPath.MoveTo(f32.Pt(minX, minY))
				eventsPath.LineTo(f32.Pt(maxX, minY))
				eventsPath.LineTo(f32.Pt(maxX, maxY))
				eventsPath.LineTo(f32.Pt(minX, maxY))
				eventsPath.Close()

				if aw.tl.activity.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= minX && track.pointerAt.X < maxX {
					spanTooltipState.eventsUnderCursor = events[oldi : i+1]
				}
			}
		}

		if spanTooltipState.spans != nil && track.spanTooltip != nil {
			// TODO have a gap between the cursor and the tooltip
			// TODO shift the tooltip to the left if otherwise it'd be too wide for the window given its position
			macro := op.Record(gtx.Ops)
			stack := op.Offset(track.pointerAt.Round()).Push(gtx.Ops)
			track.spanTooltip(gtx, aw, spanTooltipState)
			stack.Pop()
			call := macro.Stop()
			op.Defer(gtx.Ops, call)
		}

		if track.highlightSpan != nil && track.highlightSpan(aw, dspSpans) {
			minP := minP
			maxP := maxP
			minP.Y += float32((trackHeight - spanBorderWidth*2) / 2)

			highlightPath.MoveTo(minP)
			highlightPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			highlightPath.LineTo(maxP)
			highlightPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			highlightPath.Close()
		}

		if len(dspSpans) == 1 && track.spanLabel != nil && maxP.X-minP.X > float32(2*minSpanWidth) {
			// The Label callback, if set, returns a list of labels to try and use for the span. We pick the first label
			// that fits fully in the span, as it would be drawn untruncated. That is, the ideal label size depends on
			// the zoom level, not panning. If no label fits, we use the last label in the list. This label can be the
			// empty string to effectively display no label.
			//
			// We don't try to render a label for very small spans.
			if labels := track.spanLabel(aw, dspSpans); len(labels) > 0 {
				for i, label := range labels {
					if label == "" {
						continue
					}

					macro := op.Record(labelsOps)
					dims := mywidget.TextLine{Color: aw.theme.Palette.Foreground}.Layout(withOps(gtx, labelsOps), aw.theme.Shaper, text.Font{Weight: text.ExtraBold}, aw.theme.TextSize, label)
					if float32(dims.Size.X) > endPx-startPx && i != len(labels)-1 {
						// This label doesn't fit. If the callback provided more labels, try those instead.
						macro.Stop()
						continue
					}

					call := macro.Stop()
					middleOfSpan := startPx + (endPx-startPx)/2
					left := middleOfSpan - float32(dims.Size.X)/2
					if left+float32(dims.Size.X) > maxP.X {
						left = maxP.X - float32(dims.Size.X)
					}
					if left < minP.X {
						left = minP.X
					}
					stack := op.Offset(image.Pt(int(left), 0)).Push(labelsOps)
					paint.ColorOp{Color: aw.theme.Palette.Foreground}.Add(labelsOps)
					stack2 := FRect{Max: f32.Pt(maxP.X-minP.X, maxP.Y-minP.Y)}.Op(labelsOps).Push(labelsOps)
					call.Add(labelsOps)
					stack2.Pop()
					stack.Pop()
					break
				}
			}
		}

		first = false
	}

	if aw.tl.unchanged() {
		for _, prevSpans := range track.prevFrame.dspSpans {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := track.prevFrame.dspSpans[:0]
		it := renderedSpansIterator{
			tl:    aw.tl,
			spans: aw.tl.visibleSpans(track.spans),
		}
		for {
			dspSpans, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}
			allDspSpans = append(allDspSpans, struct {
				dspSpans       MergedSpans
				startPx, endPx float32
			}{dspSpans, startPx, endPx})
			doSpans(dspSpans, startPx, endPx)
		}
		track.prevFrame.dspSpans = allDspSpans
	}

	// First draw the outlines. We draw these as solid rectangles and let the spans overlay them.
	//
	// Drawing solid rectangles that get covered up seems to be much faster than using strokes, at least in this
	// specific instance.
	paint.FillShape(gtx.Ops, colors[colorSpanOutline], clip.Outline{Path: outlinesPath.End()}.Op())

	// Then draw the spans
	for cIdx := range paths {
		p := &paths[cIdx]
		if cIdx < int(colorStateLast) {
			paint.FillShape(gtx.Ops, colors[cIdx], clip.Outline{Path: p.End()}.Op())
		} else {
			stack := clip.Outline{Path: p.End()}.Op().Push(gtx.Ops)
			paint.LinearGradientOp{
				Stop1:  f32.Pt(0, 10),
				Color1: colors[cIdx-int(colorStateLast)],
				Stop2:  f32.Pt(0, 20),
				Color2: colors[colorStateMerged],
			}.Add(gtx.Ops)
			paint.PaintOp{}.Add(gtx.Ops)
			stack.Pop()
		}
	}
	paint.FillShape(gtx.Ops, colors[colorSpanWithEvents], clip.Outline{Path: highlightPath.End()}.Op())
	paint.FillShape(gtx.Ops, rgba(0x000000DD), clip.Outline{Path: eventsPath.End()}.Op())

	// Finally print labels on top
	labelsMacro.Stop().Add(gtx.Ops)

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, trackHeight)}
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
		if y < -aw.Height(gtx) {
			y += activityGap + aw.Height(gtx)
			continue
		}
		if start == -1 {
			start = i
		}
		if y > gtx.Constraints.Max.Y {
			end = i
			break
		}
		y += activityGap + aw.Height(gtx)
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

func (tl *Timeline) layoutActivities(gtx layout.Context) (layout.Dimensions, []*ActivityWidget) {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	activityGap := gtx.Dp(activityGapDp)

	// OPT(dh): at least use binary search to find the range of activities we need to draw. now that activity heights
	// aren't constant anymore, however, this is more complicated.
	start := -1
	end := -1
	y := -tl.y
	for i, aw := range tl.activities {
		if aw.LabelClicked() {
			if g, ok := aw.item.(*Goroutine); ok {
				tl.clickedGoroutineActivities = append(tl.clickedGoroutineActivities, g)
			}
		}
		// Don't draw activities that would be fully hidden, but do draw partially hidden ones
		if y < -aw.Height(gtx) {
			y += activityGap + aw.Height(gtx)
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
		aw.Layout(gtx, tl.activity.displayAllLabels, tl.activity.compact, topBorder)
		stack.Pop()

		y += activityGap + aw.Height(gtx)
	}

	var out []*ActivityWidget
	if start != -1 {
		out = tl.activities[start : end+1]
	}

	return layout.Dimensions{Size: gtx.Constraints.Max}, out
}

type GoroutineTooltip struct {
	g     *Goroutine
	theme *theme.Theme
	trace *Trace
}

func (tt GoroutineTooltip) Layout(gtx layout.Context) layout.Dimensions {
	tr := tt.trace
	start := tt.g.spans.Start(tr)
	end := tt.g.spans.End()
	d := time.Duration(end - start)

	// OPT(dh): compute these statistics when parsing the trace, instead of on each frame.
	var blockedD, inactiveD, runningD, gcAssistD time.Duration
	for i := range tt.g.spans {
		s := &tt.g.spans[i]
		d := tr.Duration(s)
		switch s.state {
		case stateInactive:
			inactiveD += d
		case stateActive, stateGCDedicated, stateGCIdle:
			runningD += d
		case stateBlocked:
			blockedD += d
		case stateBlockedSend:
			blockedD += d
		case stateBlockedRecv:
			blockedD += d
		case stateBlockedSelect:
			blockedD += d
		case stateBlockedSync:
			blockedD += d
		case stateBlockedSyncOnce:
			blockedD += d
		case stateBlockedSyncTriggeringGC:
			blockedD += d
		case stateBlockedCond:
			blockedD += d
		case stateBlockedNet:
			blockedD += d
		case stateBlockedGC:
			blockedD += d
		case stateBlockedSyscall:
			blockedD += d
		case stateStuck:
			blockedD += d
		case stateReady:
			inactiveD += d
		case stateCreated:
			inactiveD += d
		case stateGCMarkAssist:
			gcAssistD += d
		case stateGCSweep:
			gcAssistD += d
		case stateDone:
		default:
			if debug {
				panic(fmt.Sprintf("unknown state %d", s.state))
			}
		}
	}
	blockedPct := float32(blockedD) / float32(d) * 100
	inactivePct := float32(inactiveD) / float32(d) * 100
	runningPct := float32(runningD) / float32(d) * 100
	gcAssistPct := float32(gcAssistD) / float32(d) * 100
	var fnName string
	line1 := "Goroutine %[1]d\n\n"
	if tt.g.function != "" {
		fnName = tt.g.function
		line1 = "Goroutine %[1]d: %[2]s\n\n"
	}
	l := local.Sprintf(line1+
		"Created at: %[3]s\n"+
		"Returned at: %[4]s\n"+
		"Lifetime: %[5]s\n"+
		"Time in blocked states: %[6]s (%.2[7]f%%)\n"+
		"Time in inactive states: %[8]s (%.2[9]f%%)\n"+
		"Time in GC assist: %[10]s (%.2[11]f%%)\n"+
		"Time in running states: %[12]s (%.2[13]f%%)",
		tt.g.id, fnName,
		formatTimestamp(start),
		formatTimestamp(end),
		d,
		blockedD, blockedPct,
		inactiveD, inactivePct,
		gcAssistD, gcAssistPct,
		runningD, runningPct)

	return theme.Tooltip{Theme: tt.theme}.Layout(gtx, l)
}

type Command struct {
	// TODO(dh): use an enum
	command string
	data    any
}

type MainWindow struct {
	tl       Timeline
	theme    *theme.Theme
	trace    *Trace
	commands chan Command

	notifyGoroutineWindowClosed chan uint64
	goroutineWindows            map[uint64]*GoroutineWindow

	debugWindow *DebugWindow
}

func NewMainWindow() *MainWindow {
	win := &MainWindow{
		theme:                       theme.NewTheme(gofont.Collection()),
		commands:                    make(chan Command, 128),
		notifyGoroutineWindowClosed: make(chan uint64, 16),
		goroutineWindows:            make(map[uint64]*GoroutineWindow),
		debugWindow:                 NewDebugWindow(),
	}

	win.tl.theme = win.theme
	win.tl.axis.tl = &win.tl

	return win
}

type goroutineFilter struct {
	invalid bool
	parts   []struct {
		prefix string
		value  struct {
			s string
			n uint64
		}
	}
}

func newGoroutineFilter(s string) theme.Filter[*Goroutine] {
	out := &goroutineFilter{}
	for _, field := range strings.Fields(s) {
		prefix, value, found := strings.Cut(field, ":")
		if !found {
			prefix, value = value, prefix
		}

		var v struct {
			s string
			n uint64
		}
		switch prefix {
		case "gid":
			var err error
			v.n, err = strconv.ParseUint(value, 10, 64)
			if err != nil {
				out.invalid = true
			}
		default:
			v.s = value
		}

		out.parts = append(out.parts, struct {
			prefix string
			value  struct {
				s string
				n uint64
			}
		}{prefix, v})
	}
	return out
}

func (f *goroutineFilter) Filter(item *Goroutine) bool {
	if f.invalid {
		return false
	}

	for _, p := range f.parts {
		switch p.prefix {
		case "gid":
			if item.id != p.value.n {
				return false
			}

		case "":
			// TODO(dh): support case insensitive search
			if !strings.Contains(item.function, p.value.s) {
				return false
			}

		default:
			return false
		}
	}
	return true

	// XXX implement a much better filtering function that can do case-insensitive fuzzy search,
	// and allows matching goroutines by ID.
}

func (w *MainWindow) Run(win *app.Window) error {
	profileTag := new(int)
	var ops op.Ops

	var ww *theme.ListWindow[*Goroutine]
	var shortcuts int

	// TODO(dh): use enum for state
	state := "empty"
	var progress float32
	var err error

	for {
		select {
		case cmd := <-w.commands:
			switch cmd.command {
			case "setState":
				state = cmd.data.(string)
				progress = 0.0
				win.Invalidate()
			case "setProgress":
				progress = cmd.data.(float32)
				win.Invalidate()
			case "loadTrace":
				w.loadTrace(cmd.data.(*Trace))

				state = "main"
				progress = 0.0
				win.Invalidate()
				ww = nil
			case "error":
				state = "error"
				err = cmd.data.(error)
				progress = 0.0
				win.Invalidate()
			}

		case gid := <-w.notifyGoroutineWindowClosed:
			delete(w.goroutineWindows, gid)

		case e := <-win.Events():
			switch ev := e.(type) {
			case system.DestroyEvent:
				return ev.Err
			case system.FrameEvent:
				gtx := layout.NewContext(&ops, ev)
				gtx.Constraints.Min = image.Point{}

				for _, ev := range gtx.Events(profileTag) {
					// Yup, profile.Event only contains a string. No structured access to data.
					fields := strings.Fields(ev.(profile.Event).Timings)
					if len(fields) > 0 && strings.HasPrefix(fields[0], "tot:") {
						var s string
						if fields[0] == "tot:" {
							s = fields[1]
						} else {
							s = strings.TrimPrefix(fields[0], "tot:")
						}
						// Either it parses fine, or d is undefined and will likely be obvious in the debug grpah.
						d, _ := time.ParseDuration(s)
						// We're using gtx.Now because events don't have timestamps associated with them. Hopefully
						// event creation isn't too far removed from this code.
						w.debugWindow.frametimes.addValue(gtx.Now, float64(d)/float64(time.Millisecond))
					}
				}
				profile.Op{Tag: profileTag}.Add(gtx.Ops)

				// Fill background
				paint.Fill(gtx.Ops, colors[colorBackground])

				switch state {
				case "empty":

				case "error":
					paint.ColorOp{Color: w.theme.Palette.Foreground}.Add(gtx.Ops)
					m := op.Record(gtx.Ops)
					dims := widget.Label{}.Layout(gtx, w.theme.Shaper, text.Font{}, w.theme.TextSize, fmt.Sprintf("Error: %s", err))
					call := m.Stop()
					op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
					call.Add(gtx.Ops)

				case "loadingTrace":
					paint.ColorOp{Color: w.theme.Palette.Foreground}.Add(gtx.Ops)
					m := op.Record(gtx.Ops)
					dims := widget.Label{}.Layout(gtx, w.theme.Shaper, text.Font{}, w.theme.TextSize, "Loading trace...")
					op.Offset(image.Pt(0, dims.Size.Y)).Add(gtx.Ops)

					func() {
						gtx := gtx
						gtx.Constraints.Min = image.Pt(dims.Size.X, 15)
						gtx.Constraints.Max = gtx.Constraints.Min
						theme.ProgressBar(w.theme, progress).Layout(gtx)
					}()

					call := m.Stop()
					op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y/2-dims.Size.Y/2)).Add(gtx.Ops)
					call.Add(gtx.Ops)

				case "main":
					for _, ev := range gtx.Events(&shortcuts) {
						switch ev := ev.(type) {
						case key.Event:
							if ev.State == key.Press {
								switch ev.Name {
								case "G":
									ww = theme.NewListWindow[*Goroutine](w.theme)
									ww.SetItems(w.trace.gs)
									ww.BuildFilter = newGoroutineFilter
								case "H":
									w.openHeatmap()
								}
							}
						}
					}

					for _, g := range w.tl.clickedGoroutineActivities {
						w.openGoroutineWindow(g)
					}

					key.InputOp{Tag: &shortcuts, Keys: "G|H"}.Add(gtx.Ops)

					if ww != nil {
						if item, ok := ww.Confirmed(); ok {
							w.tl.scrollToGoroutine(gtx, item)
							ww = nil
						} else if ww.Cancelled() {
							ww = nil
						} else {
							macro := op.Record(gtx.Ops)

							// Draw full-screen overlay that prevents input to the timeline and closed the window if clicking
							// outside of it.
							//
							// XXX use constant for color
							paint.Fill(gtx.Ops, rgba(0x000000DD))
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

					w.tl.Layout(gtx)

					if cpuprofile != "" {
						op.InvalidateOp{}.Add(&ops)
					}
				}

				w.debugWindow.tlStart.addValue(gtx.Now, float64(w.tl.start))
				w.debugWindow.tlEnd.addValue(gtx.Now, float64(w.tl.end))
				w.debugWindow.tlY.addValue(gtx.Now, float64(w.tl.y))

				ev.Frame(&ops)
			}
		}
	}
}

func (w *MainWindow) loadTrace(t *Trace) {
	var end trace.Timestamp
	for _, g := range t.gs {
		if len(g.spans) > 0 {
			if d := g.spans.End(); d > end {
				end = d
			}
		}
	}
	for _, p := range t.ps {
		if len(p.spans) > 0 {
			if d := p.spans.End(); d > end {
				end = d
			}
		}
	}

	// Zoom out slightly beyond the end of the trace, so that the user can immediately tell that they're looking at the
	// entire trace.
	slack := float64(end) * 0.05
	start := trace.Timestamp(-slack)
	end = trace.Timestamp(float64(end) + slack)

	gsByID := map[uint64]*Goroutine{}
	for _, g := range t.gs {
		gsByID[g.id] = g
	}

	w.tl = Timeline{
		start:       start,
		end:         end,
		gs:          gsByID,
		theme:       w.theme,
		trace:       t,
		debugWindow: w.debugWindow,
	}
	w.tl.activity.displayAllLabels = true

	w.tl.axis = Axis{tl: &w.tl, theme: w.theme}
	w.tl.activities = make([]*ActivityWidget, 2, len(t.gs)+len(t.ps)+2)
	w.tl.activities[0] = NewGCWidget(w.theme, &w.tl, t, t.gc)
	w.tl.activities[1] = NewSTWWidget(w.theme, &w.tl, t, t.stw)
	for _, p := range t.ps {
		w.tl.activities = append(w.tl.activities, NewProcessorWidget(w.theme, &w.tl, p))
	}
	for _, g := range t.gs {
		w.tl.activities = append(w.tl.activities, NewGoroutineWidget(w.theme, &w.tl, g))
	}

	w.trace = t
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

//gcassert:inline
func withOps(gtx layout.Context, ops *op.Ops) layout.Context {
	gtx.Ops = ops
	return gtx
}

type Notification struct {
	message string
	shownAt time.Time
}

func (notif *Notification) Show(gtx layout.Context, msg string) {
	notif.message = msg
	notif.shownAt = gtx.Now
}

func (notif *Notification) Layout(gtx layout.Context, th *theme.Theme) layout.Dimensions {
	if gtx.Now.After(notif.shownAt.Add(1000 * time.Millisecond)) {
		return layout.Dimensions{}
	}

	// XXX compute width based on window size
	// TODO(dh): limit height to something sensible, just in case
	ngtx := gtx
	ngtx.Constraints.Max.X = 500
	macro := op.Record(gtx.Ops)
	dims := theme.BorderedText(ngtx, th, notif.message)
	call := macro.Stop()

	defer op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y-dims.Size.Y-gtx.Dp(30))).Push(gtx.Ops).Pop()
	call.Add(gtx.Ops)

	op.InvalidateOp{At: notif.shownAt.Add(1000 * time.Millisecond)}.Add(gtx.Ops)

	return dims
}

type durationNumberFormat uint8

const (
	durationNumberFormatSI durationNumberFormat = iota
	durationNumberFormatScientific
	durationNumberFormatExact
)

func (nf durationNumberFormat) format(d time.Duration) string {
	switch nf {
	case durationNumberFormatScientific:
		return scientificDuration(d, 2)
	case durationNumberFormatSI:
		switch {
		case d == 0:
			return "0"
		case d < time.Millisecond:
			return d.String()
		case d < time.Second:
			return d.Round(time.Microsecond).String()
		default:
			return d.Round(time.Millisecond).String()
		}
	case durationNumberFormatExact:
		return fmt.Sprintf("%.9f", d.Seconds())
	default:
		panic("unreachable")
	}
}

type GoroutineStats struct {
	g       *Goroutine
	stats   [stateLast]GoroutineStat
	mapping []int

	start, end trace.Timestamp

	sortCol        int
	sortDescending bool

	// TODO(dh): expose this as an option
	numberFormat durationNumberFormat

	columnClicks [7]gesture.Click
}

type GoroutineStat struct {
	count           int
	min, max, total time.Duration
	avg, p50        float32
	values          []time.Duration
}

func NewGoroutineStats(g *Goroutine, tr *Trace) *GoroutineStats {
	gst := &GoroutineStats{}

	for i := range g.spans {
		span := &g.spans[i]
		s := &gst.stats[span.state]
		s.count++
		d := tr.Duration(span)
		if d > s.max {
			s.max = d
		}
		if d < s.min || s.min == 0 {
			s.min = d
		}
		s.total += d
		s.values = append(s.values, d)
	}

	for state := range gst.stats {
		stat := &gst.stats[state]

		if len(stat.values) == 0 {
			continue
		}

		stat.avg = float32(stat.total) / float32(len(stat.values))

		sort.Slice(stat.values, func(i, j int) bool {
			return stat.values[i] < stat.values[j]
		})

		if len(stat.values)%2 == 0 {
			mid := len(stat.values) / 2
			stat.p50 = float32(stat.values[mid]+stat.values[mid-1]) / 2
		} else {
			stat.p50 = float32(stat.values[len(stat.values)/2])
		}
	}

	gst.mapping = make([]int, 0, len(gst.stats))

	for i := range gst.stats {
		s := &gst.stats[i]

		if len(s.values) == 0 {
			continue
		}

		gst.mapping = append(gst.mapping, i)
	}

	gst.start = g.spans.Start(tr)
	gst.end = g.spans.End()

	return gst
}

func sortStats[T constraints.Ordered](stats *[stateLast]GoroutineStat, mapping []int, descending bool, get func(*GoroutineStat) T) {
	if descending {
		slices.SortFunc(mapping, func(i, j int) bool {
			return get(&stats[i]) >= get(&stats[j])
		})
	} else {
		slices.SortFunc(mapping, func(i, j int) bool {
			return get(&stats[i]) < get(&stats[j])
		})
	}
}

func (gs *GoroutineStats) computeSizes(gtx layout.Context, th *theme.Theme) [numStatLabels]image.Point {
	// Column 1 and 2 (state and count) are sized individually, all other columns (min, max, ...) have the same width.
	// The last columns' labels are all roughly the same size and only differ by a few pixels, which would look
	// inconsistent. The values in the last columns all have the same width.
	//
	// We assume that all lines have the same height. This is an assumption shared by outlay.Grid.

	fLabel := goFonts[0].Font
	fLabel.Weight = text.Bold
	fContent := goFonts[0].Font

	var columnSizes [numStatLabels]image.Point

	shape := func(s string, f text.Font) image.Point {
		lines := th.Shaper.LayoutString(fLabel, fixed.I(gtx.Sp(th.TextSize)), gtx.Constraints.Max.X, gtx.Locale, s)
		firstLine := lines[0]
		spanWidth := firstLine.Width.Ceil()
		spanHeight := (firstLine.Ascent + firstLine.Descent).Ceil()

		return image.Point{spanWidth, spanHeight}
	}

	// Column 1 contains strings, so the width is that of the widest shaped string
	size := shape(statLabels[gs.numberFormat][0+numStatLabels], fLabel)
	for _, name := range stateNamesCapitalized {
		size2 := shape(name, fContent)
		if size2.X > size.X {
			size.X = size2.X
		}
	}
	columnSizes[0] = size

	// Column 2 contains numbers, so the width is either that of the column label or the widest number. Digits all have
	// the same width, so we only need to shape the largest number.
	size = shape(statLabels[gs.numberFormat][1+numStatLabels], fLabel)
	max := 0
	for _, stat := range gs.stats {
		if stat.count > max {
			max = stat.count
		}
	}
	size2 := shape(local.Sprintf("%d", max), fContent)
	if size2.X > size.X {
		size.X = size2.X
	}
	columnSizes[1] = size

	switch gs.numberFormat {
	case durationNumberFormatScientific:
		// The remaining columns contain numbers in scientific notation with fixed precision, so the width is either that of
		// the column label or that of "1.23E+99". We give all remaining columns the same size.
		size = shape("1.23E+99", fContent)
		for i := 2; i < numStatLabels; i++ {
			size2 := shape(statLabels[gs.numberFormat][i+numStatLabels], fLabel)
			if size2.X > size.X {
				size.X = size2.X
			}
		}
		for i := 2; i < numStatLabels; i++ {
			columnSizes[i] = size
		}

	default:
		// Format and shape each value to find the widest one. Unlike scientific notation, each column is sized
		// individually, in case one of them is much wider than the others.
		//
		// OPT(dh): we have to format and shape again in the Layout function. However, the number of rows are so few it
		// probably doesn't matter.

		for i := 2; i < numStatLabels; i++ {
			size = shape(statLabels[gs.numberFormat][i+numStatLabels], fLabel)
			for _, stat := range gs.stats {
				var v time.Duration
				switch i {
				case 2:
					v = stat.total
				case 3:
					v = stat.min
				case 4:
					v = stat.max
				case 5:
					v = time.Duration(stat.avg)
				case 6:
					v = time.Duration(stat.p50)
				default:
					panic("unreachable")
				}
				if size2 := shape(gs.numberFormat.format(v), fContent); size2.X > size.X {
					size.X = size2.X
				}
			}
			columnSizes[i] = size
		}
	}

	return columnSizes
}

func (gs *GoroutineStats) Layout(gtx layout.Context, th *theme.Theme) layout.Dimensions {
	for col := range gs.columnClicks {
		for _, ev := range gs.columnClicks[col].Events(gtx) {
			if ev.Type != gesture.TypeClick {
				continue
			}

			if col == gs.sortCol {
				gs.sortDescending = !gs.sortDescending
			} else {
				gs.sortCol = col
				gs.sortDescending = false
			}
			switch col {
			case 0:
				// OPT(dh): don't use sort.Slice, it allocates
				if gs.sortDescending {
					sort.Slice(gs.mapping, func(i, j int) bool {
						return gs.mapping[i] >= gs.mapping[j]
					})
				} else {
					sort.Slice(gs.mapping, func(i, j int) bool {
						return gs.mapping[i] < gs.mapping[j]
					})
				}
			case 1:
				// Count
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) int { return gs.count })
			case 2:
				// Total
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) time.Duration { return gs.total })
			case 3:
				// Min
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) time.Duration { return gs.min })
			case 4:
				// Max
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) time.Duration { return gs.max })
			case 5:
				// Avg
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) float32 { return gs.avg })
			case 6:
				// p50
				sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *GoroutineStat) float32 { return gs.p50 })
			default:
				panic("unreachable")
			}
		}
	}

	grid := mylayout.SmallGrid{
		RowPadding:    0,
		ColumnPadding: gtx.Dp(15),
	}

	// Compute the widest column label so that all columns have the same size (unless they need to be wider due to their
	// contents.)
	var labelSizes [3]image.Point
	for i := numStatLabels; i < 2*numStatLabels; i++ {
		f := goFonts[0].Font
		f.Weight = text.Bold

		l := statLabels[gs.numberFormat][i]
		lines := th.Shaper.LayoutString(f, fixed.I(gtx.Sp(th.TextSize)), gtx.Constraints.Max.X, gtx.Locale, l)
		firstLine := lines[0]
		spanWidth := firstLine.Width.Ceil()
		spanHeight := (firstLine.Ascent + firstLine.Descent).Ceil()

		j := i - numStatLabels
		if j > 2 {
			j = 2
		}
		if spanWidth > labelSizes[j].X {
			labelSizes[j].X = spanWidth
		}
		if spanHeight > labelSizes[j].Y {
			labelSizes[j].Y = spanHeight
		}
	}

	// There is probably no need to cache the sizes between frames. The window only redraws when it's being interacted
	// with, which may even change the sizes.
	sizes := gs.computeSizes(gtx, th)
	sizer := func(gtx layout.Context, row, col int) layout.Dimensions {
		return layout.Dimensions{Size: sizes[col]}
	}

	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		if row == 0 {
			var l string
			if col == gs.sortCol {
				if gs.sortDescending {
					l = statLabels[gs.numberFormat][col+numStatLabels]
				} else {
					l = statLabels[gs.numberFormat][col+numStatLabels*2]
				}
			} else {
				l = statLabels[gs.numberFormat][col]
			}

			s := spanWith(th, l, func(ss poortext.SpanStyle) poortext.SpanStyle {
				ss.Font.Weight = text.Bold
				return ss
			})
			poortext.Text(th.Shaper, s).Layout(gtx, func(i int) {
				pointer.CursorPointer.Add(gtx.Ops)
				gs.columnClicks[col].Add(gtx.Ops)
			})
		} else {
			row--
			n := gs.mapping[row]

			var l string
			switch col {
			case 0:
				// type
				l = stateNamesCapitalized[n]
			case 1:
				l = local.Sprintf("%d", gs.stats[n].count)
				if gs.stats[n].count == 0 {
					panic(row)
				}
			case 2:
				// total
				l = gs.numberFormat.format(gs.stats[n].total)
			case 3:
				// min
				l = gs.numberFormat.format(gs.stats[n].min)
			case 4:
				// max
				l = gs.numberFormat.format(gs.stats[n].max)
			case 5:
				// avg
				l = gs.numberFormat.format(time.Duration(gs.stats[n].avg))
			case 6:
				// p50
				l = gs.numberFormat.format(time.Duration(gs.stats[n].p50))
			default:
				panic("unreachable")
			}

			txt := poortext.Text(th.Shaper, span(th, l))
			if col != 0 {
				txt.Alignment = text.End
			}
			txt.Layout(gtx, nil)
		}

		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	return grid.Layout(gtx, len(gs.mapping)+1, 7, sizer, cellFn)
}

const numStatLabels = 7

var statLabels = [...][numStatLabels * 3]string{
	durationNumberFormatScientific: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
		"State▼", "Count▼", "Total (s)▼", "Min (s)▼", "Max (s)▼", "Avg (s)▼", "p50 (s)▼",
		"State▲", "Count▲", "Total (s)▲", "Min (s)▲", "Max (s)▲", "Avg (s)▲", "p50 (s)▲",
	},
	durationNumberFormatExact: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
		"State▼", "Count▼", "Total (s)▼", "Min (s)▼", "Max (s)▼", "Avg (s)▼", "p50 (s)▼",
		"State▲", "Count▲", "Total (s)▲", "Min (s)▲", "Max (s)▲", "Avg (s)▲", "p50 (s)▲",
	},
	durationNumberFormatSI: [...]string{
		"State", "Count", "Total", "Min", "Max", "Avg", "p50",
		"State▼", "Count▼", "Total▼", "Min▼", "Max▼", "Avg▼", "p50▼",
		"State▲", "Count▲", "Total▲", "Min▲", "Max▲", "Avg▲", "p50▲",
	},
}

var stateNamesCapitalized = [stateLast]string{
	stateInactive:                "Inactive",
	stateActive:                  "Active",
	stateGCIdle:                  "GC (idle)",
	stateGCDedicated:             "GC (dedicated)",
	stateBlocked:                 "Blocked",
	stateBlockedSend:             "Blocked (channel send)",
	stateBlockedRecv:             "Blocked (channel receive)",
	stateBlockedSelect:           "Blocked (select)",
	stateBlockedSync:             "Blocked (sync)",
	stateBlockedSyncOnce:         "Blocked (sync.Once)",
	stateBlockedSyncTriggeringGC: "Blocked (triggering GC)",
	stateBlockedCond:             "Blocked (sync.Cond)",
	stateBlockedNet:              "Blocked (pollable I/O)",
	stateBlockedGC:               "Blocked (GC)",
	stateBlockedSyscall:          "Blocking syscall",
	stateStuck:                   "Stuck",
	stateReady:                   "Ready",
	stateCreated:                 "Created",
	stateDone:                    "Done",
	stateGCMarkAssist:            "GC (mark assist)",
	stateGCSweep:                 "GC (sweep assist)",
}

func scientificDuration(d time.Duration, digits int) string {
	// TODO(dh): don't convert to float to use %e, implement our own algorithm
	return fmt.Sprintf("%.*e", digits, d.Seconds())
}

type Window interface {
	Run(win *app.Window) error
}

type GoroutineWindow struct {
	theme *theme.Theme
	trace *Trace
	g     *Goroutine

	stats *GoroutineStats
}

func (gwin *GoroutineWindow) Run(win *app.Window) error {
	events := Events{trace: gwin.trace, theme: gwin.theme}
	events.filter.showGoCreate.Value = true
	events.filter.showGoUnblock.Value = true
	events.filter.showGoSysCall.Value = true
	events.filter.showUserLog.Value = true
	for _, span := range gwin.g.spans {
		// XXX we don't need the slice, iterate over events in spans in the Events layouter
		events.allEvents = append(events.allEvents, span.Events(gwin.g.events, gwin.trace)...)
	}
	events.updateFilter()

	var ops op.Ops
	statsFoldable := theme.Foldable{
		Title: "Statistics",
		Theme: gwin.theme,
	}
	eventsFoldable := theme.Foldable{
		Title: "Events",
		Theme: gwin.theme,
	}
	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			gtx := layout.NewContext(&ops, ev)
			gtx.Constraints.Min = image.Point{}

			paint.Fill(gtx.Ops, colors[colorBackground])

			th := gwin.theme
			spans := []poortext.SpanStyle{
				spanWith(th, "Goroutine: ", func(ss poortext.SpanStyle) poortext.SpanStyle {
					ss.Font.Weight = text.Bold
					return ss
				}),
				span(th, local.Sprintf("%d\n", gwin.g.id)),

				spanWith(th, "Function: ", func(ss poortext.SpanStyle) poortext.SpanStyle { ss.Font.Weight = text.Bold; return ss }),
				spanWith(th, fmt.Sprintf("%s\n", gwin.g.function), func(ss poortext.SpanStyle) poortext.SpanStyle {
					// XXX make function clickable
					ss.Color = th.Palette.Link
					return ss
				}),

				spanWith(th, "Created at: ", func(ss poortext.SpanStyle) poortext.SpanStyle {
					ss.Font.Weight = text.Bold
					return ss
				}),
				span(th, fmt.Sprintf("%s\n", formatTimestamp(gwin.stats.start))),

				spanWith(th, "Returned at: ", func(ss poortext.SpanStyle) poortext.SpanStyle {
					ss.Font.Weight = text.Bold
					return ss
				}),
				span(th, fmt.Sprintf("%s\n", formatTimestamp(gwin.stats.end))),

				spanWith(th, "Lifetime: ", func(ss poortext.SpanStyle) poortext.SpanStyle {
					ss.Font.Weight = text.Bold
					return ss
				}),
				span(th, time.Duration(gwin.stats.end-gwin.stats.start).String()),
			}

			layout.Flex{Axis: layout.Vertical}.Layout(gtx,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return poortext.Text(gwin.theme.Shaper, spans...).Layout(gtx, nil)
				}),
				// XXX ideally the spacing would be one line high
				layout.Rigid(layout.Spacer{Height: 10}.Layout),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return statsFoldable.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return gwin.stats.Layout(gtx, gwin.theme)
					})
				}),
				// XXX ideally the spacing would be one line high
				layout.Rigid(layout.Spacer{Height: 10}.Layout),
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return eventsFoldable.Layout(gtx, events.Layout)
				}),
			)

			ev.Frame(gtx.Ops)
		}
	}

	return nil
}

type Events struct {
	theme     *theme.Theme
	trace     *Trace
	allEvents []EventID
	filter    struct {
		showGoCreate  widget.Bool
		showGoUnblock widget.Bool
		showGoSysCall widget.Bool
		showUserLog   widget.Bool
	}
	filteredEvents []EventID
	grid           outlay.Grid
}

func (evs *Events) updateFilter() {
	// OPT(dh): if all filters are set, all events are shown. if no filters are set, no events are shown. neither case
	//   requires us to check each event.
	evs.filteredEvents = evs.filteredEvents[:0]
	for _, ev := range evs.allEvents {
		var b bool
		switch evs.trace.Event(ev).Type {
		case trace.EvGoCreate:
			b = evs.filter.showGoCreate.Value
		case trace.EvGoUnblock:
			b = evs.filter.showGoUnblock.Value
		case trace.EvGoSysCall:
			b = evs.filter.showGoSysCall.Value
		case trace.EvUserLog:
			b = evs.filter.showUserLog.Value
		default:
			panic(fmt.Sprintf("unexpected type %v", evs.trace.Event(ev).Type))
		}

		if b {
			evs.filteredEvents = append(evs.filteredEvents, ev)
		}
	}
}

func (evs *Events) Layout(gtx layout.Context) layout.Dimensions {
	// XXX draw grid scrollbars

	if evs.filter.showGoCreate.Changed() ||
		evs.filter.showGoUnblock.Changed() ||
		evs.filter.showGoSysCall.Changed() ||
		evs.filter.showUserLog.Changed() {
		evs.updateFilter()
	}

	evs.grid.LockedRows = 1

	dimmer := func(axis layout.Axis, index, constraint int) int {
		switch axis {
		case layout.Vertical:
			// XXX return proper line height
			return 24
		case layout.Horizontal:
			// XXX don't guess the dimensions
			// XXX don't insist on a minimum if the window is too narrow or columns will overlap

			// XXX we do have to guess the dimensions. computing them accurately is too expensive if we have tens of
			// thousands of events because we can't shape them all. we can probably do some approximate math, pretending
			// the font is fixed width.
			switch index {
			case 0:
				return 200
			case 1:
				return 200
			case 2:
				w := constraint - 400
				if w < 0 {
					w = 0
				}
				return w
			default:
				panic("unreachable")
			}
		default:
			panic("unreachable")
		}
	}

	columns := [...]string{
		"Time", "Category", "Message",
	}

	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		if row == 0 {
			paint.ColorOp{Color: evs.theme.Palette.Foreground}.Add(gtx.Ops)
			return widget.Label{MaxLines: 1}.Layout(gtx, evs.theme.Shaper, text.Font{Weight: text.Bold}, evs.theme.TextSize, columns[col])
		} else {
			// XXX subtract padding from width

			ev := evs.trace.Event(evs.filteredEvents[row-1])
			// XXX poortext wraps our spans if the window is too small
			var labelSpans []poortext.SpanStyle
			switch col {
			case 0:
				labelSpans = []poortext.SpanStyle{
					span(evs.theme, formatTimestamp(ev.Ts)),
				}
			case 1:
				if ev.Type == trace.EvUserLog {
					labelSpans = []poortext.SpanStyle{span(evs.theme, evs.trace.Strings[ev.Args[trace.ArgUserLogKeyID]])}
				}
			case 2:
				switch ev.Type {
				case trace.EvGoCreate:
					// XXX linkify goroutine ID; clicking it should scroll to first event in the goroutine
					labelSpans = []poortext.SpanStyle{
						span(evs.theme, "Created "),
						spanWith(evs.theme, local.Sprintf("goroutine %d",
							ev.Args[trace.ArgGoCreateG]), func(s poortext.SpanStyle) poortext.SpanStyle {
							// XXX make clickable
							s.Color = evs.theme.Palette.Link
							return s
						}),
					}
				case trace.EvGoUnblock:
					// XXX linkify goroutine ID, clicking it should scroll to the corresponding event in the unblocked
					// goroutine
					labelSpans = []poortext.SpanStyle{
						span(evs.theme, "Unblocked "),
						spanWith(evs.theme, local.Sprintf("goroutine %d",
							ev.Args[trace.ArgGoUnblockG]), func(s poortext.SpanStyle) poortext.SpanStyle {
							// XXX make clickable
							s.Color = evs.theme.Palette.Link
							return s
						}),
					}
				case trace.EvGoSysCall:
					// XXX track syscalls in a separate list
					// XXX try to extract syscall name from stack trace
					labelSpans = []poortext.SpanStyle{
						span(evs.theme, "Syscall"),
					}
				case trace.EvUserLog:
					labelSpans = []poortext.SpanStyle{span(evs.theme, evs.trace.Strings[ev.Args[trace.ArgUserLogMessage]])}
				default:
					panic(fmt.Sprintf("unhandled type %v", ev.Type))
				}
			default:
				panic("unreachable")
			}
			// TODO(dh): clicking the entry should jump to it on the timeline
			// TODO(dh): hovering the entry should highlight the corresponding span marker
			paint.ColorOp{Color: evs.theme.Palette.Foreground}.Add(gtx.Ops)
			txt := poortext.Text(evs.theme.Shaper, labelSpans...)
			if col == 0 && row != 0 {
				txt.Alignment = text.End
			}
			return txt.Layout(gtx, nil)
		}
	}

	dims := layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoCreate, "Goroutine creations").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoUnblock, "Goroutine unblocks").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showGoSysCall, "Syscalls").Layout),
		layout.Rigid(layout.Spacer{Width: 10}.Layout),

		layout.Rigid(theme.CheckBox(evs.theme, &evs.filter.showUserLog, "User logs").Layout),
	)

	defer op.Offset(image.Pt(0, dims.Size.Y)).Push(gtx.Ops).Pop()
	return evs.grid.Layout(gtx, len(evs.filteredEvents)+1, len(columns), dimmer, cellFn)
}

func span(th *theme.Theme, text string) poortext.SpanStyle {
	return poortext.SpanStyle{
		Content: text,
		Size:    th.TextSize,
		Color:   th.Palette.Foreground,
		Font:    goFonts[0].Font,
	}
}

func spanWith(th *theme.Theme, text string, fn func(poortext.SpanStyle) poortext.SpanStyle) poortext.SpanStyle {
	return fn(span(th, text))
}

var local = message.NewPrinter(message.MatchLanguage("en"))

func formatTimestamp(ts trace.Timestamp) string {
	return local.Sprintf("%d ns", ts)
}

func main() {
	flag.StringVar(&cpuprofile, "debug.cpuprofile", "", "write CPU profile to this file")
	flag.StringVar(&memprofileLoad, "debug.memprofile-load", "", "write memory profile to this file after loading trace")
	flag.StringVar(&memprofileExit, "debug.memprofile-exit", "", "write meory profile to this file when exiting")
	flag.BoolVar(&disableCaching, "debug.disable-caching", false, "Disable caching")
	flag.BoolVar(&exitAfterLoading, "debug.exit-after-loading", false, "Exit after parsing and processing trace")
	flag.BoolVar(&exitAfterParsing, "debug.exit-after-parsing", false, "Exit after parsing trace")
	flag.Parse()

	if len(flag.Args()) != 1 {
		fmt.Fprintln(os.Stderr, "need one argument: path to trace")
		os.Exit(1)
	}

	mwin := NewMainWindow()
	if debug {
		go func() {
			win := app.NewWindow(app.Title("gotraceui - debug window"))
			mwin.debugWindow.Run(win)
		}()
	}

	commands := make(chan Command, 16)
	errs := make(chan error)
	go func() {
		commands <- Command{"setState", "loadingTrace"}
		t, err := loadTrace(flag.Args()[0], commands)
		if memprofileLoad != "" {
			writeMemprofile(memprofileLoad)
		}
		if err == errExitAfterLoading || err == errExitAfterParsing {
			errs <- err
			return
		}
		if err != nil {
			commands <- Command{"error", fmt.Errorf("couldn't load trace: %w", err)}
			return
		}
		commands <- Command{"loadTrace", t}
	}()
	go func() {
		win := app.NewWindow(app.Title("gotraceui"))
		// XXX handle error
		errs <- mwin.Run(win)
	}()

	go func() {
		if cpuprofile != "" {
			f, err := os.Create(cpuprofile)
			if err == nil {
				pprof.StartCPUProfile(f)
			} else {
				fmt.Fprintln(os.Stderr, "couldn't write CPU profile:", err)
			}
		}

	loop:
		for {
			select {
			case cmd := <-commands:
				switch cmd.command {
				case "setState", "setProgress", "loadTrace", "error":
					mwin.commands <- cmd
				default:
					panic(fmt.Sprintf("unknown command %s", cmd.command))
				}
			case err := <-errs:
				if err != nil {
					log.Println(err)
				}
				break loop
			}
		}

		if cpuprofile != "" {
			pprof.StopCPUProfile()
		}
		if memprofileExit != "" {
			writeMemprofile(memprofileExit)
		}
		os.Exit(0)
	}()
	app.Main()
}

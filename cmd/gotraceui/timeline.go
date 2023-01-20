package main

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"sort"
	"strings"
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
)

const (
	// XXX the label height depends on the font used
	timelineLabelHeightDp unit.Dp = 20
	timelineTrackHeightDp unit.Dp = 16
	timelineGapDp         unit.Dp = 5
	timelineTrackGapDp    unit.Dp = 2
)

type TimelineWidgetTrackKind uint8

const (
	TimelineWidgetTrackUnspecified TimelineWidgetTrackKind = iota
	TimelineWidgetTrackSampled
	TimelineWidgetTrackUserRegions
)

type TimelineWidget struct {
	// Inputs
	tracks            []Track
	trackWidgets      []TimelineWidgetTrack
	buildTrackWidgets func([]Track, []TimelineWidgetTrack)

	widgetTooltip   func(win *theme.Window, gtx layout.Context, tw *TimelineWidget) layout.Dimensions
	invalidateCache func(tw *TimelineWidget) bool
	cv              *Canvas
	item            any
	labelClick      gesture.Click
	label           string

	// Set to true by the Layout method. This is used to track which timelines have been shown during a frame.
	displayed bool

	labelClicks int

	pointerAt f32.Point
	hovered   bool

	// OPT(dh): Only one timeline can have hovered or activated spans, so we could track this directly in Canvas, and
	// save 48 bytes per timeline (which means per goroutine). However, the current API is cleaner, because
	// TimelineWidget doesn't have to mutate Timeline's state.
	navigatedSpans SpanSelector
	hoveredSpans   SpanSelector

	prevFrame struct {
		// State for reusing the previous frame's ops, to avoid redrawing from scratch if no relevant state has changed.
		hovered     bool
		forceLabel  bool
		compact     bool
		topBorder   bool
		constraints layout.Constraints
		ops         reusableOps
		call        op.CallOp
	}
}

type SpanTooltipState struct {
	spanSel           SpanSelector
	events            []ptrace.EventID
	eventsUnderCursor []ptrace.EventID
}

type Track struct {
	kind   TimelineWidgetTrackKind
	spans  SpanSelector
	events []ptrace.EventID
}

func Duration(spanSel SpanSelector) time.Duration {
	if spanSel.Size() == 0 {
		return 0
	}
	return time.Duration(spanSel.At(spanSel.Size()-1).End - spanSel.At(0).Start)
}

type SpanSelector interface {
	AllSpans() ptrace.Spans
	Size() int
	Slice(start, end int) SpanSelector
	At(index int) ptrace.Span
}

type spanSlice ptrace.Spans

func SliceToSpanSelector(spans ptrace.Spans) SpanSelector { return spanSlice(spans) }
func (spans spanSlice) AllSpans() ptrace.Spans            { return ptrace.Spans(spans) }
func (spans spanSlice) Size() int                         { return len(spans) }
func (spans spanSlice) Slice(start, end int) SpanSelector { return spans[start:end] }
func (spans spanSlice) At(index int) ptrace.Span          { return spans[index] }

type NoSpan struct{}

func (NoSpan) AllSpans() ptrace.Spans            { return nil }
func (NoSpan) Size() int                         { return 0 }
func (NoSpan) Slice(start, end int) SpanSelector { return NoSpan{} }
func (NoSpan) At(_ int) ptrace.Span              { panic("index out of bounds") }

func newZoomMenuItem(cv *Canvas, spanSel SpanSelector) *theme.MenuItem {
	return &theme.MenuItem{
		Label:    PlainLabel("Zoom"),
		Shortcut: key.ModShortcut.String() + "+LMB",
		Do: func(gtx layout.Context) {
			start := spanSel.At(0).Start
			end := spanSel.At(spanSel.Size() - 1).End
			cv.navigateTo(gtx, start, end, cv.y)
		},
	}
}

type TimelineWidgetTrack struct {
	*Track

	highlightSpan func(spanSel SpanSelector) bool
	// OPT(dh): pass slice to spanLabel to reuse memory between calls
	spanLabel       func(spanSel SpanSelector, tr *Trace, out []string) []string
	spanColor       func(spanSel SpanSelector, tr *Trace) [2]colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions
	spanContextMenu func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem

	// OPT(dh): Only one track can have hovered or activated spans, so we could track this directly in TimelineWidget,
	// and save 48 bytes per track. However, the current API is cleaner, because TimelineWidgetTrack doesn't have to
	// mutate TimelineWidget's state.
	navigatedSpans SpanSelector
	hoveredSpans   SpanSelector

	// op lists get reused between frames to avoid generating garbage
	ops                             [colorStateLast * 2]op.Ops
	outlinesOps                     reusableOps
	highlightedPrimaryOutlinesOps   reusableOps
	highlightedSecondaryOutlinesOps reusableOps
	eventsOps                       reusableOps
	labelsOps                       reusableOps

	pointerAt f32.Point
	hovered   bool
	click     gesture.Click

	// cached state
	prevFrame struct {
		dspSpans []struct {
			dspSpanSel     SpanSelector
			startPx, endPx float32
		}
	}
}

func (track *TimelineWidgetTrack) NavigatedSpans() SpanSelector {
	return track.navigatedSpans
}

func (track *TimelineWidgetTrack) HoveredSpans() SpanSelector {
	return track.hoveredSpans
}

func (tw *TimelineWidget) NavigatedSpans() SpanSelector {
	return tw.navigatedSpans
}

func (tw *TimelineWidget) HoveredSpans() SpanSelector {
	return tw.hoveredSpans
}

func (tw *TimelineWidget) LabelClicked() bool {
	if tw.labelClicks > 0 {
		tw.labelClicks--
		return true
	} else {
		return false
	}
}

func (tw *TimelineWidget) Height(gtx layout.Context) int {
	timelineGap := gtx.Dp(timelineGapDp)
	enabledTracks := 0
	for i := range tw.tracks {
		track := &tw.tracks[i]
		if track.kind != TimelineWidgetTrackSampled || tw.cv.timeline.displaySampleTracks {
			enabledTracks++
		}
	}
	if tw.cv.timeline.compact {
		return (gtx.Dp(timelineTrackHeightDp)+gtx.Dp(timelineTrackGapDp))*enabledTracks + timelineGap
	} else {
		return (gtx.Dp(timelineTrackHeightDp)+gtx.Dp(timelineTrackGapDp))*enabledTracks + gtx.Dp(timelineLabelHeightDp) + timelineGap
	}
}

// notifyHidden informs the widget that it is no longer visible.
func (tw *TimelineWidget) notifyHidden() {
	tw.trackWidgets = nil
}

func (tw *TimelineWidget) Layout(win *theme.Window, gtx layout.Context, forceLabel bool, compact bool, topBorder bool, trackSpanLabels *[]string) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidget.Layout").End()

	// TODO(dh): we could replace all uses of timelineHeight by using normal Gio widget patterns: lay out all the
	// tracks, sum their heights and the gaps we apply. We'd use a macro to get the total size and then set up the clip
	// and pointer input. When we reuse the previous frame and need to return dimensions, we should use a stored value
	// of the height. Making the change isn't currently worth it, though, because we still need TimelineWidget.Height to
	// exist so we can compute the scrollbar, and so we can jump to goroutines, which needs to compute an offset. In
	// both cases we don't want to lay out every widget to figure out its size.
	timelineHeight := tw.Height(gtx)
	timelineTrackGap := gtx.Dp(timelineTrackGapDp)
	timelineLabelHeight := gtx.Dp(timelineLabelHeightDp)

	tw.displayed = true

	tw.navigatedSpans = NoSpan{}
	tw.hoveredSpans = NoSpan{}

	var widgetClicked bool

	for _, e := range gtx.Events(tw) {
		ev := e.(pointer.Event)
		switch ev.Type {
		case pointer.Enter, pointer.Move:
			tw.hovered = true
			tw.pointerAt = ev.Position
		case pointer.Drag:
			tw.pointerAt = ev.Position
		case pointer.Leave, pointer.Cancel:
			tw.hovered = false
		case pointer.Press:
			// If any part of the widget has been clicked then don't reuse the cached macro; we have to figure out what
			// was clicked and react to it.
			//
			// OPT(dh): if it was the label that was clicked, then we can reuse the macro

			widgetClicked = true
		}
	}

	tw.labelClicks = 0
	// We're passing gtx.Queue instead of gtx to avoid allocations because of convT. This means gtx.Queue mustn't be
	// nil.
	for _, ev := range tw.labelClick.Events(gtx.Queue) {
		if ev.Type == gesture.TypeClick {
			if ev.Modifiers == 0 {
				tw.labelClicks++
			} else if ev.Modifiers == key.ModShortcut {
				// XXX this assumes that the first track is the widest one. This is currently true, but a brittle
				// assumption to make.
				tw.navigatedSpans = tw.tracks[0].spans
			}
		}
	}

	if !widgetClicked &&
		tw.cv.unchanged() &&
		!tw.hovered && !tw.prevFrame.hovered &&
		forceLabel == tw.prevFrame.forceLabel &&
		compact == tw.prevFrame.compact &&
		(tw.invalidateCache == nil || !tw.invalidateCache(tw)) &&
		topBorder == tw.prevFrame.topBorder &&
		gtx.Constraints == tw.prevFrame.constraints {

		// OPT(dh): instead of avoiding cached ops completely when the timeline is hovered, draw the tooltip
		// separately.
		tw.prevFrame.call.Add(gtx.Ops)
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, timelineHeight)}
	}

	tw.prevFrame.hovered = tw.hovered
	tw.prevFrame.forceLabel = forceLabel
	tw.prevFrame.compact = compact
	tw.prevFrame.topBorder = topBorder
	tw.prevFrame.constraints = gtx.Constraints

	origOps := gtx.Ops
	gtx.Ops = tw.prevFrame.ops.get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		tw.prevFrame.call = call
	}()

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, timelineHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: tw, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)

	if !compact {
		if tw.hovered || forceLabel || topBorder {
			// Draw border at top of the timeline
			paint.FillShape(gtx.Ops, colors[colorTimelineBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if tw.hovered || forceLabel {
			labelGtx := gtx
			labelGtx.Constraints.Min = image.Point{}
			labelDims := mywidget.TextLine{Color: colors[colorTimelineLabel]}.Layout(labelGtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, tw.label)

			stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
			pointer.InputOp{Tag: &tw.label, Types: pointer.Enter | pointer.Leave | pointer.Cancel | pointer.Move}.Add(gtx.Ops)
			tw.labelClick.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
			stack.Pop()
		}

		if tw.widgetTooltip != nil && tw.cv.timeline.showTooltips == showTooltipsBoth && tw.labelClick.Hovered() {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return tw.widgetTooltip(win, gtx, tw)
			})
		}

		defer op.Offset(image.Pt(0, timelineLabelHeight)).Push(gtx.Ops).Pop()
	}

	stack := op.TransformOp{}.Push(gtx.Ops)
	if tw.trackWidgets == nil {
		// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
		// timelines.
		out := make([]TimelineWidgetTrack, len(tw.tracks))
		if tw.buildTrackWidgets != nil {
			tw.buildTrackWidgets(tw.tracks, out)
		} else {
			for i := range tw.tracks {
				out[i].Track = &tw.tracks[i]
			}
		}
		tw.trackWidgets = out
	}

	for i := range tw.trackWidgets {
		track := &tw.trackWidgets[i]
		if track.kind == TimelineWidgetTrackSampled && !tw.cv.timeline.displaySampleTracks {
			continue
		}
		dims := track.Layout(win, gtx, tw.cv, trackSpanLabels)
		op.Offset(image.Pt(0, dims.Size.Y+timelineTrackGap)).Add(gtx.Ops)
		if spans := track.HoveredSpans(); spans.Size() != 0 {
			tw.hoveredSpans = spans
		}
		if spans := track.NavigatedSpans(); spans.Size() != 0 {
			tw.navigatedSpans = spans
		}
	}
	stack.Pop()

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, timelineHeight)}
}

func defaultSpanColor(spanSel SpanSelector) [2]colorIndex {
	if spanSel.Size() == 1 {
		return [2]colorIndex{stateColors[spanSel.At(0).State], 0}
	} else {
		// OPT(dh): this would benefit from iterators, for span selectors backed by data that isn't already made of
		// ptrace.Span
		spans := spanSel.AllSpans()
		c := stateColors[spans[0].State]
		for _, s := range spans[1:] {
			cc := stateColors[s.State]
			if cc != c {
				return [2]colorIndex{colorStateMerged, 0}
			}
		}
		return [2]colorIndex{c, colorStateMerged}
	}
}

type renderedSpansIterator struct {
	offset  int
	cv      *Canvas
	spanSel SpanSelector
	prevEnd trace.Timestamp
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut SpanSelector, startPx, endPx float32, ok bool) {
	offset := it.offset
	if offset >= it.spanSel.Size() {
		return nil, 0, 0, false
	}
	spans := it.spanSel.AllSpans()

	nsPerPx := float32(it.cv.nsPerPx)
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * float64(nsPerPx)))
	startOffset := offset
	cvStart := it.cv.start

	s := &spans[offset]
	offset++

	start := s.Start
	end := s.End
	if it.prevEnd > start {
		// The previous span was extended and grew into this span. This shifts our start position to the right.
		start = it.prevEnd
	}

	if time.Duration(end-start) < minSpanWidthD {
		// Merge all tiny spans until we find a span or gap that's big enough to stand on its own. We do not stop
		// merging after we've reached the minimum size because that can lead to multiple merges being next to each
		// other. Not only does this look bad, it is also prone to tiny spans toggling between two merged spans, and
		// previously merged spans becoming visible again when zooming out.
		for offset < len(spans) {
			adjustedEnd := end
			if time.Duration(end-start) < minSpanWidthD {
				adjustedEnd = start + trace.Timestamp(minSpanWidthD)
			}

			// For a span to be large enough to stand on its own, it has to end at least minSpanWidthD later than the
			// current span. Use binary search to find that span. This also finds gaps, because for a gap to be big
			// enough, it cannot occur between spans that would be too small according to this search.
			offset = sort.Search(len(spans), func(i int) bool {
				return spans[i].End >= adjustedEnd+trace.Timestamp(minSpanWidthD)
			})

			if offset == len(spans) {
				// We couldn't find a span -> merge all remaining spans
				offset = len(spans)
				end = spans[offset-1].End
				break
			} else {
				prevSpan := &spans[offset-1]
				candidateSpan := &spans[offset]

				cStart := candidateSpan.Start
				cEnd := candidateSpan.End
				prevEnd := prevSpan.End
				if adjustedEnd > cStart {
					cStart = adjustedEnd
				}
				if time.Duration(cEnd-cStart) >= minSpanWidthD || time.Duration(cStart-prevEnd) >= minSpanWidthD {
					end = spans[offset-1].End
					break
				} else {
					end = spans[offset].End
					offset++
				}
			}
		}
	}

	if time.Duration(end-start) < minSpanWidthD {
		// We're still too small, so extend the span to its minimum size.
		end = start + trace.Timestamp(minSpanWidthD)
	}

	it.offset = offset
	it.prevEnd = end
	startPx = float32(start-cvStart) / nsPerPx
	endPx = float32(end-cvStart) / nsPerPx
	return it.spanSel.Slice(startOffset, offset), startPx, endPx, true
}

func (track *TimelineWidgetTrack) Layout(win *theme.Window, gtx layout.Context, cv *Canvas, labelsOut *[]string) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Layout").End()

	tr := cv.trace
	trackHeight := gtx.Dp(timelineTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	spanHighlightedBorderWidth := gtx.Dp(spanHighlightedBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	track.navigatedSpans = NoSpan{}
	track.hoveredSpans = NoSpan{}

	trackNavigatedSpans := false
	trackContextMenuSpans := false
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
			if ev.Buttons == pointer.ButtonSecondary {
				trackContextMenuSpans = true
			}
		}
	}

	// We're passing gtx.Queue instead of gtx to avoid allocations because of convT. This means gtx.Queue mustn't be
	// nil.
	for _, ev := range track.click.Events(gtx.Queue) {
		if ev.Type != gesture.TypeClick {
			continue
		}
		if ev.Modifiers == key.ModShortcut {
			trackNavigatedSpans = true
		}
	}

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, trackHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: track, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)
	track.click.Add(gtx.Ops)

	// Draw timeline lifetimes
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
	var highlightedPrimaryOutlinesPath clip.Path
	var highlightedSecondaryOutlinesPath clip.Path
	var eventsPath clip.Path
	outlinesPath.Begin(track.outlinesOps.get())
	highlightedPrimaryOutlinesPath.Begin(track.highlightedPrimaryOutlinesOps.get())
	highlightedSecondaryOutlinesPath.Begin(track.highlightedSecondaryOutlinesOps.get())
	eventsPath.Begin(track.eventsOps.get())
	labelsOps := track.labelsOps.get()
	labelsMacro := op.Record(labelsOps)

	for i := range paths {
		paths[i].Begin(&track.ops[i])
	}

	first := true
	var prevEndPx float32
	doSpans := func(dspSpanSel SpanSelector, startPx, endPx float32) {
		hovered := false
		if track.hovered && track.pointerAt.X >= startPx && track.pointerAt.X < endPx {
			// Highlight the span under the cursor
			hovered = true
			track.hoveredSpans = dspSpanSel

			if trackNavigatedSpans {
				track.navigatedSpans = dspSpanSel
			}
			if trackContextMenuSpans {
				var items []theme.Widget
				if track.spanContextMenu != nil {
					cv.contextMenu = track.spanContextMenu(dspSpanSel, tr)
					for _, item := range cv.contextMenu {
						items = append(items, item.Layout)
					}
				} else {
					cv.contextMenu = []*theme.MenuItem{newZoomMenuItem(cv, dspSpanSel)}
					items = append(items, (cv.contextMenu[0]).Layout)
				}
				win.SetContextMenu(((&theme.MenuGroup{
					Items: items,
				}).Layout))
			}
		}

		var cs [2]colorIndex
		if track.spanColor != nil {
			cs = track.spanColor(dspSpanSel, tr)
		} else {
			cs = defaultSpanColor(dspSpanSel)
		}

		if cs[1] != 0 && cs[1] != colorStateMerged {
			panic(fmt.Sprintf("two-color spans are only supported with color₁ == colorStateMerged, got %v", cs))
		}

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt((max(startPx, 0)), 0)
		maxP = f32.Pt((min(endPx, float32(gtx.Constraints.Max.X))), float32(trackHeight))

		var highlighted bool
		if track.highlightSpan != nil {
			highlighted = track.highlightSpan(dspSpanSel)
		}
		if hovered {
			highlightedPrimaryOutlinesPath.MoveTo(minP)
			highlightedPrimaryOutlinesPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			highlightedPrimaryOutlinesPath.LineTo(maxP)
			highlightedPrimaryOutlinesPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			highlightedPrimaryOutlinesPath.Close()
		} else if highlighted {
			highlightedSecondaryOutlinesPath.MoveTo(minP)
			highlightedSecondaryOutlinesPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			highlightedSecondaryOutlinesPath.LineTo(maxP)
			highlightedSecondaryOutlinesPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			highlightedSecondaryOutlinesPath.Close()
		} else {
			// Draw outline as a rectangle, the span will draw on top of it so that only the outline remains.
			//
			// OPT(dh): for timelines that have no gaps between any of the spans this can be drawn as a single rectangle
			// covering all spans.
			outlinesPath.MoveTo(minP)
			outlinesPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			outlinesPath.LineTo(maxP)
			outlinesPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			outlinesPath.Close()
		}

		borderWidth := spanBorderWidth
		if hovered || highlighted {
			borderWidth = spanHighlightedBorderWidth
		}
		if first && startPx < 0 {
			// Never draw a left border for truncated spans
		} else if !first && startPx == prevEndPx && !(highlighted || hovered) {
			// Don't draw left border if it'd touch a right border, unless the span is highlighted
		} else {
			minP.X += float32(borderWidth)
		}
		prevEndPx = endPx

		minP.Y += float32(borderWidth)
		if endPx <= float32(gtx.Constraints.Max.X) {
			maxP.X -= float32(borderWidth)
		}
		maxP.Y -= float32(borderWidth)

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
		if cv.timeline.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= startPx && track.pointerAt.X < endPx {
			events := dspSpanSel.AllSpans().Events(track.events, tr.Trace)

			spanTooltipState = SpanTooltipState{
				spanSel: dspSpanSel,
				events:  events,
			}
		}

		dotRadiusX := float32(gtx.Dp(4))
		dotRadiusY := float32(gtx.Dp(3))
		if maxP.X-minP.X > dotRadiusX*2 && dspSpanSel.Size() == 1 {
			// We only display event dots in unmerged spans because merged spans can split into smaller spans when we
			// zoom in, causing dots to disappear and reappearappear and disappear.
			events := dspSpanSel.Slice(0, 1).AllSpans().Events(track.events, tr.Trace)

			dotGap := float32(gtx.Dp(4))
			centerY := float32(trackHeight) / 2

			for i := 0; i < len(events); i++ {
				ev := events[i]
				px := cv.tsToPx(tr.Event(ev).Ts)

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
					px := cv.tsToPx(tr.Event(ev).Ts)
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

				if cv.timeline.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= minX && track.pointerAt.X < maxX {
					spanTooltipState.eventsUnderCursor = events[oldi : i+1]
				}
			}
		}

		if spanTooltipState.spanSel != nil && track.spanTooltip != nil {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return track.spanTooltip(win, gtx, tr, spanTooltipState)
			})
		}

		if dspSpanSel.Size() == 1 && track.spanLabel != nil && maxP.X-minP.X > float32(2*minSpanWidth) {
			// The Label callback, if set, returns a list of labels to try and use for the span. We pick the first label
			// that fits fully in the span, as it would be drawn untruncated. That is, the ideal label size depends on
			// the zoom level, not panning. If no label fits, we use the last label in the list. This label can be the
			// empty string to effectively display no label.
			//
			// We don't try to render a label for very small spans.
			if *labelsOut = track.spanLabel(dspSpanSel, tr, (*labelsOut)[:0]); len(*labelsOut) > 0 {
				for _, label := range *labelsOut {
					if label == "" {
						continue
					}

					macro := op.Record(labelsOps)
					// OPT(dh): cache mapping from label to size. probably put a size limit on the cache, in case users generate millions of unique labels
					dims := mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(withOps(gtx, labelsOps), win.Theme.Shaper, text.Font{Weight: text.ExtraBold}, win.Theme.TextSize, label)
					if float32(dims.Size.X) > endPx-startPx {
						// This label doesn't fit. If the callback provided more labels, try those instead, otherwise
						// give up. Truncating labels is almost never a good idea and usually leads to ambiguous text.
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
					paint.ColorOp{Color: win.Theme.Palette.Foreground}.Add(labelsOps)
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

	if cv.unchanged() && track.prevFrame.dspSpans != nil {
		for _, prevSpans := range track.prevFrame.dspSpans {
			doSpans(prevSpans.dspSpanSel, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := track.prevFrame.dspSpans[:0]
		it := renderedSpansIterator{
			cv:      cv,
			spanSel: cv.visibleSpans(track.spans),
		}
		for {
			dspSpanSel, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}
			allDspSpans = append(allDspSpans, struct {
				dspSpanSel     SpanSelector
				startPx, endPx float32
			}{dspSpanSel, startPx, endPx})
			doSpans(dspSpanSel, startPx, endPx)
		}
		track.prevFrame.dspSpans = allDspSpans
	}

	// First draw the outlines. We draw these as solid rectangles and let the spans overlay them.
	//
	// Drawing solid rectangles that get covered up seems to be much faster than using strokes, at least in this
	// specific instance.
	paint.FillShape(gtx.Ops, colors[colorSpanOutline], clip.Outline{Path: outlinesPath.End()}.Op())
	paint.FillShape(gtx.Ops, colors[colorSpanHighlightedSecondaryOutline], clip.Outline{Path: highlightedSecondaryOutlinesPath.End()}.Op())
	paint.FillShape(gtx.Ops, colors[colorSpanHighlightedPrimaryOutline], clip.Outline{Path: highlightedPrimaryOutlinesPath.End()}.Op())

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
	paint.FillShape(gtx.Ops, rgba(0x000000DD), clip.Outline{Path: eventsPath.End()}.Op())

	// Finally print labels on top
	labelsMacro.Stop().Add(gtx.Ops)

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, trackHeight)}
}

type ProcessorTooltip struct {
	p     *ptrace.Processor
	trace *Trace
}

func (tt ProcessorTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.ProcessorTooltip.Layout").End()

	// OPT(dh): compute statistics once, not on every frame

	tr := tt.trace
	d := time.Duration(tr.Events[len(tr.Events)-1].Ts)

	var userD, gcD time.Duration
	for i := range tt.p.Spans {
		s := &tt.p.Spans[i]
		d := s.Duration()

		ev := tr.Events[s.Event]
		switch ev.Type {
		case trace.EvGoStart:
			userD += d
		case trace.EvGoStartLabel:
			gcD += d
		default:
			panic(fmt.Sprintf("unexepcted event type %d", ev.Type))
		}
	}

	userPct := float32(userD) / float32(d) * 100
	gcPct := float32(gcD) / float32(d) * 100
	inactiveD := d - userD - gcD
	inactivePct := float32(inactiveD) / float32(d) * 100

	l := local.Sprintf(
		"Processor %[1]d\n"+
			"Spans: %[2]d\n"+
			"Time running user code: %[3]s (%.2[4]f%%)\n"+
			"Time running GC workers: %[5]s (%.2[6]f%%)\n"+
			"Time inactive: %[7]s (%.2[8]f%%)",
		tt.p.ID,
		len(tt.p.Spans),
		roundDuration(userD), userPct,
		roundDuration(gcD), gcPct,
		roundDuration(inactiveD), inactivePct,
	)

	return theme.Tooltip{}.Layout(win, gtx, l)
}

func processorSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
		ev := tr.Event(s.Event)
		if s.State != ptrace.StateRunningG {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		g := tr.G(ev.G)
		label = local.Sprintf("Goroutine %d: %s\n", ev.G, g.Function)
	} else {
		label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
	}
	// OPT(dh): don't materialize all spans just to compute the duration
	label += fmt.Sprintf("Duration: %s", roundDuration(Duration(state.spanSel)))
	return theme.Tooltip{}.Layout(win, gtx, label)
}

type MachineTooltip struct {
	m     *ptrace.Machine
	trace *Trace
}

func (tt MachineTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.MachineTooltip.Layout").End()

	// OPT(dh): compute statistics once, not on every frame

	tr := tt.trace
	d := time.Duration(tr.Events[len(tr.Events)-1].Ts)

	var procD, syscallD time.Duration
	for i := range tt.m.Spans {
		s := &tt.m.Spans[i]
		d := s.Duration()

		ev := tr.Events[s.Event]
		switch ev.Type {
		case trace.EvProcStart:
			procD += d
		case trace.EvGoSysBlock:
			syscallD += d
		default:
			panic(fmt.Sprintf("unexepcted event type %d", ev.Type))
		}
	}

	procPct := float32(procD) / float32(d) * 100
	syscallPct := float32(syscallD) / float32(d) * 100
	inactiveD := d - procD - syscallD
	inactivePct := float32(inactiveD) / float32(d) * 100

	l := local.Sprintf(
		"Machine %[1]d\n"+
			"Spans: %[2]d\n"+
			"Time running processors: %[3]s (%.2[4]f%%)\n"+
			"Time blocked in syscalls: %[5]s (%.2[6]f%%)\n"+
			"Time inactive: %[7]s (%.2[8]f%%)",
		tt.m.ID,
		len(tt.m.Spans),
		roundDuration(procD), procPct,
		roundDuration(syscallD), syscallPct,
		roundDuration(inactiveD), inactivePct,
	)

	return theme.Tooltip{}.Layout(win, gtx, l)
}

func NewMachineWidget(cv *Canvas, m *ptrace.Machine) *TimelineWidget {
	if !supportMachineTimelines {
		panic("NewMachineWidget was called despite supportmachineActivities == false")
	}
	tr := cv.trace
	return &TimelineWidget{
		tracks: []Track{
			{spans: SliceToSpanSelector(m.Spans)},
			{spans: SliceToSpanSelector(m.Goroutines)},
		},
		cv:    cv,
		item:  m,
		label: local.Sprintf("Machine %d", m.ID),

		buildTrackWidgets: func(tracks []Track, out []TimelineWidgetTrack) {
			for i := range tracks {
				track := &tracks[i]
				switch i {
				case 0:
					out[i] = TimelineWidgetTrack{
						Track: track,
						highlightSpan: func(spanSel SpanSelector) bool {
							if htw := cv.timeline.hoveredTimeline; htw != nil {
								var target int32
								switch hitem := htw.item.(type) {
								case *ptrace.Processor:
									target = hitem.ID
								case *ptrace.Machine:
									if cv.timeline.hoveredSpans.Size() != 1 {
										return false
									}
									o := cv.timeline.hoveredSpans.At(0)
									if o.State != ptrace.StateRunningP {
										return false
									}
									target = tr.Event(o.Event).P
								default:
									return false
								}
								for _, span := range spanSel.AllSpans() {
									// OPT(dh): don't be O(n)
									if span.State == ptrace.StateRunningP && tr.Event(span.Event).P == target {
										return true
									}
								}
							}
							return false
						},
						spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							s := spanSel.At(0)
							switch s.State {
							case ptrace.StateRunningP:
								p := tr.P(tr.Event(s.Event).P)
								labels := tr.processorSpanLabels(p)
								return append(out, labels...)
							case ptrace.StateBlockedSyscall:
								return append(out, "syscall")
							default:
								panic(fmt.Sprintf("unexpected state %d", s.State))
							}
						},
						spanTooltip: func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
							var label string
							if state.spanSel.Size() == 1 {
								s := state.spanSel.At(0)
								ev := tr.Event(s.Event)
								switch s.State {
								case ptrace.StateRunningP:
									label = local.Sprintf("Processor %d\n", ev.P)
								case ptrace.StateBlockedSyscall:
									label = "In blocking syscall\n"
								default:
									panic(fmt.Sprintf("unexpected state %d", s.State))
								}
							} else {
								label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
							}
							label += fmt.Sprintf("Duration: %s", roundDuration(Duration(state.spanSel)))
							return theme.Tooltip{}.Layout(win, gtx, label)
						},
						spanContextMenu: func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem {
							var items []*theme.MenuItem
							items = append(items, newZoomMenuItem(cv, spanSel))

							if spanSel.Size() == 1 {
								s := spanSel.At(0)
								switch s.State {
								case ptrace.StateRunningP:
									pid := tr.Event(s.Event).P
									items = append(items, &theme.MenuItem{
										Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
										Do: func(gtx layout.Context) {
											cv.scrollToTimeline(gtx, tr.P(pid))
										},
									})
								case ptrace.StateBlockedSyscall:
								default:
									panic(fmt.Sprintf("unexpected state %d", s.State))
								}
							}

							return items
						},
					}
				case 1:
					out[i] = TimelineWidgetTrack{
						Track: track,
						highlightSpan: func(spanSel SpanSelector) bool {
							if htw := cv.timeline.hoveredTimeline; htw != nil {
								var target uint64
								switch hitem := htw.item.(type) {
								case *ptrace.Goroutine:
									target = hitem.ID
								case *ptrace.Processor:
									if cv.timeline.hoveredSpans.Size() != 1 {
										return false
									}
									o := cv.timeline.hoveredSpans.At(0)
									if o.State != ptrace.StateRunningG {
										return false
									}
									target = tr.Event(o.Event).G
								case *ptrace.Machine:
									if cv.timeline.hoveredSpans.Size() != 1 {
										return false
									}
									o := cv.timeline.hoveredSpans.At(0)
									if o.State != ptrace.StateRunningG {
										return false
									}
									target = tr.Event(o.Event).G
								default:
									return false
								}
								for _, span := range spanSel.AllSpans() {
									// OPT(dh): don't be O(n)
									if tr.Event(span.Event).G == target {
										return true
									}
								}
							}
							return false
						},
						spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							g := tr.G(tr.Event(spanSel.At(0).Event).G)
							labels := tr.goroutineSpanLabels(g)
							return append(out, labels...)
						},
						spanColor: func(spanSel SpanSelector, tr *Trace) [2]colorIndex {
							do := func(s ptrace.Span, tr *Trace) colorIndex {
								gid := tr.Events[s.Event].G
								g := tr.G(gid)
								switch fn := g.Function.Fn; fn {
								case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
									return colorStateGC
								default:
									return stateColors[s.State]
								}
							}

							if spanSel.Size() == 1 {
								return [2]colorIndex{do(spanSel.At(0), tr), 0}
							} else {
								spans := spanSel.AllSpans()
								c := do(spans[0], tr)
								for _, s := range spans[1:] {
									// OPT(dh): this can get very expensive; imagine a merged span with millions of spans, all
									// with the same color.
									cc := do(s, tr)
									if cc != c {
										return [2]colorIndex{colorStateMerged, 0}
									}
								}
								return [2]colorIndex{c, colorStateMerged}
							}
						},
						spanTooltip: processorSpanTooltip,
						spanContextMenu: func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem {
							var items []*theme.MenuItem
							items = append(items, newZoomMenuItem(cv, spanSel))

							if spanSel.Size() == 1 {
								s := spanSel.At(0)
								switch s.State {
								case ptrace.StateRunningG:
									gid := tr.Event(s.Event).G
									items = append(items, &theme.MenuItem{
										Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
										Do: func(gtx layout.Context) {
											cv.scrollToTimeline(gtx, tr.G(gid))
										},
									})
								default:
									panic(fmt.Sprintf("unexpected state %d", s.State))
								}
							}

							return items
						},
					}
				}
			}
		},

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tw *TimelineWidget) layout.Dimensions {
			return MachineTooltip{m, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: func(tw *TimelineWidget) bool {
			if cv.prevFrame.hoveredTimeline != cv.timeline.hoveredTimeline {
				return true
			}

			if cv.prevFrame.hoveredSpans.Size() == 0 && cv.timeline.hoveredSpans.Size() == 0 {
				// Nothing hovered in either frame.
				return false
			}

			if cv.prevFrame.hoveredSpans.Size() > 1 && cv.timeline.hoveredSpans.Size() > 1 {
				// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
				// frames, then nothing changes for rendering.
				return false
			}

			if cv.prevFrame.hoveredSpans.Size() != cv.timeline.hoveredSpans.Size() {
				// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
				// previously highlighted.
				//
				// The number of hovered spans changed, and at least in one frame the number was 1.
				return true
			}

			// If we got to this point, then both slices have exactly one element.
			if tr.Event(cv.prevFrame.hoveredSpans.At(0).Event).P != tr.Event(cv.timeline.hoveredSpans.At(0).Event).P {
				return true
			}

			return false
		},
	}
}

func NewProcessorWidget(cv *Canvas, p *ptrace.Processor) *TimelineWidget {
	tr := cv.trace
	return &TimelineWidget{
		tracks: []Track{{spans: SliceToSpanSelector(p.Spans)}},

		buildTrackWidgets: func(tracks []Track, out []TimelineWidgetTrack) {
			for i := range tracks {
				track := &tracks[i]
				out[i] = TimelineWidgetTrack{
					Track: track,
					highlightSpan: func(spanSel SpanSelector) bool {
						if htw := cv.timeline.hoveredTimeline; htw != nil {
							target := struct {
								g          uint64
								start, end trace.Timestamp
							}{^uint64(0), -1, -1}
							switch hitem := htw.item.(type) {
							case *ptrace.Goroutine:
								if cv.timeline.hoveredSpans.Size() == 0 {
									// A goroutine timeline is hovered, but no spans within are.
									target.g = hitem.ID
								} else {
									// A (merged) span in a goroutine timeline is hovered. Highlight processor spans for
									// the same goroutine if they overlap with the highlighted span.
									target.g = hitem.ID
									target.start = cv.timeline.hoveredSpans.At(0).Start
									target.end = cv.timeline.hoveredSpans.At(cv.timeline.hoveredSpans.Size() - 1).End
								}
							case *ptrace.Processor:
								if cv.timeline.hoveredSpans.Size() != 1 {
									return false
								}
								o := cv.timeline.hoveredSpans.At(0)
								target.g = tr.Event(o.Event).G
							case *ptrace.Machine:
								if cv.timeline.hoveredSpans.Size() != 1 {
									return false
								}
								o := cv.timeline.hoveredSpans.At(0)
								if o.State != ptrace.StateRunningG {
									return false
								}
								target.g = tr.Event(o.Event).G

							default:
								return false
							}
							for _, span := range spanSel.AllSpans() {
								// OPT(dh): don't be O(n)
								if tr.Event(span.Event).G == target.g && (target.start == -1 || ((target.start < span.End) && (target.end >= span.Start))) {
									return true
								}
							}
						}
						return false
					},
					spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
						if spanSel.Size() != 1 {
							return out
						}
						g := tr.G(tr.Event(spanSel.At(0).Event).G)
						labels := tr.goroutineSpanLabels(g)
						return append(out, labels...)
					},
					spanColor: func(spanSel SpanSelector, tr *Trace) [2]colorIndex {
						do := func(s ptrace.Span, tr *Trace) colorIndex {
							gid := tr.Events[s.Event].G
							g := tr.G(gid)
							switch fn := g.Function.Fn; fn {
							case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
								return colorStateGC
							default:
								// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
								return stateColors[s.State]
							}
						}

						if spanSel.Size() == 1 {
							return [2]colorIndex{do(spanSel.At(0), tr), 0}
						} else {
							spans := spanSel.AllSpans()
							c := do(spans[0], tr)
							for _, s := range spans[1:] {
								// OPT(dh): this can get very expensive; imagine a merged span with millions of spans, all
								// with the same color.
								cc := do(s, tr)
								if cc != c {
									return [2]colorIndex{colorStateMerged, 0}
								}
							}
							return [2]colorIndex{c, colorStateMerged}
						}
					},
					spanTooltip: processorSpanTooltip,
					spanContextMenu: func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem {
						var items []*theme.MenuItem
						items = append(items, newZoomMenuItem(cv, spanSel))

						if spanSel.Size() == 1 {
							gid := tr.Event((spanSel.At(0).Event)).G
							items = append(items, &theme.MenuItem{
								Label: PlainLabel(local.Sprintf("Scroll to goroutine %d", gid)),
								Do: func(gtx layout.Context) {
									cv.scrollToTimeline(gtx, tr.G(gid))
								},
							})
						}

						return items
					},
				}
			}
		},

		widgetTooltip: func(win *theme.Window, gtx layout.Context, tw *TimelineWidget) layout.Dimensions {
			return ProcessorTooltip{p, cv.trace}.Layout(win, gtx)
		},
		invalidateCache: func(tw *TimelineWidget) bool {
			if cv.prevFrame.hoveredTimeline != cv.timeline.hoveredTimeline {
				return true
			}

			if cv.prevFrame.hoveredSpans.Size() == 0 && cv.timeline.hoveredSpans.Size() == 0 {
				// Nothing hovered in either frame.
				return false
			}

			if cv.prevFrame.hoveredSpans.Size() > 1 && cv.timeline.hoveredSpans.Size() > 1 {
				// We don't highlight spans if a merged span has been hovered, so if we hovered merged spans in both
				// frames, then nothing changes for rendering.
				return false
			}

			if cv.prevFrame.hoveredSpans.Size() != cv.timeline.hoveredSpans.Size() {
				// OPT(dh): If we go from 1 hovered to not 1 hovered, then we only have to redraw if any spans were
				// previously highlighted.
				//
				// The number of hovered spans changed, and at least in one frame the number was 1.
				return true
			}

			// If we got to this point, then both slices have exactly one element.
			if tr.Event(cv.prevFrame.hoveredSpans.At(0).Event).G != tr.Event(cv.timeline.hoveredSpans.At(0).Event).G {
				return true
			}

			return false
		},
		cv:    cv,
		item:  p,
		label: local.Sprintf("Processor %d", p.ID),
	}
}

type GoroutineTooltip struct {
	g     *ptrace.Goroutine
	trace *Trace
}

func (tt GoroutineTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineTooltip.Layout").End()

	start := tt.g.Spans.Start()
	end := tt.g.Spans.End()
	d := time.Duration(end - start)

	var fnName string
	line1 := "Goroutine %[1]d\n\n"
	if tt.g.Function.Fn != "" {
		fnName = tt.g.Function.Fn
		line1 = "Goroutine %[1]d: %[2]s\n\n"
	}
	blocked := tt.g.Statistics.Blocked()
	inactive := tt.g.Statistics.Inactive()
	gcAssist := tt.g.Statistics.GCAssist()
	running := tt.g.Statistics.Running()
	blockedPct := float32(blocked) / float32(d) * 100
	inactivePct := float32(inactive) / float32(d) * 100
	gcAssistPct := float32(gcAssist) / float32(d) * 100
	runningPct := float32(running) / float32(d) * 100
	l := local.Sprintf(line1+
		"Created at: %[3]s\n"+
		"Returned at: %[4]s\n"+
		"Lifetime: %[5]s\n"+
		"Spans: %[14]d\n"+
		"Time in blocked states: %[6]s (%.2[7]f%%)\n"+
		"Time in inactive states: %[8]s (%.2[9]f%%)\n"+
		"Time in GC assist: %[10]s (%.2[11]f%%)\n"+
		"Time in running states: %[12]s (%.2[13]f%%)",
		tt.g.ID, fnName,
		formatTimestamp(start),
		formatTimestamp(end),
		roundDuration(d),
		roundDuration(blocked), blockedPct,
		roundDuration(inactive), inactivePct,
		roundDuration(gcAssist), gcAssistPct,
		roundDuration(running), runningPct,
		len(tt.g.Spans),
	)

	return theme.Tooltip{}.Layout(win, gtx, l)
}

var reasonLabels = [256]string{
	reasonNewlyCreated: "newly created",
	reasonGosched:      "called runtime.Gosched",
	reasonTimeSleep:    "called time.Sleep",
	reasonPreempted:    "got preempted",
}

func unblockedByGoroutine(tr *Trace, s ptrace.Span) (uint64, bool) {
	ev := tr.Event(s.Event)
	switch s.State {
	case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
		ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
		if link := ptrace.EventID(ev.Link); link != -1 {
			// g0 unblocks goroutines that are blocked on pollable I/O, for example.
			if g := tr.Event(link).G; g != 0 {
				return g, true
			}
		}
	}
	return 0, false
}

func goroutineSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if debug {
		label += local.Sprintf("Event ID: %d\n", state.spanSel.At(0).Event)
		label += fmt.Sprintf("Event type: %d\n", tr.Event(state.spanSel.At(0).Event).Type)
	}
	label += "State: "
	var at string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
		ev := tr.Event(s.Event)
		if at == "" && ev.StkID > 0 {
			at = tr.PCs[tr.Stacks[ev.StkID][s.At]].Fn
		}
		switch state := s.State; state {
		case ptrace.StateInactive:
			label += "inactive"
		case ptrace.StateActive:
			label += "active"
		case ptrace.StateGCDedicated:
			label += "GC (dedicated)"
		case ptrace.StateGCIdle:
			label += "GC (idle)"
		case ptrace.StateBlocked:
			label += "blocked"
		case ptrace.StateBlockedSend:
			label += "blocked on channel send"
		case ptrace.StateBlockedRecv:
			label += "blocked on channel recv"
		case ptrace.StateBlockedSelect:
			label += "blocked on select"
		case ptrace.StateBlockedSync:
			label += "blocked on mutex"
		case ptrace.StateBlockedSyncOnce:
			label += "blocked on sync.Once"
		case ptrace.StateBlockedSyncTriggeringGC:
			label += "blocked triggering GC"
		case ptrace.StateBlockedCond:
			label += "blocked on condition variable"
		case ptrace.StateBlockedNet:
			label += "blocked on polled I/O"
		case ptrace.StateBlockedGC:
			label += "GC assist wait"
		case ptrace.StateBlockedSyscall:
			label += "blocked on syscall"
		case ptrace.StateStuck:
			label += "stuck"
		case ptrace.StateReady:
			label += "ready"
		case ptrace.StateCreated:
			label += "ready"
		case ptrace.StateGCMarkAssist:
			label += "GC mark assist"
		case ptrace.StateGCSweep:
			label += "GC sweep"
			if ev.Link != -1 {
				l := tr.Events[ev.Link]
				label += local.Sprintf("\nSwept %d bytes, reclaimed %d bytes",
					l.Args[trace.ArgGCSweepDoneSwept], l.Args[trace.ArgGCSweepDoneReclaimed])
			}
		default:
			if debug {
				panic(fmt.Sprintf("unhandled state %d", state))
			}
		}

		tags := make([]string, 0, 4)
		if s.Tags&ptrace.SpanTagRead != 0 {
			tags = append(tags, "read")
		}
		if s.Tags&ptrace.SpanTagAccept != 0 {
			tags = append(tags, "accept")
		}
		if s.Tags&ptrace.SpanTagDial != 0 {
			tags = append(tags, "dial")
		}
		if s.Tags&ptrace.SpanTagNetwork != 0 {
			tags = append(tags, "network")
		}
		if s.Tags&ptrace.SpanTagTCP != 0 {
			tags = append(tags, "TCP")
		}
		if s.Tags&ptrace.SpanTagTLS != 0 {
			tags = append(tags, "TLS")
		}
		if s.Tags&ptrace.SpanTagHTTP != 0 {
			tags = append(tags, "HTTP")
		}
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}

		if g, ok := unblockedByGoroutine(tr, s); ok {
			label += local.Sprintf("\nUnblocked by goroutine %d (%s)", g, tr.G(g).Function)
		}
	} else {
		label += local.Sprintf("mixed (%d spans)", state.spanSel.Size())
	}
	label += "\n"

	if state.spanSel.Size() == 1 {
		if reason := reasonLabels[tr.Reason(state.spanSel.At(0))]; reason != "" {
			label += "Reason: " + reason + "\n"
		}
	}

	if at != "" {
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("In: %s\n", at)
	}
	if state.spanSel.Size() == 1 {
		switch state.spanSel.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			pid := tr.Event(state.spanSel.At(0).Event).P
			label += local.Sprintf("On: processor %d\n", pid)
		}
	}

	label += fmt.Sprintf("Duration: %s\n", roundDuration(Duration(state.spanSel)))

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

	return theme.Tooltip{}.Layout(win, gtx, label)
}

func userRegionSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
		ev := tr.Event(s.Event)
		if s.State != ptrace.StateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		if taskID := ev.Args[trace.ArgUserRegionTaskID]; taskID != 0 {
			label = local.Sprintf("User region: %s\nTask: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], tr.Task(taskID).Name)
		} else {
			label = local.Sprintf("User region: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]])
		}
	} else {
		label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
	}
	label += fmt.Sprintf("Duration: %s", roundDuration(Duration(state.spanSel)))
	return theme.Tooltip{}.Layout(win, gtx, label)
}

var spanStateLabels = [...][]string{
	ptrace.StateGCDedicated:             {"GC (dedicated)", "D"},
	ptrace.StateGCIdle:                  {"GC (idle)", "I"},
	ptrace.StateBlockedCond:             {"sync.Cond"},
	ptrace.StateBlockedGC:               {"GC assist wait", "W"},
	ptrace.StateBlockedNet:              {"I/O"},
	ptrace.StateBlockedRecv:             {"recv"},
	ptrace.StateBlockedSelect:           {"select"},
	ptrace.StateBlockedSend:             {"send"},
	ptrace.StateBlockedSync:             {"sync"},
	ptrace.StateBlockedSyncOnce:         {"sync.Once"},
	ptrace.StateBlockedSyncTriggeringGC: {"triggering GC", "T"},
	ptrace.StateBlockedSyscall:          {"syscall"},
	ptrace.StateGCMarkAssist:            {"GC mark assist", "M"},
	ptrace.StateGCSweep:                 {"GC sweep", "S"},
	ptrace.StateStuck:                   {"stuck"},
	ptrace.StateLast:                    nil,
}

func NewGoroutineWidget(cv *Canvas, g *ptrace.Goroutine) *TimelineWidget {
	var l string
	if g.Function.Fn != "" {
		l = local.Sprintf("goroutine %d: %s", g.ID, g.Function.Fn)
	} else {
		l = local.Sprintf("goroutine %d", g.ID)
	}

	tw := &TimelineWidget{
		tracks: []Track{{
			spans:  SliceToSpanSelector(g.Spans),
			events: g.Events,
		}},
		buildTrackWidgets: func(tracks []Track, out []TimelineWidgetTrack) {
			sampledTrackBase := -1
			for i := range tracks {
				i := i

				track := &tracks[i]
				switch track.kind {
				case TimelineWidgetTrackUnspecified:
					out[i] = TimelineWidgetTrack{
						Track: track,
						spanLabel: func(spanSel SpanSelector, _ *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							return append(out, spanStateLabels[spanSel.At(0).State]...)
						},
						spanTooltip: goroutineSpanTooltip,
						spanContextMenu: func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem {
							var items []*theme.MenuItem
							items = append(items, newZoomMenuItem(cv, spanSel))

							if spanSel.Size() == 1 {
								switch spanSel.At(0).State {
								case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
									// These are the states that are actually on-CPU
									pid := tr.Event((spanSel.At(0).Event)).P
									items = append(items, &theme.MenuItem{
										Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
										Do: func(gtx layout.Context) {
											cv.scrollToTimeline(gtx, tr.P(tr.Event((spanSel.At(0).Event)).P))
										},
									})

								case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
									ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
									gid, ok := unblockedByGoroutine(tr, spanSel.At(0))
									if ok {
										items = append(items, &theme.MenuItem{
											Label: PlainLabel(local.Sprintf("Scroll to unblocking goroutine %d", gid)),
											Do: func(gtx layout.Context) {
												gid, _ := unblockedByGoroutine(tr, spanSel.At(0))
												cv.scrollToTimeline(gtx, tr.G(gid))
											},
										})
									}
								}
							}

							return items
						},
					}

				case TimelineWidgetTrackUserRegions:
					out[i] = TimelineWidgetTrack{
						Track: track,
						spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							// OPT(dh): avoid this allocation
							s := tr.Strings[tr.Events[spanSel.At(0).Event].Args[trace.ArgUserRegionTypeID]]
							return append(out, s)
						},
						spanTooltip: userRegionSpanTooltip,
						spanColor: func(spanSel SpanSelector, _ *Trace) [2]colorIndex {
							if spanSel.Size() == 1 {
								return [2]colorIndex{colorStateUserRegion, 0}
							} else {
								return [2]colorIndex{colorStateUserRegion, colorStateMerged}
							}
						},
					}

				case TimelineWidgetTrackSampled:
					if sampledTrackBase == -1 {
						sampledTrackBase = i
					}
					out[i] = TimelineWidgetTrack{
						Track: track,

						// TODO(dh): should we highlight hovered spans that share the same function?
						spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							f := tr.PCs[tr.getSpanPC(spanSel.At(0).SeqID)]

							short := shortenFunctionName(f.Fn)

							if short != f.Fn {
								return append(out, f.Fn, "."+short)
							} else {
								// This branch is probably impossible; all functions should be fully qualified.
								return append(out, f.Fn)
							}
						},
						spanColor: func(spanSel SpanSelector, _ *Trace) [2]colorIndex {
							if spanSel.Size() != 1 {
								return [2]colorIndex{colorStateSample, colorStateMerged}
							}
							return [2]colorIndex{colorStateSample, 0}
						},
						spanTooltip: func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
							var label string
							if state.spanSel.Size() == 1 {
								f := tr.PCs[tr.getSpanPC(state.spanSel.At(0).SeqID)]
								label = local.Sprintf("Sampled function: %s\n", f.Fn)
								// TODO(dh): for truncated stacks we should display a relative depth instead
								label += local.Sprintf("Call depth: %d\n", i-sampledTrackBase)
							} else {
								label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
							}
							// We round the duration, in addition to saying "up to", to make it more obvious that the
							// duration is a guess
							label += fmt.Sprintf("Duration: up to %s", roundDuration(Duration(state.spanSel)))
							return theme.Tooltip{}.Layout(win, gtx, label)
						},
					}

				default:
					panic(fmt.Sprintf("unexpected timeline track kind %d", track.kind))
				}
			}
		},
		widgetTooltip: func(win *theme.Window, gtx layout.Context, tw *TimelineWidget) layout.Dimensions {
			return GoroutineTooltip{g, cv.trace}.Layout(win, gtx)
		},
		cv:    cv,
		item:  g,
		label: l,
	}

	for _, ug := range g.UserRegions {
		tw.tracks = append(tw.tracks, Track{spans: SliceToSpanSelector(ug), kind: TimelineWidgetTrackUserRegions})
	}

	addSampleTracks(tw, g, cv.trace)

	return tw
}

func addSampleTracks(tw *TimelineWidget, g *ptrace.Goroutine, tr *Trace) {
	cv := tw.cv

	var sampleTracks []Track
	offSpans := 0
	offSamples := 0
	cpuSamples := tr.CPUSamples[g.ID]

	nextEvent := func(advance bool) (ptrace.EventID, bool, bool) {
		if offSpans == len(g.Spans) && offSamples == len(cpuSamples) {
			return 0, false, false
		}

		if offSpans < len(g.Spans) {
			id := g.Spans[offSpans].Event
			if offSamples < len(cpuSamples) {
				oid := cpuSamples[offSamples]
				if id <= oid {
					if advance {
						offSpans++
					}
					return id, false, true
				} else {
					if advance {
						offSamples++
					}
					return oid, true, true
				}
			} else {
				if advance {
					offSpans++
				}
				return id, false, true
			}
		} else {
			id := cpuSamples[offSamples]
			if advance {
				offSamples++
			}
			return id, true, true
		}
	}

	var prevStk []uint64
	// function name of the previous span, indexed by track index, i.e. stack depth
	var prevFns []string
	for {
		evID, isSample, ok := nextEvent(true)
		if !ok {
			break
		}

		ev := &cv.trace.Events[evID]

		var stk []uint64
		// We benefit from primarily two kinds of events (aside from CPU samples): Blocking events (sleeps, selects,
		// I/O...) as these give us the most accurate stack trace right before a long period of inactivity, covering for
		// a lack of samples during blockedness, and preemption, as an additional periodic event, similar to sampling.
		if tr.HasCPUSamples {
			// When we have CPU samples, we display stacks from samples as well as a subset of events, primarily
			// blocking ones and preemption points. There are no gaps between stacks. A stack always continues until we
			// get a new one. This gives us stack timelines that are ~never based on actual events, and instead are
			// sampled (with increased detail at blocking points).
			switch ev.Type {
			case trace.EvGoCreate:
				// The stack is in the goroutine that created this one
			case trace.EvGoUnblock:
				// The stack is in the goroutine that unblocked this one
			case trace.EvGCSweepStart, trace.EvGCSweepDone, trace.EvGCMarkAssistStart, trace.EvGCMarkAssistDone:
				// These are very short-lived spans that would unfairly represent the time taken by allocations.
			case trace.EvGoSysBlock:
				// These are very short-lived spans that would unfairly represent the time taken by syscalls.
			default:
				stk = cv.trace.Stacks[ev.StkID]
			}
		} else {
			// When we don't have samples, we display stacks from all events, but events without stacks cause gaps to be
			// created. This is essentially just a graphical representation of event stacks, without further
			// interpretation by us and represents actual raw data.
			switch ev.Type {
			case trace.EvGoUnblock:
				// The stack is in the goroutine that unblocked this one
			case trace.EvGoStart:
				// This event doesn't have a stack; display an artificial stack representing time spent on-CPU
			default:
				stk = cv.trace.Stacks[ev.StkID]
			}
		}
		if stk == nil {
			if tr.HasCPUSamples {
				// Continue the previous stack trace if this event didn't contain a useful one. This happens both when we
				// choose to ignore an event, and when an event intrinsically has no stack trace, such as most EvGoStart
				// events.
				stk = prevStk
			} else {
				continue
			}
		}

		if isSample {
			// CPU samples include two runtime functions at the start of the stack trace that isn't present for stacks
			// collected by the runtime tracer.
			if len(stk) > 0 && cv.trace.PCs[stk[len(stk)-1]].Fn == "runtime.goexit" {
				stk = stk[:len(stk)-1]
			}
			if len(stk) > 0 && cv.trace.PCs[stk[len(stk)-1]].Fn == "runtime.main" {
				stk = stk[:len(stk)-1]
			}
		}

		if len(stk) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			stk = stk[:64]
		}

		sampleTracks = grow(sampleTracks, len(stk))
		prevFns = grow(prevFns, len(stk))
		var end trace.Timestamp
		if endEvID, _, ok := nextEvent(false); ok {
			end = cv.trace.Events[endEvID].Ts
		} else {
			end = g.Spans.End()
		}
		for i := 0; i < len(stk); i++ {
			var spans ptrace.Spans
			if s := sampleTracks[i].spans; s != nil {
				spans = s.AllSpans()
			}
			if len(spans) != 0 {
				prevSpan := &spans[len(spans)-1]
				prevFn := prevFns[i]
				fn := cv.trace.PCs[stk[len(stk)-i-1]].Fn
				if prevSpan.End == cv.trace.Events[evID].Ts && prevFn == fn {
					// This is a continuation of the previous span
					//
					// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
					// data more apparent.
					prevSpan.End = end
				} else {
					// This is a new span
					span := ptrace.Span{
						Start: ev.Ts,
						End:   end,
						Event: evID,
						State: ptrace.StateCPUSample,
						SeqID: tr.NextSpanID(),
					}
					tr.setSpanPC(span.SeqID, stk[len(stk)-i-1])
					spans = append(spans, span)
					sampleTracks[i].spans = SliceToSpanSelector(spans)
					prevFns[i] = fn
				}
			} else {
				// This is the first span
				span := ptrace.Span{
					Start: ev.Ts,
					End:   end,
					Event: evID,
					State: ptrace.StateCPUSample,
					SeqID: tr.NextSpanID(),
				}
				tr.setSpanPC(span.SeqID, stk[len(stk)-i-1])
				spans = append(spans, span)
				sampleTracks[i].spans = SliceToSpanSelector(spans)
				prevFns[i] = cv.trace.PCs[stk[len(stk)-i-1]].Fn
			}
		}

		prevStk = stk
	}

	for i := range sampleTracks {
		sampleTracks[i].kind = TimelineWidgetTrackSampled
	}

	tw.tracks = append(tw.tracks, sampleTracks...)
}

func NewGCWidget(cv *Canvas, trace *Trace, spans ptrace.Spans) *TimelineWidget {
	return &TimelineWidget{
		tracks: []Track{{spans: SliceToSpanSelector(spans)}},
		cv:     cv,
		item:   spans,
		label:  "GC",
	}
}

func NewSTWWidget(cv *Canvas, trace *Trace, spans ptrace.Spans) *TimelineWidget {
	return &TimelineWidget{
		tracks: []Track{{spans: SliceToSpanSelector(spans)}},
		cv:     cv,
		item:   spans,
		label:  "STW",
	}
}

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

type TrackKind uint8

const (
	TrackKindUnspecified TrackKind = iota
	TrackKindStack
	TrackKindUserRegions
)

type TimelineWidget struct {
	// Inputs
	tracks            []Track
	buildTrackWidgets func([]Track)

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
	//
	// OPT(dh): clicked spans and navigated spans are mutually exclusive, combine the fields
	clickedSpans   SpanSelector
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
	kind   TrackKind
	spans  SpanSelector
	events []ptrace.EventID

	*TrackWidget
}

func Duration(spanSel SpanSelector) time.Duration {
	if spanSel.Size() == 0 {
		return 0
	}
	return time.Duration(spanSel.At(spanSel.Size()-1).End - spanSel.At(0).Start)
}

type SpanSelector interface {
	Spans() ptrace.Spans
	Size() int
	Slice(start, end int) SpanSelector
	At(index int) ptrace.Span
}

type MetadataSelector[T any] interface {
	Metadata() []T
	MetadataAt(index int) T
}

type spanAndMetadataSlices[T any] struct {
	spans ptrace.Spans
	meta  []T
}

func (spans spanAndMetadataSlices[T]) Spans() ptrace.Spans { return ptrace.Spans(spans.spans) }
func (spans spanAndMetadataSlices[T]) Metadata() []T       { return spans.meta }
func (spans spanAndMetadataSlices[T]) Size() int           { return len(spans.spans) }
func (spans spanAndMetadataSlices[T]) Slice(start, end int) SpanSelector {
	return spanAndMetadataSlices[T]{
		spans: spans.spans[start:end],
		meta:  spans.meta[start:end],
	}
}
func (spans spanAndMetadataSlices[T]) At(index int) ptrace.Span { return spans.spans[index] }
func (spans spanAndMetadataSlices[T]) MetadataAt(index int) T   { return spans.meta[index] }

type spanSlice ptrace.Spans

func SliceToSpanSelector(spans ptrace.Spans) SpanSelector { return spanSlice(spans) }

func (spans spanSlice) Spans() ptrace.Spans               { return ptrace.Spans(spans) }
func (spans spanSlice) Size() int                         { return len(spans) }
func (spans spanSlice) Slice(start, end int) SpanSelector { return spans[start:end] }
func (spans spanSlice) At(index int) ptrace.Span          { return spans[index] }

type NoSpan struct{}

func (NoSpan) Spans() ptrace.Spans               { return nil }
func (NoSpan) Metadata() any                     { return nil }
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

type TrackWidget struct {
	highlightSpan func(spanSel SpanSelector) bool
	// OPT(dh): pass slice to spanLabel to reuse memory between calls
	spanLabel       func(spanSel SpanSelector, tr *Trace, out []string) []string
	spanColor       func(spanSel SpanSelector, tr *Trace) [2]colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions
	spanContextMenu func(spanSel SpanSelector, tr *Trace) []*theme.MenuItem

	// OPT(dh): Only one track can have hovered or activated spans, so we could track this directly in TimelineWidget,
	// and save 48 bytes per track. However, the current API is cleaner, because TimelineWidgetTrack doesn't have to
	// mutate TimelineWidget's state.
	//
	// OPT(dh): clickedSpans and navigatedSpans are mutually exclusive, combine the fields
	clickedSpans   SpanSelector
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

func (track *TrackWidget) ClickedSpans() SpanSelector {
	return track.clickedSpans
}

func (track *TrackWidget) NavigatedSpans() SpanSelector {
	return track.navigatedSpans
}

func (track *TrackWidget) HoveredSpans() SpanSelector {
	return track.hoveredSpans
}

func (tw *TimelineWidget) ClickedSpans() SpanSelector {
	return tw.clickedSpans
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
		if track.kind != TrackKindStack || tw.cv.timeline.displayStackTracks {
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
	for i := range tw.tracks {
		tw.tracks[i].TrackWidget = nil
	}
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

	tw.clickedSpans = NoSpan{}
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
	if len(tw.tracks) > 0 && tw.tracks[0].TrackWidget == nil {
		// If the first track doesn't have a widget then none of them do. Initialize them.
		// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
		// timelines.
		if tw.buildTrackWidgets != nil {
			tw.buildTrackWidgets(tw.tracks)
		} else {
			for i := range tw.tracks {
				tw.tracks[i].TrackWidget = &TrackWidget{}
			}
		}
	}

	for i := range tw.tracks {
		track := &tw.tracks[i]
		if track.kind == TrackKindStack && !tw.cv.timeline.displayStackTracks {
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
		if spans := track.ClickedSpans(); spans.Size() != 0 {
			tw.clickedSpans = spans
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
		spans := spanSel.Spans()
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
	spans := it.spanSel.Spans()

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

	if time.Duration(end-start) < minSpanWidthD && s.State != ptrace.StateDone {
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
				// We couldn't find a span -> merge all remaining spans, except for the optional "goroutine returned"
				// span
				if spans[len(spans)-1].State == ptrace.StateDone {
					offset = len(spans) - 1
					end = spans[offset-1].End
					break
				} else {
					offset = len(spans)
					end = spans[offset-1].End
					break
				}
			}

			candidateSpan := &spans[offset]
			prevSpan := &spans[offset-1]

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

func (track *Track) Layout(win *theme.Window, gtx layout.Context, cv *Canvas, labelsOut *[]string) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Layout").End()

	tr := cv.trace
	trackHeight := gtx.Dp(timelineTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	spanHighlightedBorderWidth := gtx.Dp(spanHighlightedBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	track.clickedSpans = NoSpan{}
	track.navigatedSpans = NoSpan{}
	track.hoveredSpans = NoSpan{}

	trackClickedSpans := false
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
		switch ev.Modifiers {
		case key.ModShortcut:
			trackNavigatedSpans = true
		case 0:
			trackClickedSpans = true
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
			if trackClickedSpans {
				track.clickedSpans = dspSpanSel
			}
			if trackContextMenuSpans {
				if track.spanContextMenu != nil {
					win.SetContextMenu(track.spanContextMenu(dspSpanSel, tr))
				} else {
					win.SetContextMenu([]*theme.MenuItem{newZoomMenuItem(cv, dspSpanSel)})
				}
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
			events := dspSpanSel.Spans().Events(track.events, tr.Trace)

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
			events := dspSpanSel.Slice(0, 1).Spans().Events(track.events, tr.Trace)

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
					var dims layout.Dimensions
					{
						gtx := gtx
						gtx.Ops = labelsOps
						gtx.Constraints.Min = image.Point{}
						dims = mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.ExtraBold}, win.Theme.TextSize, label)
					}
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

	if track.kind == TrackKindUnspecified {
		// Indicate parts of time where a goroutine or processor wasn't yet alive and where it no longer exists.
		var (
			visWidthPx    float32 = float32(gtx.Constraints.Max.X)
			unbornUntilPx float32
			deadFromPx    float32 = visWidthPx
		)
		if track.spans.Size() == 0 {
			// A track with no spans is similar to a track that's always dead
			deadFromPx = 0
		} else {
			if len(track.prevFrame.dspSpans) > 0 {
				// If the first displayed span is also the first overall span, display an indicator that the
				// goroutine/processor hasn't been created yet.
				dspFirst := track.prevFrame.dspSpans[0]
				if dspFirst.dspSpanSel.At(0) == track.spans.At(0) {
					end := dspFirst.startPx
					unbornUntilPx = end
				}

				// If the last displayed span is also the last overall span, display an indicator that the
				// goroutine/processor is dead.
				dspLast := track.prevFrame.dspSpans[len(track.prevFrame.dspSpans)-1]
				if dspLast.dspSpanSel.At(dspLast.dspSpanSel.Size()-1) == track.spans.At(track.spans.Size()-1) {
					start := dspLast.endPx
					deadFromPx = start
				}

			} else {
				// We didn't draw any spans. We're either displaying a not-yet-alive section, a dead section, or a gap
				// between spans (for processor tracks).
				born := track.spans.At(0).Start
				died := track.spans.At(track.spans.Size() - 1).End

				if cv.start >= died {
					// The goroutine is dead
					deadFromPx = 0
				} else if cv.end < born {
					// The goroutine hasn't been created yet
					unbornUntilPx = visWidthPx
				}
			}
		}
		mid := float32(trackHeight) / 2
		top := mid - 2
		bottom := mid + 2
		if unbornUntilPx > 0 {
			// Draw the unborn indicator
			paint.FillShape(gtx.Ops, rgba(0x10a56fFF), FRect{Min: f32.Pt(0, top), Max: f32.Pt(unbornUntilPx, bottom)}.Op(gtx.Ops))
		}
		if deadFromPx < visWidthPx {
			// Draw the dead indicator
			paint.FillShape(gtx.Ops, rgba(0x6F6F6FFF), FRect{Min: f32.Pt(deadFromPx, top), Max: f32.Pt(visWidthPx, bottom)}.Op(gtx.Ops))
		}
	}

	// Draw the span outlines. We draw these as solid rectangles and let the spans overlay them.
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

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
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
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
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

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
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

		buildTrackWidgets: func(tracks []Track) {
			for i := range tracks {
				track := &tracks[i]
				switch i {
				case 0:
					track.TrackWidget = &TrackWidget{
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
								for _, span := range spanSel.Spans() {
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
							return theme.Tooltip(win.Theme, label).Layout(win, gtx)
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
					track.TrackWidget = &TrackWidget{
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
								for _, span := range spanSel.Spans() {
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
								spans := spanSel.Spans()
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

		buildTrackWidgets: func(tracks []Track) {
			for i := range tracks {
				track := &tracks[i]
				track.TrackWidget = &TrackWidget{
					highlightSpan: func(spanSel SpanSelector) bool {
						if htw := cv.timeline.hoveredTimeline; htw != nil {
							target := struct {
								g          uint64
								start, end trace.Timestamp
							}{^uint64(0), -1, -1}
							switch hitem := htw.item.(type) {
							case *ptrace.Goroutine:
								switch cv.timeline.hoveredSpans.Size() {
								case 0:
									// A goroutine timeline is hovered, but no spans within are.
									target.g = hitem.ID
								case 1:
									switch cv.timeline.hoveredSpans.At(0).State {
									case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
										// A span in a goroutine timeline is hovered. Highlight processor spans for
										// the same goroutine if they overlap with the highlighted span.
										target.g = hitem.ID
										target.start = cv.timeline.hoveredSpans.At(0).Start
										target.end = cv.timeline.hoveredSpans.At(cv.timeline.hoveredSpans.Size() - 1).End
									default:
										// There's no point in looking for a non-runnable state, processor timelines
										// only show running goroutines.
									}
								default:
									// A merged span in a goroutine timeline is hovered. Highlight processor spans for
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

							if target.g == ^uint64(0) {
								return false
							}

							spans := spanSel.Spans()
							off := 0
							if target.start != -1 {
								off = sort.Search(len(spans), func(i int) bool {
									return spans[i].Start >= target.start
								})
							}
							for _, span := range spans[off:] {
								// OPT(dh): don't be O(n)

								if target.end != -1 && span.Start > target.end {
									break
								}
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
							if s.Tags&ptrace.SpanTagGC != 0 {
								return colorStateGC
							} else {
								// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
								return stateColors[s.State]
							}
						}

						if spanSel.Size() == 1 {
							return [2]colorIndex{do(spanSel.At(0), tr), 0}
						} else {
							spans := spanSel.Spans()
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

	blocked := tt.g.Statistics.Blocked()
	inactive := tt.g.Statistics.Inactive()
	gcAssist := tt.g.Statistics.GCAssist()
	running := tt.g.Statistics.Running()
	blockedPct := float32(blocked) / float32(d) * 100
	inactivePct := float32(inactive) / float32(d) * 100
	gcAssistPct := float32(gcAssist) / float32(d) * 100
	runningPct := float32(running) / float32(d) * 100

	var fmts []string
	var args []any

	if tt.g.Function.Fn != "" {
		fmts = append(fmts, "Goroutine %d: %s\n")
		args = append(args, tt.g.ID, tt.g.Function.Fn)
	} else {
		fmts = append(fmts, "Goroutine %d\n")
		args = append(args, tt.g.ID)
	}

	fmts = append(fmts, "Created at: %s")
	args = append(args, formatTimestamp(start))

	if tt.g.Spans[len(tt.g.Spans)-1].State == ptrace.StateDone {
		fmts = append(fmts, "Returned at: %s")
		args = append(args, formatTimestamp(end))
	}

	fmts = append(fmts, "Lifetime: %s")
	args = append(args, roundDuration(d))

	fmts = append(fmts, "Spans: %d")
	args = append(args, len(tt.g.Spans))

	fmts = append(fmts, "Time in blocked states: %s (%.2f%%)")
	args = append(args, roundDuration(blocked), blockedPct)

	fmts = append(fmts, "Time in inactive states: %s (%.2f%%)")
	args = append(args, roundDuration(inactive), inactivePct)

	fmts = append(fmts, "Time in GC assist: %s (%.2f%%)")
	args = append(args, roundDuration(gcAssist), gcAssistPct)

	fmts = append(fmts, "Time in running states: %s (%.2f%%)")
	args = append(args, roundDuration(running), runningPct)

	l := local.Sprintf(strings.Join(fmts, "\n"), args...)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
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
		case ptrace.StateDone:
			label += "returned"
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

	if state.spanSel.At(state.spanSel.Size()-1).State != ptrace.StateDone {
		label += fmt.Sprintf("Duration: %s\n", roundDuration(Duration(state.spanSel)))
	}

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

	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
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
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
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
		buildTrackWidgets: func(tracks []Track) {
			stackTrackBase := -1
			for i := range tracks {
				i := i

				track := &tracks[i]
				switch track.kind {
				case TrackKindUnspecified:
					track.TrackWidget = &TrackWidget{
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

				case TrackKindUserRegions:
					track.TrackWidget = &TrackWidget{
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

				case TrackKindStack:
					if stackTrackBase == -1 {
						stackTrackBase = i
					}
					track.TrackWidget = &TrackWidget{
						// TODO(dh): should we highlight hovered spans that share the same function?
						spanLabel: func(spanSel SpanSelector, tr *Trace, out []string) []string {
							if spanSel.Size() != 1 {
								return out
							}
							pc := spanSel.(MetadataSelector[stackSpanMeta]).MetadataAt(0).pc
							f := tr.PCs[pc]

							short := shortenFunctionName(f.Fn)

							if short != f.Fn {
								return append(out, f.Fn, "."+short)
							} else {
								// This branch is probably impossible; all functions should be fully qualified.
								return append(out, f.Fn)
							}
						},
						spanTooltip: func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
							var label string
							if state.spanSel.Size() == 1 {
								meta := state.spanSel.(MetadataSelector[stackSpanMeta]).MetadataAt(0)
								pc := meta.pc
								f := tr.PCs[pc]
								label = local.Sprintf("Function: %s\n", f.Fn)
								// TODO(dh): for truncated stacks we should display a relative depth instead
								label += local.Sprintf("Call depth: %d\n", i-stackTrackBase)
								if state.spanSel.At(0).State == ptrace.StateCPUSample {
									label += local.Sprintf("Samples: %d\n", meta.num)
								}
							} else {
								label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
							}
							// We round the duration, in addition to saying "up to", to make it more obvious that the
							// duration is a guess
							label += fmt.Sprintf("Duration: up to %s", roundDuration(Duration(state.spanSel)))
							return theme.Tooltip(win.Theme, label).Layout(win, gtx)
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
		tw.tracks = append(tw.tracks, Track{spans: SliceToSpanSelector(ug), kind: TrackKindUserRegions})
	}

	addStackTracks(tw, g, cv.trace)

	return tw
}

type stackSpanMeta struct {
	// OPT(dh): should we use 48 bits for the PC and 16 bits for the num?
	pc  uint64
	num int
}

func addStackTracks(tw *TimelineWidget, g *ptrace.Goroutine, tr *Trace) {
	if g.Function.Fn == "runtime.bgsweep" {
		// Go <=1.19 has a lot of spans in runtime.bgsweep, but the stacks are utterly uninteresting, containing only a
		// single frame. Save some memory by not creating stack tracks for this goroutine.
		return
	}
	cv := tw.cv

	var stackTracks []Track
	var trackSpans []ptrace.Spans
	var spanMeta [][]stackSpanMeta
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

	// function name of the previous span, indexed by track index, i.e. stack depth
	var prevFns []string
	for {
		evID, isSample, ok := nextEvent(true)
		if !ok {
			break
		}

		ev := &cv.trace.Events[evID]
		stk := cv.trace.Stacks[ev.StkID]
		switch ev.Type {
		case trace.EvGoUnblock:
			// The stack is in the goroutine that unblocked this one
			continue
		case trace.EvGoStart:
			// This event doesn't have a stack; display an artificial stack representing time spent on-CPU
			continue
		}

		state := ptrace.StateStack
		if isSample {
			state = ptrace.StateCPUSample
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

		stackTracks = grow(stackTracks, len(stk))
		prevFns = grow(prevFns, len(stk))
		trackSpans = grow(trackSpans, len(stk))
		spanMeta = grow(spanMeta, len(stk))
		var end trace.Timestamp
		if endEvID, _, ok := nextEvent(false); ok {
			end = cv.trace.Events[endEvID].Ts
		} else {
			end = g.Spans.End()
		}

		for i := 0; i < len(stk); i++ {
			spans := trackSpans[i]
			if len(spans) != 0 {
				prevSpan := &spans[len(spans)-1]
				prevFn := prevFns[i]
				fn := cv.trace.PCs[stk[len(stk)-i-1]].Fn
				if prevSpan.End == cv.trace.Events[evID].Ts && prevFn == fn && state == prevSpan.State {
					// This is a continuation of the previous span. Merging these can have massive memory usage savings,
					// which is why we do it here and not during display.
					//
					// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
					// data more apparent.
					prevSpan.End = end
					if state == ptrace.StateCPUSample {
						spanMeta[i][len(spans)-1].num++
					}
				} else {
					// This is a new span
					span := ptrace.Span{
						Start: ev.Ts,
						End:   end,
						Event: evID,
						State: state,
					}
					trackSpans[i] = append(trackSpans[i], span)
					spanMeta[i] = append(spanMeta[i], stackSpanMeta{pc: stk[len(stk)-i-1], num: 1})
					prevFns[i] = fn
				}
			} else {
				// This is the first span
				span := ptrace.Span{
					Start: ev.Ts,
					End:   end,
					Event: evID,
					State: state,
				}
				trackSpans[i] = append(trackSpans[i], span)
				spanMeta[i] = append(spanMeta[i], stackSpanMeta{pc: stk[len(stk)-i-1], num: 1})
				prevFns[i] = cv.trace.PCs[stk[len(stk)-i-1]].Fn
			}
		}
	}

	for i := range stackTracks {
		stackTracks[i].kind = TrackKindStack
		stackTracks[i].spans = spanAndMetadataSlices[stackSpanMeta]{
			spans: trackSpans[i],
			meta:  spanMeta[i],
		}
	}

	tw.tracks = append(tw.tracks, stackTracks...)
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

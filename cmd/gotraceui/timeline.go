package main

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"sort"
	"time"
	"unsafe"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/container"
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

type Timeline struct {
	// Inputs
	tracks            []*Track
	buildTrackWidgets func([]*Track)
	widgetTooltip     func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions
	invalidateCache   func(tl *Timeline, cv *Canvas) bool
	item              any
	shortName         string
	label             string
	// Set to true by Timeline.Layout. This is used to track which timelines have been shown during a frame.
	displayed bool

	// This cache is stored per timeline, not per timeline widget, because 1) only processors use it 2) it's expensive
	// to populate. There aren't enough processors or items in the cache to worry about its permanent memory usage, but
	// we definitely care about the poor performance of the first few frames rendered with an unpopulated cache.
	spanColorCache *container.IntervalTree[trace.Timestamp, [2]colorIndex]

	*TimelineWidget
}

type TimelineWidget struct {
	cv          *Canvas
	labelClick  widget.PrimaryClickable
	labelClicks int

	hover gesture.Hover

	// OPT(dh): Only one timeline can have hovered or activated spans, so we could track this directly in Canvas, and
	// save 48 bytes per timeline (which means per goroutine). However, the current API is cleaner, because
	// TimelineWidget doesn't have to mutate Timeline's state.
	//
	// OPT(dh): clicked spans and navigated spans are mutually exclusive, combine the fields
	clickedSpans   Items[ptrace.Span]
	navigatedSpans Items[ptrace.Span]
	hoveredSpans   Items[ptrace.Span]
}

func (tw *TimelineWidget) Hovered() bool {
	if tw == nil {
		return false
	}
	return tw.hover.Hovered()
}

type SpanTooltipState struct {
	spans             Items[ptrace.Span]
	events            Items[ptrace.EventID]
	eventsUnderCursor Items[ptrace.EventID]
}

type Track struct {
	parent           *Timeline
	kind             TrackKind
	spans_           Items[ptrace.Span]
	compressedSpans  compressedStackSpans
	events           []ptrace.EventID
	hideEventMarkers bool

	*TrackWidget
}

func NewTrack(parent *Timeline, kind TrackKind) *Track {
	return &Track{
		parent: parent,
		kind:   kind,
	}
}

func (tr *Track) Spans() Items[ptrace.Span] {
	if tr.spans_ != nil {
		return tr.spans_
	}
	if tr.compressedSpans.count == 0 {
		return nil
	}

	bitunpackByte := func(bits uint8, dst *uint64) {
		x64 := uint64(bits)
		x_hi := x64 & 0xFE
		r_hi := x_hi * 0b10000001000000100000010000001000000100000010000000
		r := r_hi | x64
		*dst = r & 0x0101010101010101
	}
	bitunpack := func(bits []uint64) []bool {
		if len(bits) == 0 {
			return nil
		}
		bytes := unsafe.Slice((*byte)(unsafe.Pointer(&bits[0])), len(bits)*8)
		out := boolSliceCache.Get(len(bytes) * 8)[:len(bytes)*8]
		for i, v := range bytes {
			bitunpackByte(v, (*uint64)(unsafe.Pointer(&out[i*8])))
		}
		return out
	}

	n := tr.compressedSpans.count
	c := &tr.compressedSpans

	startsEnds := uint64SliceCache.Get(n * 2)[:n*2]
	// eventIDs, pcs, and nums share the same slice length, which eliminates bounds checking when we loop over the
	// indices of eventIDs and use them to index the other slices.
	eventIDs := uint64SliceCache.Get(n)[:n]
	pcs := uint64SliceCache.Get(n)[:n]
	nums := uint64SliceCache.Get(n)[:n]

	// OPT(dh): we could reduce memory usage by decoding one property at a time and populating span fields one at a
	// time. It would, however, increase CPU usage.
	//
	// OPT(dh): we could also decode one word at a time, interleaving decoding and constructing spans. This would,
	// however, increase CPU usage to 1.3x.
	DecodeUnsafe(c.startsEnds, startsEnds)
	DecodeUnsafe(c.eventIDs, eventIDs)
	DecodeUnsafe(c.pcs, pcs)
	DecodeUnsafe(c.nums, nums)

	deltaZigZagDecode(startsEnds)
	deltaZigZagDecode(eventIDs)
	deltaZigZagDecode(pcs)
	deltaZigZagDecode(nums)
	// We slice isCPUSample to [:n] to eliminate bounds checking.
	isCPUSample := bitunpack(c.isCPUSample)[:n]

	// spans and metas share the slice length of eventIDs, eliminating bounds checking.
	spans := spanSliceCache.Get(len(eventIDs))[:n]
	metas := stackSpanMetaSliceCache.Get(len(eventIDs))[:n]

	// startsEndsPairwise and eventIDs have the same length, eliminating bounds checking.
	startsEndsPairwise := unsafe.Slice((*[2]uint64)(unsafe.Pointer(&startsEnds[0])), n)[:n]
	for i := range eventIDs {
		// OPT(dh): mind the bound checks
		state := ptrace.StateStack
		if isCPUSample[i] {
			state = ptrace.StateCPUSample
		}
		span := ptrace.Span{
			Start: trace.Timestamp(startsEndsPairwise[i][0]),
			End:   trace.Timestamp(startsEndsPairwise[i][1]),
			Event: ptrace.EventID(eventIDs[i]),
			State: state,
		}
		meta := stackSpanMeta{
			pc:  pcs[i],
			num: int(nums[i]),
		}
		spans[i] = span
		metas[i] = meta
	}

	uint64SliceCache.Put(startsEnds)
	uint64SliceCache.Put(eventIDs)
	uint64SliceCache.Put(pcs)
	uint64SliceCache.Put(nums)

	out := spanAndMetadataSlices[stackSpanMeta]{
		Items: SimpleItems[ptrace.Span]{
			items: spans,
			container: ItemContainer{
				Timeline: tr.parent,
				Track:    tr,
			},
			subslice: true,
		},
		meta: metas,
	}

	tr.spans_ = out
	boolSliceCache.Put(isCPUSample)
	return out
}

type MetadataSpans[T any] interface {
	Metadata() []T
	MetadataAt(index int) T
}

type spanAndMetadataSlices[T any] struct {
	Items[ptrace.Span]
	meta []T
}

func (spans spanAndMetadataSlices[T]) Metadata() []T { return spans.meta }
func (spans spanAndMetadataSlices[T]) Slice(start, end int) Items[ptrace.Span] {
	return spanAndMetadataSlices[T]{
		Items: spans.Items.Slice(start, end),
		meta:  spans.meta[start:end],
	}
}
func (spans spanAndMetadataSlices[T]) MetadataAt(index int) T { return spans.meta[index] }

func newZoomMenuItem(cv *Canvas, spans Items[ptrace.Span]) *theme.MenuItem {
	return &theme.MenuItem{
		Label:    PlainLabel("Zoom"),
		Shortcut: key.ModShortcut.String() + "+LMB",
		Do: func(gtx layout.Context) {
			start := spans.At(0).Start
			end := LastSpan(spans).End
			cv.navigateToStartAndEnd(gtx, start, end, cv.y)
		},
	}
}

type TrackWidget struct {
	spanLabel       func(spans Items[ptrace.Span], tr *Trace, out []string) []string
	spanColor       func(spans Items[ptrace.Span], tr *Trace) [2]colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions
	spanContextMenu func(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem

	// OPT(dh): Only one track can have hovered or activated spans, so we could track this directly in TimelineWidget,
	// and save 48 bytes per track. However, the current API is cleaner, because TimelineWidgetTrack doesn't have to
	// mutate TimelineWidget's state.
	//
	// OPT(dh): clickedSpans and navigatedSpans are mutually exclusive, combine the fields
	clickedSpans   Items[ptrace.Span]
	navigatedSpans Items[ptrace.Span]
	hoveredSpans   Items[ptrace.Span]

	// op lists get reused between frames to avoid generating garbage
	ops                             [colorStateLast * 2]op.Ops
	outlinesOps                     reusableOps
	highlightedPrimaryOutlinesOps   reusableOps
	highlightedSecondaryOutlinesOps reusableOps
	eventsOps                       reusableOps
	labelsOps                       reusableOps

	hover gesture.Hover
	click gesture.Click

	// cached state
	prevFrame struct {
		hovered     bool
		constraints layout.Constraints
		ops         reusableOps
		call        op.CallOp
		dims        layout.Dimensions

		dspSpans []struct {
			dspSpans       Items[ptrace.Span]
			startPx, endPx float32
		}
	}
}

func (track *TrackWidget) ClickedSpans() Items[ptrace.Span] {
	return track.clickedSpans
}

func (track *TrackWidget) NavigatedSpans() Items[ptrace.Span] {
	return track.navigatedSpans
}

func (track *TrackWidget) HoveredSpans() Items[ptrace.Span] {
	return track.hoveredSpans
}

func (tw *TimelineWidget) ClickedSpans() Items[ptrace.Span] {
	return tw.clickedSpans
}

func (tw *TimelineWidget) NavigatedSpans() Items[ptrace.Span] {
	return tw.navigatedSpans
}

func (tw *TimelineWidget) HoveredSpans() Items[ptrace.Span] {
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

func (tl *Timeline) Height(gtx layout.Context, cv *Canvas) int {
	timelineGap := gtx.Dp(timelineGapDp)
	enabledTracks := 0
	for _, track := range tl.tracks {
		if track.kind != TrackKindStack || cv.timeline.displayStackTracks {
			enabledTracks++
		}
	}
	if cv.timeline.compact {
		return (gtx.Dp(timelineTrackHeightDp)+gtx.Dp(timelineTrackGapDp))*enabledTracks + timelineGap
	} else {
		return (gtx.Dp(timelineTrackHeightDp)+gtx.Dp(timelineTrackGapDp))*enabledTracks + gtx.Dp(timelineLabelHeightDp) + timelineGap
	}
}

// notifyHidden informs the widget that it is no longer visible.
func (tl *Timeline) notifyHidden(cv *Canvas) {
	rtrace.Logf(context.Background(), "", "unloading track widget %q", tl.label)
	for _, track := range tl.tracks {
		cv.trackWidgetsCache.Put(track.TrackWidget)
		track.TrackWidget = nil
		if track.compressedSpans.count != 0 {
			if spans, ok := track.spans_.(spanAndMetadataSlices[stackSpanMeta]); ok {
				stackSpanMetaSliceCache.Put(spans.meta)
				if items, ok := spans.Items.(SimpleItems[ptrace.Span]); ok {
					spanSliceCache.Put(items.items)
				}
			}
			track.spans_ = nil
		}
	}
	cv.timelineWidgetsCache.Put(tl.TimelineWidget)
	tl.TimelineWidget = nil
}

func (tl *Timeline) Layout(win *theme.Window, gtx layout.Context, cv *Canvas, forceLabel bool, compact bool, topBorder bool, trackSpanLabels *[]string) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidget.Layout").End()

	if tl.TimelineWidget == nil {
		rtrace.Logf(context.Background(), "", "loading timeline widget %q", tl.label)
		tl.TimelineWidget = cv.timelineWidgetsCache.Get()
		*tl.TimelineWidget = TimelineWidget{cv: cv}
	}

	tl.hover.Update(gtx.Queue)

	// TODO(dh): we could replace all uses of timelineHeight by using normal Gio widget patterns: lay out all the
	// tracks, sum their heights and the gaps we apply. We'd use a macro to get the total size and then set up the clip
	// and pointer input. When we reuse the previous frame and need to return dimensions, we should use a stored value
	// of the height. Making the change isn't currently worth it, though, because we still need TimelineWidget.Height to
	// exist so we can compute the scrollbar, and so we can jump to goroutines, which needs to compute an offset. In
	// both cases we don't want to lay out every widget to figure out its size.
	timelineHeight := tl.Height(gtx, tl.cv)
	timelineTrackGap := gtx.Dp(timelineTrackGapDp)
	timelineLabelHeight := gtx.Dp(timelineLabelHeightDp)

	tl.displayed = true

	tl.clickedSpans = NoItems[ptrace.Span]{}
	tl.navigatedSpans = NoItems[ptrace.Span]{}
	tl.hoveredSpans = NoItems[ptrace.Span]{}

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, timelineHeight)}.Push(gtx.Ops).Pop()
	tl.hover.Add(gtx.Ops)

	if !compact {
		if tl.Hovered() || forceLabel || topBorder {
			// Draw border at top of the timeline
			paint.FillShape(gtx.Ops, colors[colorTimelineBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if tl.Hovered() || forceLabel {
			tl.labelClick.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				labelGtx := gtx
				labelGtx.Constraints.Min = image.Point{}
				labelDims := widget.Label{MaxLines: 1}.Layout(labelGtx, win.Theme.Shaper, font.Font{}, win.Theme.TextSize, tl.label, widget.ColorTextMaterial(gtx, colors[colorTimelineLabel]))
				stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
				pointer.CursorPointer.Add(gtx.Ops)
				stack.Pop()

				return labelDims
			})
		}

		if tl.widgetTooltip != nil && tl.cv.timeline.showTooltips == showTooltipsBoth && tl.labelClick.Hovered() {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return tl.widgetTooltip(win, gtx, tl)
			})
		}

		defer op.Offset(image.Pt(0, timelineLabelHeight)).Push(gtx.Ops).Pop()
	}

	stack := op.TransformOp{}.Push(gtx.Ops)
	if len(tl.tracks) > 0 && tl.tracks[0].TrackWidget == nil {
		// If the first track doesn't have a widget then none of them do. Initialize them.
		// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
		// timelines.
		for i := range tl.tracks {
			tl.tracks[i].TrackWidget = cv.trackWidgetsCache.Get()
			*tl.tracks[i].TrackWidget = TrackWidget{}
		}
		if tl.buildTrackWidgets != nil {
			tl.buildTrackWidgets(tl.tracks)
		}
	}

	for _, track := range tl.tracks {
		if track.kind == TrackKindStack && !tl.cv.timeline.displayStackTracks {
			continue
		}
		dims := track.Layout(win, gtx, tl, cv.timeline.filter, cv.timeline.automaticFilter, trackSpanLabels)
		op.Offset(image.Pt(0, dims.Size.Y+timelineTrackGap)).Add(gtx.Ops)
		if spans := track.HoveredSpans(); spans.Len() != 0 {
			tl.hoveredSpans = spans
		}
		if spans := track.NavigatedSpans(); spans.Len() != 0 {
			tl.navigatedSpans = spans
		}
		if spans := track.ClickedSpans(); spans.Len() != 0 {
			tl.clickedSpans = spans
		}
	}
	stack.Pop()

	tl.labelClicks = 0
	for _, click := range tl.labelClick.Clicks() {
		if click.Modifiers == 0 {
			tl.labelClicks++
		} else if click.Modifiers == key.ModShortcut {
			// XXX this assumes that the first track is the widest one. This is currently true, but a brittle
			// assumption to make.
			tl.navigatedSpans = tl.tracks[0].Spans()
		}
	}

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, timelineHeight)}
}

func defaultSpanColor(spans Items[ptrace.Span]) [2]colorIndex {
	if spans.Len() == 1 {
		return [2]colorIndex{stateColors[spans.At(0).State], 0}
	} else {
		// OPT(dh): this would benefit from iterators, for span selectors backed by data that isn't already made of
		// ptrace.Span
		spans := spans
		c := stateColors[spans.At(0).State]
		for i := 1; i < spans.Len(); i++ {
			s := spans.At(i)
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
	spans   Items[ptrace.Span]
	prevEnd trace.Timestamp
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut Items[ptrace.Span], startPx, endPx float32, ok bool) {
	offset := it.offset
	if offset >= it.spans.Len() {
		return nil, 0, 0, false
	}
	spans := it.spans

	nsPerPx := float32(it.cv.nsPerPx)
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * float64(nsPerPx)))
	startOffset := offset
	cvStart := it.cv.start

	s := spans.AtPtr(offset)
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
		for offset < (spans.Len()) {
			adjustedEnd := end
			if time.Duration(end-start) < minSpanWidthD {
				adjustedEnd = start + trace.Timestamp(minSpanWidthD)
			}

			// For a span to be large enough to stand on its own, it has to end at least minSpanWidthD later than the
			// current span. Use binary search to find that span. This also finds gaps, because for a gap to be big
			// enough, it cannot occur between spans that would be too small according to this search.
			offset = sort.Search((spans.Len()), func(i int) bool {
				return spans.At(i).End >= adjustedEnd+trace.Timestamp(minSpanWidthD)
			})

			if offset == (spans.Len()) {
				// We couldn't find a span -> merge all remaining spans, except for the optional "goroutine returned"
				// span
				if spans.At((spans.Len())-1).State == ptrace.StateDone {
					offset = (spans.Len()) - 1
					end = spans.At(offset - 1).End
					break
				} else {
					offset = (spans.Len())
					end = spans.At(offset - 1).End
					break
				}
			}

			candidateSpan := spans.AtPtr(offset)
			prevSpan := spans.AtPtr(offset - 1)

			cStart := candidateSpan.Start
			cEnd := candidateSpan.End
			prevEnd := prevSpan.End
			if adjustedEnd > cStart {
				cStart = adjustedEnd
			}
			if time.Duration(cEnd-cStart) >= minSpanWidthD || time.Duration(cStart-prevEnd) >= minSpanWidthD {
				end = spans.At(offset - 1).End
				break
			} else {
				end = spans.At(offset).End
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
	return it.spans.Slice(startOffset, offset), startPx, endPx, true
}

func (track *Track) Layout(win *theme.Window, gtx layout.Context, tl *Timeline, filter Filter, automaticFilter Filter, labelsOut *[]string) (dims layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Layout").End()

	cv := tl.cv
	tr := cv.trace
	trackHeight := gtx.Dp(timelineTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	spanHighlightedBorderWidth := gtx.Dp(spanHighlightedBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, trackHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: track, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)
	track.click.Add(gtx.Ops)
	track.hover.Add(gtx.Ops)

	track.clickedSpans = NoItems[ptrace.Span]{}
	track.navigatedSpans = NoItems[ptrace.Span]{}
	track.hoveredSpans = NoItems[ptrace.Span]{}

	trackClickedSpans := false
	trackNavigatedSpans := false
	trackContextMenuSpans := false

	// We're passing gtx.Queue instead of gtx to avoid allocations because of convT. This means gtx.Queue mustn't be
	// nil.
	for _, ev := range track.click.Events(gtx.Queue) {
		if ev.Type == gesture.TypeClick && ev.Button == pointer.ButtonPrimary {
			switch ev.Modifiers {
			case key.ModShortcut:
				trackNavigatedSpans = true
			case 0:
				trackClickedSpans = true
			}
		} else if ev.Type == gesture.TypePress && ev.Button == pointer.ButtonSecondary {
			trackContextMenuSpans = true
		}
	}
	track.hover.Update(gtx.Queue)

	// OPT(dh): don't redraw if the only change is cv.y
	if !track.hover.Hovered() &&
		!track.prevFrame.hovered &&
		cv.unchanged() &&
		(tl.invalidateCache == nil || !tl.invalidateCache(tl, cv)) &&
		gtx.Constraints == track.prevFrame.constraints {

		track.prevFrame.call.Add(gtx.Ops)
		debugCaching(gtx)
		return track.prevFrame.dims
	}

	track.prevFrame.hovered = track.hover.Hovered()
	track.prevFrame.constraints = gtx.Constraints

	origOps := gtx.Ops
	gtx.Ops = track.prevFrame.ops.get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		track.prevFrame.call = call
		track.prevFrame.dims = dims
	}()

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
	doSpans := func(dspSpans Items[ptrace.Span], startPx, endPx float32) {
		hovered := false
		if track.hover.Hovered() && track.hover.Pointer().X >= startPx && track.hover.Pointer().X < endPx {
			// Highlight the span under the cursor
			hovered = true
			track.hoveredSpans = dspSpans

			if trackNavigatedSpans {
				track.navigatedSpans = dspSpans
			}
			if trackClickedSpans {
				track.clickedSpans = dspSpans
			}
			if trackContextMenuSpans {
				if track.spanContextMenu != nil {
					win.SetContextMenu(track.spanContextMenu(dspSpans, cv))
				} else {
					win.SetContextMenu([]*theme.MenuItem{newZoomMenuItem(cv, dspSpans)})
				}
			}
		}

		var cs [2]colorIndex
		if track.spanColor != nil {
			cs = track.spanColor(dspSpans, tr)
		} else {
			cs = defaultSpanColor(dspSpans)
		}

		if cs[1] != 0 && cs[1] != colorStateMerged {
			panic(fmt.Sprintf("two-color spans are only supported with color₁ == colorStateMerged, got %v", cs))
		}

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt((max(startPx, 0)), 0)
		maxP = f32.Pt((min(endPx, float32(gtx.Constraints.Max.X))), float32(trackHeight))

		highlighted := filter.Match(dspSpans, ItemContainer{Timeline: tl, Track: track}) || automaticFilter.Match(dspSpans, ItemContainer{Timeline: tl, Track: track})
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
		spanTooltipState.events = NoItems[ptrace.EventID]{}
		spanTooltipState.eventsUnderCursor = NoItems[ptrace.EventID]{}
		if cv.timeline.showTooltips < showTooltipsNone && track.hover.Hovered() && track.hover.Pointer().X >= startPx && track.hover.Pointer().X < endPx {
			spanTooltipState.spans = dspSpans
			if !track.hideEventMarkers {
				spanTooltipState.events = Events(dspSpans, tr)
			}
		}

		dotRadiusX := float32(gtx.Dp(4))
		dotRadiusY := float32(gtx.Dp(3))
		if !track.hideEventMarkers && maxP.X-minP.X > dotRadiusX*2 && dspSpans.Len() == 1 {
			// We only display event dots in unmerged spans because merged spans can split into smaller spans when we
			// zoom in, causing dots to disappear and reappearappear and disappear.
			events := Events(dspSpans.Slice(0, 1), tr)

			dotGap := float32(gtx.Dp(4))
			centerY := float32(trackHeight) / 2

			for i := 0; i < events.Len(); i++ {
				ev := events.At(i)
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
				for i = i + 1; i < events.Len(); i++ {
					ev := events.At(i)
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

				if cv.timeline.showTooltips < showTooltipsNone && track.hover.Hovered() && track.hover.Pointer().X >= minX && track.hover.Pointer().X < maxX {
					spanTooltipState.eventsUnderCursor = events.Slice(oldi, i+1)
				}
			}
		}

		if spanTooltipState.spans != nil && track.spanTooltip != nil {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return track.spanTooltip(win, gtx, tr, spanTooltipState)
			})
		}

		if track.spanLabel != nil && maxP.X-minP.X > float32(2*minSpanWidth) {
			// The Label callback, if set, returns a list of labels to try and use for the span. We pick the first label
			// that fits fully in the span, as it would be drawn untruncated. That is, the ideal label size depends on
			// the zoom level, not panning. If no label fits, we use the last label in the list. This label can be the
			// empty string to effectively display no label.
			//
			// We don't try to render a label for very small spans.
			if *labelsOut = track.spanLabel(dspSpans, tr, (*labelsOut)[:0]); len(*labelsOut) > 0 {
				for i, label := range *labelsOut {
					if label == "" {
						continue
					}

					font := font.Font{Weight: font.ExtraBold}
					n := win.TextLength(gtx, widget.Label{}, font, win.Theme.TextSize, label)
					if float32(n) > endPx-startPx {
						// This label doesn't fit. If the callback provided more labels, try those instead. If it is the
						// last label, use it and let Gio truncate it, appending a truncation indicator if necessary.
						if i < len(*labelsOut)-1 {
							continue
						}
					}

					var dims layout.Dimensions
					var call op.CallOp
					{
						gtx := gtx
						gtx.Ops = labelsOps
						m := op.Record(gtx.Ops)
						gtx.Constraints.Min = image.Point{}
						gtx.Constraints.Max = image.Pt(int(round32(maxP.X-minP.X)), int(round32(maxP.Y-minP.Y)))
						dims = widget.Label{MaxLines: 1, Truncator: "…", WrapPolicy: text.WrapGraphemes}.
							Layout(gtx, win.Theme.Shaper, font, win.Theme.TextSize, label, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
						call = m.Stop()
					}
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
					stack2 := clip.FRect{Max: f32.Pt(maxP.X-minP.X, maxP.Y-minP.Y)}.Op(labelsOps).Push(labelsOps)
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
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := track.prevFrame.dspSpans[:0]
		it := renderedSpansIterator{
			cv:    cv,
			spans: cv.visibleSpans(track.Spans()),
		}
		for {
			dspSpans, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}
			allDspSpans = append(allDspSpans, struct {
				dspSpans       Items[ptrace.Span]
				startPx, endPx float32
			}{dspSpans, startPx, endPx})
			doSpans(dspSpans, startPx, endPx)
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
		// OPT(dh): don't build all spans just to check their length
		if track.Spans().Len() == 0 {
			// A track with no spans is similar to a track that's always dead
			deadFromPx = 0
		} else {
			if len(track.prevFrame.dspSpans) > 0 {
				// If the first displayed span is also the first overall span, display an indicator that the
				// goroutine/processor hasn't been created yet.
				dspFirst := track.prevFrame.dspSpans[0]
				if dspFirst.dspSpans.At(0) == track.Spans().At(0) {
					end := dspFirst.startPx
					unbornUntilPx = end
				}

				// If the last displayed span is also the last overall span, display an indicator that the
				// goroutine/processor is dead.
				dspLast := track.prevFrame.dspSpans[len(track.prevFrame.dspSpans)-1]
				if LastSpan(dspLast.dspSpans) == LastSpan(track.Spans()) {
					start := dspLast.endPx
					deadFromPx = start
				}

			} else {
				// We didn't draw any spans. We're either displaying a not-yet-alive section, a dead section, or a gap
				// between spans (for processor tracks).
				born := track.Spans().At(0).Start
				died := LastSpan(track.Spans()).End

				if cv.start >= died {
					// The goroutine is dead
					deadFromPx = 0
				} else if cv.End() < born {
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
			paint.FillShape(gtx.Ops, rgba(0x10a56fFF), clip.FRect{Min: f32.Pt(0, top), Max: f32.Pt(unbornUntilPx, bottom)}.Op(gtx.Ops))
		}
		if deadFromPx < visWidthPx {
			// Draw the dead indicator
			paint.FillShape(gtx.Ops, rgba(0x6F6F6FFF), clip.FRect{Min: f32.Pt(deadFromPx, top), Max: f32.Pt(visWidthPx, bottom)}.Op(gtx.Ops))
		}
	}

	// Draw the span outlines. We draw these as solid rectangles and let the spans overlay them.
	//
	// Drawing solid rectangles that get covered up seems to be much faster than using strokes, at least in this
	// specific instance.
	paint.FillShape(gtx.Ops, win.Theme.Palette.Foreground, clip.Outline{Path: outlinesPath.End()}.Op())
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

func singleSpanLabel(label string, showForMerged bool) func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	return func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
		if !showForMerged && spans.Len() != 1 {
			return out
		}
		return append(out, label)
	}
}

func singleSpanColor(c colorIndex) func(spans Items[ptrace.Span], tr *Trace) [2]colorIndex {
	return func(spans Items[ptrace.Span], tr *Trace) [2]colorIndex {
		if spans.Len() == 1 {
			return [2]colorIndex{c, 0}
		} else {
			return [2]colorIndex{c, colorStateMerged}
		}
	}
}

func NewGCTimeline(cv *Canvas, trace *Trace, spans []ptrace.Span) *Timeline {
	tl := &Timeline{
		label:     "GC",
		shortName: "GC",

		buildTrackWidgets: func(tracks []*Track) {
			*tracks[0].TrackWidget = TrackWidget{
				spanLabel: singleSpanLabel("GC", true),
				spanColor: singleSpanColor(colorStateGC),
			}
		},
	}
	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}

	ss := SimpleItems[ptrace.Span]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}

	tl.tracks[0].spans_ = ss
	tl.item = &GC{ss}

	return tl
}

func NewSTWTimeline(cv *Canvas, tr *Trace, spans []ptrace.Span) *Timeline {
	tl := &Timeline{
		label:     "STW",
		shortName: "STW",

		buildTrackWidgets: func(tracks []*Track) {
			*tracks[0].TrackWidget = TrackWidget{
				// spanLabel: singleSpanLabel("STW", true),
				spanLabel: func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
					if spans.Len() != 1 {
						return nil
					}
					kindID := tr.Events[spans.At(0).Event].Args[trace.ArgSTWStartKind]
					return append(out, stwSpanLabels[tr.STWReason(kindID)])
				},
				spanColor: singleSpanColor(colorStateSTW),
			}
		},
	}

	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}
	ss := SimpleItems[ptrace.Span]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}

	tl.tracks[0].spans_ = ss
	tl.item = &STW{ss}

	return tl
}

var stwSpanLabels = [trace.NumSTWReasons]string{}

func init() {
	for i := 0; i < trace.NumSTWReasons; i++ {
		stwSpanLabels[i] = fmt.Sprintf("STW (%s)", trace.STWReason(i).String())
	}
}

type GC struct {
	Spans Items[ptrace.Span]
}

type STW struct {
	Spans Items[ptrace.Span]
}

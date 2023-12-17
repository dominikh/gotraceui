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
	"unsafe"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	myunsafe "honnef.co/go/gotraceui/unsafe"
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

const statePlaceholder = ptrace.StateLast + 1

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
	cv        *Canvas

	widget *TimelineWidget
}

type TimelineWidget struct {
	labelClick       widget.Clickable
	labelClicks      int
	labelRightClicks int

	hover gesture.Hover

	// OPT(dh): Only one timeline can have hovered or activated spans, so we could track this directly in Canvas, and
	// save 48 bytes per timeline (which means per goroutine). However, the current API is cleaner, because
	// TimelineWidget doesn't have to mutate Timeline's state.
	//
	// OPT(dh): clicked spans and navigated spans are mutually exclusive, combine the fields
	clickedSpans   Items[ptrace.Span]
	navigatedSpans Items[ptrace.Span]
	hoveredSpans   Items[ptrace.Span]

	usedSuboptimalTexture time.Time
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
	spans            *theme.Future[Items[ptrace.Span]]
	compressedSpans  compressedStackSpans
	events           []ptrace.EventID
	hideEventMarkers bool

	Start trace.Timestamp
	End   trace.Timestamp
	Len   int

	spanLabel       func(spans Items[ptrace.Span], tr *Trace, out []string) []string
	spanColor       func(span ptrace.Span, tr *Trace) colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions
	spanContextMenu func(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem

	rnd    Renderer
	widget *TrackWidget
}

func (track *Track) SpanColor(span ptrace.Span, tr *Trace) colorIndex {
	if track.spanColor != nil {
		return track.spanColor(span, tr)
	} else {
		return defaultSpanColor(span, tr)
	}
}

func NewTrack(parent *Timeline, kind TrackKind) *Track {
	return &Track{
		parent: parent,
		kind:   kind,
	}
}

func (tr *Track) Spans(win *theme.Window) *theme.Future[Items[ptrace.Span]] {
	if tr.spans != nil {
		return tr.spans
	}
	if tr.compressedSpans.count == 0 {
		tr.spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
			container: ItemContainer{
				Timeline: tr.parent,
				Track:    tr,
			},
			contiguous: true,
			subslice:   true,
		})
		return tr.spans
	}

	tr.spans = theme.NewFuture(win, func(cancelled <-chan struct{}) Items[ptrace.Span] {
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
				bitunpackByte(v, myunsafe.Cast[*uint64](&out[i*8]))
			}
			return out
		}

		n := tr.compressedSpans.count
		if n == 0 || n*2 == 0 {
			// This is unreachable and only aids the bounds checker, eliminating bounds checks in calls to DecodeUnsafe for
			// the second argument.
			return nil
		}

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
		DecodeUnsafe(c.startsEnds, &startsEnds[0])
		DecodeUnsafe(c.eventIDs, &eventIDs[0])
		DecodeUnsafe(c.pcs, &pcs[0])
		DecodeUnsafe(c.nums, &nums[0])

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
		startsEndsPairwise := unsafe.Slice(myunsafe.Cast[*[2]uint64](&startsEnds[0]), n)[:n]
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

		out := SimpleItems[ptrace.Span, stackSpanMeta]{
			items: spans,
			metas: metas,
			container: ItemContainer{
				Timeline: tr.parent,
				Track:    tr,
			},
			subslice: true,
		}

		boolSliceCache.Put(isCPUSample)

		select {
		case <-cancelled:
			// The future has already been cancelled. Return slices to caches.
			spanSliceCache.Put(spans)
			stackSpanMetaSliceCache.Put(metas)
			return nil
		default:
			return out
		}
	})

	return tr.spans
}

func newZoomMenuItem(cv *Canvas, spans Items[ptrace.Span]) *theme.MenuItem {
	return &theme.MenuItem{
		Label:    PlainLabel("Zoom"),
		Shortcut: key.ModShortcut.String() + "+LMB",
		Action: func() theme.Action {
			return theme.ExecuteAction(func(gtx layout.Context) {
				start := spans.At(0).Start
				end := LastSpan(spans).End
				cv.navigateToStartAndEnd(gtx, start, end, cv.y)
			})
		},
	}
}

func newOpenSpansMenuItem(spans Items[ptrace.Span]) *theme.MenuItem {
	return &theme.MenuItem{
		Label: PlainLabel("Show span info"),
		Action: func() theme.Action {
			return &OpenSpansAction{
				Spans: spans,
			}
		},
	}
}

type TrackWidget struct {
	// OPT(dh): Only one track can have hovered or activated spans, so we could track this directly in TimelineWidget,
	// and save 48 bytes per track. However, the current API is cleaner, because TimelineWidgetTrack doesn't have to
	// mutate TimelineWidget's state.
	//
	// OPT(dh): clickedSpans and navigatedSpans are mutually exclusive, combine the fields
	clickedSpans     Items[ptrace.Span]
	navigatedSpans   Items[ptrace.Span]
	hoveredSpans     Items[ptrace.Span]
	lowQualityRender bool

	// op lists get reused between frames to avoid generating garbage
	ops                             [colorStateLast * 2]op.Ops
	outlinesOps                     mem.ReusableOps
	highlightedPrimaryOutlinesOps   mem.ReusableOps
	highlightedSecondaryOutlinesOps mem.ReusableOps
	eventsOps                       mem.ReusableOps
	labelsOps                       mem.ReusableOps

	hover gesture.Hover
	click gesture.Click

	scratchHighlighted []clip.FRect

	// cached state
	prevFrame struct {
		hovered          bool
		constraints      layout.Constraints
		ops              mem.ReusableOps
		call             op.CallOp
		dims             layout.Dimensions
		placeholder      bool
		lowQualityRender bool

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

func (tw *TimelineWidget) LabelRightClicked() bool {
	if tw.labelRightClicks > 0 {
		tw.labelRightClicks--
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
		cv.trackWidgetsCache.Put(track.widget)
		track.widget = nil
		// TODO(dh): this code is ugly and punches through abstractions.
		if track.compressedSpans.count != 0 {
			if track.spans != nil {
				if spans, ok := track.spans.ResultNoWait(); ok {
					// XXX instead of special-casing SimpleItems and stackSpanMeta here, specify some interface
					if spans, ok := spans.(SimpleItems[ptrace.Span, stackSpanMeta]); ok {
						stackSpanMetaSliceCache.Put(spans.metas)
						spanSliceCache.Put(spans.items)
					}
				}
			}
			track.spans = nil
		}
	}
	cv.timelineWidgetsCache.Put(tl.widget)
	tl.widget = nil
}

func (tl *Timeline) Layout(
	win *theme.Window,
	gtx layout.Context,
	cv *Canvas,
	forceLabel bool,
	compact bool,
	topBorder bool,
	trackSpanLabels *[]string,
) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidget.Layout").End()

	if tl.widget == nil {
		rtrace.Logf(context.Background(), "", "loading timeline widget %q", tl.label)
		tl.widget = cv.timelineWidgetsCache.Get()
		*tl.widget = TimelineWidget{}
	}

	tl.widget.hover.Update(gtx.Queue)

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

	tl.widget.clickedSpans = NoItems[ptrace.Span]{}
	tl.widget.navigatedSpans = NoItems[ptrace.Span]{}
	tl.widget.hoveredSpans = NoItems[ptrace.Span]{}

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, timelineHeight)}.Push(gtx.Ops).Pop()
	tl.widget.hover.Add(gtx.Ops)

	if !compact {
		if tl.widget.Hovered() || forceLabel || topBorder {
			// Draw border at top of the timeline
			theme.FillShape(win, gtx.Ops, colors[colorTimelineBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if tl.widget.Hovered() || forceLabel {
			tl.widget.labelClick.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				labelGtx := gtx
				labelGtx.Constraints.Min = image.Point{}
				labelDims := widget.Label{MaxLines: 1}.Layout(labelGtx, win.Theme.Shaper, font.Font{}, win.Theme.TextSize, tl.label, win.ColorMaterial(gtx, colors[colorTimelineLabel]))
				stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
				pointer.CursorPointer.Add(gtx.Ops)
				stack.Pop()

				return labelDims
			})
		}

		if tl.widgetTooltip != nil && tl.cv.timeline.showTooltips == showTooltipsBoth && tl.widget.labelClick.Hovered() {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return tl.widgetTooltip(win, gtx, tl)
			})
		}

		defer op.Offset(image.Pt(0, timelineLabelHeight)).Push(gtx.Ops).Pop()
	}

	stack := op.TransformOp{}.Push(gtx.Ops)
	if len(tl.tracks) > 0 && tl.tracks[0].widget == nil {
		// If the first track doesn't have a widget then none of them do. Initialize them.
		// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
		// timelines.
		for i := range tl.tracks {
			tl.tracks[i].widget = cv.trackWidgetsCache.Get()
			*tl.tracks[i].widget = TrackWidget{}
		}
		if tl.buildTrackWidgets != nil {
			tl.buildTrackWidgets(tl.tracks)
		}
	}

	suboptimal := false
	for _, track := range tl.tracks {
		if track.kind == TrackKindStack && !tl.cv.timeline.displayStackTracks {
			continue
		}
		dims := track.Layout(win, gtx, tl, cv.timeline.filter, cv.timeline.automaticFilter, trackSpanLabels)
		op.Offset(image.Pt(0, dims.Size.Y+timelineTrackGap)).Add(gtx.Ops)
		if spans := track.widget.HoveredSpans(); spans.Len() != 0 {
			tl.widget.hoveredSpans = spans
		}
		if spans := track.widget.NavigatedSpans(); spans.Len() != 0 {
			tl.widget.navigatedSpans = spans
		}
		if spans := track.widget.ClickedSpans(); spans.Len() != 0 {
			tl.widget.clickedSpans = spans
		}
		if track.widget.lowQualityRender {
			suboptimal = true
		}
	}
	if !suboptimal {
		tl.widget.usedSuboptimalTexture = time.Time{}
	} else if tl.widget.usedSuboptimalTexture.IsZero() {
		tl.widget.usedSuboptimalTexture = gtx.Now
	}
	stack.Pop()

	tl.widget.labelClicks = 0
	for _, click := range tl.widget.labelClick.Clicks() {
		switch click.Button {
		case pointer.ButtonPrimary:
			if click.Modifiers == 0 {
				tl.widget.labelClicks++
			} else if click.Modifiers == key.ModShortcut {
				// XXX this assumes that the first track is the widest one. This is currently true, but a brittle
				// assumption to make.
				tl.widget.navigatedSpans = tl.tracks[0].Spans(win).Wait()
			}

		case pointer.ButtonSecondary:
			tl.widget.labelRightClicks++
		}
	}

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, timelineHeight)}
}

func defaultSpanColor(span ptrace.Span, tr *Trace) colorIndex {
	return stateColors[span.State]
}

type renderedSpansIterator struct {
	offset int
	cv     *Canvas
	spans  Items[ptrace.Span]
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut Items[ptrace.Span], startPx, endPx float32, ok bool) {
	offset := it.offset
	if offset >= it.spans.Len() {
		return nil, 0, 0, false
	}
	spans := it.spans

	nsPerPx := float32(it.cv.nsPerPx)
	// Merge spans smaller than minSpanWidthD.
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * it.cv.nsPerPx))
	startOffset := offset
	cvStart := it.cv.start

	s := spans.AtPtr(offset)
	offset++

	start := s.Start
	end := s.End

	if time.Duration(end-start) < minSpanWidthD && s.State != ptrace.StateDone {
		// Merge all tiny spans until we find a span or gap that's big enough to stand on its own. We do not stop
		// merging after we've reached the minimum size because that can lead to multiple merges being next to each
		// other. Not only does this look bad, it is also prone to tiny spans toggling between two merged spans, and
		// previously merged spans becoming visible again when zooming out.
		for offset < spans.Len() {
			adjustedEnd := end

			// For a span to be large enough to stand on its own, it has to end at least minSpanWidthD later than the
			// current span. Use binary search to find that span. This also finds gaps, because for a gap to be big
			// enough, it cannot occur between spans that would be too small according to this search.
			offset = sort.Search(spans.Len(), func(i int) bool {
				return spans.At(i).End >= adjustedEnd+trace.Timestamp(minSpanWidthD)
			})

			if offset == spans.Len() {
				// We couldn't find a span -> merge all remaining spans, except for the optional "goroutine returned"
				// span
				if spans.At(spans.Len()-1).State == ptrace.StateDone {
					offset = spans.Len() - 1
					end = spans.At(offset - 1).End
					break
				} else {
					offset = spans.Len()
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

	it.offset = offset
	startPx = float32(start-cvStart) / nsPerPx
	endPx = float32(end-cvStart) / nsPerPx
	return it.spans.Slice(startOffset, offset), startPx, endPx, true
}

func (track *Track) Layout(
	win *theme.Window,
	gtx layout.Context,
	tl *Timeline,
	filter Filter,
	automaticFilter Filter,
	labelsOut *[]string,
) (dims layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Layout").End()

	cv := tl.cv
	tr := cv.trace
	trackHeight := gtx.Dp(timelineTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, trackHeight)}.Push(gtx.Ops).Pop()
	pointer.InputOp{Tag: track, Types: pointer.Enter | pointer.Leave | pointer.Move | pointer.Cancel | pointer.Press}.Add(gtx.Ops)
	track.widget.click.Add(gtx.Ops)
	track.widget.hover.Add(gtx.Ops)

	track.widget.clickedSpans = NoItems[ptrace.Span]{}
	track.widget.navigatedSpans = NoItems[ptrace.Span]{}
	track.widget.hoveredSpans = NoItems[ptrace.Span]{}
	track.widget.lowQualityRender = false

	trackClickedSpans := false
	trackNavigatedSpans := false
	trackContextMenuSpans := false

	// We're passing gtx.Queue instead of gtx to avoid allocations because of convT. This means gtx.Queue mustn't be
	// nil.
	for _, ev := range track.widget.click.Events(gtx.Queue) {
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
	track.widget.hover.Update(gtx.Queue)

	spans, haveSpans := track.Spans(win).ResultNoWait()
	if !haveSpans {
		spans = SimpleItems[ptrace.Span, any]{
			items: []ptrace.Span{
				{
					Start: track.Start,
					End:   track.End,
					State: statePlaceholder,
				},
			},
			container:  ItemContainer{Timeline: tl, Track: track},
			contiguous: false,
			subslice:   true,
		}
		track.widget.lowQualityRender = true
	}

	// // OPT(dh): don't redraw if the only change is cv.y
	if !track.widget.hover.Hovered() &&
		!track.widget.prevFrame.hovered &&
		cv.unchanged(gtx) &&
		(tl.invalidateCache == nil || !tl.invalidateCache(tl, cv)) &&
		track.widget.prevFrame.placeholder == !haveSpans &&
		!track.widget.prevFrame.lowQualityRender &&
		gtx.Constraints == track.widget.prevFrame.constraints {

		track.widget.prevFrame.call.Add(gtx.Ops)
		debugCaching(win, gtx)
		return track.widget.prevFrame.dims
	}

	track.widget.prevFrame.hovered = track.widget.hover.Hovered()
	track.widget.prevFrame.constraints = gtx.Constraints

	origOps := gtx.Ops
	gtx.Ops = track.widget.prevFrame.ops.Get()
	macro := op.Record(gtx.Ops)
	defer func() {
		call := macro.Stop()
		call.Add(origOps)
		track.widget.prevFrame.placeholder = !haveSpans
		track.widget.prevFrame.call = call
		track.widget.prevFrame.dims = dims
		track.widget.prevFrame.lowQualityRender = track.widget.lowQualityRender
	}()

	// Draw timeline lifetimes
	//
	// We batch draw operations by color to avoid making thousands of draw calls. See
	// https://lists.sr.ht/~eliasnaur/gio/%3C871qvbdx5r.fsf%40honnef.co%3E#%3C87v8smctsd.fsf@honnef.co%3E
	//
	for i := range track.widget.ops {
		track.widget.ops[i].Reset()
	}
	// one path per single-color span
	//
	//gcassert:noescape
	paths := [colorStateLast]clip.Path{}

	var hoveredSpan clip.FRect
	highlightedSpans := track.widget.scratchHighlighted[:0]

	var outlinesPath clip.Path
	var eventsPath clip.Path
	outlinesPath.Begin(track.widget.outlinesOps.Get())
	eventsPath.Begin(track.widget.eventsOps.Get())
	labelsOps := track.widget.labelsOps.Get()
	labelsMacro := op.Record(labelsOps)

	for i := range paths {
		paths[i].Begin(&track.widget.ops[i])
	}

	first := true
	var prevEndPx float32
	doSpans := func(dspSpans Items[ptrace.Span], startPx, endPx float32) {
		hovered := false
		if track.widget.hover.Hovered() && track.widget.hover.Pointer().X >= startPx && track.widget.hover.Pointer().X < endPx && haveSpans {
			// Highlight the span under the cursor
			hovered = true
			track.widget.hoveredSpans = dspSpans

			if trackNavigatedSpans {
				track.widget.navigatedSpans = dspSpans
			}
			if trackClickedSpans {
				track.widget.clickedSpans = dspSpans
			}
			if trackContextMenuSpans {
				if track.spanContextMenu != nil {
					win.SetContextMenu(track.spanContextMenu(dspSpans, cv))
				} else {
					win.SetContextMenu([]*theme.MenuItem{
						newZoomMenuItem(cv, dspSpans),
						newOpenSpansMenuItem(dspSpans),
					})
				}
			}
		}

		var cs colorIndex
		if dspSpans.Len() == 1 {
			cs = track.SpanColor(dspSpans.At(0), tr)
		}

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt(max(startPx, 0), 0)
		maxP = f32.Pt(min(endPx, float32(gtx.Constraints.Max.X)), float32(trackHeight))

		if filter.Match(dspSpans, ItemContainer{Timeline: tl, Track: track}) || automaticFilter.Match(dspSpans, ItemContainer{Timeline: tl, Track: track}) {
			highlightedSpans = append(highlightedSpans, clip.FRect{Min: minP, Max: maxP})
		}

		if dspSpans.Len() != 0 && dspSpans.At(0).State != statePlaceholder {
			off := float32(spanBorderWidth)
			leftOff := off
			rightOff := -off
			// TODO(dh): do we need 'first'? can startPx be negative for the second span?
			if (first && startPx < 0) || !first && startPx == prevEndPx {
				leftOff = 0
			}
			if endPx > float32(cv.width) {
				rightOff = 0
			}

			outlinesPath.MoveTo(minP)
			outlinesPath.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
			outlinesPath.LineTo(maxP)
			outlinesPath.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
			outlinesPath.Close()

			// Cut a hole
			outlinesPath.MoveTo(minP.Add(f32.Pt(leftOff, off)))
			outlinesPath.LineTo(f32.Point{X: minP.X + leftOff, Y: maxP.Y - off})
			outlinesPath.LineTo(maxP.Add(f32.Pt(rightOff, -off)))
			outlinesPath.LineTo(f32.Point{X: maxP.X + rightOff, Y: minP.Y + off})
			outlinesPath.Close()

			if hovered {
				hoveredSpan = clip.FRect{Min: minP, Max: maxP}
			}
		}

		if endPx-startPx >= float32(gtx.Dp(minSpanWidthDp)) {
			if dspSpans.Len() == 1 {
				if maxP.X > minP.X {
					p := &paths[cs]
					p.MoveTo(minP)
					p.LineTo(f32.Point{X: maxP.X, Y: minP.Y})
					p.LineTo(maxP)
					p.LineTo(f32.Point{X: minP.X, Y: maxP.Y})
					p.Close()
				}
			}

			var spanTooltipState SpanTooltipState
			spanTooltipState.events = NoItems[ptrace.EventID]{}
			spanTooltipState.eventsUnderCursor = NoItems[ptrace.EventID]{}
			if cv.timeline.showTooltips < showTooltipsNone && hovered {
				spanTooltipState.spans = dspSpans
				if !track.hideEventMarkers {
					spanTooltipState.events = Events(dspSpans, tr)
				}
			}

			dotRadiusX := float32(gtx.Dp(4))
			dotRadiusY := float32(gtx.Dp(3))
			if !track.hideEventMarkers && maxP.X-minP.X > dotRadiusX*2 {
				events := Events(dspSpans, tr)

				dotGap := float32(gtx.Dp(1))
				centerY := float32(trackHeight) / 2

				for i, n := 0, events.Len(); i < n; i++ {
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
					// Merge events that are too close together
					for {
						delta := dotRadiusX*2 + dotGap
						needle := end + delta
						j := sort.Search(n, func(j int) bool {
							ev := events.At(j)
							return cv.tsToPx(tr.Event(ev).Ts) >= needle
						})
						if j == n {
							// We couldn't find an event -> merge all remaining events
							i = j - 1
							end = cv.tsToPx(tr.Event(events.At(i)).Ts)
							break
						}
						candidate := tr.Event(events.At(j))
						prev := tr.Event(events.At(j - 1))
						prevPx := cv.tsToPx(prev.Ts)
						candidatePx := cv.tsToPx(candidate.Ts)
						if candidatePx > prevPx+delta {
							i = j - 1
							end = prevPx
							break
						} else {
							end = candidatePx
						}
					}

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

					if cv.timeline.showTooltips < showTooltipsNone && track.widget.hover.Hovered() && track.widget.hover.Pointer().X >= minX && track.widget.hover.Pointer().X < maxX {
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

			if track.spanLabel != nil && maxP.X-minP.X > float32(2*minSpanWidth) && dspSpans.Len() == 1 {
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
								Layout(gtx, win.Theme.Shaper, font, win.Theme.TextSize, label, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
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
						paint.ColorOp{Color: win.ConvertColor(win.Theme.Palette.Foreground)}.Add(labelsOps)
						stack2 := clip.FRect{Max: f32.Pt(maxP.X-minP.X, maxP.Y-minP.Y)}.Op(labelsOps).Push(labelsOps)
						call.Add(labelsOps)
						stack2.Pop()
						stack.Pop()
						break
					}
				}
			}
		}
		prevEndPx = endPx
		first = false
	}

	allDspSpans := track.widget.prevFrame.dspSpans[:0]
	// OPT(dh): reuse slice between frames
	var texs []TextureStack
	texs = track.rnd.Render(win, track, spans, cv.nsPerPx, cv.start, cv.End(), texs)
	for _, tex := range texs {
		if !tex.Add(win, gtx, &cv.textures, tr, gtx.Ops) {
			track.widget.lowQualityRender = true
		}
	}

	if cv.unchanged(gtx) && track.widget.prevFrame.dspSpans != nil && track.widget.prevFrame.placeholder == !haveSpans {
		for _, prevSpans := range track.widget.prevFrame.dspSpans {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		it := renderedSpansIterator{
			cv:    cv,
			spans: cv.visibleSpans(spans),
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

		track.widget.prevFrame.dspSpans = allDspSpans
	}

	if track.kind == TrackKindUnspecified {
		// Indicate parts of time where a goroutine or processor wasn't yet alive and where it no longer exists.
		var (
			visWidthPx    float32 = float32(gtx.Constraints.Max.X)
			unbornUntilPx float32
			deadFromPx    float32 = visWidthPx
		)
		if track.Len == 0 {
			// A track with no spans is similar to a track that's always dead
			deadFromPx = 0
		} else {
			if len(track.widget.prevFrame.dspSpans) > 0 {
				// If the first displayed span is also the first overall span, display an indicator that the
				// goroutine/processor hasn't been created yet.
				dspFirst := track.widget.prevFrame.dspSpans[0]
				if dspFirst.dspSpans.At(0) == spans.At(0) {
					end := dspFirst.startPx
					unbornUntilPx = end
				}

				// If the last displayed span is also the last overall span, display an indicator that the
				// goroutine/processor is dead.
				dspLast := track.widget.prevFrame.dspSpans[len(track.widget.prevFrame.dspSpans)-1]
				if LastSpan(dspLast.dspSpans) == LastSpan(spans) {
					start := dspLast.endPx
					deadFromPx = start
				}

			} else {
				// We didn't draw any spans. We're either displaying a not-yet-alive section, a dead section, or a gap
				// between spans (for processor tracks).
				born := track.Start
				died := track.End

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
			theme.FillShape(win, gtx.Ops, oklch(63.87, 0.14, 160.78), clip.FRect{Min: f32.Pt(0, top), Max: f32.Pt(unbornUntilPx, bottom)}.Op(gtx.Ops))
		}
		if deadFromPx < visWidthPx {
			// Draw the dead indicator
			theme.FillShape(win, gtx.Ops, oklch(54.17, 0, 0), clip.FRect{Min: f32.Pt(deadFromPx, top), Max: f32.Pt(visWidthPx, bottom)}.Op(gtx.Ops))
		}
	}

	// Draw the spans
	for cIdx := range paths {
		theme.FillShape(win, gtx.Ops, colors[cIdx], clip.Outline{Path: paths[cIdx].End()}.Op())
	}

	// Highlight the hovered span
	if hoveredSpan != (clip.FRect{}) {
		stack := hoveredSpan.Op(gtx.Ops).Push(gtx.Ops)
		paint.LinearGradientOp{
			Stop1:  hoveredSpan.Max,
			Stop2:  f32.Pt(hoveredSpan.Max.X, hoveredSpan.Min.Y+float32(trackHeight)/4),
			Color1: colors[colorSpanHighlightedPrimaryOutline].NRGBA(),
			Color2: color.NRGBA{0, 0, 0, 0},
		}.Add(gtx.Ops)
		paint.PaintOp{}.Add(gtx.Ops)
		stack.Pop()
	}

	// Highlight highlighted spans
	for _, r := range highlightedSpans {
		if r == hoveredSpan {
			continue
		}
		stack := r.Op(gtx.Ops).Push(gtx.Ops)
		paint.LinearGradientOp{
			Stop1:  r.Max,
			Stop2:  f32.Pt(r.Max.X, r.Min.Y+float32(trackHeight)/4),
			Color1: colors[colorSpanHighlightedSecondaryOutline].NRGBA(),
			Color2: color.NRGBA{0, 0, 0, 0},
		}.Add(gtx.Ops)
		paint.PaintOp{}.Add(gtx.Ops)
		stack.Pop()
	}

	// Draw the event markers
	theme.FillShape(win, gtx.Ops, oklcha(0, 0, 0, 0.85), clip.Outline{Path: eventsPath.End()}.Op())

	// Print labels
	labelsMacro.Stop().Add(gtx.Ops)

	// Draw the span outlines
	theme.FillShape(win, gtx.Ops, win.Theme.Palette.Foreground, clip.Outline{Path: outlinesPath.End()}.Op())

	track.widget.scratchHighlighted = highlightedSpans[:0]

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, trackHeight)}
}

func singleSpanLabel(label string) func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	return func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
		return append(out, label)
	}
}

func singleSpanColor(c colorIndex) func(span ptrace.Span, tr *Trace) colorIndex {
	return func(span ptrace.Span, tr *Trace) colorIndex {
		return c
	}
}

func NewGCTimeline(cv *Canvas, trace *Trace, spans []ptrace.Span) *Timeline {
	tl := &Timeline{
		label:     "GC",
		shortName: "GC",
		cv:        cv,
	}
	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}

	ss := SimpleItems[ptrace.Span, any]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}

	if len(spans) > 0 {
		tl.tracks[0].Start = spans[0].Start
		tl.tracks[0].End = spans[len(spans)-1].End
		tl.tracks[0].Len = len(spans)
	}
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](ss)
	tl.tracks[0].spanLabel = singleSpanLabel("GC")
	tl.tracks[0].spanColor = singleSpanColor(colorStateGC)
	tl.item = &GC{ss}

	return tl
}

func NewSTWTimeline(cv *Canvas, tr *Trace, spans []ptrace.Span) *Timeline {
	tl := &Timeline{
		label:     "STW",
		shortName: "STW",
		cv:        cv,
	}

	tl.tracks = []*Track{
		NewTrack(tl, TrackKindUnspecified),
	}
	ss := SimpleItems[ptrace.Span, any]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}

	if len(spans) > 0 {
		tl.tracks[0].Start = spans[0].Start
		tl.tracks[0].End = spans[len(spans)-1].End
		tl.tracks[0].Len = len(spans)
	}
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](ss)
	tl.tracks[0].spanLabel = func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
		span := spans.At(0)
		kindID := tr.Events[span.Event].Args[trace.ArgSTWStartKind]
		return append(out, stwSpanLabels[tr.STWReason(kindID)])
	}
	tl.tracks[0].spanColor = singleSpanColor(colorStateSTW)
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

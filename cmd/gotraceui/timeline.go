package main

import (
	"context"
	"fmt"
	"image"
	stdcolor "image/color"
	"math"
	rtrace "runtime/trace"
	"sort"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/container"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	myunsafe "honnef.co/go/gotraceui/unsafe"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/event"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	exptrace "golang.org/x/exp/trace"
)

const (
	// XXX the label height depends on the font used
	timelineLabelHeightDp     unit.Dp = 17.5
	timelineMinitrackHeightDp unit.Dp = 5
	timelineMinitrackGapDp    unit.Dp = 1
	timelineTrackHeightDp     unit.Dp = 16
	timelineGapDp             unit.Dp = 5 - timelineMinitrackGapDp
	timelineTrackGapDp        unit.Dp = timelineMinitrackHeightDp
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
	clickedSpans      Items[ptrace.Span]
	navigatedTimeSpan container.Option[TimeSpan]

	usedSuboptimalTexture time.Time
}

func (tw *TimelineWidget) Hovered(gtx layout.Context) bool {
	if tw == nil {
		return false
	}
	return tw.hover.Update(gtx.Queue)
}

type Track struct {
	parent           *Timeline
	kind             TrackKind
	spans            *theme.Future[Items[ptrace.Span]]
	compute          func(track *Track, cancelled <-chan struct{}) Items[ptrace.Span]
	events           []ptrace.EventID
	samples          []ptrace.EventID
	hideEventMarkers bool

	stackLevel int

	Start exptrace.Time
	End   exptrace.Time

	spanLabel       func(spans Items[ptrace.Span], tr *Trace, out []string) []string
	spanColor       func(span *ptrace.Span, tr *Trace) colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions
	spanContextMenu func(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem

	rnd    Renderer
	widget *TrackWidget
}

func (tl *Timeline) ensureTrackWidgets() {
	if len(tl.tracks) == 0 || tl.tracks[0].widget != nil {
		return
	}

	// If the first track doesn't have a widget then none of them do. Initialize them.
	// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
	// timelines.
	for i := range tl.tracks {
		tl.tracks[i].widget = tl.cv.trackWidgetsCache.Get()
		*tl.tracks[i].widget = TrackWidget{
			eventsMt:  MiniTrack[eventWithGetters, *eventWithGetters]{MiniTrackBehavior: mtrackEvents},
			tinyMt:    MiniTrack[spanWithGetters, *spanWithGetters]{MiniTrackBehavior: mtrackTiny},
			samplesMt: MiniTrack[eventWithGetters, *eventWithGetters]{MiniTrackBehavior: mtrackSamples},
		}
	}
	if tl.buildTrackWidgets != nil {
		tl.buildTrackWidgets(tl.tracks)
	}
}

func (track *Track) SpanColor(span *ptrace.Span, tr *Trace) colorIndex {
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
	tr.spans = theme.NewFuture(win, func(cancelled <-chan struct{}) Items[ptrace.Span] {
		return tr.compute(tr, cancelled)
	})
	return tr.spans
}

func newZoomMenuItem(cv *Canvas, spans Items[ptrace.Span]) *theme.MenuItem {
	return &theme.MenuItem{
		Label:    PlainLabel("Zoom"),
		Shortcut: key.ModShortcut.String() + "+LMB",
		Action: func() theme.Action {
			return theme.ExecuteAction(func(gtx layout.Context) {
				start := spans.AtPtr(0).Start
				end := LastItemPtr(spans).End
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
	clickedSpans      Items[ptrace.Span]
	navigatedTimeSpan container.Option[TimeSpan]
	lowQualityRender  bool

	outlinesOps mem.ReusableOps
	labelsOps   mem.ReusableOps

	hover gesture.Hover
	click gesture.Click

	tinyMt    MiniTrack[spanWithGetters, *spanWithGetters]
	eventsMt  MiniTrack[eventWithGetters, *eventWithGetters]
	samplesMt MiniTrack[eventWithGetters, *eventWithGetters]

	scratchHighlighted   []clip.FRect
	scratchTextureStacks []TextureStack
	scratchTextures      []Texture

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

func (track *TrackWidget) NavigatedTimeSpan() container.Option[TimeSpan] {
	return track.navigatedTimeSpan
}

func (tw *TimelineWidget) ClickedSpans() Items[ptrace.Span] {
	return tw.clickedSpans
}

func (tw *TimelineWidget) NavigatedTimeSpan() container.Option[TimeSpan] {
	return tw.navigatedTimeSpan
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
	var height int
	enabledTracks := 0
	for _, track := range tl.tracks {
		if track.kind != TrackKindStack || cv.timeline.displayStackTracks {
			h := track.Height(gtx)
			height += h
			enabledTracks++
		}
	}
	height += (enabledTracks-1)*gtx.Dp(timelineTrackGapDp) + gtx.Dp(timelineGapDp)
	if !cv.timeline.compact {
		height += gtx.Dp(timelineLabelHeightDp)
	}
	return height
}

// notifyHidden informs the widget that it is no longer visible.
func (tl *Timeline) notifyHidden(cv *Canvas) {
	rtrace.Logf(context.Background(), "", "unloading track widget %q", tl.label)
	for _, track := range tl.tracks {
		cv.trackWidgetsCache.Put(track.widget)
		track.widget = nil
		// TODO(dh): this code is ugly and punches through abstractions.
		if track.compute != nil {
			if track.spans != nil {
				if spans, ok := track.spans.ResultNoWait(); ok {
					// XXX instead of special-casing SimpleItems and stackSpanMeta here, specify some interface
					if spans, ok := spans.(SimpleItems[ptrace.Span, stackSpanMeta]); ok {
						stackSpanMetaSlicePool.Put(spans.metas[:0])
						spanSlicePool.Put(spans.items[:0])
					}
				}
			}
			track.spans = nil
		}
	}
	cv.timelineWidgetsCache.Put(tl.widget)
	tl.widget = nil
}

func (tl *Timeline) Plan(win *theme.Window, texs []TextureStack) []TextureStack {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidget.Plan").End()

	tl.ensureTrackWidgets()
	for _, track := range tl.tracks {
		if track.kind == TrackKindStack && !tl.cv.timeline.displayStackTracks {
			continue
		}
		texs = track.Plan(win, texs)
	}
	return texs
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

	tl.widget.clickedSpans = NoItems[ptrace.Span]{}
	tl.widget.navigatedTimeSpan = container.None[TimeSpan]()

	tl.widget.labelClicks = 0
	for _, click := range tl.widget.labelClick.Update(gtx) {
		switch click.Button {
		case pointer.ButtonPrimary:
			switch click.Modifiers {
			case 0:
				tl.widget.labelClicks++
			case key.ModShortcut:
				// XXX this assumes that the first track is the widest one. This is currently true, but a brittle
				// assumption to make.
				spans := tl.tracks[0].Spans(win).Wait()
				tl.widget.navigatedTimeSpan = container.Some(SpansTimeSpan(spans))
			}

		case pointer.ButtonSecondary:
			tl.widget.labelRightClicks++
		}
	}

	tl.widget.hover.Update(gtx.Queue)

	// TODO(dh): we could replace all uses of timelineHeight by using normal Gio widget patterns: lay out all the
	// tracks, sum their heights and the gaps we apply. We'd use a macro to get the total size and then set up the clip
	// and pointer input. When we reuse the previous frame and need to return dimensions, we should use a stored value
	// of the height. Making the change isn't currently worth it, though, because we still need TimelineWidget.Height to
	// exist so we can compute the scrollbar, and so we can jump to goroutines, which needs to compute an offset. In
	// both cases we don't want to lay out every widget to figure out its size.
	timelineHeight := tl.Height(gtx, tl.cv)
	timelineLabelHeight := gtx.Dp(timelineLabelHeightDp)
	timelineTrackGap := gtx.Dp(timelineTrackGapDp)

	tl.displayed = true

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, timelineHeight)}.Push(gtx.Ops).Pop()
	tl.widget.hover.Add(gtx.Ops)

	if !compact {
		if tl.widget.Hovered(gtx) || forceLabel || topBorder {
			// Draw border at top of the timeline
			theme.FillShape(win, gtx.Ops, colors[colorTimelineBorder], clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, gtx.Dp(1))}.Op())
		}

		if tl.widget.Hovered(gtx) || forceLabel {
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
	tl.ensureTrackWidgets()

	suboptimal := false
	for _, track := range tl.tracks {
		if track.kind == TrackKindStack && !tl.cv.timeline.displayStackTracks {
			continue
		}
		dims := track.Layout(win, gtx, tl, cv.timeline.filter, trackSpanLabels)
		if ts, ok := track.widget.NavigatedTimeSpan().Get(); ok {
			tl.widget.navigatedTimeSpan = container.Some(ts)
		}
		if spans := track.widget.ClickedSpans(); spans.Len() != 0 {
			tl.widget.clickedSpans = spans
		}
		if track.widget.lowQualityRender {
			suboptimal = true
		}

		op.Offset(image.Pt(0, dims.Size.Y+timelineTrackGap)).Add(gtx.Ops)
	}
	stack.Pop()

	if !suboptimal {
		tl.widget.usedSuboptimalTexture = time.Time{}
	} else if tl.widget.usedSuboptimalTexture.IsZero() {
		tl.widget.usedSuboptimalTexture = gtx.Now
	}

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, timelineHeight)}
}

func defaultSpanColor(span *ptrace.Span, tr *Trace) colorIndex {
	return stateColors[span.State]
}

type renderedSpansIterator[T any, PT interface {
	TimeSpanner
	*T
}] struct {
	offset      int
	cv          *Canvas
	spans       Items[T]
	initialized bool
}

func (it *renderedSpansIterator[T, PT]) findFirstVisible() {
	it.offset = sort.Search(it.spans.Len(), func(i int) bool {
		return PT(it.spans.AtPtr(i)).End() > it.cv.start
	})
}

func (it *renderedSpansIterator[T, PT]) next(gtx layout.Context) (spansOut Items[T], startPx, endPx float32, ok bool) {
	if !it.initialized {
		it.findFirstVisible()
	}

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

	if PT(s).Start() >= it.cv.End() {
		return nil, 0, 0, false
	}

	start := PT(s).Start()
	end := PT(s).End()

	if time.Duration(end-start) < minSpanWidthD {
		// Only the first span we iterate over can have off-screen spans to the left.
		if !it.initialized {
			for noffset := offset - 1; noffset >= 0; {
				// Merge tiny spans to the left

				// Find the first span that starts early enough that it might either be big enough, or have big enough a
				// gap towards us. We do this by looking for the inverse condition and taking the span before what we
				// find.
				limit := min(noffset+1, spans.Len())
				noffset = sort.Search(limit, func(i int) bool {
					return PT(spans.AtPtr(i)).Start() > start-exptrace.Time(minSpanWidthD)
				})
				assert(noffset != limit, "should've found ourselves")
				if noffset == 0 {
					// Even the first span isn't far enough away. Merge all spans.
					start = PT(spans.AtPtr(0)).Start()
					startOffset = 0
					break
				}

				noffset--

				candidateSpan := spans.AtPtr(noffset)
				nextSpan := spans.AtPtr(noffset + 1)

				cStart := PT(candidateSpan).Start()
				cEnd := PT(candidateSpan).End()
				nextStart := PT(nextSpan).Start()

				if time.Duration(cEnd-cStart) >= minSpanWidthD || time.Duration(nextStart-cEnd) >= minSpanWidthD {
					// The found span is either large, or far away. Stop merging at the span that follows it.
					start = PT(nextSpan).Start()
					startOffset = noffset + 1
					break
				} else {
					// This span isn't good enough. Keep looking.
					start = cStart
				}
			}
		}

		// Merge all tiny spans until we find a span or gap that's big enough to stand on its own. We do not stop
		// merging after we've reached the minimum size because that can lead to multiple merges being next to each
		// other. Not only does this look bad, it is also prone to tiny spans toggling between two merged spans, and
		// previously merged spans becoming visible again when zooming out.
		for offset < spans.Len() {
			// For a span to be large enough to stand on its own, it has to end at least minSpanWidthD later than the
			// current span. Use binary search to find that span. This also finds gaps, because for a gap to be big
			// enough, it cannot occur between spans that would be too small according to this search.
			offset = sort.Search(spans.Len(), func(i int) bool {
				return PT(spans.AtPtr(i)).End() >= end+exptrace.Time(minSpanWidthD)
			})

			if offset == spans.Len() {
				// We couldn't find a span -> merge all remaining spans, except for the optional "goroutine returned"
				// span
				offset = spans.Len()
				end = PT(spans.AtPtr(offset - 1)).End()
				break
			}

			candidateSpan := spans.AtPtr(offset)
			prevSpan := spans.AtPtr(offset - 1)

			cStart := PT(candidateSpan).Start()
			cEnd := PT(candidateSpan).End()
			prevEnd := PT(prevSpan).End()
			if time.Duration(cEnd-cStart) >= minSpanWidthD || time.Duration(cStart-prevEnd) >= minSpanWidthD {
				end = prevEnd
				break
			} else {
				end = cEnd
				offset++
			}
		}
	}

	it.initialized = true
	it.offset = offset
	startPx = float32(start-cvStart) / nsPerPx
	endPx = float32(end-cvStart) / nsPerPx
	return it.spans.Slice(startOffset, offset), startPx, endPx, true
}

func (track *Track) Height(gtx layout.Context) int {
	height := gtx.Dp(timelineTrackHeightDp)
	// Tiny spans mini track
	height += gtx.Dp(timelineMinitrackHeightDp) + gtx.Dp(timelineMinitrackGapDp)
	// Events mini track
	if !track.hideEventMarkers && len(track.events) != 0 {
		height += gtx.Dp(timelineMinitrackHeightDp) + gtx.Dp(timelineMinitrackGapDp)
	}
	if len(track.samples) != 0 {
		height += gtx.Dp(timelineMinitrackHeightDp) + gtx.Dp(timelineMinitrackGapDp)
	}
	return height
}

func (track *Track) Plan(win *theme.Window, texs []TextureStack) []TextureStack {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Plan").End()

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
			container:  ItemContainer{Timeline: track.parent, Track: track},
			contiguous: false,
			subslice:   true,
		}
	}

	cv := track.parent.cv
	textures := track.widget.scratchTextures[:0]
	texs, textures = track.rnd.Render(win, track, spans, cv.nsPerPx, cv.start, cv.End(), texs, textures)
	track.widget.scratchTextures = textures[:0]
	return texs
}

func (track *Track) Layout(
	win *theme.Window,
	gtx layout.Context,
	tl *Timeline,
	filter Filter,
	labelsOut *[]string,
) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TimelineWidgetTrack.Layout").End()

	// OPT(dh): both Track.Plan and Track.Layout call Renderer.Render to figure out which textures to display. However,
	// Render is currently so fast that deduplicating that work isn't worth it.
	totalTrackHeight := track.Height(gtx)
	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, totalTrackHeight)}.Push(gtx.Ops).Pop()

	// OPT(dh): reuse slice
	main := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		return track.layoutMain(win, gtx, tl, filter, labelsOut)
	})
	tiny := track.layoutTiny(win, gtx, tl.cv)

	if !track.hideEventMarkers && len(track.events) != 0 {
		defer op.Offset(image.Pt(0, tiny.Size.Y+gtx.Dp(timelineMinitrackGapDp))).Push(gtx.Ops).Pop()
		track.layoutEvents(win, gtx, tl.cv)
	}

	if len(track.samples) != 0 {
		defer op.Offset(image.Pt(0, tiny.Size.Y+gtx.Dp(timelineMinitrackGapDp))).Push(gtx.Ops).Pop()
		track.layoutSamples(win, gtx, tl.cv)
	}

	defer op.Offset(image.Pt(0, tiny.Size.Y+gtx.Dp(timelineMinitrackGapDp))).Push(gtx.Ops).Pop()
	main.Layout(win, gtx)

	return layout.Dimensions{
		Size: image.Pt(gtx.Constraints.Max.X, totalTrackHeight),
	}
}

func (track *Track) layoutMain(
	win *theme.Window,
	gtx layout.Context,
	tl *Timeline,
	filter Filter,
	labelsOut *[]string,
) (dims layout.Dimensions) {
	cv := tl.cv
	tr := cv.trace
	mainTrackHeight := gtx.Dp(timelineTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, mainTrackHeight)}.Push(gtx.Ops).Pop()
	track.widget.click.Add(gtx.Ops)
	track.widget.hover.Add(gtx.Ops)

	track.widget.clickedSpans = NoItems[ptrace.Span]{}
	track.widget.navigatedTimeSpan = container.None[TimeSpan]()
	track.widget.lowQualityRender = false

	tsi := trackSpanInteractivity{
		click: &track.widget.click,
		hover: &track.widget.hover,
		track: track,
	}
	tsi.Update(gtx)

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
	if !track.widget.hover.Update(gtx.Queue) &&
		!track.widget.prevFrame.hovered &&
		cv.unchanged(gtx) &&
		track.widget.prevFrame.placeholder == !haveSpans &&
		!track.widget.prevFrame.lowQualityRender &&
		gtx.Constraints == track.widget.prevFrame.constraints {

		track.widget.prevFrame.call.Add(gtx.Ops)
		debugCaching(win, gtx)
		return track.widget.prevFrame.dims
	}

	track.widget.prevFrame.hovered = track.widget.hover.Update(gtx.Queue)
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

	var hoveredSpan clip.FRect
	highlightedSpans := track.widget.scratchHighlighted[:0]

	var outlinesPath clip.Path
	outlinesPath.Begin(track.widget.outlinesOps.Get())
	labelsOps := track.widget.labelsOps.Get()
	labelsMacro := op.Record(labelsOps)

	first := true
	var prevEndPx float32
	doSpans := func(dspSpans Items[ptrace.Span], startPx, endPx float32) {
		if endPx < 0 {
			return
		}
		hovered := tsi.Handle(win, gtx, dspSpans, cv, startPx, endPx)

		var minP f32.Point
		var maxP f32.Point
		minP = f32.Pt(max(startPx, 0), 0)
		maxP = f32.Pt(min(endPx, float32(gtx.Constraints.Max.X)), float32(mainTrackHeight))

		if filter.Match(dspSpans, ItemContainer{Timeline: tl, Track: track}) {
			highlightedSpans = append(highlightedSpans, clip.FRect{Min: minP, Max: maxP})
		}

		if dspSpans.Len() != 0 && dspSpans.AtPtr(0).State != statePlaceholder && maxP.X-minP.X >= float32(spanBorderWidth) {
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
			if maxP.X-minP.X-leftOff-rightOff > 0 {
				outlinesPath.MoveTo(minP.Add(f32.Pt(leftOff, off)))
				outlinesPath.LineTo(f32.Point{X: minP.X + leftOff, Y: maxP.Y - off})
				outlinesPath.LineTo(maxP.Add(f32.Pt(rightOff, -off)))
				outlinesPath.LineTo(f32.Point{X: maxP.X + rightOff, Y: minP.Y + off})
				outlinesPath.Close()
			}

			if hovered {
				hoveredSpan = clip.FRect{Min: minP, Max: maxP}
			}
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

		prevEndPx = endPx
		first = false
	}

	textureStacks := track.widget.scratchTextureStacks[:0]
	textures := track.widget.scratchTextures[:0]
	textureStacks, textures = track.rnd.Render(win, track, spans, cv.nsPerPx, cv.start, cv.End(), textureStacks, textures)
	track.widget.scratchTextureStacks = textureStacks[:0]
	track.widget.scratchTextures = textures[:0]
	for i, tex := range textureStacks {
		var fudge float32
		if i == 1 {
			// This is an ugly hack to make sure neighboring textures don't have a tiny gap due to rounding
			// errors, either in our code or in the renderer. It assumes that there are at most two textures,
			// which is currently the case, because our texWidth is 8192 and the maximum width we can render
			// with Gio is twice that.
			//
			// We have to fudge by quite a significant amount, because minor overlap (as big as 0.15 pixels)
			// doesn't eliminate the artifacts. Shifting to the left by 0.5 ensures we round to the previous
			// full pixel.
			fudge = -0.5
		}
		if !tex.Add(win, gtx, &cv.textures, tr, gtx.Ops, fudge) {
			track.widget.lowQualityRender = true
		}
	}

	if cv.unchanged(gtx) && track.widget.prevFrame.dspSpans != nil && track.widget.prevFrame.placeholder == !haveSpans {
		for _, prevSpans := range track.widget.prevFrame.dspSpans {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		it := renderedSpansIterator[spanWithGetters, *spanWithGetters]{
			cv:    cv,
			spans: myunsafe.Cast[Items[spanWithGetters]](spans),
		}

		allDspSpans := track.widget.prevFrame.dspSpans[:0]
		for {
			dspSpans_, startPx, endPx, ok := it.next(gtx)
			if !ok {
				break
			}
			dspSpans := myunsafe.Cast[Items[ptrace.Span]](dspSpans_)

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
			visWidthPx    = float32(gtx.Constraints.Max.X)
			deadFromPx    = visWidthPx
			unbornUntilPx float32
		)
		if len(track.widget.prevFrame.dspSpans) > 0 {
			// If the first displayed span is also the first overall span, display an indicator that the
			// goroutine/processor hasn't been created yet.
			dspFirst := track.widget.prevFrame.dspSpans[0]
			// OPT(dh): can we use pointer identity here?
			if *dspFirst.dspSpans.AtPtr(0) == *spans.AtPtr(0) {
				end := dspFirst.startPx
				unbornUntilPx = end
			}

			// If the last displayed span is also the last overall span, display an indicator that the
			// goroutine/processor is dead.
			dspLast := track.widget.prevFrame.dspSpans[len(track.widget.prevFrame.dspSpans)-1]
			// OPT(dh): can we use pointer identity here?
			if *LastItemPtr(dspLast.dspSpans) == *LastItemPtr(spans) {
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
		mid := float32(mainTrackHeight) / 2
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

	// Draw the end of goroutine marker
	if g, ok := track.parent.item.(*ptrace.Goroutine); ok && g.End.Set() {
		px := cv.tsToPx(g.End.MustGet())
		if px > 0 && px < float32(gtx.Constraints.Max.X) {
			theme.FillShape(win, gtx.Ops, colors[colorStateDone], clip.FRect{Min: f32.Pt(px, 0), Max: f32.Pt(px+float32(minSpanWidth), float32(mainTrackHeight))}.Op(gtx.Ops))
		}
	}

	// Highlight the hovered span
	if hoveredSpan != (clip.FRect{}) {
		stack := hoveredSpan.Op(gtx.Ops).Push(gtx.Ops)
		paint.LinearGradientOp{
			Stop1:  hoveredSpan.Max,
			Stop2:  f32.Pt(hoveredSpan.Max.X, hoveredSpan.Min.Y+float32(mainTrackHeight)/4),
			Color1: colors[colorSpanHighlightedPrimaryOutline].NRGBA(),
			Color2: stdcolor.NRGBA{0, 0, 0, 0},
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
			Stop2:  f32.Pt(r.Max.X, r.Min.Y+float32(mainTrackHeight)/4),
			Color1: colors[colorSpanHighlightedSecondaryOutline].NRGBA(),
			Color2: stdcolor.NRGBA{0, 0, 0, 0},
		}.Add(gtx.Ops)
		paint.PaintOp{}.Add(gtx.Ops)
		stack.Pop()
	}

	// Print labels
	labelsMacro.Stop().Add(gtx.Ops)

	// Draw the span outlines
	theme.FillShape(win, gtx.Ops, win.Theme.Palette.Foreground, clip.Outline{Path: outlinesPath.End()}.Op())

	track.widget.scratchHighlighted = highlightedSpans[:0]

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, mainTrackHeight)}
}

type trackSpanInteractivity struct {
	track                 *Track
	hover                 *gesture.Hover
	click                 *gesture.Click
	trackNavigatedSpans   bool
	trackClickedSpans     bool
	trackContextMenuSpans bool
}

func (tsi *trackSpanInteractivity) Update(gtx layout.Context) {
	for _, ev := range tsi.click.Update(gtx.Queue) {
		if ev.Kind == gesture.KindClick && ev.Button == pointer.ButtonPrimary {
			switch ev.Modifiers {
			case key.ModShortcut:
				tsi.trackNavigatedSpans = true
			case 0:
				tsi.trackClickedSpans = true
			}
		} else if ev.Kind == gesture.KindPress && ev.Button == pointer.ButtonSecondary {
			tsi.trackContextMenuSpans = true
		}
	}

	tsi.click.Add(gtx.Ops)
	tsi.hover.Add(gtx.Ops)
}

func (tsi *trackSpanInteractivity) Hovered(q event.Queue, startPx, endPx float32) bool {
	x := tsi.hover.Pointer().X
	return tsi.hover.Update(q) && x >= startPx && x < endPx
}

func (tsi *trackSpanInteractivity) Handle(win *theme.Window, gtx layout.Context, spans Items[ptrace.Span], cv *Canvas, startPx, endPx float32) bool {
	if !tsi.Hovered(gtx.Queue, startPx, endPx) {
		return false
	}
	if tsi.trackNavigatedSpans {
		tsi.track.widget.navigatedTimeSpan = container.Some(SpansTimeSpan(spans))
	}
	if tsi.trackClickedSpans {
		tsi.track.widget.clickedSpans = spans
	}
	if tsi.trackContextMenuSpans {
		if tsi.track.spanContextMenu != nil {
			win.SetContextMenu(tsi.track.spanContextMenu(spans, cv))
		} else {
			win.SetContextMenu([]*theme.MenuItem{
				newZoomMenuItem(cv, spans),
				newOpenSpansMenuItem(spans),
			})
		}
	}

	spanTooltip := tsi.track.spanTooltip
	if spanTooltip == nil {
		spanTooltip = defaultSpanTooltip
	}
	win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		// OPT(dh): this allocates for the closure
		// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
		return spanTooltip(win, gtx, cv.trace, spans)
	})
	return true
}

type MiniTrack[T any, PT interface {
	TimeSpanner
	*T
}] struct {
	hover        gesture.Hover
	click        gesture.Click
	ops          [colorLast]mem.ReusableOps
	clickedItems Items[T]
	*MiniTrackBehavior[T]
}

type MiniTrackBehavior[T any] struct {
	skipIter          bool
	onlyTinyAndMerged bool
	tooltip           func(win *theme.Window, items Items[T], track *Track) theme.Widget
	timeSpan          func(items Items[T]) TimeSpan
	color             func(items Items[T], track *Track) colorIndex
	contextMenu       func(items Items[T], track *Track) []*theme.MenuItem
}

var (
	mtrackTiny = &MiniTrackBehavior[spanWithGetters]{
		skipIter:          true,
		onlyTinyAndMerged: true,
		tooltip: func(win *theme.Window, items Items[spanWithGetters], track *Track) theme.Widget {
			return func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				return track.spanTooltip(win, gtx, track.parent.cv.trace, myunsafe.Cast[Items[ptrace.Span]](items))
			}
		},
		timeSpan: func(items Items[spanWithGetters]) TimeSpan {
			return SpansTimeSpan(myunsafe.Cast[Items[ptrace.Span]](items))
		},
		color: func(items Items[spanWithGetters], track *Track) colorIndex {
			if items.Len() > 1 {
				return colorStateMerged
			} else {
				return track.SpanColor(&items.AtPtr(0).Span, track.parent.cv.trace)
			}
		},
		contextMenu: func(items Items[spanWithGetters], track *Track) []*theme.MenuItem {
			spans := myunsafe.Cast[Items[ptrace.Span]](items)
			if track.spanContextMenu != nil {
				return track.spanContextMenu(spans, track.parent.cv)
			} else {
				return []*theme.MenuItem{
					newZoomMenuItem(track.parent.cv, spans),
					newOpenSpansMenuItem(spans),
				}
			}
		},
	}

	mtrackEvents = &MiniTrackBehavior[eventWithGetters]{
		tooltip: func(win *theme.Window, events Items[eventWithGetters], track *Track) theme.Widget {
			tr := track.parent.cv.trace

			const (
				tSyscall = iota
				tGoCreate
				tGoUnblock
				tUserLog
			)
			eventType := func(ev exptrace.Event) uint8 {
				switch ev.Kind() {
				case exptrace.EventStateTransition:
					trans := ev.StateTransition()
					from, to := trans.Goroutine()
					if from == exptrace.GoNotExist && to == exptrace.GoRunnable {
						return tGoCreate
					} else if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
						return tGoUnblock
					} else if to == exptrace.GoSyscall {
						return tSyscall
					} else {
						panic(fmt.Sprintf("unexpected state transition %s -> %s", from, to))
					}
				case exptrace.EventLog:
					return tUserLog
				default:
					return 255
				}
			}

			var label string
			if events.Len() == 1 {
				ev := events.At(0)
				switch ev.Kind() {
				case exptrace.EventStateTransition:
					trans := ev.StateTransition()
					from, to := trans.Goroutine()
					if from == exptrace.GoNotExist && to == exptrace.GoRunnable {
						label = fmt.Sprintf("Created goroutine %s", GoroutineLabel(tr.G(trans.Resource.Goroutine())))
					} else if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
						label = fmt.Sprintf("Unblocked goroutine %s", GoroutineLabel(tr.G(trans.Resource.Goroutine())))
					} else if to == exptrace.GoSyscall {
						stk := ev.Stack()
						if stk != exptrace.NoStack {
							frame := tr.PCs[tr.Stacks[stk][0]]
							label = "Syscall: " + frame.Func
						} else {
							label = "Syscall"
						}
					} else {
						panic(fmt.Sprintf("unexpected state trans ition %s -> %s", from, to))
					}
				case exptrace.EventLog:
					l := ev.Log()
					cat := l.Category
					msg := l.Message
					if cat != "" {
						label = fmt.Sprintf("Log: <%s> %s", cat, msg)
					} else {
						label = "Log: " + msg
					}
				default:
					label = "1 event"
				}
			} else {
				kind := eventType(events.At(0).Event)
				for i := 1; i < events.Len(); i++ {
					ev := events.At(i)
					if eventType(ev.Event) != kind {
						kind = 255
						break
					}
				}
				var noun string
				switch kind {
				case tSyscall:
					noun = "syscalls"
				case tGoCreate:
					noun = "goroutine creations"
				case tGoUnblock:
					noun = "goroutine unblocks"
				case tUserLog:
					noun = "log messages"
				default:
					noun = "events"
				}
				label = local.Sprintf("%d %s\n", events.Len(), noun)
				label += fmt.Sprintf("Time span: %s\n", roundDuration(time.Duration(LastItemPtr(events).Time()-events.AtPtr(0).Time())))
			}

			return theme.Tooltip(win.Theme, label).Layout
		},
		timeSpan: func(items Items[eventWithGetters]) TimeSpan {
			return EventsRange(myunsafe.Cast[Items[exptrace.Event]](items))
		},
		color: func(items Items[eventWithGetters], _ *Track) colorIndex {
			return colorEvent
		},
	}

	mtrackSamples = &MiniTrackBehavior[eventWithGetters]{
		tooltip: func(win *theme.Window, items Items[eventWithGetters], track *Track) theme.Widget {
			if items.Len() > 1 {
				return theme.Tooltip(win.Theme, fmt.Sprintf("%d stack samples", items.Len())).Layout
			} else {
				return theme.Tooltip(win.Theme, formatStack(track.parent.cv.trace, items.AtPtr(0).Stack(), 6)).Layout
			}
		},
		timeSpan: func(items Items[eventWithGetters]) TimeSpan {
			return EventsRange(myunsafe.Cast[Items[exptrace.Event]](items))
		},
		color: func(items Items[eventWithGetters], track *Track) colorIndex {
			return colorStateCPUSample
		},
	}
)

func (mtrack *MiniTrack[T, PT]) Layout(
	win *theme.Window,
	gtx layout.Context,
	track *Track,
	cv *Canvas,
	items func(yield func(el Items[T]) bool),
) layout.Dimensions {
	timelineMinitrackHeight := gtx.Dp(timelineMinitrackHeightDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	defer clip.Rect{Max: image.Pt(gtx.Constraints.Max.X, timelineMinitrackHeight)}.Push(gtx.Ops).Pop()
	mtrack.click.Add(gtx.Ops)
	mtrack.hover.Add(gtx.Ops)

	mtrack.clickedItems = NoItems[T]{}

	var (
		trackNavigated   bool
		trackContextMenu bool
		trackClicked     bool
	)
	for _, ev := range mtrack.click.Update(gtx.Queue) {
		if ev.Kind == gesture.KindClick && ev.Button == pointer.ButtonPrimary {
			switch ev.Modifiers {
			case key.ModShortcut:
				trackNavigated = true
			case 0:
				trackClicked = true
			}
		} else if ev.Kind == gesture.KindPress && ev.Button == pointer.ButtonSecondary {
			trackContextMenu = true
		}
	}

	mtrack.hover.Update(gtx.Queue)

	var paths [colorLast]clip.Path
	var initPaths [colorLast]bool
	items(func(el Items[T]) bool {
		do := func(items Items[T], startPx, endPx float32) {
			if mtrack.onlyTinyAndMerged && items.Len() < 2 && endPx-startPx >= float32(minSpanWidth) {
				return
			}

			if endPx-startPx < float32(minSpanWidth) {
				mid := startPx + (endPx-startPx)/2
				dd := float32(minSpanWidth) / 2
				startPx = mid - dd
				endPx = mid + dd
			}

			min := f32.Pt(startPx, 0)
			max := f32.Pt(endPx, float32(timelineMinitrackHeight))

			var p *clip.Path
			if x := mtrack.hover.Pointer().X; mtrack.hover.Update(gtx.Queue) && x >= startPx && x < endPx {
				if trackNavigated {
					track.widget.navigatedTimeSpan = container.Some(mtrack.timeSpan(items))
				}
				if trackClicked {
					mtrack.clickedItems = items
				}
				if trackContextMenu && mtrack.contextMenu != nil {
					win.SetContextMenu(mtrack.contextMenu(items, track))
				}

				win.SetTooltip(mtrack.tooltip(win, items, track))

				theme.FillShape(win, gtx.Ops, colors[colorSpanHighlightedPrimaryOutline], clip.Outline{Path: clip.FRect{Min: min, Max: max}.Path(gtx.Ops)}.Op())
			} else {
				cIdx := mtrack.color(items, track)
				if !initPaths[cIdx] {
					initPaths[cIdx] = true
					paths[cIdx].Begin(mtrack.ops[cIdx].Get())
				}
				p = &paths[cIdx]
			}

			if p != nil {
				p.MoveTo(min)
				p.LineTo(f32.Pt(max.X, min.Y))
				p.LineTo(max)
				p.LineTo(f32.Pt(min.X, max.Y))
				p.Close()
			}
		}

		if mtrack.skipIter {
			items := el
			startPx := cv.tsToPx(PT(el.AtPtr(0)).Start())
			endPx := cv.tsToPx(PT(LastItemPtr(el)).End())
			do(items, startPx, endPx)
		} else {
			iter := renderedSpansIterator[T, PT]{
				cv:    cv,
				spans: el,
			}

			for {
				items, startPx, endPx, ok := iter.next(gtx)
				if !ok {
					break
				}
				do(items, startPx, endPx)
			}
		}

		return true
	})

	for i := range paths {
		if !initPaths[i] {
			continue
		}
		theme.FillShape(win, gtx.Ops, colors[i], clip.Outline{Path: paths[i].End()}.Op())
	}
	return layout.Dimensions{
		Size: image.Pt(gtx.Constraints.Max.X, timelineMinitrackHeight),
	}
}

func (track *Track) layoutTiny(win *theme.Window, gtx layout.Context, cv *Canvas) layout.Dimensions {
	dims := track.widget.tinyMt.Layout(win, gtx, track, cv, func(yield func(el Items[spanWithGetters]) bool) {
		for _, dspSpans := range track.widget.prevFrame.dspSpans {
			items := myunsafe.Cast[Items[spanWithGetters]](dspSpans.dspSpans)
			if !yield(items) {
				break
			}
		}
	})

	if spans := track.widget.tinyMt.clickedItems; spans.Len() != 0 {
		track.widget.clickedSpans = myunsafe.Cast[Items[ptrace.Span]](spans)
	}

	return dims
}

type eventsFromIDs struct {
	tr    *ptrace.Trace
	items Items[ptrace.EventID]
}

func (evs eventsFromIDs) At(idx int) exptrace.Event {
	return *evs.tr.Event(evs.items.At(idx))
}

func (evs eventsFromIDs) AtPtr(idx int) *exptrace.Event {
	return evs.tr.Event(evs.items.At(idx))
}

func (evs eventsFromIDs) Slice(start, end int) Items[exptrace.Event] {
	return eventsFromIDs{
		tr:    evs.tr,
		items: evs.items.Slice(start, end),
	}
}

func (evs eventsFromIDs) Len() int                          { return evs.items.Len() }
func (evs eventsFromIDs) Contiguous() bool                  { return evs.items.Contiguous() }
func (evs eventsFromIDs) Subslice() bool                    { return evs.items.Subslice() }
func (evs eventsFromIDs) Container() (ItemContainer, bool)  { return evs.items.Container() }
func (evs eventsFromIDs) ContainerAt(idx int) ItemContainer { return evs.items.ContainerAt(idx) }
func (evs eventsFromIDs) MetadataAtPtr(index int) any       { return evs.items.MetadataAtPtr(index) }

type eventWithGetters struct {
	exptrace.Event
}

func (ev *eventWithGetters) Start() exptrace.Time { return ev.Time() }
func (ev *eventWithGetters) End() exptrace.Time   { return ev.Time() }

// Must only be called after layoutMain.
func (track *Track) layoutEvents(win *theme.Window, gtx layout.Context, cv *Canvas) {
	track.widget.eventsMt.Layout(win, gtx, track, cv, func(yield func(el Items[eventWithGetters]) bool) {
		for _, dspSpans := range track.widget.prevFrame.dspSpans {
			eventIDs := Events(dspSpans.dspSpans, cv.trace)
			items := myunsafe.Cast[Items[eventWithGetters]](Items[exptrace.Event](eventsFromIDs{
				tr:    cv.trace.Trace,
				items: eventIDs,
			}))
			if !yield(items) {
				break
			}
		}
	})

}

func (track *Track) layoutSamples(win *theme.Window, gtx layout.Context, cv *Canvas) {
	track.widget.samplesMt.Layout(win, gtx, track, cv, func(yield func(el Items[eventWithGetters]) bool) {
		eventIDs := SimpleItems[ptrace.EventID, struct{}]{
			items: track.samples,
		}
		// XXX limit ourselves to events in the visible range?
		items := myunsafe.Cast[Items[eventWithGetters]](Items[exptrace.Event](eventsFromIDs{
			tr:    cv.trace.Trace,
			items: eventIDs,
		}))
		yield(items)
	})
}

func singleSpanLabel(label string) func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	return func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
		return append(out, label)
	}
}

func singleSpanColor(c colorIndex) func(span *ptrace.Span, tr *Trace) colorIndex {
	return func(span *ptrace.Span, tr *Trace) colorIndex {
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
	}
	tl.tracks[0].spans = theme.Immediate[Items[ptrace.Span]](ss)
	tl.tracks[0].spanLabel = func(spans Items[ptrace.Span], tr *Trace, out []string) []string {
		return append(out, tr.Event(spans.AtPtr(0).StartEvent).Range().Name)
	}
	tl.tracks[0].spanColor = singleSpanColor(colorStateSTW)
	tl.tracks[0].spanTooltip = func(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
		if spans.Len() > 1 {
			return defaultSpanTooltip(win, gtx, tr, spans)
		}
		label := tr.Event(spans.AtPtr(0).StartEvent).Range().Name + "\n"
		if d, ok := SpansDuration(spans); ok {
			label += fmt.Sprintf("Duration: %s\n", roundDuration(d))
		}
		label += fmt.Sprintf("Time span: %s\n", roundDuration(SpansTimeSpan(spans).Duration()))
		return theme.Tooltip(win.Theme, label).Layout(win, gtx)
	}
	tl.item = &STW{ss}

	return tl
}

type GC struct {
	Spans Items[ptrace.Span]
}

type STW struct {
	Spans Items[ptrace.Span]
}

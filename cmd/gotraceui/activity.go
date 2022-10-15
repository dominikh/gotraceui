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
)

const (
	// XXX the label height depends on the font used
	activityLabelHeightDp unit.Dp = 20
	activityTrackHeightDp unit.Dp = 16
	activityGapDp         unit.Dp = 5
	activityTrackGapDp    unit.Dp = 2
)

type ActivityWidgetTrackKind uint8

const (
	ActivityWidgetTrackUnspecified ActivityWidgetTrackKind = iota
	ActivityWidgetTrackSampled
	ActivityWidgetTrackUserRegions
)

type ActivityWidget struct {
	// Inputs
	tracks            []Track
	trackWidgets      []ActivityWidgetTrack
	buildTrackWidgets func([]Track, []ActivityWidgetTrack)

	widgetTooltip   func(win *theme.Window, gtx layout.Context, aw *ActivityWidget) layout.Dimensions
	invalidateCache func(aw *ActivityWidget) bool
	tl              *Timeline
	item            any
	label           string

	// Set to true by the Layout method. This is used to track which activities have been shown during a frame.
	displayed bool

	labelClicks int

	pointerAt    f32.Point
	hovered      bool
	hoveredLabel bool

	// OPT(dh): Only one activity can have hovered or activated spans, so we could track this directly in Timeline, and
	// save 48 bytes per activity (which means per goroutine). However, the current API is cleaner, because
	// ActivityWidget doesn't have to mutate Timeline's state.
	navigatedSpans MergedSpans
	hoveredSpans   MergedSpans

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
	spans             MergedSpans
	events            []EventID
	eventsUnderCursor []EventID
}

type Track struct {
	kind   ActivityWidgetTrackKind
	spans  Spans
	events []EventID
}

type ActivityWidgetTrack struct {
	*Track

	highlightSpan func(spans MergedSpans) bool
	// OPT(dh): pass slice to spanLabel to reuse memory between calls
	spanLabel       func(spans MergedSpans, tr *Trace) []string
	spanColor       func(spans MergedSpans, tr *Trace) [2]colorIndex
	spanTooltip     func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions
	spanContextMenu func(spans MergedSpans, tr *Trace) []theme.Widget

	// OPT(dh): Only one track can have hovered or activated spans, so we could track this directly in ActivityWidget,
	// and save 48 bytes per track. However, the current API is cleaner, because ActivityWidgetTrack doesn't have to
	// mutate ActivityWidget's state.
	navigatedSpans MergedSpans
	hoveredSpans   MergedSpans

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

func (track *ActivityWidgetTrack) NavigatedSpans() MergedSpans {
	return track.navigatedSpans
}

func (track *ActivityWidgetTrack) HoveredSpans() MergedSpans {
	return track.hoveredSpans
}

func (aw *ActivityWidget) NavigatedSpans() MergedSpans {
	return aw.navigatedSpans
}

func (aw *ActivityWidget) HoveredSpans() MergedSpans {
	return aw.hoveredSpans
}

func (aw *ActivityWidget) LabelClicked() bool {
	if aw.labelClicks > 0 {
		aw.labelClicks--
		return true
	} else {
		return false
	}
}

func (aw *ActivityWidget) Height(gtx layout.Context) int {
	enabledTracks := 0
	for i := range aw.tracks {
		track := &aw.tracks[i]
		if track.kind != ActivityWidgetTrackSampled || aw.tl.activity.displaySampleTracks {
			enabledTracks++
		}
	}
	if aw.tl.activity.compact {
		return (gtx.Dp(activityTrackHeightDp) + gtx.Dp(activityTrackGapDp)) * enabledTracks
	} else {
		return (gtx.Dp(activityTrackHeightDp)+gtx.Dp(activityTrackGapDp))*enabledTracks + gtx.Dp(activityLabelHeightDp)
	}
}

// notifyHidden informs the widget that it is no longer visible.
func (aw *ActivityWidget) notifyHidden() {
	aw.trackWidgets = nil
}

func (aw *ActivityWidget) Layout(win *theme.Window, gtx layout.Context, forceLabel bool, compact bool, topBorder bool) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.ActivityWidget.Layout").End()

	// TODO(dh): we could replace all uses of activityHeight by using normal Gio widget patterns: lay out all the
	// tracks, sum their heights and the gaps we apply. We'd use a macro to get the total size and then set up the clip
	// and pointer input. When we reuse the previous frame and need to return dimensions, we should use a stored value
	// of the height. Making the change isn't currently worth it, though, because we still need ActivityWidget.Height to
	// exist so we can compute the scrollbar, and so we can jump to goroutines, which needs to compute an offset. In
	// both cases we don't want to lay out every widget to figure out its size.
	activityHeight := aw.Height(gtx)
	activityTrackGap := gtx.Dp(activityTrackGapDp)
	activityLabelHeight := gtx.Dp(activityLabelHeightDp)

	aw.displayed = true

	aw.navigatedSpans = nil
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
					aw.navigatedSpans = MergedSpans(aw.tracks[0].spans)
				}
			}
		}
	}

	if !widgetClicked &&
		aw.tl.unchanged() &&
		!aw.hovered && !aw.prevFrame.hovered &&
		forceLabel == aw.prevFrame.forceLabel &&
		compact == aw.prevFrame.compact &&
		(aw.invalidateCache == nil || !aw.invalidateCache(aw)) &&
		topBorder == aw.prevFrame.topBorder &&
		gtx.Constraints == aw.prevFrame.constraints {

		// OPT(dh): instead of avoiding cached ops completely when the activity is hovered, draw the tooltip
		// separately.
		aw.prevFrame.call.Add(gtx.Ops)
		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
	}

	aw.prevFrame.hovered = aw.hovered
	aw.prevFrame.forceLabel = forceLabel
	aw.prevFrame.compact = compact
	aw.prevFrame.topBorder = topBorder
	aw.prevFrame.constraints = gtx.Constraints

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
			labelDims := mywidget.TextLine{Color: colors[colorActivityLabel]}.Layout(labelGtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, aw.label)

			stack := clip.Rect{Max: labelDims.Size}.Push(gtx.Ops)
			pointer.InputOp{Tag: &aw.label, Types: pointer.Press | pointer.Enter | pointer.Leave | pointer.Cancel | pointer.Move}.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
			stack.Pop()
		}

		if aw.widgetTooltip != nil && aw.tl.activity.showTooltips == showTooltipsBoth && aw.hoveredLabel {
			win.SetTooltip(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				// OPT(dh): this allocates for the closure
				// OPT(dh): avoid allocating a new tooltip if it's the same as last frame
				return aw.widgetTooltip(win, gtx, aw)
			})
		}

		defer op.Offset(image.Pt(0, activityLabelHeight)).Push(gtx.Ops).Pop()
	}

	stack := op.TransformOp{}.Push(gtx.Ops)
	if aw.trackWidgets == nil {
		// OPT(dh): avoid this allocation by using a pool of slices; we need at most as many as there are visible
		// activities.
		out := make([]ActivityWidgetTrack, len(aw.tracks))
		if aw.buildTrackWidgets != nil {
			aw.buildTrackWidgets(aw.tracks, out)
		} else {
			for i := range aw.tracks {
				out[i].Track = &aw.tracks[i]
			}
		}
		aw.trackWidgets = out
	}
	for i := range aw.trackWidgets {
		track := &aw.trackWidgets[i]
		if track.kind == ActivityWidgetTrackSampled && !aw.tl.activity.displaySampleTracks {
			continue
		}
		dims := track.Layout(win, gtx, aw.tl)
		op.Offset(image.Pt(0, dims.Size.Y+activityTrackGap)).Add(gtx.Ops)
		if spans := track.HoveredSpans(); len(spans) != 0 {
			aw.hoveredSpans = spans
		}
		if spans := track.NavigatedSpans(); len(spans) != 0 {
			aw.navigatedSpans = spans
		}
	}
	stack.Pop()

	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, activityHeight)}
}

func defaultSpanColor(spans MergedSpans) [2]colorIndex {
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

type renderedSpansIterator struct {
	offset  int
	tl      *Timeline
	spans   Spans
	prevEnd trace.Timestamp
}

func (it *renderedSpansIterator) next(gtx layout.Context) (spansOut MergedSpans, startPx, endPx float32, ok bool) {
	offset := it.offset
	spans := it.spans

	if offset >= len(spans) {
		return nil, 0, 0, false
	}

	nsPerPx := float32(it.tl.nsPerPx)
	minSpanWidthD := time.Duration(math.Ceil(float64(gtx.Dp(minSpanWidthDp)) * float64(nsPerPx)))
	startOffset := offset
	tlStart := it.tl.start

	s := &spans[offset]
	offset++

	start := s.start
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
		for offset < len(spans) {
			adjustedEnd := end
			if time.Duration(end-start) < minSpanWidthD {
				adjustedEnd = start + trace.Timestamp(minSpanWidthD)
			}

			// For a span to be large enough to stand on its own, it has to end at least minSpanWidthD later than the
			// current span. Use binary search to find that span. This also finds gaps, because for a gap to be big
			// enough, it cannot occur between spans that would be too small according to this search.
			offset = sort.Search(len(spans), func(i int) bool {
				return spans[i].end >= adjustedEnd+trace.Timestamp(minSpanWidthD)
			})

			if offset == len(spans) {
				// We couldn't find a span -> merge all remaining spans
				offset = len(spans)
				end = spans[offset-1].end
				break
			} else {
				prevSpan := &spans[offset-1]
				candidateSpan := &spans[offset]

				cStart := candidateSpan.start
				cEnd := candidateSpan.end
				prevEnd := prevSpan.end
				if adjustedEnd > cStart {
					cStart = adjustedEnd
				}
				if time.Duration(cEnd-cStart) >= minSpanWidthD || time.Duration(cStart-prevEnd) >= minSpanWidthD {
					end = spans[offset-1].end
					break
				} else {
					end = spans[offset].end
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
	startPx = float32(start-tlStart) / nsPerPx
	endPx = float32(end-tlStart) / nsPerPx
	return MergedSpans(spans[startOffset:offset]), startPx, endPx, true
}

func (track *ActivityWidgetTrack) Layout(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.ActivityWidgetTrack.Layout").End()

	tr := tl.trace
	trackHeight := gtx.Dp(activityTrackHeightDp)
	spanBorderWidth := gtx.Dp(spanBorderWidthDp)
	minSpanWidth := gtx.Dp(minSpanWidthDp)

	track.navigatedSpans = nil
	track.hoveredSpans = nil

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
			if ev.Buttons.Contain(pointer.ButtonTertiary) && ev.Modifiers.Contain(key.ModCtrl) {
				trackNavigatedSpans = true
			} else if ev.Buttons.Contain(pointer.ButtonSecondary) {
				trackContextMenuSpans = true
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
			if trackNavigatedSpans {
				track.navigatedSpans = dspSpans
			}
			if trackContextMenuSpans {
				tl.contextMenu.spans = dspSpans
				var items []theme.Widget
				if track.spanContextMenu != nil {
					items = track.spanContextMenu(dspSpans, tr)
				} else {
					items = []theme.Widget{
						tl.contextMenu.zoom.Layout,
					}
				}
				win.SetContextMenu((&theme.MenuGroup{
					Items: items,
				}).Layout)
			}
			track.hoveredSpans = dspSpans
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
		if tl.activity.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= startPx && track.pointerAt.X < endPx {
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
				px := tl.tsToPx(tr.Event(ev).Ts)

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
					px := tl.tsToPx(tr.Event(ev).Ts)
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

				if tl.activity.showTooltips < showTooltipsNone && track.hovered && track.pointerAt.X >= minX && track.pointerAt.X < maxX {
					spanTooltipState.eventsUnderCursor = events[oldi : i+1]
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

		if track.highlightSpan != nil && track.highlightSpan(dspSpans) {
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
			if labels := track.spanLabel(dspSpans, tr); len(labels) > 0 {
				for i, label := range labels {
					if label == "" {
						continue
					}

					macro := op.Record(labelsOps)
					// OPT(dh): cache mapping from label to size. probably put a size limit on the cache, in case users generate millions of unique labels
					dims := mywidget.TextLine{Color: win.Theme.Palette.Foreground}.Layout(withOps(gtx, labelsOps), win.Theme.Shaper, text.Font{Weight: text.ExtraBold}, win.Theme.TextSize, label)
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

	if tl.unchanged() && track.prevFrame.dspSpans != nil {
		for _, prevSpans := range track.prevFrame.dspSpans {
			doSpans(prevSpans.dspSpans, prevSpans.startPx, prevSpans.endPx)
		}
	} else {
		allDspSpans := track.prevFrame.dspSpans[:0]
		it := renderedSpansIterator{
			tl:    tl,
			spans: tl.visibleSpans(track.spans),
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

type ProcessorTooltip struct {
	p     *Processor
	trace *Trace
}

func (tt ProcessorTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.ProcessorTooltip.Layout").End()

	// OPT(dh): compute statistics once, not on every frame

	tr := tt.trace
	d := time.Duration(tr.Events[len(tr.Events)-1].Ts)

	var userD, gcD time.Duration
	for i := range tt.p.spans {
		s := &tt.p.spans[i]
		d := tr.Duration(s)

		ev := tr.Events[s.event()]
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
		tt.p.id,
		len(tt.p.spans),
		userD, userPct,
		gcD, gcPct,
		inactiveD, inactivePct,
	)

	return theme.Tooltip{}.Layout(win, gtx, l)
}

func processorSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if len(state.spans) == 1 {
		s := &state.spans[0]
		ev := tr.Event(s.event())
		if s.state != stateRunningG {
			panic(fmt.Sprintf("unexpected state %d", s.state))
		}
		g := tr.getG(ev.G)
		label = local.Sprintf("Goroutine %d: %s\n", ev.G, g.function)
	} else {
		label = local.Sprintf("mixed (%d spans)\n", len(state.spans))
	}
	label += fmt.Sprintf("Duration: %s", state.spans.Duration(tr))
	return theme.Tooltip{}.Layout(win, gtx, label)
}

func NewProcessorWidget(tl *Timeline, p *Processor) *ActivityWidget {
	tr := tl.trace
	return &ActivityWidget{
		tracks: []Track{{spans: p.spans}},

		buildTrackWidgets: func(tracks []Track, out []ActivityWidgetTrack) {
			for i := range tracks {
				track := &tracks[i]
				out[i] = ActivityWidgetTrack{
					Track: track,
					highlightSpan: func(spans MergedSpans) bool {
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
					spanLabel: func(spans MergedSpans, tr *Trace) []string {
						if len(spans) != 1 {
							return nil
						}
						// OPT(dh): cache the strings
						out := make([]string, 3)
						g := tr.getG(tr.Event(spans[0].event()).G)
						if g.function != "" {
							short := shortenFunctionName(g.function)
							out[0] = fmt.Sprintf("g%d: %s", g.id, g.function)
							if short != g.function {
								out[1] = fmt.Sprintf("g%d: .%s", g.id, short)
								out[2] = fmt.Sprintf("g%d", g.id)
							} else {
								// This branch is probably impossible; all functions should be fully qualified.
								out[1] = fmt.Sprintf("g%d", g.id)
							}
						} else {
							out[0] = fmt.Sprintf("g%d", g.id)
						}
						return out

					},
					spanColor: func(spans MergedSpans, tr *Trace) [2]colorIndex {
						do := func(s Span, tr *Trace) colorIndex {
							gid := tr.Events[s.event()].G
							g := tr.getG(gid)
							switch fn := g.function; fn {
							case "runtime.bgscavenge", "runtime.bgsweep", "runtime.gcBgMarkWorker":
								return colorStateGC
							default:
								// TODO(dh): support goroutines that are currently doing GC assist work. this would require splitting spans, however.
								return stateColors[s.state]
							}
						}

						if len(spans) == 1 {
							return [2]colorIndex{do(spans[0], tr), 0}
						} else {
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
					spanContextMenu: func(spans MergedSpans, tr *Trace) []theme.Widget {
						items := []theme.Widget{
							tl.contextMenu.zoom.Layout,
						}

						if len(spans) == 1 {
							items = append(items, tl.contextMenu.scrollToGoroutine.Layout)
						}

						return items
					},
				}
			}
		},

		widgetTooltip: func(win *theme.Window, gtx layout.Context, aw *ActivityWidget) layout.Dimensions {
			return ProcessorTooltip{p, tl.trace}.Layout(win, gtx)
		},
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
	}
}

type GoroutineTooltip struct {
	g     *Goroutine
	trace *Trace
}

func (tt GoroutineTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineTooltip.Layout").End()

	tr := tt.trace
	start := tt.g.spans.Start(tr)
	end := tt.g.spans.End()
	d := time.Duration(end - start)

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
		"Spans: %[14]d\n"+
		"Time in blocked states: %[6]s (%.2[7]f%%)\n"+
		"Time in inactive states: %[8]s (%.2[9]f%%)\n"+
		"Time in GC assist: %[10]s (%.2[11]f%%)\n"+
		"Time in running states: %[12]s (%.2[13]f%%)",
		tt.g.id, fnName,
		formatTimestamp(start),
		formatTimestamp(end),
		d,
		tt.g.statistics.blocked, tt.g.statistics.blockedPct,
		tt.g.statistics.inactive, tt.g.statistics.inactivePct,
		tt.g.statistics.gcAssist, tt.g.statistics.gcAssistPct,
		tt.g.statistics.running, tt.g.statistics.runningPct,
		len(tt.g.spans),
	)

	return theme.Tooltip{}.Layout(win, gtx, l)
}

var reasonLabels = [256]string{
	reasonNewlyCreated: "newly created",
	reasonGosched:      "called runtime.Gosched",
	reasonTimeSleep:    "called time.Sleep",
	reasonPreempted:    "got preempted",
}

func unblockedByGoroutine(tr *Trace, s *Span) (uint64, bool) {
	ev := tr.Event(s.event())
	switch s.state {
	case stateBlocked, stateBlockedSend, stateBlockedRecv, stateBlockedSelect, stateBlockedSync,
		stateBlockedSyncOnce, stateBlockedSyncTriggeringGC, stateBlockedCond, stateBlockedNet, stateBlockedGC:
		if link := EventID(fromUint40(&ev.Link)); link != ^EventID(0) {
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
		label += fmt.Sprintf("Event ID: %d\n", state.spans[0].event())
		label += fmt.Sprintf("Event type: %d\n", tr.Event(state.spans[0].event()).Type)
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
			g := tr.getG(ev.G)
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

		if g, ok := unblockedByGoroutine(tr, s); ok {
			label += local.Sprintf("\nUnblocked by goroutine %d (%s)", g, tr.getG(g).function)
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

	return theme.Tooltip{}.Layout(win, gtx, label)
}

func userRegionSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if len(state.spans) == 1 {
		s := &state.spans[0]
		ev := tr.Event(s.event())
		if s.state != stateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.state))
		}
		if taskID := ev.Args[trace.ArgUserRegionTaskID]; taskID != 0 {
			label = local.Sprintf("User region: %s\nTask: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], tr.Task(taskID).name)
		} else {
			label = local.Sprintf("User region: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]])
		}
	} else {
		label = local.Sprintf("mixed (%d spans)\n", len(state.spans))
	}
	label += fmt.Sprintf("Duration: %s", state.spans.Duration(tr))
	return theme.Tooltip{}.Layout(win, gtx, label)
}

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

func NewGoroutineWidget(tl *Timeline, g *Goroutine) *ActivityWidget {
	var l string
	if g.function != "" {
		l = local.Sprintf("goroutine %d: %s", g.id, g.function)
	} else {
		l = local.Sprintf("goroutine %d", g.id)
	}

	aw := &ActivityWidget{
		tracks: []Track{{
			spans:  g.spans,
			events: g.events,
		}},
		buildTrackWidgets: func(tracks []Track, out []ActivityWidgetTrack) {
			for i := range tracks {
				i := i

				track := &tracks[i]
				switch track.kind {
				case ActivityWidgetTrackUnspecified:
					out[i] = ActivityWidgetTrack{
						Track: track,
						spanLabel: func(spans MergedSpans, _ *Trace) []string {
							if len(spans) != 1 {
								return nil
							}
							return spanStateLabels[spans[0].state]
						},
						spanTooltip: goroutineSpanTooltip,
						spanContextMenu: func(spans MergedSpans, tr *Trace) []theme.Widget {
							items := []theme.Widget{
								tl.contextMenu.zoom.Layout,
							}

							if len(spans) == 1 {
								switch spans[0].state {
								case stateActive, stateGCIdle, stateGCDedicated, stateGCMarkAssist, stateGCSweep:
									// These are the states that are actually on-CPU
									items = append(items, tl.contextMenu.scrollToProcessor.Layout)
								}
							}

							return items
						},
					}

				case ActivityWidgetTrackUserRegions:
					out[i] = ActivityWidgetTrack{
						Track: track,
						spanLabel: func(spans MergedSpans, tr *Trace) []string {
							if len(spans) != 1 {
								return nil
							}
							// OPT(dh): avoid this allocation
							s := tr.Strings[tr.Events[spans[0].event()].Args[trace.ArgUserRegionTypeID]]
							return []string{s}
						},
						spanTooltip: userRegionSpanTooltip,
						spanColor: func(spans MergedSpans, _ *Trace) [2]colorIndex {
							if len(spans) == 1 {
								return [2]colorIndex{colorStateUserRegion, 0}
							} else {
								return [2]colorIndex{colorStateUserRegion, colorStateMerged}
							}
						},
					}

				case ActivityWidgetTrackSampled:
					out[i] = ActivityWidgetTrack{
						Track: track,

						// TODO(dh): should we highlight hovered spans that share the same function?
						spanLabel: func(spans MergedSpans, tr *Trace) []string {
							if len(spans) != 1 {
								return nil
							}
							f := tr.PCs[track.spans[0].pc]

							short := shortenFunctionName(f.Fn)

							if short != f.Fn {
								return []string{f.Fn, "." + short}
							} else {
								// This branch is probably impossible; all functions should be fully qualified.
								return []string{f.Fn}
							}
						},
						spanColor: func(spans MergedSpans, _ *Trace) [2]colorIndex {
							if len(spans) != 1 {
								return [2]colorIndex{colorStateSample, colorStateMerged}
							}
							return [2]colorIndex{colorStateSample, 0}
						},
						spanTooltip: func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
							var label string
							if len(state.spans) == 1 {
								f := tr.PCs[state.spans[0].pc]
								label = local.Sprintf("Sampled function: %s\n", f.Fn)
								// TODO(dh): for truncated stacks we should display a relative depth instead
								label += local.Sprintf("Call depth: %d\n", i)
							} else {
								label = local.Sprintf("mixed (%d spans)\n", len(state.spans))
							}
							// We round the duration, in addition to saying "up to", to make it more obvious that the
							// duration is a guess
							label += fmt.Sprintf("Duration: up to %s", roundDuration(state.spans.Duration(tr)))
							return theme.Tooltip{}.Layout(win, gtx, label)
						},
					}

				default:
					panic(fmt.Sprintf("unexpected activity track kind %d", track.kind))
				}
			}
		},
		widgetTooltip: func(win *theme.Window, gtx layout.Context, aw *ActivityWidget) layout.Dimensions {
			return GoroutineTooltip{g, tl.trace}.Layout(win, gtx)
		},
		tl:    tl,
		item:  g,
		label: l,
	}

	for _, ug := range g.userRegions {
		aw.tracks = append(aw.tracks, Track{spans: ug, kind: ActivityWidgetTrackUserRegions})
	}

	addSampleTracks(aw, g)

	return aw
}

func addSampleTracks(aw *ActivityWidget, g *Goroutine) {
	tl := aw.tl

	var sampleTracks []Track
	offSpans := 0
	offSamples := 0

	nextEvent := func(advance bool) (EventID, bool, bool) {
		if offSpans == len(g.spans) && offSamples == len(g.cpuSamples) {
			return 0, false, false
		}

		if offSpans < len(g.spans) {
			id := g.spans[offSpans].event()
			if offSamples < len(g.cpuSamples) {
				oid := g.cpuSamples[offSamples]
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
			id := g.cpuSamples[offSamples]
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

		ev := &tl.trace.Events[evID]

		var stk []uint64
		// We benefit from primarily two kinds of events (aside from CPU samples): Blocking events (sleeps, selects,
		// I/O...) as these give us the most accurate stack trace right before a long period of inactivity, covering for
		// a lack of samples during blockedness, and preemption, as an additional periodic event, similar to sampling.
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
			stk = tl.trace.Stacks[ev.StkID]
		}
		if stk == nil {
			// Continue the previous stack trace if this event didn't contain a useful one. This happens both when we
			// choose to ignore an event, and when an event intrinsically has no stack trace, such as most EvGoStart
			// events.
			stk = prevStk
		}

		if isSample {
			// CPU samples include two runtime functions at the start of the stack trace that isn't present for stacks
			// collected by the runtime tracer.
			if len(stk) > 0 && tl.trace.PCs[stk[len(stk)-1]].Fn == "runtime.goexit" {
				stk = stk[:len(stk)-1]
			}
			if len(stk) > 0 && tl.trace.PCs[stk[len(stk)-1]].Fn == "runtime.main" {
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
			end = tl.trace.Events[endEvID].Ts
		} else {
			end = g.spans.End()
		}
		for i := 0; i < len(stk); i++ {
			spans := sampleTracks[i].spans
			if len(spans) != 0 {
				prevSpan := &spans[len(spans)-1]
				prevFn := prevFns[i]
				fn := tl.trace.PCs[stk[len(stk)-i-1]].Fn
				if prevSpan.end == tl.trace.Events[evID].Ts && prevFn == fn {
					// This is a continuation of the previous span
					//
					// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
					// data more apparent.
					prevSpan.end = end
				} else {
					// This is a new span
					span := Span{
						start:  ev.Ts,
						end:    end,
						pc:     stk[len(stk)-i-1],
						event_: packEventID(evID),
						state:  stateCPUSample,
					}
					spans = append(spans, span)
					sampleTracks[i].spans = spans
					prevFns[i] = fn
				}
			} else {
				// This is the first span
				span := Span{
					start:  ev.Ts,
					end:    end,
					pc:     stk[len(stk)-i-1],
					event_: packEventID(evID),
					state:  stateCPUSample,
				}
				spans = append(spans, span)
				sampleTracks[i].spans = spans
				prevFns[i] = tl.trace.PCs[stk[len(stk)-i-1]].Fn
			}
		}

		prevStk = stk
	}

	for i := range sampleTracks {
		sampleTracks[i].kind = ActivityWidgetTrackSampled
	}

	aw.tracks = append(aw.tracks, sampleTracks...)
}

func NewGCWidget(tl *Timeline, trace *Trace, spans Spans) *ActivityWidget {
	return &ActivityWidget{
		tracks: []Track{{spans: spans}},
		tl:     tl,
		item:   spans,
		label:  "GC",
	}
}

func NewSTWWidget(tl *Timeline, trace *Trace, spans Spans) *ActivityWidget {
	return &ActivityWidget{
		tracks: []Track{{spans: spans}},
		tl:     tl,
		item:   spans,
		label:  "STW",
	}
}

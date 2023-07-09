package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"strings"
	"sync"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/op"
	"gioui.org/text"
)

func LastSpan(sel ptrace.Spans) ptrace.Span {
	return sel.At(sel.Len() - 1)
}

func SpansDuration(sel Items[ptrace.Span]) time.Duration {
	if sel.Len() == 0 {
		return 0
	}
	if sel.Contiguous() {
		return time.Duration(LastSpan(sel).End - sel.At(0).Start)
	} else {
		var total time.Duration
		for i := 0; i < sel.Len(); i++ {
			total += time.Duration(sel.At(i).End - sel.At(i).Start)
		}
		return total
	}
}

type ItemContainer struct {
	Timeline *Timeline
	Track    *Track
}

type SpansInfo struct {
	mwin         MainWindowIface
	spans        *theme.Future[Items[ptrace.Span]]
	trace        *Trace
	allTimelines []*Timeline

	cfg SpansInfoConfig

	eventsList EventList
	spansList  SpanList

	buttons struct {
		scrollAndPanToSpans widget.PrimaryClickable
		zoomToSpans         widget.PrimaryClickable
		copyAsCSV           widget.PrimaryClickable
		selectUserRegion    widget.PrimaryClickable
	}

	tabbedState     theme.TabbedState
	stackSelectable widget.Selectable

	descriptionBuilder func(win *theme.Window, gtx layout.Context) Description
	descriptionText    Text

	stacktraceList widget.List
	statsList      widget.List

	statistics *theme.Future[*SpansStats]
	hist       InteractiveHistogram

	duration *theme.Future[time.Duration]
	state    *theme.Future[string]

	initOnce sync.Once

	theme.PanelButtons
}

type SpansInfoConfig struct {
	Title              string
	Label              string
	DescriptionBuilder func(win *theme.Window, gtx layout.Context) Description
	Stacktrace         string
	Statistics         func(win *theme.Window) *theme.Future[*SpansStats]
	Navigations        SpansInfoConfigNavigations
	ShowHistogram      bool
}

type SpansInfoConfigNavigations struct {
	ScrollLabel string
	ScrollFn    func() Link

	ZoomLabel string
	ZoomFn    func() Link
}

func NewSpansInfo(cfg SpansInfoConfig, tr *Trace, mwin MainWindowIface, spans *theme.Future[Items[ptrace.Span]], allTimelines []*Timeline) *SpansInfo {
	si := &SpansInfo{
		mwin:         mwin,
		spans:        spans,
		trace:        tr,
		cfg:          cfg,
		allTimelines: allTimelines,
	}

	return si
}

func (si *SpansInfo) init(win *theme.Window) {
	spans := si.spans.MustResult()
	c, haveContainer := spans.Container()
	if si.cfg.Title == "" {
		firstSpan := spans.At(0)
		lastSpan := LastSpan(spans)
		if haveContainer {
			si.cfg.Title = local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, c.Timeline.shortName)
		} else {
			si.cfg.Title = local.Sprintf("%d ns–%d ns\n", firstSpan.Start, lastSpan.End)
		}
	}

	if si.cfg.Stacktrace == "" && haveContainer && spans.Len() == 1 {
		ev := si.trace.Events[spans.At(0).Event]
		stk := si.trace.Stacks[ev.StkID]
		sb := strings.Builder{}
		for _, f := range stk {
			frame := si.trace.PCs[f]
			fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Fn, frame.File, frame.Line)
		}
		stacktrace := sb.String()
		if len(stacktrace) > 0 && stacktrace[len(stacktrace)-1] == '\n' {
			stacktrace = stacktrace[:len(stacktrace)-1]
		}

		si.cfg.Stacktrace = stacktrace
	}

	if si.cfg.Statistics == nil {
		si.cfg.Statistics = func(win *theme.Window) *theme.Future[*SpansStats] {
			return theme.NewFuture(win, func(cancelled <-chan struct{}) *SpansStats {
				return NewSpansStats(spans)
			})
		}
	}

	si.spansList = SpanList{
		Spans: spans,
	}

	si.eventsList = EventList{Trace: si.trace}
	si.eventsList.Filter.ShowGoCreate.Value = true
	si.eventsList.Filter.ShowGoUnblock.Value = true
	si.eventsList.Filter.ShowGoSysCall.Value = true
	si.eventsList.Filter.ShowUserLog.Value = true
	si.eventsList.Events = Events(spans, si.trace)
	si.eventsList.UpdateFilter()

	if si.cfg.DescriptionBuilder != nil {
		si.descriptionBuilder = si.cfg.DescriptionBuilder
	} else {
		si.descriptionBuilder = si.buildDefaultDescription
	}

	si.statistics = si.cfg.Statistics(win)
	histCfg := &widget.HistogramConfig{RejectOutliers: true, Bins: widget.DefaultHistogramBins}
	// XXX computeHistogram looks at all spans before starting a future; that part should probably be concurrent, too.
	si.computeHistogram(win, histCfg)

	si.state = theme.NewFuture(win, func(cancelled <-chan struct{}) string {
		firstSpan := spans.At(0)
		state := stateNames[firstSpan.State]
		for i := 1; i < spans.Len(); i++ {
			s := spans.At(i).State
			if s != firstSpan.State {
				state = "mixed"
				break
			}
		}
		return state
	})

	si.duration = theme.NewFuture(win, func(cancelled <-chan struct{}) time.Duration {
		return SpansDuration(spans)
	})
}

func (si *SpansInfo) computeHistogram(win *theme.Window, cfg *widget.HistogramConfig) {
	var spanDurations []time.Duration

	for i := 0; i < si.spans.MustResult().Len(); i++ {
		s := si.spans.MustResult().At(i)
		d := time.Duration(s.End - s.Start)
		spanDurations = append(spanDurations, d)
	}
	si.hist.Set(win, spanDurations)
}

func (si *SpansInfo) Title() string {
	return si.cfg.Title
}

func textSpinner(now time.Time) string {
	switch (now.UnixMilli() / 500) % 3 {
	case 0:
		return "."
	case 1:
		return ".."
	case 2:
		return "..."
	default:
		panic("unreachable")
	}
}

func (si *SpansInfo) buildDefaultDescription(win *theme.Window, gtx layout.Context) Description {
	// OPT(dh): there's no need to store the spans in a slice, so TextBuilder isn't what we want
	// OPT(dh): reuse memory
	tb := TextBuilder{Theme: win.Theme}
	var attrs []DescriptionAttribute
	if si.cfg.Label != "" {
		attrs = append(attrs, DescriptionAttribute{Key: "Label", Value: *tb.Span(si.cfg.Label)})
	}

	spans := si.spans.MustResult()

	firstSpan := spans.At(0)
	lastSpan := LastSpan(spans)
	attrs = append(attrs, DescriptionAttribute{
		Key:   "Start",
		Value: *tb.DefaultLink(formatTimestamp(firstSpan.Start), firstSpan.Start),
	})

	attrs = append(attrs, DescriptionAttribute{
		Key:   "End",
		Value: *tb.DefaultLink(formatTimestamp(lastSpan.End), lastSpan.End),
	})

	a := DescriptionAttribute{
		Key: "Duration",
	}
	if v, ok := si.duration.Result(); ok {
		a.Value = *tb.Span(v.String())
	} else {
		a.Value = *tb.Span(textSpinner(gtx.Now))
		op.InvalidateOp{}.Add(gtx.Ops)
	}
	attrs = append(attrs, a)

	a = DescriptionAttribute{
		Key: "State",
	}
	if v, ok := si.state.Result(); ok {
		a.Value = *tb.Span(v)
	} else {
		a.Value = *tb.Span(textSpinner(gtx.Now))
		op.InvalidateOp{}.Add(gtx.Ops)
	}
	attrs = append(attrs, a)

	if spans.Len() == 1 && firstSpan.Tags != 0 {
		tags := spanTagStrings(firstSpan.Tags)
		attrs = append(attrs, DescriptionAttribute{
			Key:   "Tags",
			Value: *(tb.Span(strings.Join(tags, ", "))),
		})
	}

	if c, ok := spans.Container(); ok {
		tl := c.Timeline
		attrs = append(attrs, DescriptionAttribute{
			Key:   "In",
			Value: *tb.DefaultLink(tl.shortName, tl.item),
		})
	}

	return Description{Attributes: attrs}
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpansInfo.Layout").End()

	// Inset of 5 pixels on all sides. We can't use layout.Inset because it doesn't decrease the minimum constraint,
	// which we do care about here.
	gtx.Constraints.Min = gtx.Constraints.Min.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints.Max = gtx.Constraints.Max.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints = layout.Normalize(gtx.Constraints)

	if gtx.Constraints.Max.X <= 5 || gtx.Constraints.Max.Y <= 5 {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	defer op.Offset(image.Pt(5, 5)).Push(gtx.Ops).Pop()
	nothing := func(gtx layout.Context) layout.Dimensions {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	var dims layout.Dimensions
	spans, haveSpans := si.spans.Result()
	if haveSpans {
		si.initOnce.Do(func() { si.init(win) })

		dims = layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				type button struct {
					w     *widget.Clickable
					label string
				}

				var buttonsLeft []button
				if _, ok := spans.Container(); ok {
					buttonsLeft = []button{
						{&si.buttons.scrollAndPanToSpans.Clickable, "Scroll and pan to spans"},
						{&si.buttons.zoomToSpans.Clickable, "Zoom to spans"},
					}
					if si.cfg.Navigations.ScrollLabel != "" {
						buttonsLeft[0].label = si.cfg.Navigations.ScrollLabel
					}
					if si.cfg.Navigations.ZoomLabel != "" {
						buttonsLeft[1].label = si.cfg.Navigations.ZoomLabel
					}
				}

				children := make([]layout.FlexChild, 0, len(buttonsLeft)+2)
				for _, btn := range buttonsLeft {
					btn := btn
					children = append(children,
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							return theme.Button(win.Theme, btn.w, btn.label).Layout(win, gtx)
						}),
						layout.Rigid(layout.Spacer{Width: 5}.Layout),
					)
				}
				children = append(children, layout.Flexed(1, nothing))
				children = append(children, layout.Rigid(theme.Dumb(win, si.PanelButtons.Layout)))

				// Right-aligned buttons should be aligned with the right side of the visible panel, not the width of the
				// panel contents, nor the infinite width of a possible surrounding list.
				gtx.Constraints.Max.X = gtx.Constraints.Min.X
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx, children...)
			}),

			layout.Rigid(layout.Spacer{Height: 10}.Layout),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min = image.Point{}
				si.descriptionText.Reset(win.Theme)
				return si.descriptionBuilder(win, gtx).Layout(win, gtx, &si.descriptionText)
			}),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				if spans.Len() == 1 && spans.At(0).State == ptrace.StateUserRegion {
					gtx.Constraints.Min = image.Point{}
					return theme.Button(win.Theme, &si.buttons.selectUserRegion.Clickable, "Select user region").Layout(win, gtx)
				}
				return layout.Dimensions{}
			}),

			layout.Rigid(layout.Spacer{Height: 10}.Layout),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				tabs := []string{"Statistics", "Spans"}
				if si.eventsList.Events.Len() != 0 {
					tabs = append(tabs, "Events")
				}
				if si.cfg.Stacktrace != "" {
					tabs = append(tabs, "Stack trace")
				}
				if si.cfg.ShowHistogram {
					tabs = append(tabs, "Histogram")
				}
				return theme.Tabbed(&si.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					switch tabs[si.tabbedState.Current] {
					case "Stack trace":
						return theme.List(win.Theme, &si.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
							if index != 0 {
								panic("impossible")
							}
							if si.stackSelectable.Text() == "" {
								si.stackSelectable.SetText(si.cfg.Stacktrace)
							}

							return si.stackSelectable.Layout(gtx, win.Theme.Shaper, font.Font{}, win.Theme.TextSize, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground), widget.ColorTextMaterial(gtx, win.Theme.Palette.PrimarySelection))
						})

					case "Statistics":
						return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
							layout.Rigid(func(gtx layout.Context) layout.Dimensions {
								return theme.List(win.Theme, &si.statsList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
									if index != 0 {
										panic("impossible")
									}
									if stats, ok := si.statistics.Result(); ok {
										return stats.Layout(win, gtx)
									} else {
										return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, "Computing statistics…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
									}
								})
							}),

							layout.Rigid(layout.Spacer{Height: 1}.Layout),

							layout.Rigid(func(gtx layout.Context) layout.Dimensions {
								gtx.Constraints.Min.X = 0
								return theme.Button(win.Theme, &si.buttons.copyAsCSV.Clickable, "Copy as CSV").Layout(win, gtx)
							}),
						)

					case "Spans":
						return si.spansList.Layout(win, gtx)

					case "Events":
						return si.eventsList.Layout(win, gtx)

					case "Histogram":
						return si.hist.Layout(win, gtx)

					default:
						panic("impossible")
					}
				})
			}),
		)
	} else {
		dims = layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				type button struct {
					w     *widget.Clickable
					label string
				}

				children := []layout.FlexChild{
					layout.Flexed(1, nothing),
					layout.Rigid(theme.Dumb(win, si.PanelButtons.Layout)),
				}

				// Right-aligned buttons should be aligned with the right side of the visible panel, not the width of the
				// panel contents, nor the infinite width of a possible surrounding list.
				gtx.Constraints.Max.X = gtx.Constraints.Min.X
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx, children...)
			}),

			layout.Rigid(layout.Spacer{Height: 10}.Layout),
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				l := "Collecting spans" + textSpinner(gtx.Now)
				op.InvalidateOp{}.Add(gtx.Ops)
				return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, l, widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
			}),
		)
	}

	for si.buttons.copyAsCSV.Clicked() {
		if stats, ok := si.statistics.Result(); ok {
			win.AppWindow.WriteClipboard(statisticsToCSV(&stats.stats))
		}
	}

	for _, ev := range si.descriptionText.Events() {
		handleLinkClick(win, si.mwin, ev)
	}

	for _, ev := range si.eventsList.Clicked() {
		handleLinkClick(win, si.mwin, ev)
	}
	for _, ev := range si.spansList.Clicked() {
		handleLinkClick(win, si.mwin, ev)
	}

	for si.buttons.scrollAndPanToSpans.Clicked() {
		if si.cfg.Navigations.ScrollFn != nil {
			si.mwin.OpenLink(si.cfg.Navigations.ScrollFn())
		} else {
			si.mwin.OpenLink(&ScrollAndPanToSpansLink{
				Spans: spans,
			})
		}
	}
	for si.buttons.zoomToSpans.Clicked() {
		if si.cfg.Navigations.ZoomFn != nil {
			si.mwin.OpenLink(si.cfg.Navigations.ZoomFn())
		} else {
			si.mwin.OpenLink(&ZoomToSpansLink{
				Spans: spans,
			})
		}
	}
	for si.PanelButtons.Backed() {
		si.mwin.PrevPanel()
	}
	for si.buttons.selectUserRegion.Clicked() {
		needle := si.trace.Strings[si.trace.Event(spans.At(0).Event).Args[2]]
		ft := theme.NewFuture[Items[ptrace.Span]](win, func(cancelled <-chan struct{}) Items[ptrace.Span] {
			var bases []Items[ptrace.Span]
			for _, tl := range si.allTimelines {
				select {
				case <-cancelled:
					return nil
				default:
				}
				for _, track := range tl.tracks {
					if track.kind != TrackKindUserRegions {
						continue
					}
					filtered := FilterItems(track.spans, func(span ptrace.Span) bool {
						label := si.trace.Strings[si.trace.Event(span.Event).Args[2]]
						return label == needle
					})

					bases = append(bases, filtered)
				}
			}

			return MergeItems(bases, func(a, b *ptrace.Span) bool {
				return a.Start < b.Start
			})
		})
		cfg := SpansInfoConfig{
			Title:         fmt.Sprintf("All %q user regions", needle),
			Label:         fmt.Sprintf("All %q user regions", needle),
			ShowHistogram: true,
		}
		si.mwin.OpenPanel(NewSpansInfo(cfg, si.trace, si.mwin, ft, si.allTimelines))
	}

	if si.hist.Changed() {
		si.computeHistogram(win, &si.hist.Config)
	}

	return dims
}

func spanTagStrings(tags ptrace.SpanTags) []string {
	if tags == 0 {
		return nil
	}

	out := make([]string, 0, 4)
	if tags&ptrace.SpanTagRead != 0 {
		out = append(out, "read")
	}
	if tags&ptrace.SpanTagAccept != 0 {
		out = append(out, "accept")
	}
	if tags&ptrace.SpanTagDial != 0 {
		out = append(out, "dial")
	}
	if tags&ptrace.SpanTagNetwork != 0 {
		out = append(out, "network")
	}
	if tags&ptrace.SpanTagTCP != 0 {
		out = append(out, "TCP")
	}
	if tags&ptrace.SpanTagTLS != 0 {
		out = append(out, "TLS")
	}
	if tags&ptrace.SpanTagHTTP != 0 {
		out = append(out, "HTTP")
	}
	return out
}

var spanListColumns = []theme.TableListColumn{
	{
		Name: "Time",
		// XXX the width depends on the font and scaling
		MinWidth: 200,
		MaxWidth: 200,
	},

	{
		Name: "Duration",
		// XXX the width depends on the font and scaling
		MinWidth: 120,
		MaxWidth: 120,
	},

	{
		Name: "State",
	},
}

type SpanList struct {
	Spans Items[ptrace.Span]
	list  widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (spans *SpanList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpanList.Layout").End()

	spans.list.Axis = layout.Vertical
	spans.timestampObjects.Reset()

	var txtCnt int
	// OPT(dh): reuse memory
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		tb := TextBuilder{Theme: win.Theme}
		var txt *Text
		if txtCnt < spans.texts.Len() {
			txt = spans.texts.Ptr(txtCnt)
		} else {
			txt = spans.texts.Allocate(Text{})
		}
		txtCnt++
		txt.Reset(win.Theme)

		span := spans.Spans.At(row)
		switch col {
		case 0: // Time
			tb.Link(formatTimestamp(span.Start), span, &SpansObjectLink{
				Spans: spans.Spans.Slice(row, row+1),
			})
			txt.Alignment = text.End
		case 1: // Duration
			value, unit := durationNumberFormatSITable.format(span.Duration())
			tb.Span(value)
			tb.Span(" ")
			s := tb.Span(unit)
			s.Font.Variant = "Mono"
			txt.Alignment = text.End
		case 2: // State
			label := stateNamesCapitalized[span.State]
			tb.Span(label)
		}

		dims := txt.Layout(win, gtx, tb.Spans)
		dims.Size = gtx.Constraints.Constrain(dims.Size)
		return dims
	}

	tbl := theme.TableListStyle{
		Columns:       spanListColumns,
		List:          &spans.list,
		ColumnPadding: gtx.Dp(10),
	}

	gtx.Constraints.Min = gtx.Constraints.Max
	return tbl.Layout(win, gtx, spans.Spans.Len(), cellFn)
}

// Clicked returns all objects of text spans that have been clicked since the last call to Layout.
func (spans *SpanList) Clicked() []TextEvent {
	// This only allocates when links have been clicked, which is a very low frequency event.
	var out []TextEvent
	for i := 0; i < spans.texts.Len(); i++ {
		txt := spans.texts.Ptr(i)
		out = append(out, txt.Events()...)
	}
	return out
}

/*
   Notes for redesigning spans data structures

   ptrace turns the events in a trace into spans. A span is defined by its start and end point, the event that caused
   it, and some other metadata.

   Spans are used in many places. Goroutines have spans for goroutine activity, as well as spans for user regions (one
   set of spans per nesting depth). Processors have spans describing the scheduled goroutines. GC and STW are each
   represented by a series of spans.

   Spans for goroutine activity are contiguous, that is, there are no gaps between spans. All other spans can have gaps
   between them.

   Goroutines have events. These are instantaneous.


   In gotraceui, spans are displayed in tracks, which are displayed in timelines. For example, a goroutine timeline has
   a track for the goroutine activity, and one track per level of nesting of user regions. Additionally, CPU samples —
   which ptrace exposes as a slice of events — are turned into spans, which are displayed in tracks, one per level of
   stack depth.

   When displaying individual spans, spans that are too small get merged into one big span. This big span is, too,
   represented as a ptrace.Spans, and is a subslice of the track's overall spans.

   We want to apply the following operations on spans:

   - Compute their total duration. For contiguous spans, this is simply the end time minus the start time. For
     non-contiguous spans, we have to sum the durations of all individual spans.

   - Zoom to spans. This requires scrolling to the correct track and zooming to the area between the start and end time.

   - Filter spans. Of all spans in a track, we may only be interested in a subset. After that, we can no longer assume
     that the spans are contiguous.

   - Merge multiple sets of spans. For example, we may want to represent all spans that describe a user region with a
     certain label. This will include spans from different tracks. Spans have to be ordered by start time, for binary
     search to work correctly.

   - We want to correlate spans with the events that happened during those spans. We haven't yet decided if events are
     scoped to a timeline or a track. If they are scoped to timelines then we have to deduplicate events when merging
     multiple tracks. When rendering tracks, events are certainly track-scoped: only the first track in a goroutine
     timeline displays the events. However, when looking at user regions in the spans info panel, we likely want to see
     goroutine events that happened during the user region.

   - Spans may all be in the same container (timeline + track), or in multiple different ones, depending if we've merged
     sets of spans or not.

   When filtering and merging spans, we'd like not to create completely new slices, and to instead reuse the existing
   memory.

   Tracks may display filtered/merged/... spans, not just plain []Span from ptrace.
*/

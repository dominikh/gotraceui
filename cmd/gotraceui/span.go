package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"strings"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/op"
	"gioui.org/text"
	exptrace "golang.org/x/exp/trace"
)

func LastItemPtr[E any, T Items[E]](sel T) *E {
	return sel.AtPtr(sel.Len() - 1)
}

func spansDuration(sel Items[ptrace.Span], giveUp bool) (time.Duration, bool) {
	if sel.Len() == 0 {
		return 0, true
	}
	if sel.Contiguous() {
		return time.Duration(LastItemPtr(sel).End - sel.AtPtr(0).Start), true
	} else if !giveUp || sel.Len() < 1e5 {
		var total time.Duration
		for i := 0; i < sel.Len(); i++ {
			total += time.Duration(sel.AtPtr(i).End - sel.AtPtr(i).Start)
		}
		return total, true
	} else {
		// Computing an accurate duration for more than 1e5 spans is too slow, so signal failure, upon which the caller
		// should fall back to SpansTimeSpan.
		return 0, false
	}
}

func AccurateSpansDuration(sel Items[ptrace.Span]) time.Duration {
	d, ok := spansDuration(sel, false)
	assert(ok, "expected spans duration computation not to fail")
	return d
}

func SpansDuration(sel Items[ptrace.Span]) (time.Duration, bool) {
	return spansDuration(sel, true)
}

type ItemContainer struct {
	Timeline *Timeline
	Track    *Track
}

type SpansInfo struct {
	mwin         *theme.Window
	spans        *theme.Future[Items[ptrace.Span]]
	trace        *Trace
	allTimelines []*Timeline

	cfg SpansInfoConfig

	eventList EventList
	spanList  SpanList

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
	hoveredLink        ObjectLink
	prevSpans          []TextSpan

	stacktraceList widget.List

	statistics *theme.Future[*SpansStats]
	hist       InteractiveHistogram

	duration *theme.Future[time.Duration]
	state    *theme.Future[string]

	initialized bool

	theme.ComponentButtons
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
	Scroll struct {
		ButtonLabel string
		Fn          func() theme.Action
	}

	Zoom struct {
		ButtonLabel string
		Fn          func() theme.Action
	}
}

func NewSpansInfo(cfg SpansInfoConfig, tr *Trace, mwin *theme.Window, spans *theme.Future[Items[ptrace.Span]], allTimelines []*Timeline) *SpansInfo {
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
		firstStart := spans.AtPtr(0).Start
		lastEnd := LastItemPtr(spans).End
		if haveContainer {
			si.cfg.Title = local.Sprintf("%d ns–%d ns @ %s", firstStart, lastEnd, c.Timeline.shortName)
		} else {
			si.cfg.Title = local.Sprintf("%d ns–%d ns", firstStart, lastEnd)
		}
	}

	if si.cfg.Stacktrace == "" && haveContainer && spans.Len() == 1 {
		ev := si.trace.Event(spans.AtPtr(0).StartEvent)
		stk := ev.Stack()
		sb := strings.Builder{}
		stk.Frames(func(frame exptrace.StackFrame) bool {
			fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Func, frame.File, frame.Line)
			return true
		})
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

	si.spanList = SpanList{
		Spans: NewSortedItems(spans),
	}

	si.eventList = EventList{Trace: si.trace}
	si.eventList.Filter.ShowGoCreate.Value = true
	si.eventList.Filter.ShowGoUnblock.Value = true
	si.eventList.Filter.ShowGoSysCall.Value = true
	si.eventList.Filter.ShowUserLog.Value = true
	si.eventList.Filter.ShowTasks.Value = true
	si.eventList.Events = Events(spans, si.trace)
	si.eventList.UpdateFilter()

	if si.cfg.DescriptionBuilder != nil {
		si.descriptionBuilder = si.cfg.DescriptionBuilder
	} else {
		si.descriptionBuilder = si.buildDefaultDescription
	}

	si.statistics = si.cfg.Statistics(win)
	if si.cfg.ShowHistogram {
		// XXX computeHistogram looks at all spans before starting a future; that part should probably be concurrent, too.
		histCfg := &widget.HistogramConfig{RejectOutliers: true, Bins: widget.DefaultHistogramBins}
		si.computeHistogram(win, histCfg)
	}

	si.state = theme.NewFuture(win, func(cancelled <-chan struct{}) string {
		firstState := spans.AtPtr(0).State
		state := stateNames[firstState]
		for i := 1; i < spans.Len(); i++ {
			s := spans.AtPtr(i).State
			if s != firstState {
				state = "mixed"
				break
			}
		}
		return state
	})

	si.duration = theme.NewFuture(win, func(cancelled <-chan struct{}) time.Duration {
		return AccurateSpansDuration(spans)
	})
}

func (si *SpansInfo) computeHistogram(win *theme.Window, cfg *widget.HistogramConfig) {
	spans := si.spans.MustResult()
	n := spans.Len()
	spanDurations := make([]time.Duration, n)
	for i := 0; i < n; i++ {
		s := spans.AtPtr(i)
		d := time.Duration(s.End - s.Start)
		spanDurations[i] = d
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
	tb := TextBuilder{Window: win}
	var attrs []DescriptionAttribute
	if si.cfg.Label != "" {
		attrs = append(attrs, DescriptionAttribute{Key: "Label", Value: *tb.Span(si.cfg.Label)})
	}

	spans := si.spans.MustResult()

	firstSpan := spans.AtPtr(0)
	lastSpan := LastItemPtr(spans)
	link := *tb.DefaultLink(formatTimestamp(nil, si.trace.AdjustedTime(firstSpan.Start)), "Start of current spans", firstSpan.Start)
	attrs = append(attrs, DescriptionAttribute{
		Key:   "Start",
		Value: link,
	})

	link = *tb.DefaultLink(formatTimestamp(nil, si.trace.AdjustedTime(lastSpan.End)), "End of current spans", lastSpan.End)
	attrs = append(attrs, DescriptionAttribute{
		Key:   "End",
		Value: link,
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

	if spans.Len() > 1 && !spans.Contiguous() {
		a = DescriptionAttribute{
			Key:   "Time span",
			Value: *tb.Span(SpansTimeSpan(spans).Duration().String()),
		}
		attrs = append(attrs, a)
	}

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
			Value: *tb.Span(strings.Join(tags, ", ")),
		})
	}

	if c, ok := spans.Container(); ok {
		tl := c.Timeline
		link := *tb.DefaultLink(tl.shortName, "Timeline containing current spans", tl.item)
		attrs = append(attrs, DescriptionAttribute{
			Key:   "In",
			Value: link,
		})
	}

	return Description{Attributes: attrs}
}

func (si *SpansInfo) scrollAndPanToSpans(win *theme.Window) {
	if si.cfg.Navigations.Scroll.Fn != nil {
		win.EmitAction(si.cfg.Navigations.Scroll.Fn())
	} else {
		win.EmitAction(&ScrollAndPanToSpansAction{
			Spans: si.spans.MustResult(),
		})
	}
}

func (si *SpansInfo) zoomToSpans(win *theme.Window) {
	if si.cfg.Navigations.Zoom.Fn != nil {
		win.EmitAction(si.cfg.Navigations.Zoom.Fn())
	} else {
		win.EmitAction(&ZoomToSpansAction{
			Spans: si.spans.MustResult(),
		})
	}
}

func (si *SpansInfo) HoveredLink() ObjectLink {
	return si.hoveredLink
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpansInfo.Layout").End()

	// Inset of 5 pixels on all sides. We can't use layout.Inset because it doesn't decrease the minimum constraint,
	// which we do care about here.
	gtx.Constraints.Min = gtx.Constraints.Min.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints.Max = gtx.Constraints.Max.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints = layout.Normalize(gtx.Constraints)
	spans, haveSpans := si.spans.Result()

	for si.buttons.copyAsCSV.Clicked(gtx) {
		if stats, ok := si.statistics.Result(); ok {
			win.AppWindow.WriteClipboard(statisticsToCSV(stats.stats.Items))
		}
	}

	for _, ev := range si.descriptionText.Update(gtx, si.prevSpans) {
		handleLinkClick(win, ev.Event, ev.Span.ObjectLink)
	}
	for _, ev := range si.eventList.Update(gtx) {
		handleLinkClick(win, ev.Event, ev.Span.ObjectLink)
	}

	firstNonNil := func(els ...ObjectLink) ObjectLink {
		for _, el := range els {
			if el != nil {
				return el
			}
		}
		return nil
	}
	si.hoveredLink = firstNonNil(
		si.descriptionText.HoveredLink(),
		si.eventList.HoveredLink(),
		si.spanList.HoveredLink(),
	)

	for si.buttons.scrollAndPanToSpans.Clicked(gtx) {
		si.scrollAndPanToSpans(win)
	}
	for si.buttons.zoomToSpans.Clicked(gtx) {
		si.zoomToSpans(win)
	}
	for si.ComponentButtons.Backed(gtx) {
		si.mwin.EmitAction(&PrevPanelAction{})
	}
	for si.buttons.selectUserRegion.Clicked(gtx) {
		r := si.trace.Event(spans.AtPtr(0).StartEvent).Region()
		needle := r.Type
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
					filtered := FilterItems(track.Spans(win).Wait(), func(span *ptrace.Span) bool {
						label := si.trace.Event(span.StartEvent).Region().Type
						return label == needle
					})

					if filtered.Len() > 0 {
						bases = append(bases, filtered)
					}
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
		si.mwin.EmitAction(&OpenPanelAction{NewSpansInfo(cfg, si.trace, si.mwin, ft, si.allTimelines)})
	}

	if si.hist.Update(gtx) {
		si.computeHistogram(win, &si.hist.Config)
	}

	if gtx.Constraints.Max.X <= 5 || gtx.Constraints.Max.Y <= 5 {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	defer op.Offset(image.Pt(5, 5)).Push(gtx.Ops).Pop()
	nothing := func(gtx layout.Context) layout.Dimensions {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	var dims layout.Dimensions
	if haveSpans {
		if !si.initialized {
			si.init(win)
			si.initialized = true
		}

		dims = layout.Rigids(gtx, layout.Vertical,
			func(gtx layout.Context) layout.Dimensions {
				type button struct {
					w     *widget.Clickable
					label string
				}

				var buttonsLeft []button
				if _, ok := spans.Container(); ok {
					buttonsLeft = []button{
						{
							&si.buttons.scrollAndPanToSpans.Clickable,
							"Scroll and pan to spans",
						},

						{
							&si.buttons.zoomToSpans.Clickable,
							"Zoom to spans",
						},
					}
					if si.cfg.Navigations.Scroll.ButtonLabel != "" {
						buttonsLeft[0].label = si.cfg.Navigations.Scroll.ButtonLabel
					}
					if si.cfg.Navigations.Zoom.ButtonLabel != "" {
						buttonsLeft[1].label = si.cfg.Navigations.Zoom.ButtonLabel
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
				children = append(children, layout.Rigid(theme.Dumb(win, si.ComponentButtons.Layout)))

				// Right-aligned buttons should be aligned with the right side of the visible panel, not the width of the
				// panel contents, nor the infinite width of a possible surrounding list.
				gtx.Constraints.Max.X = gtx.Constraints.Min.X
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx, children...)
			},

			layout.Spacer{Height: 10}.Layout,
			func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min = image.Point{}
				si.descriptionText.Reset(win.Theme)
				desc := si.descriptionBuilder(win, gtx)
				dims, spans := desc.Layout(win, gtx, &si.descriptionText)
				si.prevSpans = spans
				return dims
			},
			func(gtx layout.Context) layout.Dimensions {
				if spans.Len() == 1 && spans.AtPtr(0).State == ptrace.StateUserRegion {
					gtx.Constraints.Min = image.Point{}
					return theme.Button(win.Theme, &si.buttons.selectUserRegion.Clickable, "Select user region").Layout(win, gtx)
				}
				return layout.Dimensions{}
			},

			layout.Spacer{Height: 10}.Layout,
			func(gtx layout.Context) layout.Dimensions {
				tabs := []string{"Statistics", "Spans"}
				if si.eventList.Events.Len() != 0 {
					tabs = append(tabs, "Events")
				}
				if si.cfg.Stacktrace != "" {
					tabs = append(tabs, "Stack trace")
				}
				if si.cfg.ShowHistogram {
					tabs = append(tabs, "Histogram")
				}
				return theme.Tabbed(&si.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					gtx.Constraints.Min = gtx.Constraints.Max
					switch tabs[si.tabbedState.Current] {
					case "Stack trace":
						return theme.List(win.Theme, &si.stacktraceList).Layout(
							win,
							gtx,
							1,
							func(gtx layout.Context, index int) layout.Dimensions {
								if index != 0 {
									panic("impossible")
								}
								if si.stackSelectable.Text() == "" {
									si.stackSelectable.SetText(si.cfg.Stacktrace)
								}

								return si.stackSelectable.Layout(
									gtx,
									win.Theme.Shaper,
									font.Font{},
									win.Theme.TextSize,
									win.ColorMaterial(gtx, win.Theme.Palette.Foreground),
									win.ColorMaterial(gtx, win.Theme.Palette.PrimarySelection))
							},
						)

					case "Statistics":
						return layout.Rigids(gtx, layout.Vertical,
							func(gtx layout.Context) layout.Dimensions {
								if stats, ok := si.statistics.Result(); ok {
									return stats.Layout(win, gtx)
								} else {
									return theme.Label(win.Theme, "Computing statistics…").Layout(win, gtx)
								}
							},

							layout.Spacer{Height: 1}.Layout,

							func(gtx layout.Context) layout.Dimensions {
								gtx.Constraints.Min.X = 0
								return theme.Button(win.Theme, &si.buttons.copyAsCSV.Clickable, "Copy as CSV").Layout(win, gtx)
							},
						)

					case "Spans":
						return si.spanList.Layout(win, gtx, si.trace)

					case "Events":
						return si.eventList.Layout(win, gtx)

					case "Histogram":
						return si.hist.Layout(win, gtx)

					default:
						panic("impossible")
					}
				})
			},
		)
	} else {
		dims = layout.Rigids(gtx, layout.Vertical,
			func(gtx layout.Context) layout.Dimensions {
				children := []layout.FlexChild{
					layout.Flexed(1, nothing),
					layout.Rigid(theme.Dumb(win, si.ComponentButtons.Layout)),
				}

				// Right-aligned buttons should be aligned with the right side of the visible panel, not the width of the
				// panel contents, nor the infinite width of a possible surrounding list.
				gtx.Constraints.Max.X = gtx.Constraints.Min.X
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx, children...)
			},

			layout.Spacer{Height: 10}.Layout,
			func(gtx layout.Context) layout.Dimensions {
				l := "Collecting spans" + textSpinner(gtx.Now)
				op.InvalidateOp{}.Add(gtx.Ops)
				return theme.Label(win.Theme, l).Layout(win, gtx)
			},
		)
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

type SpanList struct {
	Spans         SortedItems[ptrace.Span]
	table         theme.Table
	scrollState   theme.YScrollableListState
	cellFormatter CellFormatter
}

func (spans *SpanList) sort() {
	switch spans.table.SortedBy {
	case 0: // Span, impossible
	case 1: // Start time
		spans.Spans.Sort(func(a, b *ptrace.Span) int {
			return cmp(a.Start, b.Start, spans.table.SortOrder == theme.SortDescending)
		})
	case 2: // Duration
		spans.Spans.Sort(func(a, b *ptrace.Span) int {
			return cmp(a.Duration(), b.Duration(), spans.table.SortOrder == theme.SortDescending)
		})
	case 3: // State
		spans.Spans.Sort(func(a, b *ptrace.Span) int {
			sa := stateNames[a.State]
			sb := stateNames[b.State]
			return cmp(sa, sb, spans.table.SortOrder == theme.SortDescending)
		})
	default:
		panic(fmt.Sprintf("unreachable: %d", spans.table.SortedBy))
	}
}

func (spans *SpanList) Layout(win *theme.Window, gtx layout.Context, tr *Trace) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpanList.Layout").End()

	spans.cellFormatter.Update(win, gtx)

	if spans.table.Columns == nil {
		cols := []theme.Column{
			{Name: "Span", Clickable: false, Alignment: text.Start},
			{Name: "Start time", Clickable: true, Alignment: text.End},
			{Name: "Duration", Clickable: true, Alignment: text.End},
			{Name: "State", Clickable: true, Alignment: text.Start},
		}
		spans.table.SetColumns(win, gtx, cols)
		spans.table.SortedBy = 1
		spans.table.SortOrder = theme.SortAscending
	}

	spans.table.Update(gtx)
	if _, ok := spans.table.SortByClickedColumn(); ok {
		spans.sort()
	}

	cellFn := func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		span := spans.Spans.AtPtr(row)
		switch col {
		case 0:
			return spans.cellFormatter.Spans(win, gtx, spans.Spans.Slice(row, row+1))
		case 1: // Time
			return spans.cellFormatter.Timestamp(win, gtx, tr, span.Start, "")
		case 2: // Duration
			return spans.cellFormatter.Duration(win, gtx, span.Duration(), false)
		case 3: // State
			return spans.cellFormatter.Text(win, gtx, stateNamesCapitalized[span.State])
		default:
			panic(fmt.Sprintf("unreachable: %d", col))
		}
	}

	gtx.Constraints.Min = gtx.Constraints.Max
	return theme.SimpleTable(win, gtx, &spans.table, &spans.scrollState, spans.Spans.Len(), cellFn)
}

// HoveredLink returns the link that has been hovered during the last call to Layout.
func (spans *SpanList) HoveredLink() ObjectLink {
	return spans.cellFormatter.HoveredLink()
}

type TimeSpan struct {
	Start exptrace.Time
	End   exptrace.Time
}

type TimeSpanner interface {
	Start() exptrace.Time
	End() exptrace.Time
}

type spanWithGetters struct {
	ptrace.Span
}

func (s *spanWithGetters) Start() exptrace.Time { return s.Span.Start }
func (s *spanWithGetters) End() exptrace.Time   { return s.Span.End }

func SpansTimeSpan(spans Items[ptrace.Span]) TimeSpan {
	return TimeSpan{
		Start: spans.AtPtr(0).Start,
		End:   LastItemPtr(spans).End,
	}
}

func (ts TimeSpan) Duration() time.Duration {
	return time.Duration(ts.End - ts.Start)
}

func defaultSpanTooltip(
	win *theme.Window,
	gtx layout.Context,
	tr *Trace,
	spans Items[ptrace.Span],
) layout.Dimensions {
	var label string
	if n := spans.Len(); n > 1 {
		label = local.Sprintf("%d spans\n", n)
	}
	label += spansDurationForTooltip(spans)
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func spansDurationForTooltip(spans Items[ptrace.Span]) string {
	return spansDurationForTooltipWithQualifier(spans, "")
}

func spansDurationForTooltipWithQualifier(spans Items[ptrace.Span], q string) string {
	var label string
	if d, ok := SpansDuration(spans); ok {
		if q != "" {
			label = fmt.Sprintf("Duration: %s %s\n", q, roundDuration(d))
		} else {
			label = fmt.Sprintf("Duration: %s\n", roundDuration(d))
		}
	}
	if !spans.Contiguous() && spans.Len() > 1 {
		label += fmt.Sprintf("Time span: %s\n", roundDuration(SpansTimeSpan(spans).Duration()))
	}
	return label
}

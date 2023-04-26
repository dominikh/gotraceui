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

func SpansDuration(sel ptrace.Spans) time.Duration {
	if sel.Len() == 0 {
		return 0
	}
	return time.Duration(LastSpan(sel).End - sel.At(0).Start)
}

type SpanContainer struct {
	Timeline *Timeline
	Track    *Track
}

type SpansInfo struct {
	mwin      MainWindowIface
	allEvents []ptrace.EventID
	spans     ptrace.Spans
	trace     *Trace
	timelines []*Timeline

	cfg SpansInfoConfig

	events    EventList
	spansList SpanList

	buttons struct {
		scrollAndPanToSpans widget.PrimaryClickable
		zoomToSpans         widget.PrimaryClickable
		copyAsCSV           widget.PrimaryClickable
		selectUserRegion    widget.PrimaryClickable
	}

	tabbedState     theme.TabbedState
	stackSelectable widget.Selectable

	descriptionBuilder func(win *theme.Window) Description
	description        Description

	stacktraceList widget.List
	statsList      widget.List

	statistics *theme.Future[*SpansStats]
	hist       InteractiveHistogram

	initOnce sync.Once

	theme.PanelButtons
}

type SpansInfoConfig struct {
	Title              string
	Label              string
	Container          SpanContainer
	DescriptionBuilder func(win *theme.Window) Description
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

func NewSpansInfo(cfg SpansInfoConfig, tr *Trace, mwin MainWindowIface, spans ptrace.Spans, timelines []*Timeline, allEvents []ptrace.EventID) *SpansInfo {
	si := &SpansInfo{
		mwin:      mwin,
		spans:     spans,
		trace:     tr,
		allEvents: allEvents,
		cfg:       cfg,
		timelines: timelines,
	}

	if si.cfg.Title == "" {
		firstSpan := si.spans.At(0)
		lastSpan := LastSpan(si.spans)
		si.cfg.Title = local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, si.cfg.Container.Timeline.shortName)
	}

	if si.cfg.Stacktrace == "" && (si.cfg.Container != SpanContainer{}) && spans.Len() == 1 {
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
				return NewSpansStats(si.spans)
			})
		}
	}

	si.spansList = SpanList{
		Spans: si.spans,
	}

	si.events = EventList{Trace: si.trace}
	si.events.Filter.ShowGoCreate.Value = true
	si.events.Filter.ShowGoUnblock.Value = true
	si.events.Filter.ShowGoSysCall.Value = true
	si.events.Filter.ShowUserLog.Value = true
	si.events.UpdateFilter()

	si.events.Events = si.spans.Events(si.allEvents, si.trace.Trace)
	si.events.UpdateFilter()

	if cfg.DescriptionBuilder != nil {
		si.descriptionBuilder = cfg.DescriptionBuilder
	} else {
		si.descriptionBuilder = si.buildDefaultDescription
	}

	return si
}

func (si *SpansInfo) init(win *theme.Window) {
	si.statistics = si.cfg.Statistics(win)
	histCfg := &widget.HistogramConfig{RejectOutliers: true, Bins: widget.DefaultHistogramBins}
	si.computeHistogram(win, histCfg)
	si.description = si.descriptionBuilder(win)
}

func (si *SpansInfo) computeHistogram(win *theme.Window, cfg *widget.HistogramConfig) {
	var spanDurations []time.Duration

	for i := 0; i < si.spans.Len(); i++ {
		s := si.spans.At(i)
		d := time.Duration(s.End - s.Start)
		spanDurations = append(spanDurations, d)
	}
	si.hist.Set(win, spanDurations)
}

func (si *SpansInfo) Title() string {
	return si.cfg.Title
}

func (si *SpansInfo) buildDefaultDescription(win *theme.Window) Description {
	value := func(s *TextSpan) *theme.Future[TextSpan] {
		return theme.Immediate(*s)
	}

	// OPT(dh): there's no need to store the spans in a slice, so TextBuilder isn't what we want
	tb := TextBuilder{Theme: win.Theme}
	var attrs []DescriptionAttribute
	if si.cfg.Label != "" {
		attrs = append(attrs, DescriptionAttribute{Key: "Label", Value: value(tb.Span(si.cfg.Label))})
	}

	firstSpan := si.spans.At(0)
	lastSpan := LastSpan(si.spans)
	attrs = append(attrs, DescriptionAttribute{
		Key:   "Start",
		Value: value(tb.Link(formatTimestamp(firstSpan.Start), firstSpan.Start)),
	})

	attrs = append(attrs, DescriptionAttribute{
		Key:   "End",
		Value: value(tb.Link(formatTimestamp(lastSpan.End), lastSpan.End)),
	})

	// XXX reconsider this. for goroutines and possibly merged spans it makes sense to define duration as end-start. but
	// for filtered spans we probably want to compute the sum instead.
	attrs = append(attrs, DescriptionAttribute{
		Key:   "Duration",
		Value: value(tb.Span(SpansDuration(si.spans).String())),
	})

	attrs = append(attrs, DescriptionAttribute{
		Key: "State",
		Value: theme.NewFuture(win, func(cancelled <-chan struct{}) TextSpan {
			state := stateNames[firstSpan.State]
			for i := 1; i < si.spans.Len(); i++ {
				s := si.spans.At(i).State
				if s != firstSpan.State {
					state = "mixed"
					break
				}
			}
			return *tb.Span(state)
		}),
	})

	if si.spans.Len() == 1 && firstSpan.Tags != 0 {
		tags := spanTagStrings(firstSpan.Tags)
		attrs = append(attrs, DescriptionAttribute{
			Key:   "Tags",
			Value: value(tb.Span(strings.Join(tags, ", "))),
		})
	}

	if si.cfg.Container.Timeline != nil {
		attrs = append(attrs, DescriptionAttribute{
			Key:   "In",
			Value: value(tb.Link(si.cfg.Container.Timeline.shortName, si.cfg.Container.Timeline.item)),
		})
	}

	return Description{Attributes: attrs}
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpansInfo.Layout").End()

	si.initOnce.Do(func() { si.init(win) })

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

	dims := layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			type button struct {
				w     *widget.Clickable
				label string
			}

			var buttonsLeft []button
			if si.cfg.Container.Timeline != nil {
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
			return si.description.Layout(win, gtx)
		}),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			if si.spans.Len() == 1 && si.spans.At(0).State == ptrace.StateUserRegion {
				gtx.Constraints.Min = image.Point{}
				return theme.Button(win.Theme, &si.buttons.selectUserRegion.Clickable, "Select user region").Layout(win, gtx)
			}
			return layout.Dimensions{}
		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			tabs := []string{"Statistics", "Spans"}
			if len(si.events.Events) != 0 {
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
					return si.events.Layout(win, gtx)

				case "Histogram":
					return si.hist.Layout(win, gtx)

				default:
					panic("impossible")
				}
			})
		}),
	)

	for si.buttons.copyAsCSV.Clicked() {
		if stats, ok := si.statistics.Result(); ok {
			win.AppWindow.WriteClipboard(statisticsToCSV(&stats.stats))
		}
	}

	for _, ev := range si.description.Events() {
		handleLinkClick(win, si.mwin, ev)
	}

	for _, ev := range si.events.Clicked() {
		handleLinkClick(win, si.mwin, ev)
	}
	for _, ev := range si.spansList.Clicked() {
		handleLinkClick(win, si.mwin, ev)
	}

	for si.buttons.scrollAndPanToSpans.Clicked() {
		if si.cfg.Navigations.ScrollFn != nil {
			si.mwin.OpenLink(si.cfg.Navigations.ScrollFn())
		} else {
			si.mwin.OpenLink(&SpansLink{
				Timeline: si.cfg.Container.Timeline,
				Track:    si.cfg.Container.Track,
				Spans:    si.spans,
				Kind:     SpanLinkKindScrollAndPan,
			})
		}
	}
	for si.buttons.zoomToSpans.Clicked() {
		if si.cfg.Navigations.ZoomFn != nil {
			si.mwin.OpenLink(si.cfg.Navigations.ZoomFn())
		} else {
			si.mwin.OpenLink(&SpansLink{
				Timeline: si.cfg.Container.Timeline,
				Track:    si.cfg.Container.Track,
				Spans:    si.spans,
				Kind:     SpanLinkKindZoom,
			})
		}
	}
	for si.PanelButtons.Backed() {
		si.mwin.PrevPanel()
	}
	for si.buttons.selectUserRegion.Clicked() {
		needle := si.trace.Strings[si.trace.Event(si.spans.At(0).Event).Args[2]]
		var out MergedSpans
		for _, tl := range si.timelines {
			for _, track := range tl.tracks {
				if track.kind != TrackKindUserRegions {
					continue
				}
				filtered := FilterSpans(track.spans, func(span ptrace.Span) bool {
					label := si.trace.Strings[si.trace.Event(span.Event).Args[2]]
					return label == needle
				})

				out.Bases = append(out.Bases, filtered)
			}
		}

		cfg := SpansInfoConfig{
			Title:         fmt.Sprintf("All %q user regions", needle),
			Label:         fmt.Sprintf("All %q user regions", needle),
			ShowHistogram: true,
		}
		si.mwin.OpenPanel(NewSpansInfo(cfg, si.trace, si.mwin, out, si.timelines, nil))
	}

	if si.hist.Changed() {
		si.computeHistogram(win, &si.hist.Config)
	}

	return dims
}

type SpansLink struct {
	aLink

	Timeline *Timeline
	Track    *Track
	Spans    ptrace.Spans
	Kind     SpanLinkKind
}

type SpanLinkKind uint8

const (
	SpanLinkKindScrollAndPan SpanLinkKind = iota
	SpanLinkKindZoom
)

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
	Spans ptrace.Spans
	list  widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (spans *SpanList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.SpanList.Layout").End()

	spans.list.Axis = layout.Vertical
	spans.timestampObjects.Reset()

	var txtCnt int
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

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
			txt.Link(formatTimestamp(span.Start), spans.timestampObjects.Allocate(span.Start))
			txt.Alignment = text.End
		case 1: // Duration
			value, unit := durationNumberFormatSITable.format(span.Duration())
			txt.Span(value)
			txt.Span(" ")
			s := txt.Span(unit)
			s.Font.Variant = "Mono"
			txt.Alignment = text.End
		case 2: // State
			label := stateNamesCapitalized[span.State]
			txt.Span(label)
		}

		dims := txt.Layout(win, gtx)
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

type SpansSubset struct {
	Base   ptrace.Spans
	Subset []int
}

var _ ptrace.Spans = SpansSubset{}

// At implements ptrace.Spans
func (spans SpansSubset) At(idx int) ptrace.Span {
	return spans.Base.At(spans.Subset[idx])
}

// AtPtr implements ptrace.Spans
func (spans SpansSubset) AtPtr(idx int) *ptrace.Span {
	return spans.Base.AtPtr(spans.Subset[idx])
}

// End implements ptrace.Spans
func (spans SpansSubset) End() trace.Timestamp {
	return ptrace.End(spans)
}

// Events implements ptrace.Spans
func (spans SpansSubset) Events(all []ptrace.EventID, tr *ptrace.Trace) []ptrace.EventID {
	return ptrace.Events(spans, all, tr)
}

// Len implements ptrace.Spans
func (spans SpansSubset) Len() int {
	return len(spans.Subset)
}

// Slice implements ptrace.Spans
func (spans SpansSubset) Slice(start int, end int) ptrace.Spans {
	return SpansSubset{
		Base:   spans.Base,
		Subset: spans.Subset[start:end],
	}
}

// Start implements ptrace.Spans
func (spans SpansSubset) Start() trace.Timestamp {
	return ptrace.Start(spans)
}

func FilterSpans(spans ptrace.Spans, fn func(span ptrace.Span) bool) ptrace.Spans {
	var subset []int
	for i := 0; i < spans.Len(); i++ {
		if fn(spans.At(i)) {
			subset = append(subset, i)
		}
	}
	if len(subset) == spans.Len() {
		return spans
	}

	return SpansSubset{
		Base:   spans,
		Subset: subset,
	}
}

type MergedSpans struct {
	Bases []ptrace.Spans
}

func (spans MergedSpans) index(idx int) (int, int) {
	for i, s := range spans.Bases {
		if s.Len() > idx {
			return i, idx
		} else {
			idx -= s.Len()
		}
	}
	if idx == 0 {
		return len(spans.Bases) - 1, spans.Bases[len(spans.Bases)-1].Len()
	}
	panic(fmt.Sprintf("index %d is out of bounds", idx))
}

// At implements ptrace.Spans
func (spans MergedSpans) At(idx int) ptrace.Span {
	a, b := spans.index(idx)
	return spans.Bases[a].At(b)
}

// AtPtr implements ptrace.Spans
func (spans MergedSpans) AtPtr(idx int) *ptrace.Span {
	a, b := spans.index(idx)
	return spans.Bases[a].AtPtr(b)
}

// End implements ptrace.Spans
func (spans MergedSpans) End() trace.Timestamp {
	return ptrace.End(spans)
}

// Events implements ptrace.Spans
func (spans MergedSpans) Events(all []ptrace.EventID, tr *ptrace.Trace) []ptrace.EventID {
	return ptrace.Events(spans, all, tr)
}

// Len implements ptrace.Spans
func (spans MergedSpans) Len() int {
	var n int
	for _, s := range spans.Bases {
		n += s.Len()
	}
	return n
}

// Slice implements ptrace.Spans
func (spans MergedSpans) Slice(start int, end int) ptrace.Spans {
	startBase, startIdx := spans.index(start)
	endBase, endIdx := spans.index(end)

	if startBase == endBase {
		return spans.Bases[startBase].Slice(startIdx, endIdx)
	}

	var newBases []ptrace.Spans
	newBases = append(newBases, spans.Bases[startBase].Slice(startIdx, spans.Bases[startBase].Len()))
	newBases = append(newBases, spans.Bases[startBase+1:endBase]...)
	newBases = append(newBases, spans.Bases[endBase].Slice(0, endIdx))

	return MergedSpans{
		Bases: newBases,
	}
}

// Start implements ptrace.Spans
func (spans MergedSpans) Start() trace.Timestamp {
	return ptrace.Start(spans)
}

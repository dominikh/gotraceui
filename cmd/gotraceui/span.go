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
	mwin       *MainWindow
	allEvents  []ptrace.EventID
	spans      ptrace.Spans
	label      string
	trace      *Trace
	stacktrace string

	navigations SpansInfoConfigNavigations
	title       string

	events    EventList
	spansList SpanList
	stats     *theme.Future[*SpansStats]

	container SpanContainer

	buttons struct {
		scrollAndPanToSpans widget.PrimaryClickable
		zoomToSpans         widget.PrimaryClickable
		copyAsCSV           widget.PrimaryClickable
	}

	tabbedState     theme.TabbedState
	stackSelectable widget.Selectable

	description Description

	stacktraceList widget.List
	statsList      widget.List

	theme.PanelButtons
}

type SpansInfoConfig struct {
	Title       string
	Label       string
	Container   SpanContainer
	Description *Description
	Stacktrace  string
	Statistics  *theme.Future[*SpansStats]
	Navigations SpansInfoConfigNavigations
}

type SpansInfoConfigNavigations struct {
	ScrollLabel string
	ScrollFn    func() Link

	ZoomLabel string
	ZoomFn    func() Link
}

func NewSpansInfo(cfg SpansInfoConfig, mwin *MainWindow, spans ptrace.Spans, allEvents []ptrace.EventID) *SpansInfo {
	si := &SpansInfo{
		mwin:        mwin,
		spans:       spans,
		trace:       mwin.trace,
		allEvents:   allEvents,
		title:       cfg.Title,
		label:       cfg.Label,
		container:   cfg.Container,
		stacktrace:  cfg.Stacktrace,
		stats:       cfg.Statistics,
		navigations: cfg.Navigations,
	}

	if si.title == "" {
		firstSpan := si.spans.At(0)
		lastSpan := LastSpan(si.spans)
		si.title = local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, si.container.Timeline.shortName)
	}

	if si.stacktrace == "" && (si.container != SpanContainer{}) && spans.Len() == 1 {
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

		si.stacktrace = stacktrace
	}

	if si.stats == nil {
		si.stats = theme.NewFuture(mwin.twin, func(cancelled <-chan struct{}) *SpansStats {
			return NewSpansStats(si.spans)
		})
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

	if cfg.Description != nil {
		si.description = *cfg.Description
	} else {
		si.buildDefaultDescription()
	}

	return si
}

func (si *SpansInfo) Title() string {
	return si.title
}

func (si *SpansInfo) buildDefaultDescription() {
	value := func(s *TextSpan) *theme.Future[TextSpan] {
		return theme.Immediate(*s)
	}

	// OPT(dh): there's no need to store the spans in a slice, so TextBuilder isn't what we want
	tb := TextBuilder{Theme: si.mwin.twin.Theme}
	var attrs []DescriptionAttribute
	if si.label != "" {
		attrs = append(attrs, DescriptionAttribute{Key: "Label", Value: value(tb.Span(si.label))})
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

	attrs = append(attrs, DescriptionAttribute{
		Key:   "Duration",
		Value: value(tb.Span(SpansDuration(si.spans).String())),
	})

	attrs = append(attrs, DescriptionAttribute{
		Key: "State",
		Value: theme.NewFuture(si.mwin.twin, func(cancelled <-chan struct{}) TextSpan {
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

	attrs = append(attrs, DescriptionAttribute{
		Key:   "In",
		Value: value(tb.Link(si.container.Timeline.shortName, si.container.Timeline.item)),
	})

	si.description.Attributes = attrs
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
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

			buttonsLeft := []button{
				{&si.buttons.scrollAndPanToSpans.Clickable, "Scroll and pan to spans"},
				{&si.buttons.zoomToSpans.Clickable, "Zoom to spans"},
			}
			if si.navigations.ScrollLabel != "" {
				buttonsLeft[0].label = si.navigations.ScrollLabel
			}
			if si.navigations.ZoomLabel != "" {
				buttonsLeft[1].label = si.navigations.ZoomLabel
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

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			tabs := []string{"Statistics", "Spans"}
			if len(si.events.Events) != 0 {
				tabs = append(tabs, "Events")
			}
			if si.stacktrace != "" {
				tabs = append(tabs, "Stack trace")
			}
			return theme.Tabbed(&si.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				switch tabs[si.tabbedState.Current] {
				case "Stack trace":
					return theme.List(win.Theme, &si.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
						if index != 0 {
							panic("impossible")
						}
						if si.stackSelectable.Text() == "" {
							si.stackSelectable.SetText(si.stacktrace)
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
								if stats, ok := si.stats.Result(); ok {
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

				default:
					panic("impossible")
				}
			})
		}),
	)

	for si.buttons.copyAsCSV.Clicked() {
		if stats, ok := si.stats.Result(); ok {
			si.mwin.win.WriteClipboard(statisticsToCSV(&stats.stats))
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
		if si.navigations.ScrollFn != nil {
			si.mwin.OpenLink(si.navigations.ScrollFn())
		} else {
			si.mwin.OpenLink(&SpansLink{
				Timeline: si.container.Timeline,
				Track:    si.container.Track,
				Spans:    si.spans,
				Kind:     SpanLinkKindScrollAndPan,
			})
		}
	}
	for si.buttons.zoomToSpans.Clicked() {
		if si.navigations.ZoomFn != nil {
			si.mwin.OpenLink(si.navigations.ZoomFn())
		} else {
			si.mwin.OpenLink(&SpansLink{
				Timeline: si.container.Timeline,
				Track:    si.container.Track,
				Spans:    si.spans,
				Kind:     SpanLinkKindZoom,
			})
		}
	}
	for si.PanelButtons.Backed() {
		si.mwin.prevPanel()
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

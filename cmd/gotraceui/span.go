package main

import (
	"context"
	"image"
	rtrace "runtime/trace"
	"strings"
	"time"

	"gioui.org/op"
	"gioui.org/text"
	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

func LastSpan(sel SpanSelector) ptrace.Span {
	return sel.At(sel.Size() - 1)
}

func SpansDuration(sel SpanSelector) time.Duration {
	return time.Duration(LastSpan(sel).End - sel.At(0).Start)
}

type SpanContainer struct {
	Timeline *Timeline
	Track    *Track
}

type SpansInfo struct {
	MainWindow *MainWindow
	Trace      *Trace
	AllEvents  []ptrace.EventID
	Spans      SpanSelector
	Label      string

	EventList EventList
	SpanList  SpanList
	stats     *Future[*SpansStats]

	Container SpanContainer

	initialized bool

	buttons struct {
		scrollAndPanToSpan widget.PrimaryClickable
		zoomToSpan         widget.PrimaryClickable
		copyAsCSV          widget.PrimaryClickable
	}

	tabbedState theme.TabbedState

	description Text

	statsList widget.List

	theme.PanelButtons
}

func (si *SpansInfo) Title() string {
	firstSpan := si.Spans.At(0)
	lastSpan := si.Spans.At(si.Spans.Size() - 1)
	return local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, si.Container.Timeline.shortName)
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	if !si.initialized {
		si.initialized = true

		si.stats = NewFuture(win.AppWindow, func() *SpansStats {
			return NewSpansStats(si.Spans.Spans())
		})

		events := si.Spans.Spans().Events(si.AllEvents, si.Trace.Trace)
		si.EventList = EventList{
			Trace:  si.Trace,
			Events: events,
		}

		si.SpanList = SpanList{
			Spans: si.Spans,
		}

		si.EventList.Filter.ShowGoCreate.Value = true
		si.EventList.Filter.ShowGoUnblock.Value = true
		si.EventList.Filter.ShowGoSysCall.Value = true
		si.EventList.Filter.ShowUserLog.Value = true
		si.EventList.UpdateFilter()

		si.description.Reset(win.Theme)

		if len(si.Label) > 0 {
			si.description.Bold("Label: ")
			si.description.Span(si.Label)
			si.description.Span("\n")
		}

		firstSpan := si.Spans.At(0)
		lastSpan := LastSpan(si.Spans)
		si.description.Bold("Start: ")
		si.description.Link(
			formatTimestamp(firstSpan.Start),
			firstSpan.Start,
		)
		si.description.Span("\n")

		si.description.Bold("End: ")
		si.description.Link(
			formatTimestamp(lastSpan.End),
			lastSpan.End,
		)
		si.description.Span("\n")

		si.description.Bold("Duration: ")
		si.description.Span(SpansDuration(si.Spans).String())
		si.description.Span("\n")

		si.description.Bold("State: ")
		if si.Spans.Size() == 1 {
			si.description.Span(stateNames[firstSpan.State])
		} else {
			si.description.Span("mixed")
		}
		si.description.Span("\n")

		if si.Spans.Size() == 1 && firstSpan.Tags != 0 {
			si.description.Bold("Tags: ")
			tags := spanTagStrings(firstSpan.Tags)
			si.description.Span(strings.Join(tags, ", "))
			si.description.Span("\n")
		}

		si.description.Bold("In: ")
		si.description.Link(
			si.Container.Timeline.shortName,
			si.Container.Timeline.item,
		)
	}

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

	dims := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			type button struct {
				w     *widget.Clickable
				label string
			}
			buttonsLeft := []button{
				{&si.buttons.scrollAndPanToSpan.Clickable, "Scroll and pan to spans"},
				{&si.buttons.zoomToSpan.Clickable, "Zoom to spans"},
			}

			var children []layout.FlexChild
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
		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			tabs := []string{"Statistics", "Spans", "Events"}
			return theme.Tabbed(&si.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				switch tabs[si.tabbedState.Current] {
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
									return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, "Computing statistics…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
								}
							})
						}),

						layout.Rigid(layout.Spacer{Height: 1}.Layout),

						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							if _, ok := si.stats.Result(); ok {
								gtx := gtx
								gtx.Constraints.Min.X = 0
								return theme.Button(win.Theme, &si.buttons.copyAsCSV.Clickable, "Copy as CSV").Layout(win, gtx)
							} else {
								return layout.Dimensions{Size: gtx.Constraints.Min}
							}
						}),
					)
				case "Spans":
					return si.SpanList.Layout(win, gtx)
				case "Events":
					return si.EventList.Layout(win, gtx)
				default:
					panic("unreachable")
				}
			})
		}),
	)

	for si.buttons.copyAsCSV.Clicked() {
		if stats, ok := si.stats.Result(); ok {
			si.MainWindow.win.WriteClipboard(statisticsToCSV(&stats.stats))
		}
	}
	for _, ev := range si.description.Events() {
		handleLinkClick(win, si.MainWindow, ev)
	}

	for _, ev := range si.EventList.Clicked() {
		handleLinkClick(win, si.MainWindow, ev)
	}

	for _, ev := range si.SpanList.Clicked() {
		handleLinkClick(win, si.MainWindow, ev)
	}

	for si.buttons.scrollAndPanToSpan.Clicked() {
		si.MainWindow.OpenLink(&SpansLink{
			Timeline: si.Container.Timeline,
			Track:    si.Container.Track,
			Spans:    si.Spans,
			Kind:     SpanLinkKindScrollAndPan,
		})
	}

	for si.buttons.zoomToSpan.Clicked() {
		si.MainWindow.OpenLink(&SpansLink{
			Timeline: si.Container.Timeline,
			Track:    si.Container.Track,
			Spans:    si.Spans,
			Kind:     SpanLinkKindZoom,
		})
	}

	for si.PanelButtons.Backed() {
		si.MainWindow.prevPanel()
	}

	return dims
}

type SpansLink struct {
	aLink

	Timeline *Timeline
	Track    *Track
	Spans    SpanSelector
	Kind     SpanLinkKind
}

type aLink struct{}

func (aLink) isLink() {}

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
	Spans SpanSelector
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
			txt.Span(unit)
			txt.styles[len(txt.styles)-1].Font.Variant = "Mono"
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
	return tbl.Layout(win, gtx, spans.Spans.Size(), cellFn)
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

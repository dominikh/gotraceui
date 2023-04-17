package main

import (
	"context"
	"fmt"
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
	if sel.Size() == 0 {
		return 0
	}
	return time.Duration(LastSpan(sel).End - sel.At(0).Start)
}

type SpanContainer struct {
	Timeline *Timeline
	Track    *Track
}

type SpansInfo struct {
	MainWindow *MainWindow
	AllEvents  []ptrace.EventID
	Goroutine  *ptrace.Goroutine
	Spans      SpanSelector
	Label      string
	Trace      *Trace

	events EventList
	spans  SpanList
	stats  *Future[*SpansStats]

	Container SpanContainer

	initialized bool

	buttons struct {
		scrollAndPanToSpans widget.PrimaryClickable
		zoomToSpans         widget.PrimaryClickable
		copyAsCSV           widget.PrimaryClickable
	}

	tabbedState     theme.TabbedState
	stackSelectable widget.Selectable

	description Text

	stacktraceList widget.List
	statsList      widget.List

	theme.PanelButtons
}

func (gi *SpansInfo) Title() string {
	g := gi.Goroutine
	if g == nil {
		firstSpan := gi.Spans.At(0)
		lastSpan := LastSpan(gi.Spans)
		return local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, gi.Container.Timeline.shortName)
	} else {
		if g.Function.Fn != "" {
			return local.Sprintf("goroutine %d: %s", g.ID, g.Function)
		} else {
			return local.Sprintf("goroutine %d", g.ID)
		}
	}
}

func (gi *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	if !gi.initialized {
		gi.initialized = true

		gi.stats = NewFuture(win.AppWindow, func() *SpansStats {
			if gi.Goroutine != nil {
				// We special-case goroutines because large ones cache computed statistics
				return NewGoroutineStats(gi.Goroutine)
			} else {
				return NewSpansStats(gi.Spans.Spans())
			}
		})

		gi.spans = SpanList{
			Spans: gi.Spans,
		}

		gi.events = EventList{Trace: gi.Trace}
		gi.events.Filter.ShowGoCreate.Value = true
		gi.events.Filter.ShowGoUnblock.Value = true
		gi.events.Filter.ShowGoSysCall.Value = true
		gi.events.Filter.ShowUserLog.Value = true
		gi.events.UpdateFilter()

		gi.events.Events = gi.Spans.Spans().Events(gi.AllEvents, gi.Trace.Trace)
		gi.events.UpdateFilter()

		start := gi.Spans.At(0).Start
		end := LastSpan(gi.Spans).End
		d := time.Duration(end - start)
		observedStart := gi.Spans.At(0).State == ptrace.StateCreated
		observedEnd := LastSpan(gi.Spans).State == ptrace.StateDone

		gi.description.Reset(win.Theme)

		if gi.Goroutine == nil {
			if len(gi.Label) > 0 {
				gi.description.Bold("Label: ")
				gi.description.Span(gi.Label)
				gi.description.Span("\n")
			}

			firstSpan := gi.Spans.At(0)
			lastSpan := LastSpan(gi.Spans)
			gi.description.Bold("Start: ")
			gi.description.Link(
				formatTimestamp(firstSpan.Start),
				firstSpan.Start,
			)
			gi.description.Span("\n")

			gi.description.Bold("End: ")
			gi.description.Link(
				formatTimestamp(lastSpan.End),
				lastSpan.End,
			)
			gi.description.Span("\n")

			gi.description.Bold("Duration: ")
			gi.description.Span(SpansDuration(gi.Spans).String())
			gi.description.Span("\n")

			gi.description.Bold("State: ")
			if gi.Spans.Size() == 1 {
				gi.description.Span(stateNames[firstSpan.State])
			} else {
				gi.description.Span("mixed")
			}
			gi.description.Span("\n")

			if gi.Spans.Size() == 1 && firstSpan.Tags != 0 {
				gi.description.Bold("Tags: ")
				tags := spanTagStrings(firstSpan.Tags)
				gi.description.Span(strings.Join(tags, ", "))
				gi.description.Span("\n")
			}

			gi.description.Bold("In: ")
			gi.description.Link(
				gi.Container.Timeline.shortName,
				gi.Container.Timeline.item,
			)
		} else {
			gi.description.Bold("Goroutine: ")
			gi.description.Span(local.Sprintf("%d\n", gi.Goroutine.ID))

			gi.description.Bold("Function: ")
			gi.description.Span(fmt.Sprintf("%s\n", gi.Goroutine.Function.Fn))

			if observedStart {
				gi.description.Bold("Created at: ")
				gi.description.Link(
					fmt.Sprintf("%s\n", formatTimestamp(start)),
					start,
				)
			} else {
				gi.description.Bold("Created at: ")
				gi.description.Link(
					"before trace start\n",
					start,
				)
			}

			if observedEnd {
				gi.description.Bold("Returned at: ")
				gi.description.Link(
					fmt.Sprintf("%s\n", formatTimestamp(end)),
					end,
				)
			} else {
				gi.description.Bold("Returned at: ")
				gi.description.Link(
					"after trace end\n",
					end,
				)
			}

			if observedStart && observedEnd {
				gi.description.Bold("Lifetime: ")
				gi.description.Span(d.String())
			} else {
				gi.description.Bold("Observed duration: ")
				gi.description.Span(d.String())
			}
		}
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

	dims := layout.Flex{Axis: layout.Vertical, WeightSum: 1}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			type button struct {
				w     *widget.Clickable
				label string
			}

			var buttonsLeft []button
			if gi.Goroutine == nil {
				buttonsLeft = []button{
					{&gi.buttons.scrollAndPanToSpans.Clickable, "Scroll and pan to spans"},
					{&gi.buttons.zoomToSpans.Clickable, "Zoom to spans"},
				}
			} else {
				buttonsLeft = []button{
					{&gi.buttons.scrollAndPanToSpans.Clickable, "Scroll to goroutine"},
					{&gi.buttons.zoomToSpans.Clickable, "Zoom to goroutine"},
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
			children = append(children, layout.Rigid(theme.Dumb(win, gi.PanelButtons.Layout)))

			// Right-aligned buttons should be aligned with the right side of the visible panel, not the width of the
			// panel contents, nor the infinite width of a possible surrounding list.
			gtx.Constraints.Max.X = gtx.Constraints.Min.X
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx, children...)
		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			return gi.description.Layout(win, gtx)
		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			tabs := []string{"Statistics", "Spans", "Events", "Stack trace"}
			return theme.Tabbed(&gi.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				switch tabs[gi.tabbedState.Current] {
				case "Stack trace":
					if gi.Goroutine != nil && gi.Spans.At(0).State != ptrace.StateCreated {
						// The goroutine existed before the start of the trace and we do not have the stack trace of where it
						// was created.

						// XXX display some string instead
						return layout.Dimensions{}
					}
					if gi.Goroutine == nil && gi.Spans.Size() != 1 {
						// Multiple spans, no stack trace we can show

						// XXX display some string instead
						return layout.Dimensions{}
					}

					return theme.List(win.Theme, &gi.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
						if index != 0 {
							panic("impossible")
						}
						if gi.stackSelectable.Text() == "" {
							ev := gi.Trace.Events[gi.Spans.At(0).Event]
							stk := gi.Trace.Stacks[ev.StkID]
							sb := strings.Builder{}
							for _, f := range stk {
								frame := gi.Trace.PCs[f]
								fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Fn, frame.File, frame.Line)
							}
							s := sb.String()
							if len(s) > 0 && s[len(s)-1] == '\n' {
								s = s[:len(s)-1]
							}

							gi.stackSelectable.SetText(s)
						}

						return gi.stackSelectable.Layout(gtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground), widget.ColorTextMaterial(gtx, win.Theme.Palette.PrimarySelection))
					})

				case "Statistics":
					return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							return theme.List(win.Theme, &gi.statsList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
								if index != 0 {
									panic("impossible")
								}
								if stats, ok := gi.stats.Result(); ok {
									return stats.Layout(win, gtx)
								} else {
									return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, "Computing statistics…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
								}
							})
						}),

						layout.Rigid(layout.Spacer{Height: 1}.Layout),

						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							gtx.Constraints.Min.X = 0
							return theme.Button(win.Theme, &gi.buttons.copyAsCSV.Clickable, "Copy as CSV").Layout(win, gtx)
						}),
					)

				case "Spans":
					return gi.spans.Layout(win, gtx)

				case "Events":
					return gi.events.Layout(win, gtx)

				default:
					panic("impossible")
				}
			})
		}),
	)

	for gi.buttons.copyAsCSV.Clicked() {
		if stats, ok := gi.stats.Result(); ok {
			gi.MainWindow.win.WriteClipboard(statisticsToCSV(&stats.stats))
		}
	}

	for _, ev := range gi.description.Events() {
		handleLinkClick(win, gi.MainWindow, ev)
	}

	for _, ev := range gi.events.Clicked() {
		handleLinkClick(win, gi.MainWindow, ev)
	}
	for _, ev := range gi.spans.Clicked() {
		handleLinkClick(win, gi.MainWindow, ev)
	}

	for gi.buttons.scrollAndPanToSpans.Clicked() {
		// TODO(dh): see if we really need both branches
		if gi.Goroutine == nil {
			gi.MainWindow.OpenLink(&SpansLink{
				Timeline: gi.Container.Timeline,
				Track:    gi.Container.Track,
				Spans:    gi.Spans,
				Kind:     SpanLinkKindScrollAndPan,
			})
		} else {
			gi.MainWindow.OpenLink(&GoroutineLink{
				Goroutine: gi.Goroutine,
				Kind:      GoroutineLinkKindScroll},
			)
		}
	}
	for gi.buttons.zoomToSpans.Clicked() {
		// TODO(dh): see if we really need both branches
		if gi.Goroutine == nil {
			gi.MainWindow.OpenLink(&SpansLink{
				Timeline: gi.Container.Timeline,
				Track:    gi.Container.Track,
				Spans:    gi.Spans,
				Kind:     SpanLinkKindZoom,
			})
		} else {
			gi.MainWindow.OpenLink(&GoroutineLink{
				Goroutine: gi.Goroutine,
				Kind:      GoroutineLinkKindZoom,
			})
		}
	}
	for gi.PanelButtons.Backed() {
		gi.MainWindow.prevPanel()
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

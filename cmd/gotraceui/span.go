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
	stats  *theme.Future[*SpansStats]

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

func (si *SpansInfo) Title() string {
	g := si.Goroutine
	if g == nil {
		firstSpan := si.Spans.At(0)
		lastSpan := LastSpan(si.Spans)
		return local.Sprintf("%d ns–%d ns @ %s\n", firstSpan.Start, lastSpan.End, si.Container.Timeline.shortName)
	} else {
		if g.Function.Fn != "" {
			return local.Sprintf("goroutine %d: %s", g.ID, g.Function)
		} else {
			return local.Sprintf("goroutine %d", g.ID)
		}
	}
}

func (si *SpansInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	if !si.initialized {
		si.initialized = true

		si.stats = theme.NewFuture(win, func(cancelled <-chan struct{}) *SpansStats {
			if si.Goroutine != nil {
				// We special-case goroutines because large ones cache computed statistics
				return NewGoroutineStats(si.Goroutine)
			} else {
				return NewSpansStats(si.Spans.Spans())
			}
		})

		si.spans = SpanList{
			Spans: si.Spans,
		}

		si.events = EventList{Trace: si.Trace}
		si.events.Filter.ShowGoCreate.Value = true
		si.events.Filter.ShowGoUnblock.Value = true
		si.events.Filter.ShowGoSysCall.Value = true
		si.events.Filter.ShowUserLog.Value = true
		si.events.UpdateFilter()

		si.events.Events = si.Spans.Spans().Events(si.AllEvents, si.Trace.Trace)
		si.events.UpdateFilter()

		start := si.Spans.At(0).Start
		end := LastSpan(si.Spans).End
		d := time.Duration(end - start)
		observedStart := si.Spans.At(0).State == ptrace.StateCreated
		observedEnd := LastSpan(si.Spans).State == ptrace.StateDone

		si.description.Reset(win.Theme)

		if si.Goroutine == nil {
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
		} else {
			si.description.Bold("Goroutine: ")
			si.description.Span(local.Sprintf("%d\n", si.Goroutine.ID))

			si.description.Bold("Function: ")
			si.description.Span(fmt.Sprintf("%s\n", si.Goroutine.Function.Fn))

			if observedStart {
				si.description.Bold("Created at: ")
				si.description.Link(
					fmt.Sprintf("%s\n", formatTimestamp(start)),
					start,
				)
			} else {
				si.description.Bold("Created at: ")
				si.description.Link(
					"before trace start\n",
					start,
				)
			}

			if observedEnd {
				si.description.Bold("Returned at: ")
				si.description.Link(
					fmt.Sprintf("%s\n", formatTimestamp(end)),
					end,
				)
			} else {
				si.description.Bold("Returned at: ")
				si.description.Link(
					"after trace end\n",
					end,
				)
			}

			if observedStart && observedEnd {
				si.description.Bold("Lifetime: ")
				si.description.Span(d.String())
			} else {
				si.description.Bold("Observed duration: ")
				si.description.Span(d.String())
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
			if si.Goroutine == nil {
				buttonsLeft = []button{
					{&si.buttons.scrollAndPanToSpans.Clickable, "Scroll and pan to spans"},
					{&si.buttons.zoomToSpans.Clickable, "Zoom to spans"},
				}
			} else {
				buttonsLeft = []button{
					{&si.buttons.scrollAndPanToSpans.Clickable, "Scroll to goroutine"},
					{&si.buttons.zoomToSpans.Clickable, "Zoom to goroutine"},
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

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			tabs := []string{"Statistics", "Spans", "Events", "Stack trace"}
			return theme.Tabbed(&si.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				switch tabs[si.tabbedState.Current] {
				case "Stack trace":
					if si.Goroutine != nil && si.Spans.At(0).State != ptrace.StateCreated {
						// The goroutine existed before the start of the trace and we do not have the stack trace of where it
						// was created.

						// XXX display some string instead
						return layout.Dimensions{}
					}
					if si.Goroutine == nil && si.Spans.Size() != 1 {
						// Multiple spans, no stack trace we can show

						// XXX display some string instead
						return layout.Dimensions{}
					}

					return theme.List(win.Theme, &si.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
						if index != 0 {
							panic("impossible")
						}
						if si.stackSelectable.Text() == "" {
							ev := si.Trace.Events[si.Spans.At(0).Event]
							stk := si.Trace.Stacks[ev.StkID]
							sb := strings.Builder{}
							for _, f := range stk {
								frame := si.Trace.PCs[f]
								fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Fn, frame.File, frame.Line)
							}
							s := sb.String()
							if len(s) > 0 && s[len(s)-1] == '\n' {
								s = s[:len(s)-1]
							}

							si.stackSelectable.SetText(s)
						}

						return si.stackSelectable.Layout(gtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground), widget.ColorTextMaterial(gtx, win.Theme.Palette.PrimarySelection))
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
									return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, "Computing statistics…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
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
					return si.spans.Layout(win, gtx)

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
			si.MainWindow.win.WriteClipboard(statisticsToCSV(&stats.stats))
		}
	}

	for _, ev := range si.description.Events() {
		handleLinkClick(win, si.MainWindow, ev)
	}

	for _, ev := range si.events.Clicked() {
		handleLinkClick(win, si.MainWindow, ev)
	}
	for _, ev := range si.spans.Clicked() {
		handleLinkClick(win, si.MainWindow, ev)
	}

	for si.buttons.scrollAndPanToSpans.Clicked() {
		// TODO(dh): see if we really need both branches
		if si.Goroutine == nil {
			si.MainWindow.OpenLink(&SpansLink{
				Timeline: si.Container.Timeline,
				Track:    si.Container.Track,
				Spans:    si.Spans,
				Kind:     SpanLinkKindScrollAndPan,
			})
		} else {
			si.MainWindow.OpenLink(&GoroutineLink{
				Goroutine: si.Goroutine,
				Kind:      GoroutineLinkKindScroll},
			)
		}
	}
	for si.buttons.zoomToSpans.Clicked() {
		// TODO(dh): see if we really need both branches
		if si.Goroutine == nil {
			si.MainWindow.OpenLink(&SpansLink{
				Timeline: si.Container.Timeline,
				Track:    si.Container.Track,
				Spans:    si.Spans,
				Kind:     SpanLinkKindZoom,
			})
		} else {
			si.MainWindow.OpenLink(&GoroutineLink{
				Goroutine: si.Goroutine,
				Kind:      GoroutineLinkKindZoom,
			})
		}
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

package main

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"runtime"
	rtrace "runtime/trace"
	"strings"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/text"
)

type FunctionLink struct {
	aLink

	Fn *ptrace.Function
}

type FunctionInfo struct {
	fn            *ptrace.Function
	mwin          *MainWindow
	title         string
	description   Text
	tabbedState   theme.TabbedState
	goroutineList GoroutineList

	histState theme.HistogramState
	hist      *theme.Future[*widget.Histogram]
	histClick widget.Clickable

	theme.PanelButtons
}

func NewFunctionInfo(mwin *MainWindow, fn *ptrace.Function) *FunctionInfo {
	fi := &FunctionInfo{
		fn:   fn,
		mwin: mwin,
		goroutineList: GoroutineList{
			Goroutines: fn.Goroutines,
		},
	}

	// Build description
	{
		goroot := runtime.GOROOT()

		fi.description.Reset(mwin.twin.Theme)

		fi.description.Bold("Function: ")
		fi.description.Span(fn.Fn)
		fi.description.Span("\n")

		fi.description.Bold("Location: ")
		// TODO(dh): make file link clickable
		l := fn.File
		if strings.HasPrefix(l, goroot) {
			// TODO(dh): support windows path
			// TODO(dh): support configurable GOROOT for traces from other systems
			// TODO(dh): we can auto-detect GOROOT from the trace because we know which files are in the standard library
			// TODO(dh): support GOPATH
			l = "$GOROOT" + strings.TrimPrefix(l, goroot)
		}
		fi.description.Span(fmt.Sprintf("%s:%d", l, fn.Line))
		fi.description.Span("\n")

		fi.description.Bold("# of goroutines: ")
		fi.description.Span(local.Sprintf("%d", len(fn.Goroutines)))
		fi.description.Span("\n")

		var total time.Duration
		for _, g := range fn.Goroutines {
			d := time.Duration(g.Spans[len(g.Spans)-1].End - g.Spans[0].Start)
			total += d
		}

		fi.description.Bold("Total time: ")
		fi.description.Span(total.String())
	}

	// Build histogram
	fi.computeHistogram(mwin.twin, 0, 0)

	return fi
}

func (fi *FunctionInfo) Title() string {
	return fi.title
}

func (fi *FunctionInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	// Inset of 5 pixels on all sides. We can't use layout.Inset because it doesn't decrease the minimum constraint,
	// which we do care about here.
	gtx.Constraints.Min = gtx.Constraints.Min.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints.Max = gtx.Constraints.Max.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints = layout.Normalize(gtx.Constraints)
	defer op.Offset(image.Pt(5, 5)).Push(gtx.Ops).Pop()

	nothing := func(gtx layout.Context) layout.Dimensions {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	tabs := []string{"Goroutines", "Histogram"}

	dims := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
				layout.Flexed(1, nothing),
				layout.Rigid(theme.Dumb(win, fi.PanelButtons.Layout)),
			)
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: 10}.Layout(gtx) }),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			return fi.description.Layout(win, gtx)
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: 10}.Layout(gtx) }),
		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			return theme.Tabbed(&fi.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				switch tabs[fi.tabbedState.Current] {
				case "Goroutines":
					return fi.goroutineList.Layout(win, gtx)
				case "Histogram":
					hist, ok := fi.hist.Result()
					if ok {
						thist := theme.Histogram(win.Theme, &fi.histState)
						thist.XLabel = "Duration"
						thist.YLabel = "Count"
						return fi.histClick.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							return thist.Layout(win, gtx, hist)
						})
					} else {
						return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, "Computing histogram…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
					}
				default:
					panic("unreachable")
				}
			})
		}),
	)

	if fi.histState.Activated != (struct {
		Start widget.FloatDuration
		End   widget.FloatDuration
	}{}) {
		fi.computeHistogram(win, fi.histState.Activated.Start, fi.histState.Activated.End)
	}

	for _, ev := range fi.goroutineList.Clicked() {
		handleLinkClick(win, fi.mwin, ev)
	}

	for _, ev := range fi.description.Events() {
		handleLinkClick(win, fi.mwin, ev)
	}

	for fi.PanelButtons.Backed() {
		fi.mwin.prevPanel()
	}

	for {
		click, ok := fi.histClick.Clicked()
		if !ok {
			break
		}
		if click.Button != pointer.ButtonSecondary {
			continue
		}

		menu := []*theme.MenuItem{
			{
				// TODO disable when there is nothing to zoom out to
				Label: PlainLabel("Zoom out"),
				Do: func(gtx layout.Context) {
					fi.computeHistogram(win, 0, 0)
				},
			},
		}
		win.SetContextMenu(menu)
	}

	return dims
}

func (fi *FunctionInfo) computeHistogram(win *theme.Window, start, end widget.FloatDuration) {
	var goroutineDurations []time.Duration

	for _, g := range fi.fn.Goroutines {
		d := time.Duration(g.Spans[len(g.Spans)-1].End - g.Spans[0].Start)
		goroutineDurations = append(goroutineDurations, d)
	}

	var cfg widget.HistogramConfig
	cfg.Start = start
	cfg.End = end
	cfg.RejectOutliers = true
	fi.hist = theme.NewFuture(win, func(cancelled <-chan struct{}) *widget.Histogram {
		return widget.NewHistogram(&cfg, goroutineDurations)
	})
}

type GoroutineList struct {
	Goroutines []*ptrace.Goroutine
	list       widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (gs *GoroutineList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineList.Layout").End()

	gs.list.Axis = layout.Vertical
	gs.timestampObjects.Reset()

	var txtCnt int
	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		var txt *Text
		if txtCnt < gs.texts.Len() {
			txt = gs.texts.Ptr(txtCnt)
		} else {
			txt = gs.texts.Allocate(Text{})
		}
		txtCnt++
		txt.Reset(win.Theme)

		g := gs.Goroutines[row]
		switch col {
		case 0: // ID
			txt.Link(local.Sprintf("%d", g.ID), g)
			txt.Alignment = text.End
		case 1: // Time
			start := g.Spans[0].Start
			txt.Link(formatTimestamp(start), gs.timestampObjects.Allocate(start))
			txt.Alignment = text.End
		case 2: // Duration
			start := g.Spans[0].Start
			end := g.Spans[len(g.Spans)-1].End
			d := time.Duration(end - start)
			value, unit := durationNumberFormatSITable.format(d)
			txt.Span(value)
			txt.Span(" ")
			txt.Span(unit)
			txt.styles[len(txt.styles)-1].Font.Variant = "Mono"
			txt.Alignment = text.End
		}

		dims := txt.Layout(win, gtx)
		dims.Size = gtx.Constraints.Constrain(dims.Size)
		return dims
	}

	var goroutineListColumns = []theme.TableListColumn{
		{
			Name: "Goroutine",
			// XXX the width depends on the font and scaling
			MinWidth: 120,
			MaxWidth: 120,
		},

		{
			Name: "Start time",
			// XXX the width depends on the font and scaling
			MinWidth: 200,
			MaxWidth: 200,
		},

		{
			Name: "Duration",
			// XXX the width depends on the font and scaling
			MinWidth: 200,
			MaxWidth: 200,
		},
	}

	// Find space needed for largest goroutine ID
	n := len(gs.Goroutines)
	s := n - 32
	if s < 0 {
		s = 0
	}
	var maxID uint64
	// Look at the last 32 goroutines for this function. This has a high likelyhood of telling us the greatest ID.
	for _, g := range gs.Goroutines[s:n] {
		if g.ID > maxID {
			maxID = g.ID
		}
	}
	r0 := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, 12, goroutineListColumns[0].Name, widget.ColorTextMaterial(gtx, color.NRGBA{}))
	})
	r1 := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, local.Sprintf("%d", maxID), widget.ColorTextMaterial(gtx, color.NRGBA{}))
	})
	w := r0.Dimensions.Size.X
	if x := r1.Dimensions.Size.X; x > w {
		w = x
	}
	goroutineListColumns[0].MinWidth = w + 20
	goroutineListColumns[0].MaxWidth = goroutineListColumns[0].MinWidth

	tbl := theme.TableListStyle{
		Columns:       goroutineListColumns,
		List:          &gs.list,
		ColumnPadding: gtx.Dp(10),
	}

	gtx.Constraints.Min = gtx.Constraints.Max
	return tbl.Layout(win, gtx, len(gs.Goroutines), cellFn)
}

// Clicked returns all objects of text spans that have been clicked since the last call to Layout.
func (gs *GoroutineList) Clicked() []TextEvent {
	// This only allocates when links have been clicked, which is a very low frequency event.
	var out []TextEvent
	for i := 0; i < gs.texts.Len(); i++ {
		txt := gs.texts.Ptr(i)
		out = append(out, txt.Events()...)
	}
	return out
}

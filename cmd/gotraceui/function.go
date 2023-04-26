package main

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"path/filepath"
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

type FunctionLink struct {
	aLink

	Fn *ptrace.Function
}

type FunctionInfo struct {
	mwin          MainWindowIface
	fn            *ptrace.Function
	trace         *Trace
	title         string
	description   Description
	tabbedState   theme.TabbedState
	goroutineList GoroutineList

	filterGoroutines widget.Bool
	histGoroutines   []*ptrace.Goroutine
	hist             InteractiveHistogram

	initOnce sync.Once

	theme.PanelButtons
}

func NewFunctionInfo(tr *Trace, mwin MainWindowIface, fn *ptrace.Function) *FunctionInfo {
	fi := &FunctionInfo{
		fn:             fn,
		mwin:           mwin,
		histGoroutines: fn.Goroutines,
		trace:          tr,
	}

	return fi
}

func (fi *FunctionInfo) init(win *theme.Window) {
	// Build description
	{
		value := func(s *TextSpan) *theme.Future[TextSpan] {
			return theme.Immediate(*s)
		}
		tb := TextBuilder{Theme: win.Theme}
		var attrs []DescriptionAttribute

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Function",
			Value: value(tb.Span(fi.fn.Fn)),
		})

		// TODO(dh): make file link clickable
		displayPath := fi.fn.File
		if goroot := fi.trace.GOROOT; goroot != "" && strings.HasPrefix(fi.fn.File, goroot) {
			displayPath = filepath.Join("$GOROOT", strings.TrimPrefix(fi.fn.File, goroot))
		} else if gopath := fi.trace.GOPATH; gopath != "" && strings.HasPrefix(fi.fn.File, gopath) {
			displayPath = filepath.Join("$GOPATH", strings.TrimPrefix(fi.fn.File, gopath))
		} else if goroot == "" && gopath == "" {
			// We couldn't detect goroot, which makes it very likely that the executable had paths trimmed. Detect if
			// the trimmed path is in GOROOT or GOPATH based on if the first path element has a dot in it or not. Module
			// paths without dots are reserved for the standard library. This has a small but negligible chance of false
			// positives.

			left, _, ok := strings.Cut(fi.fn.File, "/")
			if ok {
				if strings.Contains(left, ".") {
					if strings.Contains(fi.fn.File, "@v") {
						displayPath = filepath.Join("$GOPATH", "pkg", "mod", fi.fn.File)
					} else {
						displayPath = filepath.Join("$GOPATH", "src", fi.fn.File)
					}
				} else {
					displayPath = filepath.Join("$GOROOT", "src", fi.fn.File)
				}
			}
		}
		attrs = append(attrs, DescriptionAttribute{
			Key:   "Location",
			Value: value(tb.Span(fmt.Sprintf("%s:%d", displayPath, fi.fn.Line))),
		})

		attrs = append(attrs, DescriptionAttribute{
			Key:   "# of goroutines",
			Value: value(tb.Span(local.Sprintf("%d", len(fi.fn.Goroutines)))),
		})

		var total time.Duration
		for _, g := range fi.fn.Goroutines {
			d := time.Duration(g.Spans.At((g.Spans.Len())-1).End - g.Spans.At(0).Start)
			total += d
		}

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Total time",
			Value: value(tb.Span(total.String())),
		})

		fi.description.Attributes = attrs
	}

	// Build histogram
	cfg := &widget.HistogramConfig{RejectOutliers: true, Bins: widget.DefaultHistogramBins}
	fi.computeHistogram(win, cfg)
}

func (fi *FunctionInfo) Title() string {
	return fi.title
}

func (fi *FunctionInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.FunctionInfo.Layout").End()

	fi.initOnce.Do(func() { fi.init(win) })

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
					return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							return theme.CheckBox(win.Theme, &fi.filterGoroutines, "Filter list to range of durations selected in histogram").Layout(win, gtx)
						}),

						layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
							var gs []*ptrace.Goroutine
							if fi.filterGoroutines.Value {
								gs = fi.histGoroutines
							} else {
								gs = fi.fn.Goroutines
							}
							return fi.goroutineList.Layout(win, gtx, gs)
						}),
					)
				case "Histogram":
					return fi.hist.Layout(win, gtx)
				default:
					panic("unreachable")
				}
			})
		}),
	)

	for _, ev := range fi.goroutineList.Clicked() {
		handleLinkClick(win, fi.mwin, ev)
	}

	for _, ev := range fi.description.Events() {
		handleLinkClick(win, fi.mwin, ev)
	}

	for fi.PanelButtons.Backed() {
		fi.mwin.PrevPanel()
	}

	if fi.hist.Changed() {
		fi.histGoroutines = fi.computeHistogram(win, &fi.hist.Config)
	}

	return dims
}

func (fi *FunctionInfo) computeHistogram(win *theme.Window, cfg *widget.HistogramConfig) []*ptrace.Goroutine {
	var goroutineDurations []time.Duration

	var gs []*ptrace.Goroutine
	for _, g := range fi.fn.Goroutines {
		d := time.Duration(g.Spans.At((g.Spans.Len())-1).End - g.Spans.At(0).Start)
		if fd := widget.FloatDuration(d); fd >= cfg.Start && (cfg.End == 0 || fd <= cfg.End) {
			goroutineDurations = append(goroutineDurations, d)
			gs = append(gs, g)
		}
	}

	fi.hist.Set(win, goroutineDurations)

	return gs
}

type GoroutineList struct {
	list widget.List

	timestampObjects allocator[trace.Timestamp]
	texts            allocator[Text]
}

func (gs *GoroutineList) Layout(win *theme.Window, gtx layout.Context, goroutines []*ptrace.Goroutine) layout.Dimensions {
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

		g := goroutines[row]
		switch col {
		case 0: // ID
			txt.Link(local.Sprintf("%d", g.ID), g)
			txt.Alignment = text.End
		case 1: // Time
			start := g.Spans.At(0).Start
			txt.Link(formatTimestamp(start), gs.timestampObjects.Allocate(start))
			txt.Alignment = text.End
		case 2: // Duration
			start := g.Spans.At(0).Start
			end := g.Spans.At((g.Spans.Len()) - 1).End
			d := time.Duration(end - start)
			value, unit := durationNumberFormatSITable.format(d)
			txt.Span(value)
			txt.Span(" ")
			s := txt.Span(unit)
			s.Font.Variant = "Mono"
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
	n := len(goroutines)
	s := n - 32
	if s < 0 {
		s = 0
	}
	var maxID uint64
	// Look at the last 32 goroutines for this function. This has a high likelyhood of telling us the greatest ID.
	for _, g := range goroutines[s:n] {
		if g.ID > maxID {
			maxID = g.ID
		}
	}
	r0 := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, goroutineListColumns[0].Name, widget.ColorTextMaterial(gtx, color.NRGBA{}))
	})
	r1 := Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, local.Sprintf("%d", maxID), widget.ColorTextMaterial(gtx, color.NRGBA{}))
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
	return tbl.Layout(win, gtx, len(goroutines), cellFn)
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

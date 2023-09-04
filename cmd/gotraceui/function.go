package main

import (
	"context"
	"fmt"
	"image"
	"path/filepath"
	rtrace "runtime/trace"
	"strings"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/op"
)

type FunctionInfo struct {
	mwin          *theme.Window
	fn            *ptrace.Function
	trace         *Trace
	title         string
	tabbedState   theme.TabbedState
	goroutineList GoroutineList

	filterGoroutines widget.Bool
	// OPT(dh): avoid the pointer by using the goroutine's SeqID instead
	histGoroutines []*ptrace.Goroutine
	hist           InteractiveHistogram

	descriptionText Text
	hoveredLink     ObjectLink

	initialized bool

	theme.PanelButtons
}

func NewFunctionInfo(tr *Trace, mwin *theme.Window, fn *ptrace.Function) *FunctionInfo {
	fi := &FunctionInfo{
		fn:             fn,
		mwin:           mwin,
		histGoroutines: fn.Goroutines,
		trace:          tr,
	}

	return fi
}

func (fi *FunctionInfo) HoveredLink() ObjectLink {
	return fi.hoveredLink
}

func (fi *FunctionInfo) buildDescription(win *theme.Window, gtx layout.Context) Description {
	tb := TextBuilder{Theme: win.Theme}
	var attrs []DescriptionAttribute

	attrs = append(attrs, DescriptionAttribute{
		Key:   "Function",
		Value: *(tb.Span(fi.fn.Fn)),
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
		Value: *(tb.Span(fmt.Sprintf("%s:%d", displayPath, fi.fn.Line))),
	})

	attrs = append(attrs, DescriptionAttribute{
		Key:   "# of goroutines",
		Value: *(tb.Span(local.Sprintf("%d", len(fi.fn.Goroutines)))),
	})

	var total time.Duration
	for _, g := range fi.fn.Goroutines {
		d := time.Duration(g.Spans[len(g.Spans)-1].End - g.Spans[0].Start)
		total += d
	}

	attrs = append(attrs, DescriptionAttribute{
		Key:   "Total time",
		Value: *(tb.Span(total.String())),
	})

	desc := Description{Attributes: attrs}
	return desc
}

func (fi *FunctionInfo) init(win *theme.Window) {
	// Build histogram
	cfg := &widget.HistogramConfig{RejectOutliers: true, Bins: widget.DefaultHistogramBins}
	fi.computeHistogram(win, cfg)
	fi.goroutineList.HiddenColumns.Function = true
}

func (fi *FunctionInfo) Title() string {
	return fi.title
}

func (fi *FunctionInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.FunctionInfo.Layout").End()

	if !fi.initialized {
		fi.init(win)
		fi.initialized = true
	}

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

	dims := layout.Rigids(gtx, layout.Vertical,
		func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
				layout.Flexed(1, nothing),
				layout.Rigid(theme.Dumb(win, fi.PanelButtons.Layout)),
			)
		},

		func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: 10}.Layout(gtx) },
		func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			fi.descriptionText.Reset(win.Theme)
			return fi.buildDescription(win, gtx).Layout(win, gtx, &fi.descriptionText)
		},

		func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: 10}.Layout(gtx) },
		func(gtx layout.Context) layout.Dimensions {
			return theme.Tabbed(&fi.tabbedState, tabs).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min = gtx.Constraints.Max
				switch tabs[fi.tabbedState.Current] {
				case "Goroutines":
					return layout.Rigids(gtx, layout.Vertical,
						func(gtx layout.Context) layout.Dimensions {
							return theme.CheckBox(win.Theme, &fi.filterGoroutines, "Filter list to range of durations selected in histogram").Layout(win, gtx)
						},

						layout.Spacer{Height: 5}.Layout,

						func(gtx layout.Context) layout.Dimensions {
							var gs []*ptrace.Goroutine
							if fi.filterGoroutines.Value {
								gs = fi.histGoroutines
							} else {
								gs = fi.fn.Goroutines
							}
							if fi.goroutineList.Goroutines.Len() == 0 || len(gs) == 0 || &gs[0] != &fi.goroutineList.Goroutines.Items[0] {
								fi.goroutineList.SetGoroutines(win, gtx, gs)
							}
							return fi.goroutineList.Layout(win, gtx)
						},
					)
				case "Histogram":
					return fi.hist.Layout(win, gtx)
				default:
					panic("unreachable")
				}
			})
		},
	)

	for _, ev := range fi.goroutineList.Clicked() {
		handleLinkClick(win, ev.Event, ev.Span.ObjectLink)
	}
	for _, ev := range fi.descriptionText.Events() {
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
	fi.hoveredLink = firstNonNil(
		fi.goroutineList.HoveredLink(),
		fi.descriptionText.HoveredLink(),
	)

	for fi.PanelButtons.Backed() {
		fi.mwin.EmitAction(&PrevPanelAction{})
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
		d := time.Duration(g.Spans[len(g.Spans)-1].End - g.Spans[0].Start)
		if fd := widget.FloatDuration(d); fd >= cfg.Start && (cfg.End == 0 || fd <= cfg.End) {
			goroutineDurations = append(goroutineDurations, d)
			gs = append(gs, g)
		}
	}

	fi.hist.Set(win, goroutineDurations)

	return gs
}

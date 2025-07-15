package main

import (
	"fmt"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/text"
	exptrace "golang.org/x/exp/trace"
)

func scale[T float32 | float64](oldStart, oldEnd, newStart, newEnd, v T) T {
	slope := (newEnd - newStart) / (oldEnd - oldStart)
	output := newStart + slope*(v-oldStart)
	return output
}

func last[E any, S ~[]E](s S) E {
	return s[len(s)-1]
}

func lastPtr[E any, S ~[]E](s S) *E {
	return &s[len(s)-1]
}

type CellFormatter struct {
	Clicks   mem.BucketSlice[Link]
	nfTs     *NumberFormatter[AdjustedTime]
	nfUint64 *NumberFormatter[uint64]
	nfInt    *NumberFormatter[int]
}

func (cf *CellFormatter) Reset() {
	cf.Clicks.Reset()
	if cf.nfTs == nil {
		cf.nfTs = NewNumberFormatter[AdjustedTime](local)
		cf.nfUint64 = NewNumberFormatter[uint64](local)
		cf.nfInt = NewNumberFormatter[int](local)
	}
}

func (cf *CellFormatter) Update(win *theme.Window, gtx layout.Context) {
	handleLinkClicks(win, gtx, &cf.Clicks)
	cf.Reset()
}

func (cl *CellFormatter) HoveredLink() ObjectLink {
	for i := range cl.Clicks.Len() {
		c := cl.Clicks.Ptr(i)
		if c.Click.Hovered() {
			return c.Link
		}
	}
	return nil
}

func (cf *CellFormatter) Timestamp(win *theme.Window, gtx layout.Context, tr *Trace, ts exptrace.Time, label string) layout.Dimensions {
	return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
		link := cf.Clicks.Grow()
		link.Link = &TimestampObjectLink{Timestamp: ts}
		return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			if label == "" {
				label = formatTimestamp(cf.nfTs, tr.AdjustedTime(ts))
			}
			return widget.Label{
				MaxLines:  1,
				Alignment: text.Start,
			}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.NavigationLink))
		})
	})
}

func (cf *CellFormatter) Goroutine(win *theme.Window, gtx layout.Context, g *ptrace.Goroutine, label string) layout.Dimensions {
	return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
		link := cf.Clicks.Grow()
		link.Link = &GoroutineObjectLink{Goroutine: g}
		return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			if label == "" {
				label = cf.nfUint64.Format("%d", uint64(g.ID))
			}
			return widget.Label{
				MaxLines:  1,
				Alignment: text.Start,
			}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.OpenLink))
		})
	})
}

func (cf *CellFormatter) Task(win *theme.Window, gtx layout.Context, t *ptrace.Task, label string) layout.Dimensions {
	return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
		link := cf.Clicks.Grow()
		link.Link = &TaskObjectLink{Task: t}
		return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			if label == "" {
				label = cf.nfUint64.Format("%d", uint64(t.ID))
			}
			return widget.Label{
				MaxLines:  1,
				Alignment: text.Start,
			}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.OpenLink))
		})
	})
}

func (cf *CellFormatter) Duration(win *theme.Window, gtx layout.Context, d time.Duration, approx bool) layout.Dimensions {
	return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
		value, unit := durationNumberFormatSITable.format(d)
		// XXX the unit should be set in monospace
		if approx {
			value = "≥ " + value
		}
		return widget.Label{
			MaxLines:  1,
			Alignment: text.Start,
		}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, fmt.Sprintf("%s %s", value, unit), win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
	})
}

func (cf *CellFormatter) Function(win *theme.Window, gtx layout.Context, fn *ptrace.Function) layout.Dimensions {
	if fn == nil {
		return layout.Dimensions{
			Size: gtx.Constraints.Min,
		}
	}

	link := cf.Clicks.Grow()
	link.Link = &FunctionObjectLink{Function: fn}
	return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		label := fn.Func
		return widget.Label{
			MaxLines:  1,
			Alignment: text.Start,
		}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.OpenLink))
	})
}

func (cf *CellFormatter) Number(win *theme.Window, gtx layout.Context, num int) layout.Dimensions {
	label := cf.nfInt.Format("%d", num)
	return widget.Label{
		MaxLines:  1,
		Alignment: text.End,
	}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
}

func (cf *CellFormatter) EventID(win *theme.Window, gtx layout.Context, num ptrace.EventID) layout.Dimensions {
	label := cf.nfInt.Format("%d", int(num))
	return widget.Label{
		MaxLines:  1,
		Alignment: text.End,
	}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
}

func (cf *CellFormatter) Text(win *theme.Window, gtx layout.Context, l string) layout.Dimensions {
	return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, l, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
}

func (cf *CellFormatter) Spans(win *theme.Window, gtx layout.Context, spans Items[ptrace.Span]) layout.Dimensions {
	link := cf.Clicks.Grow()
	link.Link = &SpansObjectLink{Spans: spans}
	return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		label := "<Span>"
		return widget.Label{
			MaxLines:  1,
			Alignment: text.Start,
		}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, label, win.ColorMaterial(gtx, win.Theme.Palette.OpenLink))
	})
}

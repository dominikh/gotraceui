package main

import (
	"bytes"
	"context"
	"encoding/csv"
	"fmt"
	"image"
	"image/color"
	rtrace "runtime/trace"
	"sort"
	"time"

	"honnef.co/go/gotraceui/clip"
	ourfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/text"
	"gioui.org/x/styledtext"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
)

const numStatLabels = 7

var statLabels = [...][numStatLabels * 3]string{
	durationNumberFormatScientific: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
		"State▼", "Count▼", "Total (s)▼", "Min (s)▼", "Max (s)▼", "Avg (s)▼", "p50 (s)▼",
		"State▲", "Count▲", "Total (s)▲", "Min (s)▲", "Max (s)▲", "Avg (s)▲", "p50 (s)▲",
	},
	durationNumberFormatExact: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
		"State▼", "Count▼", "Total (s)▼", "Min (s)▼", "Max (s)▼", "Avg (s)▼", "p50 (s)▼",
		"State▲", "Count▲", "Total (s)▲", "Min (s)▲", "Max (s)▲", "Avg (s)▲", "p50 (s)▲",
	},
	durationNumberFormatSI: [...]string{
		"State", "Count", "Total", "Min", "Max", "Avg", "p50",
		"State▼", "Count▼", "Total▼", "Min▼", "Max▼", "Avg▼", "p50▼",
		"State▲", "Count▲", "Total▲", "Min▲", "Max▲", "Avg▲", "p50▲",
	},
	durationNumberFormatSITable: [...]string{
		"State", "Count", "Total", "Min", "Max", "Avg", "p50",
		"State▼", "Count▼", "Total▼", "Min▼", "Max▼", "Avg▼", "p50▼",
		"State▲", "Count▲", "Total▲", "Min▲", "Max▲", "Avg▲", "p50▲",
	},
}

type SpansStats struct {
	stats ptrace.Statistics
	// mapping maps from indices of displayed statistics to indices in the stats field
	mapping []int

	sortCol        int
	sortDescending bool

	numberFormat durationNumberFormat

	columnClicks [7]widget.PrimaryClickable
}

func statisticsToCSV(stats *ptrace.Statistics) string {
	var buf bytes.Buffer
	w := csv.NewWriter(&buf)

	w.Write([]string{"State", "Count", "Min", "Max", "Total", "Average", "Median"})

	for state := range stats {
		if state == int(ptrace.StateNone) {
			continue
		}

		if stateNamesCapitalized[state] == "" {
			continue
		}
		stat := &stats[state]
		fields := []string{
			stateNamesCapitalized[state],
			fmt.Sprintf("%d", stat.Count),
			fmt.Sprintf("%d", stat.Min),
			fmt.Sprintf("%d", stat.Max),
			fmt.Sprintf("%d", stat.Total),
			fmt.Sprintf("%f", stat.Average),
			fmt.Sprintf("%f", stat.Median),
		}
		w.Write(fields)
	}

	w.Flush()
	return buf.String()
}

func NewStats(stats ptrace.Statistics) *SpansStats {
	gst := &SpansStats{stats: stats}

	gst.mapping = make([]int, 0, len(gst.stats))

	for i := range gst.stats {
		s := &gst.stats[i]

		if s.Count == 0 {
			continue
		}

		gst.mapping = append(gst.mapping, i)
	}

	gst.sort()

	return gst
}

func NewSpansStats(spans ptrace.Spans) *SpansStats {
	return NewStats(ptrace.ComputeStatistics(spans))
}

func NewGoroutineStats(g *ptrace.Goroutine) *SpansStats {
	return NewStats(g.Statistics())
}

func (gs *SpansStats) computeSizes(gtx layout.Context, th *theme.Theme) [numStatLabels]image.Point {
	// Column 1 and 2 (state and count) are sized individually, all other columns (min, max, ...) have the same width.
	// The last columns' labels are all roughly the same size and only differ by a few pixels, which would look
	// inconsistent. The values in the last columns all have the same width.
	//
	// We assume that all lines have the same height. This is an assumption shared by outlay.Grid.

	fLabel := ourfont.Collection()[0].Font
	fLabel.Weight = font.Bold
	fContent := ourfont.Collection()[0].Font
	fValue := ourfont.Collection()[0].Font
	fUnit := ourfont.Collection()[0].Font
	fUnit.Variant = "Mono"

	var columnSizes [numStatLabels]image.Point

	shape := func(s string, f font.Font) image.Point {
		m := op.Record(gtx.Ops)
		dims := widget.Label{MaxLines: 1}.Layout(gtx, th.Shaper, f, th.TextSize, s, widget.ColorTextMaterial(gtx, color.NRGBA{}))
		m.Stop()

		spanWidth := dims.Size.X
		spanHeight := dims.Size.Y

		return image.Point{spanWidth, spanHeight}
	}

	// Column 1 contains strings, so the width is that of the widest shaped string
	size := shape(statLabels[gs.numberFormat][0+numStatLabels], fLabel)
	for _, name := range stateNamesCapitalized {
		size2 := shape(name, fContent)
		if size2.X > size.X {
			size.X = size2.X
		}
	}
	columnSizes[0] = size

	// Column 2 contains numbers, so the width is either that of the column label or the widest number. Digits all have
	// the same width, so we only need to shape the largest number.
	size = shape(statLabels[gs.numberFormat][1+numStatLabels], fLabel)
	max := 0
	for _, stat := range gs.stats {
		if stat.Count > max {
			max = stat.Count
		}
	}
	size2 := shape(local.Sprintf("%d", max), fContent)
	if size2.X > size.X {
		size.X = size2.X
	}
	columnSizes[1] = size

	switch gs.numberFormat {
	case durationNumberFormatScientific:
		// The remaining columns contain numbers in scientific notation with fixed precision, so the width is either that of
		// the column label or that of "1.23E+99". We give all remaining columns the same size.
		size = shape("1.23E+99", fContent)
		for i := 2; i < numStatLabels; i++ {
			size2 := shape(statLabels[gs.numberFormat][i+numStatLabels], fLabel)
			if size2.X > size.X {
				size.X = size2.X
			}
		}
		for i := 2; i < numStatLabels; i++ {
			columnSizes[i] = size
		}

	default:
		// Format and shape each value to find the widest one. Unlike scientific notation, each column is sized
		// individually, in case one of them is much wider than the others.
		//
		// OPT(dh): we have to format and shape again in the Layout function. However, the number of rows are so few it
		// probably doesn't matter.

		for i := 2; i < numStatLabels; i++ {
			size = shape(statLabels[gs.numberFormat][i+numStatLabels], fLabel)
			for _, stat := range gs.stats {
				var v time.Duration
				switch i {
				case 2:
					v = stat.Total
				case 3:
					v = stat.Min
				case 4:
					v = stat.Max
				case 5:
					v = time.Duration(stat.Average)
				case 6:
					v = time.Duration(stat.Median)
				default:
					panic("unreachable")
				}
				value, unit := gs.numberFormat.format(v)
				s1 := shape(value, fValue)
				s2 := shape(" ", fValue)
				s3 := shape(unit, fUnit)
				if size2 := s1.X + s2.X + s3.X; size2 > size.X {
					size.X = size2
				}
			}
			columnSizes[i] = size
		}
	}

	return columnSizes
}

func sortStats[T constraints.Ordered](stats *ptrace.Statistics, mapping []int, descending bool, get func(*ptrace.Statistic) T) {
	if descending {
		slices.SortFunc(mapping, func(i, j int) bool {
			return get(&stats[i]) >= get(&stats[j])
		})
	} else {
		slices.SortFunc(mapping, func(i, j int) bool {
			return get(&stats[i]) < get(&stats[j])
		})
	}
}

func (gs *SpansStats) sort() {
	switch gs.sortCol {
	case 0:
		// OPT(dh): don't use sort.Slice, it allocates
		if gs.sortDescending {
			sort.Slice(gs.mapping, func(i, j int) bool {
				return stateNamesCapitalized[gs.mapping[i]] >= stateNamesCapitalized[gs.mapping[j]]
			})
		} else {
			sort.Slice(gs.mapping, func(i, j int) bool {
				return stateNamesCapitalized[gs.mapping[i]] < stateNamesCapitalized[gs.mapping[j]]
			})
		}
	case 1:
		// Count
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) int { return gs.Count })
	case 2:
		// Total
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Total })
	case 3:
		// Min
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Min })
	case 4:
		// Max
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Max })
	case 5:
		// Avg
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) float64 { return gs.Average })
	case 6:
		// p50
		sortStats(&gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) float64 { return gs.Median })
	default:
		panic("unreachable")
	}
}

func (gs *SpansStats) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineStats.Layout").End()

	grid := layout.SmallGrid{
		RowPadding:    0,
		ColumnPadding: gtx.Dp(10),
	}

	// Compute the widest column label so that all columns have the same size (unless they need to be wider due to their
	// contents.)
	var labelSizes [3]image.Point
	for i := numStatLabels; i < 2*numStatLabels; i++ {
		f := ourfont.Collection()[0].Font
		f.Weight = font.Bold

		l := statLabels[gs.numberFormat][i]
		m := op.Record(gtx.Ops)
		gtx := gtx
		gtx.Constraints.Min.X = 0
		dims := widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, l, widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground))
		m.Stop()
		spanWidth := dims.Size.X
		spanHeight := dims.Size.Y

		j := i - numStatLabels
		if j > 2 {
			j = 2
		}
		if spanWidth > labelSizes[j].X {
			labelSizes[j].X = spanWidth
		}
		if spanHeight > labelSizes[j].Y {
			labelSizes[j].Y = spanHeight
		}
	}

	sizes := gs.computeSizes(gtx, win.Theme)
	sizer := func(gtx layout.Context, row, col int) layout.Dimensions {
		return layout.Dimensions{Size: sizes[col]}
	}

	cellFn := func(gtx layout.Context, row, col int) layout.Dimensions {
		if row == 0 {
			var l string
			if col == gs.sortCol {
				if gs.sortDescending {
					l = statLabels[gs.numberFormat][col+numStatLabels]
				} else {
					l = statLabels[gs.numberFormat][col+numStatLabels*2]
				}
			} else {
				l = statLabels[gs.numberFormat][col]
			}

			s := spanWith(win.Theme, l, func(ss styledtext.SpanStyle) styledtext.SpanStyle {
				ss.Font.Weight = font.Bold
				return ss
			})
			gs.columnClicks[col].Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return styledtext.Text(win.Theme.Shaper, s).Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
					pointer.CursorPointer.Add(gtx.Ops)
				})
			})
		} else {
			row--
			n := gs.mapping[row]

			var value, unit string
			switch col {
			case 0:
				// type
				value = stateNamesCapitalized[n]
			case 1:
				value = local.Sprintf("%d", gs.stats[n].Count)
				if gs.stats[n].Count == 0 {
					panic(row)
				}
			case 2:
				// total
				value, unit = gs.numberFormat.format(gs.stats[n].Total)
			case 3:
				// min
				value, unit = gs.numberFormat.format(gs.stats[n].Min)
			case 4:
				// max
				value, unit = gs.numberFormat.format(gs.stats[n].Max)
			case 5:
				// avg
				value, unit = gs.numberFormat.format(time.Duration(gs.stats[n].Average))
			case 6:
				// p50
				value, unit = gs.numberFormat.format(time.Duration(gs.stats[n].Median))
			default:
				panic("unreachable")
			}

			// TODO(dh): explicitly select tabular figures from the font. It's not crucial because most fonts default to
			// it, anyway.
			txt := styledtext.Text(win.Theme.Shaper, span(win.Theme, value), span(win.Theme, " "), span(win.Theme, unit))
			txt.Styles[2].Font.Variant = "Mono"
			if col != 0 {
				txt.Alignment = text.End
			}
			txt.Layout(gtx, nil)
		}

		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	for col := range gs.columnClicks {
		for gs.columnClicks[col].Clicked() {
			if col == gs.sortCol {
				gs.sortDescending = !gs.sortDescending
			} else {
				gs.sortCol = col
				gs.sortDescending = false
			}
			gs.sort()
		}
	}

	return grid.Layout(gtx, len(gs.mapping)+1, 7, sizer, cellFn)
}

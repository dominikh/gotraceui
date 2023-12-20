package main

import (
	"bytes"
	"context"
	"encoding/csv"
	"fmt"
	"image"
	"image/color"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/op"
	"gioui.org/text"
	"gioui.org/x/styledtext"
)

const numStatLabels = 7

var statLabels = [...][numStatLabels]string{
	durationNumberFormatScientific: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
	},
	durationNumberFormatExact: [...]string{
		"State", "Count", "Total (s)", "Min (s)", "Max (s)", "Avg (s)", "p50 (s)",
	},
	durationNumberFormatSI: [...]string{
		"State", "Count", "Total", "Min", "Max", "Avg", "p50",
	},
	durationNumberFormatSITable: [...]string{
		"State", "Count", "Total", "Min", "Max", "Avg", "p50",
	},
}

type SpansStats struct {
	stats        SortedIndices[ptrace.Statistic, []ptrace.Statistic]
	table        theme.Table
	scrollState  theme.YScrollableListState
	numberFormat durationNumberFormat
}

func statisticsToCSV(stats []ptrace.Statistic) string {
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
	gst := &SpansStats{
		stats: SortedIndices[ptrace.Statistic, []ptrace.Statistic]{
			Items: stats[:],
			Order: make([]int, 0, len(stats)),
		},
	}

	for i := range gst.stats.Items {
		s := &gst.stats.Items[i]

		if s.Count == 0 {
			continue
		}

		gst.stats.Order = append(gst.stats.Order, i)
	}

	gst.sort()

	return gst
}

func NewSpansStats(spans ptrace.Spans) *SpansStats {
	return NewStats(ptrace.ComputeStatistics(spans))
}

func NewGoroutineStats(g *ptrace.Goroutine) *SpansStats {
	// XXX reintroduce caching of statistics
	return NewStats(ptrace.ComputeStatistics(ptrace.ToSpans(g.Spans)))
}

func (gs *SpansStats) computeSizes(gtx layout.Context, th *theme.Theme) [numStatLabels]image.Point {
	// Column 1 and 2 (state and count) are sized individually, all other columns (min, max, ...) have the same width.
	// The last columns' labels are all roughly the same size and only differ by a few pixels, which would look
	// inconsistent. The values in the last columns all have the same width.
	//
	// We assume that all lines have the same height. This is an assumption shared by outlay.Grid.

	fLabel := font.Font{
		Weight: font.Bold,
	}
	fContent := font.Font{}
	fValue := font.Font{}
	fUnit := font.Font{
		Typeface: "Go Mono",
	}

	var columnSizes [numStatLabels]image.Point

	shape := func(s string, f font.Font) image.Point {
		m := op.Record(gtx.Ops)
		gtx.Constraints.Min = image.Point{}
		dims := widget.Label{MaxLines: 1}.Layout(gtx, th.Shaper, f, th.TextSize, s, widget.ColorTextMaterial(gtx, color.NRGBA{}))
		m.Stop()

		spanWidth := dims.Size.X
		spanHeight := dims.Size.Y

		return image.Point{spanWidth, spanHeight}
	}

	// Column 1 contains strings, so the width is that of the widest shaped string
	size := shape(statLabels[gs.numberFormat][0], fLabel)
	for _, name := range stateNamesCapitalized {
		size2 := shape(name, fContent)
		if size2.X > size.X {
			size.X = size2.X
		}
	}
	columnSizes[0] = size

	// Column 2 contains numbers, so the width is either that of the column label or the widest number. Digits all have
	// the same width, so we only need to shape the largest number.
	size = shape(statLabels[gs.numberFormat][1], fLabel)
	max := 0
	for _, stat := range gs.stats.Items {
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
			size2 := shape(statLabels[gs.numberFormat][i], fLabel)
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
			size = shape(statLabels[gs.numberFormat][i], fLabel)
			for _, stat := range gs.stats.Items {
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

	for i := range columnSizes {
		// TODO(dh): this is way too dependent on the implementation of Table
		// Allow for some padding, and the sort indicator
		columnSizes[i].X += gtx.Dp(10) + shape("▲", fLabel).X
	}

	return columnSizes
}

func (gs *SpansStats) sort() {
	switch gs.table.SortedBy {
	case 0: // Name
		gs.stats.SortIndex(func(a, b int) int {
			return cmp(stateNamesCapitalized[a], stateNamesCapitalized[b], gs.table.SortOrder == theme.SortDescending)
		})
	case 1: // Count
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Count, b.Count, gs.table.SortOrder == theme.SortDescending)
		})
	case 2: // Total
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Total, b.Total, gs.table.SortOrder == theme.SortDescending)
		})
	case 3: // Min
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Min, b.Min, gs.table.SortOrder == theme.SortDescending)
		})
	case 4: // Max
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Max, b.Max, gs.table.SortOrder == theme.SortDescending)
		})
	case 5: // Avg
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Average, b.Average, gs.table.SortOrder == theme.SortDescending)
		})
	case 6: // p50
		gs.stats.Sort(func(a, b ptrace.Statistic) int {
			return cmp(a.Median, b.Median, gs.table.SortOrder == theme.SortDescending)
		})
	default:
		panic("unreachable")
	}
}

func (gs *SpansStats) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineStats.Layout").End()

	if gs.table.Columns == nil {
		sizes := gs.computeSizes(gtx, win.Theme)

		cols := []theme.Column{
			{Name: statLabels[gs.numberFormat][0], Width: float32(sizes[0].X), MinWidth: float32(sizes[0].X), Clickable: true},
			{Name: statLabels[gs.numberFormat][1], Width: float32(sizes[1].X), MinWidth: float32(sizes[1].X), Clickable: true, Alignment: text.End},
			{Name: statLabels[gs.numberFormat][2], Width: float32(sizes[2].X), MinWidth: float32(sizes[2].X), Clickable: true, Alignment: text.End},
			{Name: statLabels[gs.numberFormat][3], Width: float32(sizes[3].X), MinWidth: float32(sizes[3].X), Clickable: true, Alignment: text.End},
			{Name: statLabels[gs.numberFormat][4], Width: float32(sizes[4].X), MinWidth: float32(sizes[4].X), Clickable: true, Alignment: text.End},
			{Name: statLabels[gs.numberFormat][5], Width: float32(sizes[5].X), MinWidth: float32(sizes[5].X), Clickable: true, Alignment: text.End},
			{Name: statLabels[gs.numberFormat][6], Width: float32(sizes[6].X), MinWidth: float32(sizes[6].X), Clickable: true, Alignment: text.End},
		}
		gs.table.SetColumns(win, gtx, cols)
		gs.table.SortOrder = theme.SortAscending
		gs.table.SortedBy = 0
	}

	if _, ok := gs.table.SortByClickedColumn(); ok {
		gs.sort()
	}

	cellFn := func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
		var value, unit string
		switch col {
		case 0:
			// type
			n := gs.stats.Order[row]
			value = stateNamesCapitalized[n]
		case 1:
			value = local.Sprintf("%d", gs.stats.At(row).Count)
		case 2:
			// total
			value, unit = gs.numberFormat.format(gs.stats.At(row).Total)
		case 3:
			// min
			value, unit = gs.numberFormat.format(gs.stats.At(row).Min)
		case 4:
			// max
			value, unit = gs.numberFormat.format(gs.stats.At(row).Max)
		case 5:
			// avg
			value, unit = gs.numberFormat.format(time.Duration(gs.stats.At(row).Average))
		case 6:
			// p50
			value, unit = gs.numberFormat.format(time.Duration(gs.stats.At(row).Median))
		default:
			panic("unreachable")
		}

		// TODO(dh): explicitly select tabular figures from the font. It's not crucial because most fonts default to
		// it, anyway.
		txt := styledtext.Text(win.Theme.Shaper, span(win, value), span(win, " "), span(win, unit))
		txt.Styles[2].Font.Typeface = "Go Mono"
		if col != 0 {
			txt.Alignment = text.End
		}
		return txt.Layout(gtx, nil)
	}

	dims := theme.SimpleTable(win, gtx, &gs.table, &gs.scrollState, gs.stats.Len(), cellFn)

	return dims
}

package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"sort"
	"strings"
	"time"

	mylayout "honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"

	"gioui.org/app"
	"gioui.org/gesture"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/x/styledtext"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
	"golang.org/x/image/math/fixed"
)

type GoroutineStats struct {
	stats *ptrace.Statistics
	// mapping maps from indices of displayed statistics to indices in the stats field
	mapping []int

	start, end trace.Timestamp

	sortCol        int
	sortDescending bool

	numberFormat durationNumberFormat

	columnClicks [7]gesture.Click
}

type GoroutineWindow struct {
	theme *theme.Theme
	mwin  *MainWindow
	trace *Trace
	g     *ptrace.Goroutine

	stats *GoroutineStats
}

func (gwin *GoroutineWindow) Run(win *app.Window) error {
	events := Events{Trace: gwin.trace}
	events.Filter.ShowGoCreate.Value = true
	events.Filter.ShowGoUnblock.Value = true
	events.Filter.ShowGoSysCall.Value = true
	events.Filter.ShowUserLog.Value = true
	for _, span := range gwin.g.Spans {
		// OPT(dh): isn't this equivalent to gwin.g.Events?

		// XXX we don't need the slice, iterate over events in spans in the Events layouter
		events.Events = append(events.Events, span.Events(gwin.g.Events, gwin.trace.Trace)...)
	}
	events.UpdateFilter()

	var ops op.Ops
	stacktraceFoldable := theme.Foldable{
		Title:  "Creation stack trace",
		Closed: widget.Bool{Value: true},
	}
	statsFoldable := theme.Foldable{
		Title: "Statistics",
	}
	eventsFoldable := theme.Foldable{
		Title: "Events",
	}

	txt := Text{
		theme: gwin.theme,
	}
	{
		txt.Bold("Goroutine: ")
		txt.Span(local.Sprintf("%d\n", gwin.g.ID))

		txt.Bold("Function: ")
		txt.Span(fmt.Sprintf("%s\n", gwin.g.Function.Fn))

		txt.Bold("Created at: ")
		txt.Link(
			fmt.Sprintf("%s\n", formatTimestamp(gwin.stats.start)),
			&TimestampLink{gwin.stats.start},
		)

		txt.Bold("Returned at: ")
		txt.Link(
			fmt.Sprintf("%s\n", formatTimestamp(gwin.stats.end)),
			&TimestampLink{gwin.stats.end},
		)

		txt.Bold("Lifetime: ")
		txt.Span(time.Duration(gwin.stats.end - gwin.stats.start).String())
	}

	var scrollToGoroutine, zoomToGoroutine widget.Clickable
	tWin := &theme.Window{Theme: gwin.theme}

	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			tWin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				for i := range txt.Spans {
					if s := &txt.Spans[i]; s.Clickable != nil {
						for s.Clickable.Clicked() {
							gwin.mwin.OpenLink(txt.Spans[i].Link)
						}
					}
				}

				for _, link := range events.ClickedLinks() {
					gwin.mwin.OpenLink(link)
				}

				for scrollToGoroutine.Clicked() {
					gwin.mwin.OpenLink(&GoroutineLink{Goroutine: gwin.g, Kind: GoroutineLinkKindScroll})
				}
				for zoomToGoroutine.Clicked() {
					gwin.mwin.OpenLink(&GoroutineLink{Goroutine: gwin.g, Kind: GoroutineLinkKindZoom})
				}

				paint.Fill(gtx.Ops, colors[colorBackground])

				return layout.UniformInset(1).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
								// TODO(dh): these buttons should reflow into multiple rows if the window is too small
								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									return theme.Button(&scrollToGoroutine, "Scroll to goroutine").Layout(win, gtx)
								}),
								layout.Rigid(layout.Spacer{Width: 5}.Layout),
								layout.Rigid(func(gtx layout.Context) layout.Dimensions {
									return theme.Button(&zoomToGoroutine, "Zoom to goroutine").Layout(win, gtx)
								}),
							)
						}),

						layout.Rigid(layout.Spacer{Height: 5}.Layout),

						layout.Rigid(theme.Dumb(win, txt.Layout)),
						// XXX ideally the spacing would be one line high
						layout.Rigid(layout.Spacer{Height: 10}.Layout),
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							// TODO(dh): stack traces can get quite long, making it even more important that this window
							// gets scrollbars
							return stacktraceFoldable.Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
								// OPT(dh): compute the string form of the backtrace once, not each frame
								// XXX don't let Gio wrap our text, add horizontal scrollbars instead
								ev := gwin.trace.Events[gwin.g.Spans[0].Event]
								stk := gwin.trace.Stacks[ev.StkID]
								sb := strings.Builder{}
								for _, f := range stk {
									frame := gwin.trace.PCs[f]
									fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Fn, frame.File, frame.Line)
								}
								s := sb.String()
								if len(s) > 0 && s[len(s)-1] == '\n' {
									s = s[:len(s)-1]
								}
								return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, s)
							})
						}),
						// XXX ideally the spacing would be one line high
						layout.Rigid(layout.Spacer{Height: 10}.Layout),
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							// TODO(dh): this needs a horizontal scrollbar for small windows
							return statsFoldable.Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
								return gwin.stats.Layout(win, gtx)
							})
						}),
						// XXX ideally the spacing would be one line high
						layout.Rigid(layout.Spacer{Height: 10}.Layout),
						layout.Rigid(func(gtx layout.Context) layout.Dimensions {
							return eventsFoldable.Layout(win, gtx, events.Layout)
						}),
					)
				})
			})

			ev.Frame(&ops)
		}
	}

	return nil
}

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
}

var stateNamesCapitalized = [ptrace.StateLast]string{
	ptrace.StateInactive:                "Inactive",
	ptrace.StateActive:                  "Active",
	ptrace.StateGCIdle:                  "GC (idle)",
	ptrace.StateGCDedicated:             "GC (dedicated)",
	ptrace.StateBlocked:                 "Blocked (other)",
	ptrace.StateBlockedSend:             "Blocked (channel send)",
	ptrace.StateBlockedRecv:             "Blocked (channel receive)",
	ptrace.StateBlockedSelect:           "Blocked (select)",
	ptrace.StateBlockedSync:             "Blocked (sync)",
	ptrace.StateBlockedSyncOnce:         "Blocked (sync.Once)",
	ptrace.StateBlockedSyncTriggeringGC: "Blocked (triggering GC)",
	ptrace.StateBlockedCond:             "Blocked (sync.Cond)",
	ptrace.StateBlockedNet:              "Blocked (pollable I/O)",
	ptrace.StateBlockedGC:               "Blocked (GC)",
	ptrace.StateBlockedSyscall:          "Blocked (syscall)",
	ptrace.StateStuck:                   "Stuck",
	ptrace.StateReady:                   "Ready",
	ptrace.StateCreated:                 "Created",
	ptrace.StateDone:                    "Done",
	ptrace.StateGCMarkAssist:            "GC (mark assist)",
	ptrace.StateGCSweep:                 "GC (sweep assist)",
}

func NewGoroutineStats(g *ptrace.Goroutine) *GoroutineStats {
	gst := &GoroutineStats{stats: &g.Statistics}

	gst.mapping = make([]int, 0, len(gst.stats))

	for i := range gst.stats {
		s := &gst.stats[i]

		if s.Count == 0 {
			continue
		}

		gst.mapping = append(gst.mapping, i)
	}

	gst.sort()

	gst.start = g.Spans.Start()
	gst.end = g.Spans.End()

	return gst
}

func (gs *GoroutineStats) computeSizes(gtx layout.Context, th *theme.Theme) [numStatLabels]image.Point {
	// Column 1 and 2 (state and count) are sized individually, all other columns (min, max, ...) have the same width.
	// The last columns' labels are all roughly the same size and only differ by a few pixels, which would look
	// inconsistent. The values in the last columns all have the same width.
	//
	// We assume that all lines have the same height. This is an assumption shared by outlay.Grid.

	fLabel := goFonts[0].Font
	fLabel.Weight = text.Bold
	fContent := goFonts[0].Font

	var columnSizes [numStatLabels]image.Point

	shape := func(s string, f text.Font) image.Point {
		lines := th.Shaper.LayoutString(fLabel, fixed.I(gtx.Sp(th.TextSize)), gtx.Constraints.Max.X, gtx.Locale, s)
		firstLine := lines[0]
		spanWidth := firstLine.Width.Ceil()
		spanHeight := (firstLine.Ascent + firstLine.Descent).Ceil()

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
				if size2 := shape(gs.numberFormat.format(v), fContent); size2.X > size.X {
					size.X = size2.X
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

func (gs *GoroutineStats) sort() {
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
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) int { return gs.Count })
	case 2:
		// Total
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Total })
	case 3:
		// Min
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Min })
	case 4:
		// Max
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) time.Duration { return gs.Max })
	case 5:
		// Avg
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) float64 { return gs.Average })
	case 6:
		// p50
		sortStats(gs.stats, gs.mapping, gs.sortDescending, func(gs *ptrace.Statistic) float64 { return gs.Median })
	default:
		panic("unreachable")
	}
}

func (gs *GoroutineStats) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineStats.Layout").End()

	for col := range gs.columnClicks {
		// We're passing gtx.Queue instead of gtx to avoid allocations because of convT. This means gtx.Queue mustn't be
		// nil.
		for _, ev := range gs.columnClicks[col].Events(gtx.Queue) {
			if ev.Type != gesture.TypeClick {
				continue
			}

			if col == gs.sortCol {
				gs.sortDescending = !gs.sortDescending
			} else {
				gs.sortCol = col
				gs.sortDescending = false
			}
			gs.sort()
		}
	}

	grid := mylayout.SmallGrid{
		RowPadding:    0,
		ColumnPadding: gtx.Dp(15),
	}

	// Compute the widest column label so that all columns have the same size (unless they need to be wider due to their
	// contents.)
	var labelSizes [3]image.Point
	for i := numStatLabels; i < 2*numStatLabels; i++ {
		f := goFonts[0].Font
		f.Weight = text.Bold

		l := statLabels[gs.numberFormat][i]
		lines := win.Theme.Shaper.LayoutString(f, fixed.I(gtx.Sp(win.Theme.TextSize)), gtx.Constraints.Max.X, gtx.Locale, l)
		firstLine := lines[0]
		spanWidth := firstLine.Width.Ceil()
		spanHeight := (firstLine.Ascent + firstLine.Descent).Ceil()

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

	// There is probably no need to cache the sizes between frames. The window only redraws when it's being interacted
	// with, which may even change the sizes.
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
				ss.Font.Weight = text.Bold
				return ss
			})
			styledtext.Text(win.Theme.Shaper, s).Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
				defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
				pointer.CursorPointer.Add(gtx.Ops)
				gs.columnClicks[col].Add(gtx.Ops)
			})
		} else {
			row--
			n := gs.mapping[row]

			var l string
			switch col {
			case 0:
				// type
				l = stateNamesCapitalized[n]
			case 1:
				l = local.Sprintf("%d", gs.stats[n].Count)
				if gs.stats[n].Count == 0 {
					panic(row)
				}
			case 2:
				// total
				l = gs.numberFormat.format(gs.stats[n].Total)
			case 3:
				// min
				l = gs.numberFormat.format(gs.stats[n].Min)
			case 4:
				// max
				l = gs.numberFormat.format(gs.stats[n].Max)
			case 5:
				// avg
				l = gs.numberFormat.format(time.Duration(gs.stats[n].Average))
			case 6:
				// p50
				l = gs.numberFormat.format(time.Duration(gs.stats[n].Median))
			default:
				panic("unreachable")
			}

			txt := styledtext.Text(win.Theme.Shaper, span(win.Theme, l))
			if col != 0 {
				txt.Alignment = text.End
			}
			txt.Layout(gtx, nil)
		}

		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	return grid.Layout(gtx, len(gs.mapping)+1, 7, sizer, cellFn)
}

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

	"gioui.org/gesture"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/text"
	"gioui.org/widget"
	"gioui.org/x/styledtext"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
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

type GoroutineInfo struct {
	MainWindow *MainWindow
	Goroutine  *ptrace.Goroutine
	Trace      *Trace

	initialized bool

	events Events
	stats  *GoroutineStats

	buttons struct {
		scrollToGoroutine widget.Clickable
		zoomToGoroutine   widget.Clickable
	}

	foldables struct {
		stacktrace theme.Foldable
		stats      theme.Foldable
		events     theme.Foldable
	}

	description Text

	stacktraceList widget.List
	statsList      widget.List

	theme.PanelButtons
}

func (gi *GoroutineInfo) Title() string {
	g := gi.Goroutine
	if g.Function.Fn != "" {
		return local.Sprintf("goroutine %d: %s", g.ID, g.Function)
	} else {
		return local.Sprintf("goroutine %d", g.ID)
	}
}

func (gi *GoroutineInfo) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	if !gi.initialized {
		gi.initialized = true

		gi.stats = NewGoroutineStats(gi.Goroutine)

		gi.events = Events{Trace: gi.Trace}
		gi.events.Filter.ShowGoCreate.Value = true
		gi.events.Filter.ShowGoUnblock.Value = true
		gi.events.Filter.ShowGoSysCall.Value = true
		gi.events.Filter.ShowUserLog.Value = true
		for _, span := range gi.Goroutine.Spans {
			// OPT(dh): isn't this equivalent to gi.GoroutineEvents?

			// XXX we don't need the slice, iterate over events in spans in the Events layouter
			gi.events.Events = append(gi.events.Events, span.Events(gi.Goroutine.Events, gi.Trace.Trace)...)
		}
		gi.events.UpdateFilter()

		gi.foldables.stacktrace = theme.Foldable{
			Title:  "Creation stack trace",
			Closed: widget.Bool{Value: true},
		}
		gi.foldables.stats = theme.Foldable{
			Title: "Statistics",
		}
		gi.foldables.events = theme.Foldable{
			Title: "Events",
		}

		gi.description = Text{
			theme: win.Theme,
		}
		gi.description.Bold("Goroutine: ")
		gi.description.Span(local.Sprintf("%d\n", gi.Goroutine.ID))

		gi.description.Bold("Function: ")
		gi.description.Span(fmt.Sprintf("%s\n", gi.Goroutine.Function.Fn))

		gi.description.Bold("Created at: ")
		gi.description.Link(
			fmt.Sprintf("%s\n", formatTimestamp(gi.stats.start)),
			&TimestampLink{gi.stats.start},
		)

		gi.description.Bold("Returned at: ")
		gi.description.Link(
			fmt.Sprintf("%s\n", formatTimestamp(gi.stats.end)),
			&TimestampLink{gi.stats.end},
		)

		gi.description.Bold("Lifetime: ")
		gi.description.Span(time.Duration(gi.stats.end - gi.stats.start).String())
	}

	// Inset of 5 pixels on all sides. We can't use layout.Inset because it doesn't decrease the minimum constraint,
	// which we do care about here.
	gtx.Constraints.Min = gtx.Constraints.Min.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints.Max = gtx.Constraints.Max.Sub(image.Pt(2*5, 2*5))
	gtx.Constraints = mylayout.Normalize(gtx.Constraints)

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
			buttonsLeft := []button{
				{&gi.buttons.scrollToGoroutine, "Scroll to goroutine"},
				{&gi.buttons.zoomToGoroutine, "Zoom to goroutine"},
			}

			var children []layout.FlexChild
			for _, btn := range buttonsLeft {
				btn := btn
				children = append(children,
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return theme.Button(btn.w, btn.label).Layout(win, gtx)
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
			// TODO(dh): stack traces can get quite long, making it even more important that this window
			// gets scrollbars
			return theme.List(win.Theme, &gi.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
				if index != 0 {
					panic("impossible")
				}
				return gi.foldables.stacktrace.Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					// OPT(dh): compute the string form of the backtrace once, not each frame
					ev := gi.Trace.Events[gi.Goroutine.Spans[0].Event]
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
					return widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, win.Theme.TextSize, s)
				})
			})

		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return theme.List(win.Theme, &gi.statsList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
				if index != 0 {
					panic("impossible")
				}
				return gi.foldables.stats.Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
					return gi.stats.Layout(win, gtx)
				})
			})

		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			return gi.foldables.events.Layout(win, gtx, gi.events.Layout)
		}),
	)

	for i := range gi.description.Spans {
		if s := &gi.description.Spans[i]; s.Clickable != nil {
			for s.Clickable.Clicked() {
				gi.MainWindow.OpenLink(gi.description.Spans[i].Link)
			}
		}
	}

	for _, link := range gi.events.ClickedLinks() {
		gi.MainWindow.OpenLink(link)
	}

	for gi.buttons.scrollToGoroutine.Clicked() {
		gi.MainWindow.OpenLink(&GoroutineLink{Goroutine: gi.Goroutine, Kind: GoroutineLinkKindScroll})
	}
	for gi.buttons.zoomToGoroutine.Clicked() {
		gi.MainWindow.OpenLink(&GoroutineLink{Goroutine: gi.Goroutine, Kind: GoroutineLinkKindZoom})
	}
	for gi.PanelButtons.Backed() {
		gi.MainWindow.prevPanel()
	}

	return dims
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
		m := op.Record(gtx.Ops)
		dims := widget.Label{MaxLines: 1}.Layout(gtx, th.Shaper, f, th.TextSize, s)
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
		ColumnPadding: gtx.Dp(10),
	}

	// Compute the widest column label so that all columns have the same size (unless they need to be wider due to their
	// contents.)
	var labelSizes [3]image.Point
	for i := numStatLabels; i < 2*numStatLabels; i++ {
		f := goFonts[0].Font
		f.Weight = text.Bold

		l := statLabels[gs.numberFormat][i]
		m := op.Record(gtx.Ops)
		dims := widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, l)
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

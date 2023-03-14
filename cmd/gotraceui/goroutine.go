package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"sort"
	"strings"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/text"
	"gioui.org/x/styledtext"
	"golang.org/x/exp/constraints"
	"golang.org/x/exp/slices"
)

type GoroutineStats struct {
	stats ptrace.Statistics
	// mapping maps from indices of displayed statistics to indices in the stats field
	mapping []int

	sortCol        int
	sortDescending bool

	numberFormat durationNumberFormat

	columnClicks [7]widget.PrimaryClickable
}

type GoroutineInfo struct {
	MainWindow *MainWindow
	Goroutine  *ptrace.Goroutine
	Trace      *Trace

	initialized bool

	events Events
	stats  *GoroutineStats

	buttons struct {
		scrollToGoroutine widget.PrimaryClickable
		zoomToGoroutine   widget.PrimaryClickable
	}

	foldables struct {
		stacktrace widget.Bool
		stats      widget.Bool
		events     widget.Bool
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

		gi.foldables.stacktrace.Value = true

		start := gi.Goroutine.Spans.Start()
		end := gi.Goroutine.Spans.End()
		d := time.Duration(end - start)
		observedStart := gi.Goroutine.Spans[0].State == ptrace.StateCreated
		observedEnd := gi.Goroutine.Spans[len(gi.Goroutine.Spans)-1].State == ptrace.StateDone

		gi.description.Reset(win.Theme)
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
				"After trace end\n",
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
			buttonsLeft := []button{
				{&gi.buttons.scrollToGoroutine.Clickable, "Scroll to goroutine"},
				{&gi.buttons.zoomToGoroutine.Clickable, "Zoom to goroutine"},
			}

			var children []layout.FlexChild
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
			if gi.Goroutine.Spans[0].State != ptrace.StateCreated {
				// The goroutine existed before the start of the trace and we do not have the stack trace of where it
				// was created.
				return layout.Dimensions{}
			}

			return theme.List(win.Theme, &gi.stacktraceList).Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
				if index != 0 {
					panic("impossible")
				}
				return theme.Foldable(win.Theme, &gi.foldables.stacktrace, "Creation stack trace").
					Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
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
				return theme.Foldable(win.Theme, &gi.foldables.stats, "Statistics").Layout(win, gtx, gi.stats.Layout)
			})

		}),

		layout.Rigid(layout.Spacer{Height: 10}.Layout),
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			return theme.Foldable(win.Theme, &gi.foldables.events, "Events").Layout(win, gtx, gi.events.Layout)
		}),
	)

	for i := range gi.description.Spans {
		if s := &gi.description.Spans[i]; s.Clickable != nil {
			for s.Clickable.Clicked() {
				gi.MainWindow.OpenLink(defaultLink(s.Object))
			}
		}
	}

	for _, obj := range gi.events.Clicked() {
		gi.MainWindow.OpenLink(defaultLink(obj))
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
	gst := &GoroutineStats{stats: g.Statistics()}

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

func (gs *GoroutineStats) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineStats.Layout").End()

	grid := layout.SmallGrid{
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
			gs.columnClicks[col].Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return styledtext.Text(win.Theme.Shaper, s).Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
					pointer.CursorPointer.Add(gtx.Ops)
				})
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

func goroutineTrack0SpanLabel(spanSel SpanSelector, _ *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	return append(out, spanStateLabels[spanSel.At(0).State]...)
}

func goroutineTrack0SpanContextMenu(spanSel SpanSelector, cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spanSel))

	if spanSel.Size() == 1 {
		switch spanSel.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			// These are the states that are actually on-CPU
			pid := cv.trace.Event((spanSel.At(0).Event)).P
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Do: func(gtx layout.Context) {
					cv.scrollToTimeline(gtx, cv.trace.P(cv.trace.Event((spanSel.At(0).Event)).P))
				},
			})

		case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
			ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
			gid, ok := unblockedByGoroutine(cv.trace, spanSel.At(0))
			if ok {
				items = append(items, &theme.MenuItem{
					Label: PlainLabel(local.Sprintf("Scroll to unblocking goroutine %d", gid)),
					Do: func(gtx layout.Context) {
						gid, _ := unblockedByGoroutine(cv.trace, spanSel.At(0))
						cv.scrollToTimeline(gtx, cv.trace.G(gid))
					},
				})
			}
		}
	}

	return items
}

func userRegionSpanLabel(spanSel SpanSelector, tr *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	// OPT(dh): avoid this allocation
	s := tr.Strings[tr.Events[spanSel.At(0).Event].Args[trace.ArgUserRegionTypeID]]
	return append(out, s)
}

func stackSpanLabel(spanSel SpanSelector, tr *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	pc := spanSel.(MetadataSelector[stackSpanMeta]).MetadataAt(0).pc
	f := tr.PCs[pc]

	short := shortenFunctionName(f.Fn)

	if short != f.Fn {
		return append(out, f.Fn, "."+short)
	} else {
		// This branch is probably impossible; all functions should be fully qualified.
		return append(out, f.Fn)
	}
}

func stackSpanTooltip(level int) func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	return func(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
		var label string
		if state.spanSel.Size() == 1 {
			meta := state.spanSel.(MetadataSelector[stackSpanMeta]).MetadataAt(0)
			pc := meta.pc
			f := tr.PCs[pc]
			label = local.Sprintf("Function: %s\n", f.Fn)
			// TODO(dh): for truncated stacks we should display a relative depth instead
			label += local.Sprintf("Call depth: %d\n", level)
			if state.spanSel.At(0).State == ptrace.StateCPUSample {
				label += local.Sprintf("Samples: %d\n", meta.num)
			}
		} else {
			label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
		}
		// We round the duration, in addition to saying "up to", to make it more obvious that the
		// duration is a guess
		//
		// TODO(dh): don't do this for the stacks of blocking events, we know their exact duration
		label += fmt.Sprintf("Duration: up to %s", roundDuration(Duration(state.spanSel)))
		return theme.Tooltip(win.Theme, label).Layout(win, gtx)
	}
}

func NewGoroutineTimeline(tr *Trace, cv *Canvas, g *ptrace.Goroutine) *Timeline {
	var l string
	if g.Function.Fn != "" {
		l = local.Sprintf("goroutine %d: %s", g.ID, g.Function.Fn)
	} else {
		l = local.Sprintf("goroutine %d", g.ID)
	}

	tl := &Timeline{
		tracks: []Track{{
			spans:  SliceToSpanSelector(g.Spans),
			events: g.Events,
		}},
		buildTrackWidgets: func(tracks []Track) {
			stackTrackBase := -1
			for i := range tracks {
				i := i

				track := &tracks[i]
				switch track.kind {
				case TrackKindUnspecified:
					*track.TrackWidget = TrackWidget{
						spanLabel:       goroutineTrack0SpanLabel,
						spanTooltip:     goroutineSpanTooltip,
						spanContextMenu: goroutineTrack0SpanContextMenu,
					}

				case TrackKindUserRegions:
					*track.TrackWidget = TrackWidget{
						spanLabel:   userRegionSpanLabel,
						spanTooltip: userRegionSpanTooltip,
						spanColor:   singleSpanColor(colorStateUserRegion),
					}

				case TrackKindStack:
					if stackTrackBase == -1 {
						stackTrackBase = i
					}
					*track.TrackWidget = TrackWidget{
						// TODO(dh): should we highlight hovered spans that share the same function?
						spanLabel:   stackSpanLabel,
						spanTooltip: stackSpanTooltip(i - stackTrackBase),
					}

				default:
					panic(fmt.Sprintf("unexpected timeline track kind %d", track.kind))
				}
			}
		},
		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return GoroutineTooltip{g, cv.trace}.Layout(win, gtx)
		},
		item:  g,
		label: l,
	}

	for _, ug := range g.UserRegions {
		tl.tracks = append(tl.tracks, Track{spans: SliceToSpanSelector(ug), kind: TrackKindUserRegions})
	}

	addStackTracks(tl, g, tr)

	return tl
}

type GoroutineTooltip struct {
	g     *ptrace.Goroutine
	trace *Trace
}

func (tt GoroutineTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineTooltip.Layout").End()

	start := tt.g.Spans.Start()
	end := tt.g.Spans.End()
	d := time.Duration(end - start)

	stats := tt.g.Statistics()
	blocked := stats.Blocked()
	inactive := stats.Inactive()
	gcAssist := stats.GCAssist()
	running := stats.Running()
	blockedPct := float32(blocked) / float32(d) * 100
	inactivePct := float32(inactive) / float32(d) * 100
	gcAssistPct := float32(gcAssist) / float32(d) * 100
	runningPct := float32(running) / float32(d) * 100

	var fmts []string
	var args []any

	if tt.g.Function.Fn != "" {
		fmts = append(fmts, "Goroutine %d: %s\n")
		args = append(args, tt.g.ID, tt.g.Function.Fn)
	} else {
		fmts = append(fmts, "Goroutine %d\n")
		args = append(args, tt.g.ID)
	}

	observedStart := tt.g.Spans[0].State == ptrace.StateCreated
	observedEnd := tt.g.Spans[len(tt.g.Spans)-1].State == ptrace.StateDone
	if observedStart {
		fmts = append(fmts, "Created at: %s")
		args = append(args, formatTimestamp(start))
	} else {
		fmts = append(fmts, "Created at: before trace start")
	}

	if observedEnd {
		fmts = append(fmts, "Returned at: %s")
		args = append(args, formatTimestamp(end))
	} else {
		fmts = append(fmts, "Returned at: after trace end")
	}

	if observedStart && observedEnd {
		fmts = append(fmts, "Lifetime: %s")
		args = append(args, roundDuration(d))
	} else {
		fmts = append(fmts, "Observed duration: %s")
		args = append(args, roundDuration(d))
	}

	fmts = append(fmts, "Spans: %d")
	args = append(args, len(tt.g.Spans))

	fmts = append(fmts, "Time in blocked states: %s (%.2f%%)")
	args = append(args, roundDuration(blocked), blockedPct)

	fmts = append(fmts, "Time in inactive states: %s (%.2f%%)")
	args = append(args, roundDuration(inactive), inactivePct)

	fmts = append(fmts, "Time in GC assist: %s (%.2f%%)")
	args = append(args, roundDuration(gcAssist), gcAssistPct)

	fmts = append(fmts, "Time in running states: %s (%.2f%%)")
	args = append(args, roundDuration(running), runningPct)

	l := local.Sprintf(strings.Join(fmts, "\n"), args...)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
}

var reasonLabels = [256]string{
	reasonNewlyCreated: "newly created",
	reasonGosched:      "called runtime.Gosched",
	reasonTimeSleep:    "called time.Sleep",
	reasonPreempted:    "got preempted",
}

func unblockedByGoroutine(tr *Trace, s ptrace.Span) (uint64, bool) {
	ev := tr.Event(s.Event)
	switch s.State {
	case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
		ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
		if link := ptrace.EventID(ev.Link); link != -1 {
			// g0 unblocks goroutines that are blocked on pollable I/O, for example.
			if g := tr.Event(link).G; g != 0 {
				return g, true
			}
		}
	}
	return 0, false
}

func goroutineSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if debug {
		label += local.Sprintf("Event ID: %d\n", state.spanSel.At(0).Event)
		label += fmt.Sprintf("Event type: %d\n", tr.Event(state.spanSel.At(0).Event).Type)
	}
	label += "State: "
	var at string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
		ev := tr.Event(s.Event)
		if at == "" && ev.StkID > 0 {
			at = tr.PCs[tr.Stacks[ev.StkID][s.At]].Fn
		}
		switch state := s.State; state {
		case ptrace.StateInactive:
			label += "inactive"
		case ptrace.StateActive:
			label += "active"
		case ptrace.StateGCDedicated:
			label += "GC (dedicated)"
		case ptrace.StateGCIdle:
			label += "GC (idle)"
		case ptrace.StateBlocked:
			label += "blocked"
		case ptrace.StateBlockedSend:
			label += "blocked on channel send"
		case ptrace.StateBlockedRecv:
			label += "blocked on channel recv"
		case ptrace.StateBlockedSelect:
			label += "blocked on select"
		case ptrace.StateBlockedSync:
			label += "blocked on mutex"
		case ptrace.StateBlockedSyncOnce:
			label += "blocked on sync.Once"
		case ptrace.StateBlockedSyncTriggeringGC:
			label += "blocked triggering GC"
		case ptrace.StateBlockedCond:
			label += "blocked on condition variable"
		case ptrace.StateBlockedNet:
			label += "blocked on polled I/O"
		case ptrace.StateBlockedGC:
			label += "GC assist wait"
		case ptrace.StateBlockedSyscall:
			label += "blocked on syscall"
		case ptrace.StateDone:
			label += "returned"
		case ptrace.StateStuck:
			label += "stuck"
		case ptrace.StateReady:
			label += "ready"
		case ptrace.StateCreated:
			label += "ready"
		case ptrace.StateGCMarkAssist:
			label += "GC mark assist"
		case ptrace.StateGCSweep:
			label += "GC sweep"
			if ev.Link != -1 {
				l := tr.Events[ev.Link]
				label += local.Sprintf("\nSwept %d bytes, reclaimed %d bytes",
					l.Args[trace.ArgGCSweepDoneSwept], l.Args[trace.ArgGCSweepDoneReclaimed])
			}
		default:
			if debug {
				panic(fmt.Sprintf("unhandled state %d", state))
			}
		}

		tags := make([]string, 0, 4)
		if s.Tags&ptrace.SpanTagRead != 0 {
			tags = append(tags, "read")
		}
		if s.Tags&ptrace.SpanTagAccept != 0 {
			tags = append(tags, "accept")
		}
		if s.Tags&ptrace.SpanTagDial != 0 {
			tags = append(tags, "dial")
		}
		if s.Tags&ptrace.SpanTagNetwork != 0 {
			tags = append(tags, "network")
		}
		if s.Tags&ptrace.SpanTagTCP != 0 {
			tags = append(tags, "TCP")
		}
		if s.Tags&ptrace.SpanTagTLS != 0 {
			tags = append(tags, "TLS")
		}
		if s.Tags&ptrace.SpanTagHTTP != 0 {
			tags = append(tags, "HTTP")
		}
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}

		if g, ok := unblockedByGoroutine(tr, s); ok {
			label += local.Sprintf("\nUnblocked by goroutine %d (%s)", g, tr.G(g).Function)
		}
	} else {
		label += local.Sprintf("mixed (%d spans)", state.spanSel.Size())
	}
	label += "\n"

	if state.spanSel.Size() == 1 {
		if reason := reasonLabels[tr.Reason(state.spanSel.At(0))]; reason != "" {
			label += "Reason: " + reason + "\n"
		}
	}

	if at != "" {
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("In: %s\n", at)
	}
	if state.spanSel.Size() == 1 {
		switch state.spanSel.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			pid := tr.Event(state.spanSel.At(0).Event).P
			label += local.Sprintf("On: processor %d\n", pid)
		}
	}

	if state.spanSel.At(state.spanSel.Size()-1).State != ptrace.StateDone {
		label += fmt.Sprintf("Duration: %s\n", roundDuration(Duration(state.spanSel)))
	}

	if len(state.events) > 0 {
		label += local.Sprintf("Events in span: %d\n", len(state.events))
	}

	if len(state.eventsUnderCursor) > 0 {
		kind := tr.Event(state.eventsUnderCursor[0]).Type
		for _, ev := range state.eventsUnderCursor[1:] {
			if tr.Event(ev).Type != kind {
				kind = 255
				break
			}
		}
		if kind != 255 {
			var noun string
			switch kind {
			case trace.EvGoSysCall:
				noun = "syscalls"
				if len(state.eventsUnderCursor) == 1 {
					stk := tr.Stacks[tr.Event(state.eventsUnderCursor[0]).StkID]
					if len(stk) != 0 {
						frame := tr.PCs[stk[0]]
						noun += fmt.Sprintf(" (%s)", frame.Fn)
					}
				}
			case trace.EvGoCreate:
				noun = "goroutine creations"
			case trace.EvGoUnblock:
				noun = "goroutine unblocks"
			default:
				if debug {
					panic(fmt.Sprintf("unhandled kind %d", kind))
				}
			}
			label += local.Sprintf("Events under cursor: %d %s\n", len(state.eventsUnderCursor), noun)
		} else {
			label += local.Sprintf("Events under cursor: %d\n", len(state.eventsUnderCursor))
		}
	}

	if n := len(label) - 1; label[n] == '\n' {
		label = label[:n]
	}

	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func userRegionSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spanSel.Size() == 1 {
		s := state.spanSel.At(0)
		ev := tr.Event(s.Event)
		if s.State != ptrace.StateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		if taskID := ev.Args[trace.ArgUserRegionTaskID]; taskID != 0 {
			label = local.Sprintf("User region: %s\nTask: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], tr.Task(taskID).Name)
		} else {
			label = local.Sprintf("User region: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]])
		}
	} else {
		label = local.Sprintf("mixed (%d spans)\n", state.spanSel.Size())
	}
	label += fmt.Sprintf("Duration: %s", roundDuration(Duration(state.spanSel)))
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

var spanStateLabels = [...][]string{
	ptrace.StateInactive: {"Inactive"},
	// StateActive isn't needed, those spans have custom labels
	ptrace.StateActive:                  {},
	ptrace.StateGCIdle:                  {"GC (idle)", "I"},
	ptrace.StateGCDedicated:             {"GC (dedicated)", "D"},
	ptrace.StateBlocked:                 {"blocked"},
	ptrace.StateBlockedSend:             {"send"},
	ptrace.StateBlockedRecv:             {"recv"},
	ptrace.StateBlockedSelect:           {"select"},
	ptrace.StateBlockedSync:             {"sync"},
	ptrace.StateBlockedSyncOnce:         {"sync.Once"},
	ptrace.StateBlockedSyncTriggeringGC: {"triggering GC", "T"},
	ptrace.StateBlockedCond:             {"sync.Cond"},
	ptrace.StateBlockedNet:              {"I/O"},
	ptrace.StateBlockedGC:               {"GC assist wait", "W"},
	ptrace.StateBlockedSyscall:          {"syscall"},
	ptrace.StateStuck:                   {"stuck"},
	ptrace.StateReady:                   {"ready"},
	ptrace.StateCreated:                 {"created"},
	// StateDone spans will never be big enough to contain labels
	ptrace.StateDone:         {},
	ptrace.StateGCMarkAssist: {"GC mark assist", "M"},
	ptrace.StateGCSweep:      {"GC sweep", "S"},
	ptrace.StateLast:         nil,
}

type stackSpanMeta struct {
	// OPT(dh): should we use 48 bits for the PC and 16 bits for the num?
	pc  uint64
	num int
}

func addStackTracks(tl *Timeline, g *ptrace.Goroutine, tr *Trace) {
	if g.Function.Fn == "runtime.bgsweep" {
		// Go <=1.19 has a lot of spans in runtime.bgsweep, but the stacks are utterly uninteresting, containing only a
		// single frame. Save some memory by not creating stack tracks for this goroutine.
		return
	}

	var stackTracks []Track
	var trackSpans []ptrace.Spans
	var spanMeta [][]stackSpanMeta
	offSpans := 0
	offSamples := 0
	cpuSamples := tr.CPUSamples[g.ID]

	nextEvent := func(advance bool) (ptrace.EventID, bool, bool) {
		if offSpans == len(g.Spans) && offSamples == len(cpuSamples) {
			return 0, false, false
		}

		if offSpans < len(g.Spans) {
			id := g.Spans[offSpans].Event
			if offSamples < len(cpuSamples) {
				oid := cpuSamples[offSamples]
				if id <= oid {
					if advance {
						offSpans++
					}
					return id, false, true
				} else {
					if advance {
						offSamples++
					}
					return oid, true, true
				}
			} else {
				if advance {
					offSpans++
				}
				return id, false, true
			}
		} else {
			id := cpuSamples[offSamples]
			if advance {
				offSamples++
			}
			return id, true, true
		}
	}

	// function name of the previous span, indexed by track index, i.e. stack depth
	var prevFns []string
	for {
		evID, isSample, ok := nextEvent(true)
		if !ok {
			break
		}

		ev := &tr.Events[evID]
		stk := tr.Stacks[ev.StkID]
		switch ev.Type {
		case trace.EvGoUnblock:
			// The stack is in the goroutine that unblocked this one
			continue
		case trace.EvGoStart:
			// This event doesn't have a stack; display an artificial stack representing time spent on-CPU
			continue
		}

		state := ptrace.StateStack
		if isSample {
			state = ptrace.StateCPUSample
			// CPU samples include two runtime functions at the start of the stack trace that isn't present for stacks
			// collected by the runtime tracer.
			if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Fn == "runtime.goexit" {
				stk = stk[:len(stk)-1]
			}
			if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Fn == "runtime.main" {
				stk = stk[:len(stk)-1]
			}
		}

		if len(stk) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			stk = stk[:64]
		}

		stackTracks = grow(stackTracks, len(stk))
		prevFns = grow(prevFns, len(stk))
		trackSpans = grow(trackSpans, len(stk))
		spanMeta = grow(spanMeta, len(stk))
		var end trace.Timestamp
		if endEvID, _, ok := nextEvent(false); ok {
			end = tr.Events[endEvID].Ts
		} else {
			end = g.Spans.End()
		}

		for i := 0; i < len(stk); i++ {
			spans := trackSpans[i]
			if len(spans) != 0 {
				prevSpan := &spans[len(spans)-1]
				prevFn := prevFns[i]
				fn := tr.PCs[stk[len(stk)-i-1]].Fn
				if prevSpan.End == tr.Events[evID].Ts && prevFn == fn && state == prevSpan.State {
					// This is a continuation of the previous span. Merging these can have massive memory usage savings,
					// which is why we do it here and not during display.
					//
					// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
					// data more apparent.
					prevSpan.End = end
					if state == ptrace.StateCPUSample {
						spanMeta[i][len(spans)-1].num++
					}
				} else {
					// This is a new span
					span := ptrace.Span{
						Start: ev.Ts,
						End:   end,
						Event: evID,
						State: state,
					}
					trackSpans[i] = append(trackSpans[i], span)
					spanMeta[i] = append(spanMeta[i], stackSpanMeta{pc: stk[len(stk)-i-1], num: 1})
					prevFns[i] = fn
				}
			} else {
				// This is the first span
				span := ptrace.Span{
					Start: ev.Ts,
					End:   end,
					Event: evID,
					State: state,
				}
				trackSpans[i] = append(trackSpans[i], span)
				spanMeta[i] = append(spanMeta[i], stackSpanMeta{pc: stk[len(stk)-i-1], num: 1})
				prevFns[i] = tr.PCs[stk[len(stk)-i-1]].Fn
			}
		}
	}

	for i := range stackTracks {
		stackTracks[i].kind = TrackKindStack
		stackTracks[i].spans = spanAndMetadataSlices[stackSpanMeta]{
			spans: trackSpans[i],
			meta:  spanMeta[i],
		}
	}

	tl.tracks = append(tl.tracks, stackTracks...)
}

package main

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"strings"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/text"
	exptrace "golang.org/x/exp/trace"
)

func goroutineTrack0SpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	span := spans.AtPtr(0)
	state := span.State
	if state == ptrace.StateBlockedSyscall {
		ev := tr.Event(span.StartEvent)
		stk := ev.Stack()
		if stk != exptrace.NoStack {
			fn := tr.PCs[tr.Stacks[stk][0]].Func
			return append(out,
				fmt.Sprintf("syscall (%s)", fn),
				fmt.Sprintf("syscall (.%s)", shortenFunctionName(fn)),
				"syscall",
			)
		}
	}
	return append(out, spanStateLabels[state]...)
}

func goroutineTrack0SpanContextMenu(spans Items[ptrace.Span], cv *Canvas) []*theme.MenuItem {
	items := []*theme.MenuItem{
		newZoomMenuItem(cv, spans),
		newOpenSpansMenuItem(spans),
	}

	if spans.Len() == 1 {
		switch spans.AtPtr(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCFractional, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			// These are the states that are actually on-CPU
			pid := cv.trace.Event(spans.AtPtr(0).StartEvent).Proc()
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Action: func() theme.Action {
					return &ScrollToObjectAction{
						Object: cv.trace.P(pid),
					}
				},
			})

		case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
			ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
			gid, ok := unblockedByGoroutine(cv.trace, spans.AtPtr(0))
			if ok {
				items = append(items, &theme.MenuItem{
					Label: PlainLabel(local.Sprintf("Scroll to unblocking goroutine %d", gid)),
					Action: func() theme.Action {
						gid, _ := unblockedByGoroutine(cv.trace, spans.AtPtr(0))
						return &ScrollToObjectAction{
							Object: cv.trace.G(gid),
						}
					},
				})
			}
		}
	}

	return items
}

func userRegionSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	ev := tr.Event(spans.AtPtr(0).StartEvent)
	return append(out, ev.Region().Type)
}

func stackSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	if spans.AtPtr(0).State == statePlaceholder {
		return out
	}
	pc := spans.MetadataAtPtr(0).(*stackSpanMeta).pc
	f := tr.PCs[pc]

	short := shortenFunctionName(f.Func)

	if short != f.Func {
		return append(out, f.Func, "."+short)
	} else {
		// This branch is probably impossible; all functions should be fully qualified.
		return append(out, f.Func)
	}
}

func stackSpanTooltip(level int) func(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	return func(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
		var label string
		if spans.Len() == 1 {
			if spans.AtPtr(0).State != statePlaceholder {
				meta := spans.MetadataAtPtr(0).(*stackSpanMeta)
				pc := meta.pc
				f := tr.PCs[pc]
				label = local.Sprintf("Function: %s\n", f.Func)
				// TODO(dh): for truncated stacks we should display a relative depth instead
				label += local.Sprintf("Call depth: %d\n", level)
				if spans.AtPtr(0).State == ptrace.StateCPUSample {
					label += local.Sprintf("Samples: %d\n", meta.num)
				}
			}
		} else {
			label = local.Sprintf("%d spans\n", spans.Len())
		}
		// We round the duration, in addition to saying "up to", to make it more obvious that the
		// duration is a guess
		//
		// TODO(dh): don't do this for the stacks of blocking events, we know their exact duration
		label += spansDurationForTooltipWithQualifier(spans, "up to")
		return theme.Tooltip(win.Theme, label).Layout(win, gtx)
	}
}

func NewGoroutineTimeline(tr *Trace, cv *Canvas, g *ptrace.Goroutine) *Timeline {
	shortName := local.Sprintf("goroutine %d", g.ID)
	l := shortName
	if g.Function != nil {
		l = local.Sprintf("goroutine %d: %s", g.ID, g.Function.Func)
	}

	tl := &Timeline{
		cv: cv,
		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return GoroutineTooltip{g, cv.trace}.Layout(win, gtx)
		},
		item:      g,
		label:     l,
		shortName: shortName,
	}

	track := NewTrack(tl, TrackKindUnspecified)
	track.Start = g.EffectiveStart()
	track.End = g.EffectiveEnd()
	track.spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
		items: g.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    track,
		},
		contiguous: true,
		subslice:   true,
	})

	track.spanLabel = goroutineTrack0SpanLabel
	track.spanTooltip = goroutineSpanTooltip
	track.spanContextMenu = goroutineTrack0SpanContextMenu
	track.events = g.Events
	track.samples = tr.CPUSamplesByG[g.ID]
	tl.tracks = []*Track{track}

	for _, ug := range g.UserRegions {
		track := NewTrack(tl, TrackKindUserRegions)
		track.Start = ug[0].Start
		track.End = ug[len(ug)-1].End
		track.events = tl.tracks[0].events
		track.hideEventMarkers = true
		track.spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
			items: ug,
			container: ItemContainer{
				Timeline: tl,
				Track:    track,
			},
			subslice: true,
		})
		track.spanLabel = userRegionSpanLabel
		track.spanTooltip = userRegionSpanTooltip
		track.spanColor = singleSpanColor(colorStateUserRegion)
		tl.tracks = append(tl.tracks, track)
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

	start := tt.g.EffectiveStart()
	end := tt.g.EffectiveEnd()
	d := time.Duration(end - start)

	// XXX reintroduce caching of statistics
	stats := ptrace.ComputeStatistics(ptrace.ToSpans(tt.g.Spans))
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

	if tt.g.Function != nil {
		fmts = append(fmts, "Goroutine %d: %s\n")
		args = append(args, tt.g.ID, tt.g.Function.Func)
	} else {
		fmts = append(fmts, "Goroutine %d\n")
		args = append(args, tt.g.ID)
	}

	observedStart := !tt.g.Spans[0].StartedBeforeTrace(tt.trace.Trace)
	observedEnd := tt.g.End.Set()
	if observedStart {
		fmts = append(fmts, "Created at: %s")
		args = append(args, formatTimestamp(nil, tt.trace.AdjustedTime(start)))
	} else {
		fmts = append(fmts, "Created at: before trace start")
	}

	if observedEnd {
		fmts = append(fmts, "Returned at: %s")
		args = append(args, formatTimestamp(nil, tt.trace.AdjustedTime(end)))
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

func unblockedByGoroutine(tr *Trace, s *ptrace.Span) (exptrace.GoID, bool) {
	switch s.State {
	case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
		ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
		if s.EndEvent == ptrace.NoEvent {
			return 0, false
		}
		endEv := tr.Event(s.EndEvent)
		if endEv.Kind() == exptrace.EventStateTransition {
			if gid := endEv.Goroutine(); gid != exptrace.NoGoroutine {
				return gid, true
			}
		}
	}
	return 0, false
}

func goroutineSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	var label string
	if debug {
		label += local.Sprintf("Event ID: %d\n", spans.AtPtr(0).StartEvent)
		label += fmt.Sprintf("Event kind: %s\n", tr.Event(spans.AtPtr(0).StartEvent).Kind())
	}
	var at string
	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		ev := tr.Event(s.StartEvent)
		stk := ev.Stack()
		if at == "" && stk != exptrace.NoStack {
			at = tr.PCs[tr.Stacks[stk][s.At]].Func
		}
		label += tooltipStateLabels[s.State]
		tags := spanTagStrings(s.Tags)
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}
		if s.State == ptrace.StateGCSweep {
			if endEv := tr.Event(s.EndEvent); endEv.Kind() == exptrace.EventRangeEnd {
				attrs := endEv.RangeAttributes()
				var swept, reclaimed uint64
				for _, attr := range attrs {
					switch attr.Name {
					case "bytes swept":
						swept = attr.Value.Uint64()
					case "bytes reclaimed":
						reclaimed = attr.Value.Uint64()
					}
				}
				label += local.Sprintf("\nSwept %d bytes, reclaimed %d bytes", swept, reclaimed)
			}
		}

		if g, ok := unblockedByGoroutine(tr, s); ok {
			label += local.Sprintf("\nUnblocked by goroutine %d (%s)", g, tr.G(g).Function)
		}
	} else {
		label = local.Sprintf("%d spans", spans.Len())
	}
	label += "\n"

	if spans.Len() == 1 {
		ev := tr.Event(spans.AtPtr(0).StartEvent)
		if ev.Kind() == exptrace.EventStateTransition {
			trans := ev.StateTransition()
			if ptrace.IsGoroutineCreation(&trans) {
				label += "Reason: newly created\n"
			}
		}
	}

	if at != "" {
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("In: %s\n", at)
	}
	if spans.Len() == 1 {
		switch spans.AtPtr(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			pid := tr.Event(spans.AtPtr(0).StartEvent).Proc()
			label += local.Sprintf("On: processor %d\n", pid)
		}
	}

	label += spansDurationForTooltip(spans)

	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func userRegionSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	var label string
	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		ev := tr.Event(s.StartEvent)
		r := ev.Region()
		if s.State != ptrace.StateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		if r.Task != 0 {
			task := tr.Task(r.Task)
			if task.Stub() {
				label = local.Sprintf("User region: %s\nTask: %d\n", r.Type, r.Task)
			} else {
				label = local.Sprintf("User region: %s\nTask: %s\n", r.Type, task.Name)
			}
		} else {
			label = local.Sprintf("User region: %s\n", r.Type)
		}
	} else {
		label = local.Sprintf("%d spans\n", spans.Len())
	}
	label += spansDurationForTooltip(spans)
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

type stackSpanMeta struct {
	// OPT(dh): should we use 48 bits for the PC and 16 bits for the num?
	pc  uint64
	num int
}

func NewGoroutineInfo(tr *Trace, mwin *theme.Window, canvas *Canvas, g *ptrace.Goroutine, allTimelines []*Timeline) *SpansInfo {
	var title string
	if g.Function != nil {
		title = local.Sprintf("goroutine %d: %s", g.ID, g.Function)
	} else {
		title = local.Sprintf("goroutine %d", g.ID)
	}

	spans := g.Spans

	var stacktrace string
	if spans[0].State == ptrace.StateCreated {
		stk := tr.Event(spans[0].StartEvent).Stack()
		stacktrace = formatStack(tr, stk, 0)
	}

	buildDescription := func(win *theme.Window, gtx layout.Context) Description {
		var attrs []DescriptionAttribute
		// OPT(dh): we don't need TextBuilder to collect the spans in this case.
		tb := TextBuilder{Window: win}

		start := spans[0].Start
		end := spans[len(spans)-1].End
		d := time.Duration(end - start)
		observedStart := !spans[0].StartedBeforeTrace(tr.Trace)
		observedEnd := g.End.Set()

		if g.Parent != 0 {
			parent := tr.G(g.Parent)

			if parent.Function != nil {
				link := *tb.DefaultLink(
					local.Sprintf("Goroutine %d (%s)", g.Parent, parent.Function.Func),
					"Parent of current goroutine",
					parent)
				attrs = append(attrs, DescriptionAttribute{
					Key:   "Parent",
					Value: link,
				})
			} else {
				link := *tb.DefaultLink(
					local.Sprintf("Goroutine %d", g.Parent),
					"Parent of current goroutine",
					parent)
				attrs = append(attrs, DescriptionAttribute{
					Key:   "Parent",
					Value: link,
				})
			}
		}

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Goroutine",
			Value: *tb.Span(local.Sprintf("%d", g.ID)),
		})

		if g.Function != nil {
			link := *tb.DefaultLink(g.Function.Func, "Function of current goroutine", g.Function)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Function",
				Value: link,
			})
		}

		if observedStart {
			link := *tb.DefaultLink(formatTimestamp(nil, tr.AdjustedTime(start)), "Start of current goroutine", start)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Created at",
				Value: link,
			})
		} else {
			link := *tb.DefaultLink("before trace start", "Start of trace", start)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Created at",
				Value: link,
			})
		}

		if observedEnd {
			link := *tb.DefaultLink(formatTimestamp(nil, tr.AdjustedTime(end)), "End of current goroutine", end)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Returned at",
				Value: link,
			})
		} else {
			link := *tb.DefaultLink("after trace end", "End of trace", end)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Returned at",
				Value: link,
			})
		}

		if observedStart && observedEnd {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Lifetime",
				Value: *tb.Span(d.String()),
			})
		} else {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Observed duration",
				Value: *tb.Span(d.String()),
			})
		}
		var desc Description
		desc.Attributes = attrs
		return desc
	}

	cfg := SpansInfoConfig{
		Title:      title,
		Stacktrace: stacktrace,
		Navigations: SpansInfoConfigNavigations{
			Scroll: struct {
				ButtonLabel string
				Fn          func() theme.Action
			}{
				ButtonLabel: "Scroll to goroutine",
				Fn: func() theme.Action {
					return &ScrollToObjectAction{Object: g}
				},
			},

			Zoom: struct {
				ButtonLabel string
				Fn          func() theme.Action
			}{
				ButtonLabel: "Zoom to goroutine",
				Fn: func() theme.Action {
					return &ZoomToObjectAction{Object: g}
				},
			},
		},
		Statistics: func(win *theme.Window) *theme.Future[*SpansStats] {
			return theme.NewFuture(win, func(cancelled <-chan struct{}) *SpansStats {
				return NewGoroutineStats(g)
			})
		},
		DescriptionBuilder: buildDescription,
	}

	tl := canvas.itemToTimeline[g]
	ss := SimpleItems[ptrace.Span, any]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}
	return NewSpansInfo(cfg, tr, mwin, theme.Immediate[Items[ptrace.Span]](ss), allTimelines)
}

type GoroutineList struct {
	Trace         *Trace
	Goroutines    SortedIndices[*ptrace.Goroutine, []*ptrace.Goroutine]
	HiddenColumns struct {
		ID        bool
		Function  bool
		StartTime bool
		EndTime   bool
		Duration  bool
	}

	table         *theme.Table
	scrollState   theme.YScrollableListState
	cellFormatter CellFormatter
}

func (evs *GoroutineList) HoveredLink() ObjectLink {
	return evs.cellFormatter.HoveredLink()
}

func (gl *GoroutineList) SetGoroutines(win *theme.Window, gtx layout.Context, gs []*ptrace.Goroutine) {
	gl.initTable(win, gtx)
	gl.setGoroutines(gtx, gs)
}

func (gl *GoroutineList) setGoroutines(gtx layout.Context, gs []*ptrace.Goroutine) {
	gl.Goroutines.Reset(gs)

	switch gl.table.Columns[gl.table.SortedBy].Name {
	case "Goroutine":
		gl.Goroutines.Sort(func(gi, gj *ptrace.Goroutine) int {
			return cmp(gi.ID, gj.ID, gl.table.SortOrder == theme.SortDescending)
		})
	case "Function":
		gl.Goroutines.Sort(func(gi, gj *ptrace.Goroutine) int {
			var fn1, fn2 string
			if gi.Function != nil {
				fn1 = gi.Function.Func
			}
			if gj.Function != nil {
				fn2 = gj.Function.Func
			}

			return cmp(fn1, fn2, gl.table.SortOrder == theme.SortDescending)
		})
	case "Start time":
		gl.Goroutines.Sort(func(gi, gj *ptrace.Goroutine) int {
			starti := gi.Start.GetOr(-1)
			startj := gj.Start.GetOr(-1)
			return cmp(starti, startj, gl.table.SortOrder == theme.SortDescending)
		})
	case "End time":
		gl.Goroutines.Sort(func(gi, gj *ptrace.Goroutine) int {
			endi := gi.End.GetOr(math.MaxInt64)
			endj := gj.End.GetOr(math.MaxInt64)
			return cmp(endi, endj, gl.table.SortOrder == theme.SortDescending)
		})
	case "Duration":
		gl.Goroutines.Sort(func(gi, gj *ptrace.Goroutine) int {
			starti := gi.Start.GetOr(-1)
			startj := gj.Start.GetOr(-1)
			// We use traceEnd + 1 instead of MaxInt64 so that durations still sort usefully even if one of
			// start or end is missing. For example, even if the end is unknown, one event happening before
			// the other will have a longer duration.
			endi := gi.End.GetOr(gi.EffectiveEnd() + 1)
			endj := gj.End.GetOr(gj.EffectiveEnd() + 1)

			di := endi - starti
			dj := endj - startj

			return cmp(di, dj, gl.table.SortOrder == theme.SortDescending)
		})
	}
}

func (gs *GoroutineList) initTable(win *theme.Window, gtx layout.Context) {
	if gs.table != nil {
		return
	}
	gs.table = &theme.Table{}
	cols := []theme.Column{}
	if !gs.HiddenColumns.ID {
		cols = append(cols, theme.Column{
			Name:      "Goroutine",
			Alignment: text.End,
			Clickable: true,
		})
	}
	if !gs.HiddenColumns.Function {
		cols = append(cols, theme.Column{
			Name:      "Function",
			Alignment: text.Start,
			Clickable: true,
		})
	}
	if !gs.HiddenColumns.StartTime {
		cols = append(cols, theme.Column{
			Name:      "Start time",
			Alignment: text.End,
			Clickable: true,
		})
	}
	if !gs.HiddenColumns.EndTime {
		cols = append(cols, theme.Column{
			Name:      "End time",
			Alignment: text.End,
			Clickable: true,
		})
	}
	if !gs.HiddenColumns.Duration {
		cols = append(cols, theme.Column{
			Name:      "Duration",
			Alignment: text.End,
			Clickable: true,
		})
	}
	gs.table.SetColumns(win, gtx, cols)
	gs.table.SortedBy = 0
	gs.table.SortOrder = theme.SortAscending

	// Find space needed for largest goroutine ID
	n := gs.Goroutines.Len()
	s := max(n-32, 0)
	var maxID exptrace.GoID
	// Look at the last 32 goroutines for this function. This has a high likelyhood of telling us the greatest ID.
	for _, g := range gs.Goroutines.Items[s:n] {
		if g.ID > maxID {
			maxID = g.ID
		}
	}
	r0 := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, "Goroutine", win.ColorMaterial(gtx, color.Oklch{}))
	})
	r1 := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, local.Sprintf("%d", maxID), win.ColorMaterial(gtx, color.Oklch{}))
	})
	w := r0.Dimensions.Size.X
	if x := r1.Dimensions.Size.X; x > w {
		w = x
	}

	w += gtx.Dp(5) * 2
	d := float32(w) - gs.table.Columns[0].Width
	gs.table.Columns[0].Width = float32(w)
	gs.table.Columns[1].Width = max(0, gs.table.Columns[1].Width-float32(d))
}

func (gs *GoroutineList) Update(gtx layout.Context) {
	gs.table.Update(gtx)
	if _, ok := gs.table.SortByClickedColumn(); ok {
		// Trigger resorting.
		gs.setGoroutines(gtx, gs.Goroutines.Items)
	}
}

func (gs *GoroutineList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.GoroutineList.Layout").End()

	gs.initTable(win, gtx)
	gs.Update(gtx)
	gs.cellFormatter.Update(win, gtx)

	cellFn := func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		g := gs.Goroutines.At(row)
		switch colName := gs.table.Columns[col].Name; colName {
		case "Goroutine": // ID
			return gs.cellFormatter.Goroutine(win, gtx, g, "")
		case "Function": // Function
			return gs.cellFormatter.Function(win, gtx, g.Function)
		case "Start time": // Start time
			var l string
			var ts exptrace.Time
			if start, ok := g.Start.Get(); ok {
				ts = start
			} else {
				ts = g.EffectiveStart()
				l = "before trace start"
			}
			return gs.cellFormatter.Timestamp(win, gtx, gs.Trace, ts, l)
		case "End time": // End time
			var l string
			var ts exptrace.Time
			if end, ok := g.End.Get(); ok {
				ts = end
			} else {
				ts = g.EffectiveEnd()
				l = "after trace end"
			}
			return gs.cellFormatter.Timestamp(win, gtx, gs.Trace, ts, l)
		case "Duration": // Duration
			// If the goroutine's end wasn't observed, then traceEnd is equal to the trace's end
			traceEnd := g.EffectiveEnd()

			start, sok := g.Start.Get()
			end, eok := g.End.Get()

			var d time.Duration
			var approx bool
			if !sok && !eok {
				d = time.Duration(traceEnd)
				approx = true
			} else if !sok {
				d = time.Duration(end)
				approx = true
			} else if !eok {
				d = time.Duration(traceEnd - start)
				approx = true
			} else {
				d = time.Duration(end - start)
			}

			return gs.cellFormatter.Duration(win, gtx, d, approx)
		default:
			panic(colName)
		}
	}

	dims := theme.SimpleTable(win,
		gtx,
		gs.table,
		&gs.scrollState,
		gs.Goroutines.Len(),
		cellFn,
	)

	return dims
}

type GoroutinesComponent struct {
	list GoroutineList
}

func NewGoroutinesComponent(gs []*ptrace.Goroutine, tr *Trace) *GoroutinesComponent {
	return &GoroutinesComponent{
		list: GoroutineList{
			Trace:      tr,
			Goroutines: NewSortedIndices(gs),
		},
	}
}

// Title implements theme.Component.
func (*GoroutinesComponent) Title() string {
	return "Goroutines"
}

// Transition implements theme.Component.
func (*GoroutinesComponent) Transition(state theme.ComponentState) {}

// WantsTransition implements theme.Component.
func (*GoroutinesComponent) WantsTransition(gtx layout.Context) theme.ComponentState {
	return theme.ComponentStateNone
}

// Layout implements theme.Component.
func (gc *GoroutinesComponent) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	return gc.list.Layout(win, gtx)
}

// GoroutineLabel returns a label describing the goroutine, of the form "<gid>[ (<function name>)]".
func GoroutineLabel(g *ptrace.Goroutine) string {
	// TODO(dh): use this function everywhere
	if g.Function != nil && g.Function.Func != "" {
		return local.Sprintf("%d (%s)", g.ID, g.Function.Func)
	} else {
		return local.Sprintf("%d", g.ID)
	}
}

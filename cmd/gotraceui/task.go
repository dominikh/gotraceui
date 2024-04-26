package main

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"strings"
	"time"

	"gioui.org/font"
	"gioui.org/text"
	"golang.org/x/exp/trace"
	exptrace "golang.org/x/exp/trace"
	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

func NewTaskTimeline(tr *Trace, cv *Canvas, t *ptrace.Task) *Timeline {
	shortName := local.Sprintf("task %d", t.ID)
	l := shortName
	if t.Name != "" {
		l = local.Sprintf("task %d: %s", t.ID, t.Name)
	}

	tl := &Timeline{
		cv: cv,
		widgetTooltip: func(win *theme.Window, gtx layout.Context, tl *Timeline) layout.Dimensions {
			return TaskTooltip{t, cv.trace}.Layout(win, gtx)
		},
		item:      t,
		label:     l,
		shortName: shortName,
	}

	track := NewTrack(tl, TrackKindUnspecified)
	track.Start = t.EffectiveStart()
	track.End = t.EffectiveEnd()
	track.spans = theme.Immediate[Items[ptrace.Span]](SimpleItems[ptrace.Span, any]{
		items: t.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    track,
		},
		contiguous: true,
		subslice:   true,
	})

	track.spanLabel = taskSpanLabel
	track.spanTooltip = taskSpanTooltip
	track.spanContextMenu = nil
	track.events = t.Events
	tl.tracks = []*Track{track}

	return tl
}

func taskForSpan(span *ptrace.Span, tr *Trace) *ptrace.Task {
	if span.State != ptrace.StateTask {
		panic("not a task span")
	}
	var eventID ptrace.EventID
	if span.StartEvent != 0 {
		eventID = span.StartEvent
	} else if span.EndEvent != 0 {
		eventID = span.EndEvent
	} else {
		return nil
	}
	if eventID == ptrace.NoEvent {
		return nil
	}
	ev := tr.Event(eventID)
	return tr.Task(ev.Task().ID)
}

func taskSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	span := spans.AtPtr(0)
	state := span.State
	if state == ptrace.StateTask {
		task := taskForSpan(span, tr)
		if task == nil {
			return append(out, "<unknown task>")
		}
		return append(out, task.Name)
	}
	return append(out, spanStateLabels[state]...)
}

func taskSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	var label string
	if spans.Len() == 1 {
		s := spans.AtPtr(0)
		if s.State != ptrace.StateTask {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		task := taskForSpan(s, tr)
		if task == nil {
			label = local.Sprintf("Unknown task\n")
		} else {
			goID := goroutineIDForTask(task, tr)
			if goID != trace.NoGoroutine {
				label = local.Sprintf("Task: %s\nCreated by goroutine %d\n", task.Name, goID)
			} else {
				label = local.Sprintf("Task: %s\n", task.Name)
			}
		}
	} else {
		label = local.Sprintf("%d spans\n", spans.Len())
	}
	label += spansDurationForTooltip(spans)
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

type TaskTooltip struct {
	t     *ptrace.Task
	trace *Trace
}

func (tt TaskTooltip) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TaskTooltip.Layout").End()

	start := tt.t.EffectiveStart()
	end := tt.t.EffectiveEnd()
	d := time.Duration(end - start)

	var fmts []string
	var args []any

	fmts = append(fmts, "Task %d: %s\n")
	args = append(args, tt.t.ID, tt.t.Name)

	observedStart := !tt.t.Spans[0].StartedBeforeTrace(tt.trace.Trace)
	observedEnd := tt.t.End.Set()
	if observedStart {
		fmts = append(fmts, "Created at: %s")
		args = append(args, formatTimestamp(nil, tt.trace.AdjustedTime(start)))
	} else {
		fmts = append(fmts, "Created at: before trace start")
	}

	if observedEnd {
		fmts = append(fmts, "Ended at: %s")
		args = append(args, formatTimestamp(nil, tt.trace.AdjustedTime(end)))
	} else {
		fmts = append(fmts, "Ended at: after trace end")
	}

	if observedStart && observedEnd {
		fmts = append(fmts, "Lifetime: %s")
		args = append(args, roundDuration(d))
	} else {
		fmts = append(fmts, "Observed duration: %s")
		args = append(args, roundDuration(d))
	}

	goroutineID := goroutineIDForTask(tt.t, tt.trace)
	if goroutineID != trace.NoGoroutine {
		fmts = append(fmts, "Created by goroutine: %d")
		args = append(args, goroutineID)
	}

	l := local.Sprintf(strings.Join(fmts, "\n"), args...)

	return theme.Tooltip(win.Theme, l).Layout(win, gtx)
}

// goroutineIDForTask returns the goroutine ID that created a given task, or trace.NoGoroutine if we don't know.
func goroutineIDForTask(t *ptrace.Task, tr *Trace) trace.GoID {
	if t == nil || t.StartEvent <= 0 {
		return trace.NoGoroutine
	}
	ev := tr.Event(t.StartEvent)
	return ev.Goroutine()
}

// NewTaskInfo builds information for a panel about a task.
func NewTaskInfo(tr *Trace, mwin *theme.Window, canvas *Canvas, t *ptrace.Task, allTimelines []*Timeline) *SpansInfo {
	var title string
	if t.Name != "" {
		title = local.Sprintf("task %d: %s", t.ID, t.Name)
	} else {
		title = local.Sprintf("task %d", t.ID)
	}

	spans := t.Spans

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
		observedEnd := t.End.Set()

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Task",
			Value: *tb.Span(local.Sprintf("%d", t.ID)),
		})

		if t.Name != "" {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Name",
				Value: *tb.Span(t.Name),
			})
		}

		if t.Parent != trace.NoTask {
			parent := tr.Task(t.Parent)

			var parentLabel string
			if parent.Name != "" {
				parentLabel = local.Sprintf("Task %d (%s)", t.Parent, parent.Name)
			} else {
				parentLabel = local.Sprintf("Task %d", t.Parent)
			}

			link := *tb.DefaultLink(
				parentLabel,
				"Parent of current task",
				parent)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Parent",
				Value: link,
			})
		}

		parentGoroutineID := goroutineIDForTask(t, tr)

		if parentGoroutineID != 0 && parentGoroutineID != trace.NoGoroutine {
			parentG := tr.G(parentGoroutineID)

			var parentGLabel string

			if parentG.Function != nil {
				parentGLabel = local.Sprintf("Goroutine %d (%s)", parentGoroutineID, parentG.Function.Func)
			} else {
				parentGLabel = local.Sprintf("Goroutine %d", parentGoroutineID)
			}

			link := *tb.DefaultLink(
				parentGLabel,
				"Creator of current task",
				parentG)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Created by",
				Value: link,
			})
		}

		if observedStart {
			link := *tb.DefaultLink(formatTimestamp(nil, tr.AdjustedTime(start)), "Start of current task", start)
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
			link := *tb.DefaultLink(formatTimestamp(nil, tr.AdjustedTime(end)), "End of current task", end)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Ended at",
				Value: link,
			})
		} else {
			link := *tb.DefaultLink("after trace end", "End of trace", end)
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Ended at",
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
				ButtonLabel: "Scroll to task",
				Fn: func() theme.Action {
					return &ScrollToObjectAction{Object: t}
				},
			},

			Zoom: struct {
				ButtonLabel string
				Fn          func() theme.Action
			}{
				ButtonLabel: "Zoom to task",
				Fn: func() theme.Action {
					return &ZoomToObjectAction{Object: t}
				},
			},
		},
		Statistics:         nil,
		DescriptionBuilder: buildDescription,
	}

	tl := canvas.itemToTimeline[t]
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

type TaskList struct {
	Trace         *Trace
	Tasks         SortedIndices[*ptrace.Task, []*ptrace.Task]
	HiddenColumns struct {
		ID        bool
		Name      bool
		StartTime bool
		EndTime   bool
		Duration  bool
	}

	table         *theme.Table
	scrollState   theme.YScrollableListState
	cellFormatter CellFormatter
}

func (evs *TaskList) HoveredLink() ObjectLink {
	return evs.cellFormatter.HoveredLink()
}

func (gl *TaskList) SetTasks(win *theme.Window, gtx layout.Context, tasks []*ptrace.Task) {
	gl.initTable(win, gtx)
	gl.setTasks(gtx, tasks)
}

func (gl *TaskList) setTasks(gtx layout.Context, tasks []*ptrace.Task) {
	gl.Tasks.Reset(tasks)

	switch gl.table.Columns[gl.table.SortedBy].Name {
	case "Task":
		gl.Tasks.Sort(func(gi, gj *ptrace.Task) int {
			return cmp(gi.ID, gj.ID, gl.table.SortOrder == theme.SortDescending)
		})
	case "Name":
		gl.Tasks.Sort(func(gi, gj *ptrace.Task) int {
			return cmp(gi.Name, gj.Name, gl.table.SortOrder == theme.SortDescending)
		})
	case "Start time":
		gl.Tasks.Sort(func(gi, gj *ptrace.Task) int {
			starti := gi.Start.GetOr(-1)
			startj := gj.Start.GetOr(-1)
			return cmp(starti, startj, gl.table.SortOrder == theme.SortDescending)
		})
	case "End time":
		gl.Tasks.Sort(func(gi, gj *ptrace.Task) int {
			endi := gi.End.GetOr(math.MaxInt64)
			endj := gj.End.GetOr(math.MaxInt64)
			return cmp(endi, endj, gl.table.SortOrder == theme.SortDescending)
		})
	case "Duration":
		gl.Tasks.Sort(func(gi, gj *ptrace.Task) int {
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

func (gs *TaskList) initTable(win *theme.Window, gtx layout.Context) {
	if gs.table != nil {
		return
	}
	gs.table = &theme.Table{}
	cols := []theme.Column{}
	if !gs.HiddenColumns.ID {
		cols = append(cols, theme.Column{
			Name:      "Task",
			Alignment: text.End,
			Clickable: true,
		})
	}
	if !gs.HiddenColumns.Name {
		cols = append(cols, theme.Column{
			Name:      "Name",
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
	n := gs.Tasks.Len()
	s := n - 32
	if s < 0 {
		s = 0
	}
	var maxID exptrace.TaskID
	// Look at the last 32 goroutines for this function. This has a high likelyhood of telling us the greatest ID.
	for _, g := range gs.Tasks.Items[s:n] {
		if g.ID > maxID {
			maxID = g.ID
		}
	}
	r0 := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		gtx.Constraints.Min = image.Point{}
		gtx.Constraints.Max = image.Pt(99999, 99999)
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, "Task", win.ColorMaterial(gtx, color.Oklch{}))
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

func (gs *TaskList) Update(gtx layout.Context) {
	gs.table.Update(gtx)
	if _, ok := gs.table.SortByClickedColumn(); ok {
		// Trigger resorting.
		gs.setTasks(gtx, gs.Tasks.Items)
	}
}

func (gs *TaskList) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.TaskList.Layout").End()

	gs.initTable(win, gtx)
	gs.Update(gtx)
	gs.cellFormatter.Update(win, gtx)

	cellFn := func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		t := gs.Tasks.At(row)
		switch colName := gs.table.Columns[col].Name; colName {
		case "Task": // ID
			return gs.cellFormatter.Task(win, gtx, t, "")
		case "Name":
			return gs.cellFormatter.Text(win, gtx, t.Name)
		case "Start time": // Start time
			var l string
			var ts exptrace.Time
			if start, ok := t.Start.Get(); ok {
				ts = start
			} else {
				ts = t.EffectiveStart()
				l = "before trace start"
			}
			return gs.cellFormatter.Timestamp(win, gtx, gs.Trace, ts, l)
		case "End time": // End time
			var l string
			var ts exptrace.Time
			if end, ok := t.End.Get(); ok {
				ts = end
			} else {
				ts = t.EffectiveEnd()
				l = "after trace end"
			}
			return gs.cellFormatter.Timestamp(win, gtx, gs.Trace, ts, l)
		case "Duration": // Duration
			// If the task's end wasn't observed, then traceEnd is equal to the trace's end
			traceEnd := t.EffectiveEnd()

			start, sok := t.Start.Get()
			end, eok := t.End.Get()

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
		gs.Tasks.Len(),
		cellFn,
	)

	return dims
}

type TasksComponent struct {
	list TaskList
}

func NewTasksComponent(tasks []*ptrace.Task, tr *Trace) *TasksComponent {
	return &TasksComponent{
		list: TaskList{
			Trace: tr,
			Tasks: NewSortedIndices(tasks),
		},
	}
}

// Title implements theme.Component.
func (*TasksComponent) Title() string {
	return "Tasks"
}

// Transition implements theme.Component.
func (*TasksComponent) Transition(state theme.ComponentState) {}

// WantsTransition implements theme.Component.
func (*TasksComponent) WantsTransition(gtx layout.Context) theme.ComponentState {
	return theme.ComponentStateNone
}

// Layout implements theme.Component.
func (gc *TasksComponent) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	return gc.list.Layout(win, gtx)
}

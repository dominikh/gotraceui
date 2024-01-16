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
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/text"
)

var stateNames = [ptrace.StateLast]string{
	ptrace.StateInactive:                "inactive",
	ptrace.StateActive:                  "active",
	ptrace.StateGCIdle:                  "GC (idle)",
	ptrace.StateGCDedicated:             "GC (dedicated)",
	ptrace.StateGCFractional:            "GC (fractional)",
	ptrace.StateBlocked:                 "blocked (other)",
	ptrace.StateBlockedSend:             "blocked (channel send)",
	ptrace.StateBlockedRecv:             "blocked (channel receive)",
	ptrace.StateBlockedSelect:           "blocked (select)",
	ptrace.StateBlockedSync:             "blocked (sync)",
	ptrace.StateBlockedSyncOnce:         "blocked (sync.Once)",
	ptrace.StateBlockedSyncTriggeringGC: "blocked (triggering GC)",
	ptrace.StateBlockedCond:             "blocked (sync.Cond)",
	ptrace.StateBlockedNet:              "blocked (pollable I/O)",
	ptrace.StateBlockedGC:               "blocked (GC)",
	ptrace.StateBlockedSyscall:          "blocked (syscall)",
	ptrace.StateStuck:                   "stuck",
	ptrace.StateReady:                   "ready",
	ptrace.StateCreated:                 "created",
	ptrace.StateGCMarkAssist:            "GC (mark assist)",
	ptrace.StateGCSweep:                 "GC (sweep assist)",
	ptrace.StateRunningG:                "active",
	ptrace.StateUserRegion:              "user region",
	ptrace.StateStack:                   "stack frame",
}

var stateNamesCapitalized = [ptrace.StateLast]string{
	ptrace.StateInactive:                "Inactive",
	ptrace.StateActive:                  "Active",
	ptrace.StateGCIdle:                  "GC (idle)",
	ptrace.StateGCDedicated:             "GC (dedicated)",
	ptrace.StateGCFractional:            "GC (fractional)",
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
	ptrace.StateGCMarkAssist:            "GC (mark assist)",
	ptrace.StateGCSweep:                 "GC (sweep assist)",
	ptrace.StateRunningG:                "Active",
	ptrace.StateUserRegion:              "User region",
	ptrace.StateStack:                   "Stack frame",
	ptrace.StateCPUSample:               "Stack frame (sampled)",
}

func goroutineTrack0SpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	span := spans.AtPtr(0)
	state := span.State
	if state == ptrace.StateBlockedSyscall {
		ev := tr.Event(span.Event)
		if ev.StkID != 0 {
			frames := tr.Stacks[ev.StkID]
			fn := tr.PCs[frames[0]].Fn
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
			pid := cv.trace.Event(spans.AtPtr(0).Event).P
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Action: func() theme.Action {
					return &ScrollToObjectAction{
						Object: cv.trace.P(cv.trace.Event(spans.AtPtr(0).Event).P),
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
	// OPT(dh): avoid this allocation
	s := tr.Strings[tr.Events[spans.AtPtr(0).Event].Args[trace.ArgUserRegionTypeID]]
	return append(out, s)
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

	short := shortenFunctionName(f.Fn)

	if short != f.Fn {
		return append(out, f.Fn, "."+short)
	} else {
		// This branch is probably impossible; all functions should be fully qualified.
		return append(out, f.Fn)
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
				label = local.Sprintf("Function: %s\n", f.Fn)
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
	if g.Function.Fn != "" {
		l = local.Sprintf("goroutine %d: %s", g.ID, g.Function.Fn)
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

	if tt.g.Function.Fn != "" {
		fmts = append(fmts, "Goroutine %d: %s\n")
		args = append(args, tt.g.ID, tt.g.Function.Fn)
	} else {
		fmts = append(fmts, "Goroutine %d\n")
		args = append(args, tt.g.ID)
	}

	observedStart := tt.g.Spans[0].State == ptrace.StateCreated
	observedEnd := tt.g.End.Set()
	if observedStart {
		fmts = append(fmts, "Created at: %s")
		args = append(args, formatTimestamp(nil, start))
	} else {
		fmts = append(fmts, "Created at: before trace start")
	}

	if observedEnd {
		fmts = append(fmts, "Returned at: %s")
		args = append(args, formatTimestamp(nil, end))
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

func unblockedByGoroutine(tr *Trace, s *ptrace.Span) (uint64, bool) {
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

func goroutineSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, spans Items[ptrace.Span]) layout.Dimensions {
	var label string
	if debug {
		label += local.Sprintf("Event ID: %d\n", spans.AtPtr(0).Event)
		label += fmt.Sprintf("Event type: %d\n", tr.Event(spans.AtPtr(0).Event).Type)
	}
	label += "State: "
	var at string
	if spans.Len() == 1 {
		s := spans.AtPtr(0)
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
		case ptrace.StateGCFractional:
			label += "GC (fractional)"
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

		tags := spanTagStrings(s.Tags)
		if len(tags) != 0 {
			label += " (" + strings.Join(tags, ", ") + ")"
		}

		if g, ok := unblockedByGoroutine(tr, s); ok {
			label += local.Sprintf("\nUnblocked by goroutine %d (%s)", g, tr.G(g).Function)
		}
	} else {
		label = local.Sprintf("%d spans", spans.Len())
	}
	label += "\n"

	if spans.Len() == 1 {
		if reason := reasonLabels[tr.Reason(spans.AtPtr(0).Event)]; reason != "" {
			label += "Reason: " + reason + "\n"
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
			pid := tr.Event(spans.AtPtr(0).Event).P
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
		ev := tr.Event(s.Event)
		if s.State != ptrace.StateUserRegion {
			panic(fmt.Sprintf("unexpected state %d", s.State))
		}
		if taskID := ev.Args[trace.ArgUserRegionTaskID]; taskID != 0 {
			task := tr.Task(taskID)
			if task.Stub() {
				label = local.Sprintf("User region: %s\nTask: %d\n",
					tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], taskID)
			} else {
				label = local.Sprintf("User region: %s\nTask: %s\n",
					tr.Strings[ev.Args[trace.ArgUserRegionTypeID]], task.Name)
			}
		} else {
			label = local.Sprintf("User region: %s\n",
				tr.Strings[ev.Args[trace.ArgUserRegionTypeID]])
		}
	} else {
		label = local.Sprintf("%d spans\n", spans.Len())
	}
	label += spansDurationForTooltip(spans)
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

var spanStateLabels = [...][]string{
	ptrace.StateInactive:                {"inactive"},
	ptrace.StateActive:                  {"active"},
	ptrace.StateGCIdle:                  {"GC (idle)", "I"},
	ptrace.StateGCDedicated:             {"GC (dedicated)", "D"},
	ptrace.StateGCFractional:            {"GC (fractional)", "F"},
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
	ptrace.StateGCMarkAssist:            {"GC mark assist", "M"},
	ptrace.StateGCSweep:                 {"GC sweep", "S"},
	ptrace.StateLast:                    nil,
}

type stackSpanMeta struct {
	// OPT(dh): should we use 48 bits for the PC and 16 bits for the num?
	pc  uint64
	num int
}

type samplesAndSpansIterator struct {
	trace      *Trace
	g          *ptrace.Goroutine
	cpuSamples []ptrace.EventID
	offSpans   int
	offSamples int
}

func (it *samplesAndSpansIterator) next(advance bool) (evID ptrace.EventID, isSample bool, ok bool) {
	for {
		if it.offSpans == len(it.g.Spans) && it.offSamples == len(it.cpuSamples) {
			return 0, false, false
		}

		evID = 0
		isSample = false
		ok = false

		if it.offSpans < len(it.g.Spans) {
			id := it.g.Spans[it.offSpans].Event
			if it.offSamples < len(it.cpuSamples) {
				oid := it.cpuSamples[it.offSamples]
				if id <= oid {
					if advance {
						it.offSpans++
					}
					evID, isSample, ok = id, false, true
				} else {
					if advance {
						it.offSamples++
					}
					evID, isSample, ok = oid, true, true
				}
			} else {
				if advance {
					it.offSpans++
				}
				evID, isSample, ok = id, false, true
			}
		} else {
			id := it.cpuSamples[it.offSamples]
			if advance {
				it.offSamples++
			}
			evID, isSample, ok = id, true, true
		}

		if !ok || isSample || !advance {
			return
		}

		ev := &it.trace.Events[evID]
		switch ev.Type {
		case trace.EvGoUnblock:
			// The stack is in the goroutine that unblocked this one
			continue
		case trace.EvGoStart:
			// This event doesn't have a stack; display an artificial stack representing time spent on-CPU
			continue
		default:
			return
		}
	}
}

func trimSampleRuntimeFrames(stk []uint64, tr *Trace) []uint64 {
	// CPU samples include two runtime functions at the start of the stack trace that isn't present for stacks
	// collected by the runtime tracer.
	if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Fn == "runtime.goexit" {
		stk = stk[:len(stk)-1]
	}
	if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Fn == "runtime.main" {
		stk = stk[:len(stk)-1]
	}
	return stk
}

func addStackTracks(tl *Timeline, g *ptrace.Goroutine, tr *Trace) {
	if g.Function.Fn == "runtime.bgsweep" {
		// Go <=1.19 has a lot of spans in runtime.bgsweep, but the stacks are utterly uninteresting, containing only a
		// single frame. Save some memory by not creating stack tracks for this goroutine.
		return
	}

	var timeRanges []struct {
		start, end uint64
	}
	it := &samplesAndSpansIterator{
		trace:      tr,
		g:          g,
		cpuSamples: tr.CPUSamples[g.ID],
	}
	processEvent := func(evID ptrace.EventID, isSample bool) {
		ev := &tr.Events[evID]
		stk := tr.Stacks[ev.StkID]

		if isSample {
			stk = trimSampleRuntimeFrames(stk, tr)
		}

		if len(stk) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			stk = stk[:64]
		}

		timeRanges = mem.EnsureLen(timeRanges, len(stk))

		var end trace.Timestamp
		if endEvID, _, ok := it.next(false); ok {
			end = tr.Events[endEvID].Ts
		} else {
			end = g.Spans[len(g.Spans)-1].End
		}
		for i := range stk {
			if uint64(end) > timeRanges[i].end {
				timeRanges[i].end = uint64(end)
			}
			if math.MaxUint64-uint64(ev.Ts) > timeRanges[i].start {
				timeRanges[i].start = math.MaxUint64 - uint64(ev.Ts)
			}
		}
	}
	for {
		evID, isSample, ok := it.next(true)
		if !ok {
			break
		}
		processEvent(evID, isSample)
	}

	for i, tsr := range timeRanges {
		track := &Track{
			parent:     tl,
			kind:       TrackKindStack,
			Start:      trace.Timestamp(math.MaxUint64 - tsr.start),
			End:        trace.Timestamp(tsr.end),
			stackLevel: i,
			spanLabel:  stackSpanLabel,
			// OPT(dh): this allocates a closure for every stack track. it's not even stored in TrackWidget.
			spanTooltip: stackSpanTooltip(i + 1),
			spanColor: func(span *ptrace.Span, tr *Trace) colorIndex {
				if state := span.State; state == statePlaceholder {
					return colorStatePlaceholderStackSpan
				} else {
					return stateColors[state]
				}
			},
			compute: func(track *Track, cancelled <-chan struct{}) Items[ptrace.Span] {
				return computeStackTrack(track, cancelled)
			},
		}
		tl.tracks = append(tl.tracks, track)
	}
}

func computeStackTrack(track *Track, cancelled <-chan struct{}) Items[ptrace.Span] {
	defer rtrace.StartRegion(context.Background(), "main.computeStackTrack").End()

	tr := track.parent.cv.trace
	g := track.parent.item.(*ptrace.Goroutine)

	it := &samplesAndSpansIterator{
		trace:      tr,
		g:          g,
		cpuSamples: tr.CPUSamples[g.ID],
	}

	spans := spanSlicePool.Get()[:0]
	metas := stackSpanMetaSlicePool.Get()[:0]

	var prevFn string
	i := 0
	for {
		if i%20000 == 0 && TryRecv(cancelled) {
			return nil
		}
		evID, isSample, ok := it.next(true)
		if !ok {
			break
		}

		ev := &tr.Events[evID]
		stk := tr.Stacks[ev.StkID]
		state := ptrace.StateStack
		if isSample {
			state = ptrace.StateCPUSample
			stk = trimSampleRuntimeFrames(stk, tr)
		}

		if len(stk) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			stk = stk[:64]
		}

		var end trace.Timestamp
		if endEvID, _, ok := it.next(false); ok {
			end = tr.Events[endEvID].Ts
		} else {
			end = g.Spans[len(g.Spans)-1].End
		}

		if track.stackLevel >= len(stk) {
			continue
		}
		pc := stk[len(stk)-1-track.stackLevel]

		if len(spans) != 0 {
			// This isn't the first span. Check if we should merge this stack into the previous span.
			prevEnd := trace.Timestamp(last(spans).End)
			prevState := last(spans).State
			fn := tr.PCs[pc].Fn
			if prevEnd == tr.Events[evID].Ts && prevFn == fn && state == prevState {
				// This is a continuation of the previous span. Merging these can have massive memory usage savings,
				// which is why we do it here and not during display.
				//
				// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
				// data more apparent.
				lastPtr(spans).End = end
				if state == ptrace.StateCPUSample {
					lastPtr(metas).num++
				}
			} else {
				// This is a new span
				spans = append(spans, ptrace.Span{
					Start: ev.Ts,
					End:   end,
					Event: evID,
					State: state,
				})
				metas = append(metas, stackSpanMeta{
					pc:  pc,
					num: 1,
				})
				prevFn = tr.PCs[pc].Fn
			}
		} else {
			// This is the first span
			spans = append(spans, ptrace.Span{
				Start: ev.Ts,
				End:   end,
				Event: evID,
				State: state,
			})
			metas = append(metas, stackSpanMeta{
				pc:  pc,
				num: 1,
			})
			prevFn = tr.PCs[pc].Fn
		}
	}

	return SimpleItems[ptrace.Span, stackSpanMeta]{
		items: spans,
		container: ItemContainer{
			Timeline: track.parent,
			Track:    track,
		},
		metas: metas,
	}
}

func NewGoroutineInfo(tr *Trace, mwin *theme.Window, canvas *Canvas, g *ptrace.Goroutine, allTimelines []*Timeline) *SpansInfo {
	var title string
	if g.Function.Fn != "" {
		title = local.Sprintf("goroutine %d: %s", g.ID, g.Function)
	} else {
		title = local.Sprintf("goroutine %d", g.ID)
	}

	spans := g.Spans

	var stacktrace string
	if spans[0].State == ptrace.StateCreated {
		ev := tr.Events[spans[0].Event]
		stk := tr.Stacks[ev.StkID]
		sb := strings.Builder{}
		for _, f := range stk {
			frame := tr.PCs[f]
			fmt.Fprintf(&sb, "%s\n        %s:%d\n", frame.Fn, frame.File, frame.Line)
		}
		stacktrace = sb.String()
		if len(stacktrace) > 0 && stacktrace[len(stacktrace)-1] == '\n' {
			stacktrace = stacktrace[:len(stacktrace)-1]
		}
	}

	buildDescription := func(win *theme.Window, gtx layout.Context) Description {
		var attrs []DescriptionAttribute
		// OPT(dh): we don't need TextBuilder to collect the spans in this case.
		tb := TextBuilder{Window: win}

		start := spans[0].Start
		end := spans[len(spans)-1].End
		d := time.Duration(end - start)
		observedStart := spans[0].State == ptrace.StateCreated
		observedEnd := g.End.Set()

		if g.Parent != 0 {
			parent := tr.G(g.Parent)

			if parent.Function != nil {
				link := *tb.DefaultLink(
					local.Sprintf("Goroutine %d (%s)", g.Parent, parent.Function.Fn),
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

		link := *tb.DefaultLink(g.Function.Fn, "Function of current goroutine", g.Function)
		attrs = append(attrs, DescriptionAttribute{
			Key:   "Function",
			Value: link,
		})

		if observedStart {
			link := *tb.DefaultLink(formatTimestamp(nil, start), "Start of current goroutine", start)
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
			link := *tb.DefaultLink(formatTimestamp(nil, end), "End of current goroutine", end)
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
				fn1 = gi.Function.Fn
			}
			if gj.Function != nil {
				fn2 = gj.Function.Fn
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
	s := n - 32
	if s < 0 {
		s = 0
	}
	var maxID uint64
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
			var ts trace.Timestamp
			if start, ok := g.Start.Get(); ok {
				ts = start
			} else {
				ts = g.EffectiveStart()
				l = "before trace start"
			}
			return gs.cellFormatter.Timestamp(win, gtx, ts, l)
		case "End time": // End time
			var l string
			var ts trace.Timestamp
			if end, ok := g.End.Get(); ok {
				ts = end
			} else {
				ts = g.EffectiveEnd()
				l = "after trace end"
			}
			return gs.cellFormatter.Timestamp(win, gtx, ts, l)
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

func NewGoroutinesComponent(gs []*ptrace.Goroutine) *GoroutinesComponent {
	return &GoroutinesComponent{
		list: GoroutineList{
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
	if g.Function != nil && g.Function.Fn != "" {
		return local.Sprintf("%d (%s)", g.ID, g.Function.Fn)
	} else {
		return local.Sprintf("%d", g.ID)
	}
}

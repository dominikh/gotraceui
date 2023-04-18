package main

import (
	"context"
	"fmt"
	rtrace "runtime/trace"
	"strings"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
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
	ptrace.StateDone:                    "done",
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
	ptrace.StateDone:                    "Done",
	ptrace.StateGCMarkAssist:            "GC (mark assist)",
	ptrace.StateGCSweep:                 "GC (sweep assist)",
	ptrace.StateRunningG:                "Active",
	ptrace.StateUserRegion:              "User region",
	ptrace.StateStack:                   "Stack frame",
}

func goroutineTrack0SpanLabel(spanSel SpanSelector, tr *Trace, out []string) []string {
	if spanSel.Size() != 1 {
		return out
	}
	span := spanSel.At(0)
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

func goroutineTrack0SpanContextMenu(spanSel SpanSelector, cv *Canvas) []*theme.MenuItem {
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spanSel))

	if spanSel.Size() == 1 {
		switch spanSel.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCFractional, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
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
		label += fmt.Sprintf("Duration: up to %s", roundDuration(SpansDuration(state.spanSel)))
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
		item:      g,
		label:     l,
		shortName: shortName,
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

		tags := spanTagStrings(s.Tags)
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

	if LastSpan(state.spanSel).State != ptrace.StateDone {
		label += fmt.Sprintf("Duration: %s\n", roundDuration(SpansDuration(state.spanSel)))
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
	label += fmt.Sprintf("Duration: %s", roundDuration(SpansDuration(state.spanSel)))
	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

var spanStateLabels = [...][]string{
	ptrace.StateInactive: {"inactive"},
	// StateActive isn't needed, those spans have custom labels
	ptrace.StateActive:                  {},
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

func goroutineLinkContextMenu(mwin *MainWindow, obj *ptrace.Goroutine) []*theme.MenuItem {
	return []*theme.MenuItem{
		{
			Label: PlainLabel("Scroll to goroutine"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindScroll})
			},
		},
		{
			Label: PlainLabel("Zoom to goroutine"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindZoom})
			},
		},
		{
			Label: PlainLabel("Show goroutine information"),
			Do: func(gtx layout.Context) {
				mwin.OpenLink(&GoroutineLink{Goroutine: obj, Kind: GoroutineLinkKindOpen})
			},
		},
	}
}

func NewGoroutineInfo(mwin *MainWindow, g *ptrace.Goroutine) *SpansInfo {
	var title string
	if g.Function.Fn != "" {
		title = local.Sprintf("goroutine %d: %s", g.ID, g.Function)
	} else {
		title = local.Sprintf("goroutine %d", g.ID)
	}

	spans := SliceToSpanSelector(g.Spans)

	var stacktrace string
	tr := mwin.trace
	if spans.At(0).State == ptrace.StateCreated {
		ev := tr.Events[spans.At(0).Event]
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

	cfg := SpansInfoConfig{
		Title:      title,
		Stacktrace: stacktrace,
		Navigations: SpansInfoConfigNavigations{
			ScrollLabel: "Scroll to goroutine",
			ScrollFn: func() Link {
				return &GoroutineLink{
					Goroutine: g,
					Kind:      GoroutineLinkKindScroll,
				}
			},

			ZoomLabel: "Zoom to goroutine",
			ZoomFn: func() Link {
				return &GoroutineLink{
					Goroutine: g,
					Kind:      GoroutineLinkKindZoom,
				}
			},
		},
		Statistics: theme.NewFuture(mwin.twin, func(cancelled <-chan struct{}) *SpansStats {
			return NewGoroutineStats(g)
		}),
		Description: func(text *Text) {
			start := spans.At(0).Start
			end := LastSpan(spans).End
			d := time.Duration(end - start)
			observedStart := spans.At(0).State == ptrace.StateCreated
			observedEnd := LastSpan(spans).State == ptrace.StateDone

			text.Bold("Goroutine: ")
			text.Span(local.Sprintf("%d\n", g.ID))

			text.Bold("Function: ")
			text.Link(fmt.Sprintf("%s\n", g.Function.Fn), g.Function)

			if observedStart {
				text.Bold("Created at: ")
				text.Link(
					fmt.Sprintf("%s\n", formatTimestamp(start)),
					start,
				)
			} else {
				text.Bold("Created at: ")
				text.Link(
					"before trace start\n",
					start,
				)
			}

			if observedEnd {
				text.Bold("Returned at: ")
				text.Link(
					fmt.Sprintf("%s\n", formatTimestamp(end)),
					end,
				)
			} else {
				text.Bold("Returned at: ")
				text.Link(
					"after trace end\n",
					end,
				)
			}

			if observedStart && observedEnd {
				text.Bold("Lifetime: ")
				text.Span(d.String())
			} else {
				text.Bold("Observed duration: ")
				text.Span(d.String())
			}
		},
	}

	return NewSpansInfo(cfg, mwin, spans, g.Events)
}

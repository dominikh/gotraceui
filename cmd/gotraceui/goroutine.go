package main

import (
	"context"
	"fmt"
	rtrace "runtime/trace"
	"strings"
	"time"
	"unsafe"

	"golang.org/x/exp/slices"
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

func goroutineTrack0SpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	span := spans.At(0)
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
	var items []*theme.MenuItem
	items = append(items, newZoomMenuItem(cv, spans))

	if spans.Len() == 1 {
		switch spans.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCFractional, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			// These are the states that are actually on-CPU
			pid := cv.trace.Event((spans.At(0).Event)).P
			items = append(items, &theme.MenuItem{
				Label: PlainLabel(local.Sprintf("Scroll to processor %d", pid)),
				Do: func(gtx layout.Context) {
					cv.scrollToTimeline(gtx, cv.trace.P(cv.trace.Event((spans.At(0).Event)).P))
				},
			})

		case ptrace.StateBlocked, ptrace.StateBlockedSend, ptrace.StateBlockedRecv, ptrace.StateBlockedSelect, ptrace.StateBlockedSync,
			ptrace.StateBlockedSyncOnce, ptrace.StateBlockedSyncTriggeringGC, ptrace.StateBlockedCond, ptrace.StateBlockedNet, ptrace.StateBlockedGC:
			gid, ok := unblockedByGoroutine(cv.trace, spans.At(0))
			if ok {
				items = append(items, &theme.MenuItem{
					Label: PlainLabel(local.Sprintf("Scroll to unblocking goroutine %d", gid)),
					Do: func(gtx layout.Context) {
						gid, _ := unblockedByGoroutine(cv.trace, spans.At(0))
						cv.scrollToTimeline(gtx, cv.trace.G(gid))
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
	s := tr.Strings[tr.Events[spans.At(0).Event].Args[trace.ArgUserRegionTypeID]]
	return append(out, s)
}

func stackSpanLabel(spans Items[ptrace.Span], tr *Trace, out []string) []string {
	if spans.Len() != 1 {
		return out
	}
	pc := spans.(MetadataSpans[stackSpanMeta]).MetadataAt(0).pc
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
		if state.spans.Len() == 1 {
			meta := state.spans.(MetadataSpans[stackSpanMeta]).MetadataAt(0)
			pc := meta.pc
			f := tr.PCs[pc]
			label = local.Sprintf("Function: %s\n", f.Fn)
			// TODO(dh): for truncated stacks we should display a relative depth instead
			label += local.Sprintf("Call depth: %d\n", level)
			if state.spans.At(0).State == ptrace.StateCPUSample {
				label += local.Sprintf("Samples: %d\n", meta.num)
			}
		} else {
			label = local.Sprintf("mixed (%d spans)\n", state.spans.Len())
		}
		// We round the duration, in addition to saying "up to", to make it more obvious that the
		// duration is a guess
		//
		// TODO(dh): don't do this for the stacks of blocking events, we know their exact duration
		label += fmt.Sprintf("Duration: up to %s", roundDuration(SpansDuration(state.spans)))
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
		buildTrackWidgets: func(tracks []*Track) {
			stackTrackBase := -1
			for i, track := range tracks {
				i := i

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

	track := NewTrack(tl, TrackKindUnspecified)
	track.spans_ = SimpleItems[ptrace.Span]{
		items: g.Spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    track,
		},
		subslice: true,
	}
	track.events = g.Events
	tl.tracks = []*Track{track}

	for _, ug := range g.UserRegions {
		track := NewTrack(tl, TrackKindUserRegions)
		track.events = tl.tracks[0].events
		track.hideEventMarkers = true
		track.spans_ = SimpleItems[ptrace.Span]{
			items: ug,
			container: ItemContainer{
				Timeline: tl,
				Track:    track,
			},
			subslice: true,
		}
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

	start := tt.g.Spans[0].Start
	end := tt.g.Spans[len(tt.g.Spans)-1].End
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
		label += local.Sprintf("Event ID: %d\n", state.spans.At(0).Event)
		label += fmt.Sprintf("Event type: %d\n", tr.Event(state.spans.At(0).Event).Type)
	}
	label += "State: "
	var at string
	if state.spans.Len() == 1 {
		s := state.spans.At(0)
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
		label += local.Sprintf("mixed (%d spans)", state.spans.Len())
	}
	label += "\n"

	if state.spans.Len() == 1 {
		if reason := reasonLabels[tr.Reason(state.spans.At(0))]; reason != "" {
			label += "Reason: " + reason + "\n"
		}
	}

	if at != "" {
		// TODO(dh): document what In represents. If possible, it is the last frame in user space that triggered this
		// state. We try to pattern match away the runtime when it makes sense.
		label += fmt.Sprintf("In: %s\n", at)
	}
	if state.spans.Len() == 1 {
		switch state.spans.At(0).State {
		case ptrace.StateActive, ptrace.StateGCIdle, ptrace.StateGCDedicated, ptrace.StateGCMarkAssist, ptrace.StateGCSweep:
			pid := tr.Event(state.spans.At(0).Event).P
			label += local.Sprintf("On: processor %d\n", pid)
		}
	}

	if LastSpan(state.spans).State != ptrace.StateDone {
		label += fmt.Sprintf("Duration: %s\n", roundDuration(SpansDuration(state.spans)))
	}

	if state.events.Len() > 0 {
		label += local.Sprintf("Events in span: %d\n", state.events.Len())
	}

	if state.eventsUnderCursor.Len() > 0 {
		kind := tr.Event(state.eventsUnderCursor.At(0)).Type
		for i := 1; i < state.eventsUnderCursor.Len(); i++ {
			ev := state.eventsUnderCursor.At(i)
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
				if state.eventsUnderCursor.Len() == 1 {
					stk := tr.Stacks[tr.Event(state.eventsUnderCursor.At(0)).StkID]
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
			label += local.Sprintf("Events under cursor: %d %s\n", state.eventsUnderCursor.Len(), noun)
		} else {
			label += local.Sprintf("Events under cursor: %d\n", state.eventsUnderCursor.Len())
		}
	}

	if n := len(label) - 1; label[n] == '\n' {
		label = label[:n]
	}

	return theme.Tooltip(win.Theme, label).Layout(win, gtx)
}

func userRegionSpanTooltip(win *theme.Window, gtx layout.Context, tr *Trace, state SpanTooltipState) layout.Dimensions {
	var label string
	if state.spans.Len() == 1 {
		s := state.spans.At(0)
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
		label = local.Sprintf("mixed (%d spans)\n", state.spans.Len())
	}
	label += fmt.Sprintf("Duration: %s", roundDuration(SpansDuration(state.spans)))
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

	type track struct {
		startsEnds  []uint64
		eventIDs    []uint64
		pcs         []uint64
		nums        []uint64
		isCPUSample []bool
		prevFn      string
	}
	var tracks []track
	grow := func(n int) {
		if len(tracks) < n {
			tracks = slices.Grow(tracks, n-len(tracks))[:n]
		}
	}
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

		grow(len(stk))
		var end trace.Timestamp
		if endEvID, _, ok := nextEvent(false); ok {
			end = tr.Events[endEvID].Ts
		} else {
			end = g.Spans[len(g.Spans)-1].End
		}

		for i := 0; i < len(stk); i++ {
			track := &tracks[i]
			if track.eventIDs == nil {
				// We tried encoding values incrementally, but this had worse memory usage because of slice
				// growth in append.
				n := len(g.Spans) + len(cpuSamples)
				track.startsEnds = uint64SliceCache.Get(2 * n)
				track.eventIDs = uint64SliceCache.Get(n)
				track.isCPUSample = boolSliceCache.Get(n)
				track.pcs = uint64SliceCache.Get(n)
				track.nums = uint64SliceCache.Get(n)
			}
			if len(track.startsEnds) != 0 {
				// This isn't the first span. Check if we should merge this stack into the previous span.
				prevFn := track.prevFn
				prevEnd := trace.Timestamp(last(track.startsEnds))
				var prevState ptrace.SchedulingState
				if last(track.isCPUSample) {
					prevState = ptrace.StateCPUSample
				} else {
					prevState = ptrace.StateStack
				}
				fn := tr.PCs[stk[len(stk)-i-1]].Fn
				if prevEnd == tr.Events[evID].Ts && prevFn == fn && state == prevState {
					// This is a continuation of the previous span. Merging these can have massive memory usage savings,
					// which is why we do it here and not during display.
					//
					// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
					// data more apparent.
					*lastPtr(track.startsEnds) = uint64(end)
					if state == ptrace.StateCPUSample {
						*lastPtr(track.nums)++
					}
				} else {
					// This is a new span
					track.startsEnds = append(track.startsEnds, uint64(ev.Ts), uint64(end))
					track.eventIDs = append(track.eventIDs, uint64(evID))
					track.isCPUSample = append(track.isCPUSample, state == ptrace.StateCPUSample)
					track.pcs = append(track.pcs, stk[len(stk)-i-1])
					track.nums = append(track.nums, 1)
					track.prevFn = fn
				}
			} else {
				// This is the first span
				track.startsEnds = append(track.startsEnds, uint64(ev.Ts), uint64(end))
				track.eventIDs = append(track.eventIDs, uint64(evID))
				track.isCPUSample = append(track.isCPUSample, state == ptrace.StateCPUSample)
				track.pcs = append(track.pcs, stk[len(stk)-i-1])
				track.nums = append(track.nums, 1)
				track.prevFn = tr.PCs[stk[len(stk)-i-1]].Fn
			}
		}
	}

	bitpack := func(bs []bool) []uint64 {
		out := make([]uint64, (len(bs)+63)/64)
		for i, b := range bs {
			out[i/64] |= uint64(*(*byte)(unsafe.Pointer(&b))) << (i % 64)
		}
		return out
	}

	dup := func(in []uint64) []uint64 {
		out := make([]uint64, len(in))
		copy(out, in)
		uint64SliceCache.Put(in)
		return out
	}
	stackTracks := make([]*Track, len(tracks))
	for i, track := range tracks {
		deltaZigZagEncode(track.startsEnds)
		deltaZigZagEncode(track.eventIDs)
		deltaZigZagEncode(track.pcs)
		deltaZigZagEncode(track.nums)

		tr := NewTrack(tl, TrackKindStack)
		tr.compressedSpans = compressedStackSpans{
			count: len(track.eventIDs),
			// We can encode in place because n >= 1 values are consumed to produce one value of output.
			// This lets us avoid runtime.growslice. Instead, we copy the slice once when we're done.
			startsEnds:  dup(Encode(track.startsEnds, track.startsEnds[:0])),
			eventIDs:    dup(Encode(track.eventIDs, track.eventIDs[:0])),
			pcs:         dup(Encode(track.pcs, track.pcs[:0])),
			nums:        dup(Encode(track.nums, track.nums[:0])),
			isCPUSample: bitpack(track.isCPUSample),
		}
		boolSliceCache.Put(track.isCPUSample)
		stackTracks[i] = tr
	}

	tl.tracks = append(tl.tracks, stackTracks...)
}

func NewGoroutineInfo(tr *Trace, mwin MainWindowIface, canvas *Canvas, g *ptrace.Goroutine, allTimelines []*Timeline) *SpansInfo {
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
		tb := TextBuilder{Theme: win.Theme}

		start := spans[0].Start
		end := spans[len(spans)-1].End
		d := time.Duration(end - start)
		observedStart := spans[0].State == ptrace.StateCreated
		observedEnd := spans[len(spans)-1].State == ptrace.StateDone

		if g.Parent != 0 {
			parent := tr.G(g.Parent)

			if parent.Function != nil {
				attrs = append(attrs, DescriptionAttribute{
					Key:   "Parent",
					Value: *tb.DefaultLink(local.Sprintf("Goroutine %d (%s)\n", g.Parent, parent.Function.Fn), parent),
				})
			} else {
				attrs = append(attrs, DescriptionAttribute{
					Key:   "Parent",
					Value: *tb.DefaultLink(local.Sprintf("Goroutine %d\n", g.Parent), parent),
				})
			}
		}

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Goroutine",
			Value: *tb.Span(local.Sprintf("%d\n", g.ID)),
		})

		attrs = append(attrs, DescriptionAttribute{
			Key:   "Function",
			Value: *(tb.DefaultLink(g.Function.Fn, g.Function)),
		})

		if observedStart {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Created at",
				Value: *(tb.DefaultLink(formatTimestamp(start), start)),
			})
		} else {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Created at",
				Value: *(tb.DefaultLink("before trace start", start)),
			})
		}

		if observedEnd {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Returned at",
				Value: *(tb.DefaultLink(formatTimestamp(end), end)),
			})
		} else {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Returned at",
				Value: *(tb.DefaultLink("after trace end", end)),
			})
		}

		if observedStart && observedEnd {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Lifetime",
				Value: *(tb.Span(d.String())),
			})
		} else {
			attrs = append(attrs, DescriptionAttribute{
				Key:   "Observed duration",
				Value: *(tb.Span(d.String())),
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
			ScrollLabel: "Scroll to goroutine",
			ScrollFn: func() Link {
				return (*ScrollToGoroutineLink)(g)
			},

			ZoomLabel: "Zoom to goroutine",
			ZoomFn: func() Link {
				return (*ZoomToGoroutineLink)(g)
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
	ss := SimpleItems[ptrace.Span]{
		items: spans,
		container: ItemContainer{
			Timeline: tl,
			Track:    tl.tracks[0],
		},
		subslice: true,
	}
	return NewSpansInfo(cfg, tr, mwin, theme.Immediate[Items[ptrace.Span]](ss), allTimelines)
}

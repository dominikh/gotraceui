// Package ptrace processes a runtime trace and enriches it with additional information.
package ptrace

import (
	"errors"
	"fmt"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"

	"golang.org/x/exp/slices"
	"honnef.co/go/gotraceui/trace"
)

// This boolean guards all code involving displaying machine timelines. That feature is currently broken, because the
// trace parser only produces an event ordering that is consistent for goroutines, but not for machines. For example, it
// may try to start a P on an M that is currently blocked on a syscall.
const supportMachineTimelines = false

type SchedulingState uint8

const (
	StateNone SchedulingState = iota

	// Goroutine states
	StateInactive
	StateActive
	StateGCIdle
	StateGCDedicated
	StateBlocked
	StateBlockedSend
	StateBlockedRecv
	StateBlockedSelect
	StateBlockedSync
	StateBlockedSyncOnce
	StateBlockedSyncTriggeringGC
	StateBlockedCond
	StateBlockedNet
	StateBlockedGC
	StateBlockedSyscall
	StateStuck
	StateReady
	StateCreated
	StateDone
	StateGCMarkAssist
	StateGCSweep

	// Special states used by user regions and CPU sampling
	StateUserRegion
	StateCPUSample

	// Processor states
	StateRunningG

	// Machine states
	StateRunningP

	StateLast
)

type Trace struct {
	// OPT(dh): can we get rid of all these pointers?
	Goroutines []*Goroutine
	Processors []*Processor
	Machines   []*Machine
	GC         Spans
	STW        Spans
	Tasks      []*Task
	// Mapping from Goroutine ID to list of CPU sample events
	CPUSamples map[uint64][]EventID

	gsByID        map[uint64]*Goroutine
	HasCPUSamples bool

	trace.ParseResult
}

type Goroutine struct {
	ID uint64
	// Sequential ID of goroutine in the trace
	SeqID       int
	Function    string
	Spans       Spans
	UserRegions []Spans
	Events      []EventID

	Statistics struct {
		Blocked, Inactive, Running, GCAssist             time.Duration
		BlockedPct, InactivePct, RunningPct, GCAssistPct float32
	}
}

type Machine struct {
	ID int32
	// Sequential ID of machine in the trace
	SeqID int
	// OPT(dh): using Span for Ms is wasteful. We don't need tags, stacktrace offsets etc. We only care about what
	// processor is running at what time. The only benefit of reusing Span is that we can use the same code for
	// rendering Gs and Ms, but that doesn't seem worth the added cost.
	Spans      Spans
	Goroutines Spans
}

type Processor struct {
	ID int32
	// Sequential ID of processor in the trace
	SeqID int
	// OPT(dh): using Span for Ps is wasteful. We don't need tags, stacktrace offsets etc. We only care about what
	// goroutine is running at what time. The only benefit of reusing Span is that we can use the same code for
	// rendering Gs and Ps, but that doesn't seem worth the added cost.
	Spans Spans

	// Labels used for spans representing this processor
	// spanLabels []string
	// Strings used for matching timeline filters
	// filterLabels []string
}

type Task struct {
	// OPT(dh): Technically we only need the EventID field, everything else can be extracted from the event on demand.
	// But there are probably not enough tasks to make that worth it.
	ID uint64
	// Sequential ID of task in the trace
	SeqID int
	Name  string
	Event EventID
}

type Span struct {
	// The Span type is carefully laid out to optimize its size and to avoid pointers, the latter so that the garbage
	// collector won't have to scan any memory of our millions of events. It is currently 32 bytes large, with no padding.
	//
	// Instead of pointers, fields like pc and event_ are indices into slices. event_ is a uint40, allowing for a total
	// of 1 trillion events, or 64 TiB worth of events. This size was chosen to eliminate padding.

	Start trace.Timestamp
	End   trace.Timestamp
	// Stack frame this span represents, for sample tracks
	PC    uint64
	Event EventID
	// At is an offset from the top of the stack, skipping over uninteresting runtime frames.
	At uint8
	// We track the scheduling State explicitly, instead of mapping from trace.Event.Type, because we apply pattern
	// matching to stack traces that may result in more accurate states. For example, we can determine
	// stateBlockedSyncOnce from the stack trace, and we would otherwise use stateBlockedSync.
	State SchedulingState
	Tags  SpanTags
}
type EventID uint64

func (g *Goroutine) computeStatistics() {
	start := g.Spans.Start()
	end := g.Spans.End()
	d := time.Duration(end - start)

	var blocked, inactive, running, gcAssist time.Duration
	for i := range g.Spans {
		s := &g.Spans[i]
		d := s.Duration()
		switch s.State {
		case StateInactive:
			inactive += d
		case StateActive, StateGCDedicated, StateGCIdle:
			running += d
		case StateBlocked:
			blocked += d
		case StateBlockedSend:
			blocked += d
		case StateBlockedRecv:
			blocked += d
		case StateBlockedSelect:
			blocked += d
		case StateBlockedSync:
			blocked += d
		case StateBlockedSyncOnce:
			blocked += d
		case StateBlockedSyncTriggeringGC:
			blocked += d
		case StateBlockedCond:
			blocked += d
		case StateBlockedNet:
			blocked += d
		case StateBlockedGC:
			blocked += d
		case StateBlockedSyscall:
			blocked += d
		case StateStuck:
			blocked += d
		case StateReady:
			inactive += d
		case StateCreated:
			inactive += d
		case StateGCMarkAssist:
			gcAssist += d
		case StateGCSweep:
			gcAssist += d
		case StateDone:
		default:
			if debug {
				panic(fmt.Sprintf("unknown state %d", s.State))
			}
		}
	}

	g.Statistics = struct {
		Blocked     time.Duration
		Inactive    time.Duration
		Running     time.Duration
		GCAssist    time.Duration
		BlockedPct  float32
		InactivePct float32
		RunningPct  float32
		GCAssistPct float32
	}{
		Blocked:  blocked,
		Inactive: inactive,
		Running:  running,
		GCAssist: gcAssist,

		BlockedPct:  float32(blocked) / float32(d) * 100,
		InactivePct: float32(inactive) / float32(d) * 100,
		RunningPct:  float32(running) / float32(d) * 100,
		GCAssistPct: float32(gcAssist) / float32(d) * 100,
	}
}

func Parse(res trace.ParseResult, progress func(float64)) (*Trace, error) {
	var ms []*Machine
	var gs []*Goroutine
	var ps []*Processor
	var gc Spans
	var stw Spans
	var tasks []*Task
	var hasCPUSamples bool

	var evTypeToState = [...]SchedulingState{
		trace.EvGoBlockSend:   StateBlockedSend,
		trace.EvGoBlockRecv:   StateBlockedRecv,
		trace.EvGoBlockSelect: StateBlockedSelect,
		trace.EvGoBlockSync:   StateBlockedSync,
		trace.EvGoBlockCond:   StateBlockedCond,
		trace.EvGoBlockNet:    StateBlockedNet,
		trace.EvGoBlockGC:     StateBlockedGC,
		trace.EvGoBlock:       StateBlocked,
	}

	gsByID := map[uint64]*Goroutine{}
	getG := func(gid uint64) *Goroutine {
		g, ok := gsByID[gid]
		if ok {
			return g
		}
		g = &Goroutine{ID: gid}
		gsByID[gid] = g
		return g
	}
	psByID := map[int32]*Processor{}
	getP := func(pid int32) *Processor {
		p, ok := psByID[pid]
		if ok {
			return p
		}
		p = &Processor{
			ID: pid,
			// spanLabels: []string{local.Sprintf("p%d", pid)},
		}
		psByID[pid] = p
		return p
	}
	msByID := map[int32]*Machine{}
	getM := func(mid int32) *Machine {
		if !supportMachineTimelines {
			panic("getM was called despite supportmachineActivities == false")
		}
		m, ok := msByID[mid]
		if ok {
			return m
		}
		m = &Machine{ID: mid}
		msByID[mid] = m
		return m
	}

	// map from gid to stack ID
	lastSyscall := map[uint64]uint32{}
	// map from P to last M it ran on
	lastMPerP := map[int32]int32{}
	// set of gids currently in mark assist
	inMarkAssist := map[uint64]struct{}{}
	blockingSyscallPerP := map[int32]EventID{}
	blockingSyscallMPerG := map[uint64]int32{}
	// map from gid to CPU samples
	cpuSamples := map[uint64][]EventID{}

	// FIXME(dh): rename function. or remove it alright
	addEventToCurrentSpan := func(gid uint64, ev EventID) {
		if gid == 0 {
			// FIXME(dh): figure out why we have events for g0 when there are no spans on g0.
			return
		}
		g := getG(gid)
		g.Events = append(g.Events, ev)
	}

	// Count the number of events per goroutine to get an estimate of spans per goroutine, to preallocate slices.
	eventsPerG := map[uint64]int{}
	eventsPerP := map[int32]int{}
	eventsPerM := map[int32]int{}
	for evID := range res.Events {
		ev := &res.Events[evID]
		var gid uint64
		switch ev.Type {
		case trace.EvGoCreate, trace.EvGoUnblock:
			gid = ev.Args[0]
		case trace.EvGoStart, trace.EvGoStartLabel:
			eventsPerP[ev.P]++
			gid = ev.G
		case trace.EvProcStart:
			if supportMachineTimelines {
				eventsPerM[int32(ev.Args[0])]++
			}
			continue
		case trace.EvGCStart, trace.EvGCSTWStart, trace.EvGCDone, trace.EvGCSTWDone,
			trace.EvHeapAlloc, trace.EvHeapGoal, trace.EvGomaxprocs, trace.EvUserTaskCreate,
			trace.EvUserTaskEnd, trace.EvUserRegion, trace.EvUserLog, trace.EvCPUSample,
			trace.EvProcStop, trace.EvGoSysCall:
			continue
		default:
			gid = ev.G
		}
		eventsPerG[gid]++
	}
	for gid, n := range eventsPerG {
		getG(gid).Spans = make(Spans, 0, n)
	}
	for pid, n := range eventsPerP {
		getP(pid).Spans = make(Spans, 0, n)
	}
	if supportMachineTimelines {
		for mid, n := range eventsPerM {
			getM(mid).Spans = make(Spans, 0, n)
		}
	}

	userRegionDepths := map[uint64]int{}
	for evID := range res.Events {
		ev := &res.Events[evID]
		if evID%10000 == 0 {
			progress(float64(evID) / float64(len(res.Events)))
		}
		var gid uint64
		var state SchedulingState
		var pState int

		const (
			pNone = iota
			pRunG
			pStopG
		)

		switch ev.Type {
		case trace.EvGoCreate:
			// ev.G creates ev.Args[0]
			if ev.G != 0 {
				addEventToCurrentSpan(ev.G, EventID(evID))
			}
			gid = ev.Args[trace.ArgGoCreateG]
			if stkID := ev.Args[trace.ArgGoCreateStack]; stkID != 0 {
				stack := res.Stacks[uint32(stkID)]
				if len(stack) != 0 {
					getG(gid).Function = res.PCs[stack[0]].Fn
				}
			}
			// FIXME(dh): when tracing starts after goroutines have already been created then we receive an EvGoCreate
			// for them. But those goroutines may not necessarily be in a non-running state. We do receive EvGoWaiting
			// and EvGoInSyscall for goroutines that are blocked or in a syscall when tracing starts; does that mean
			// that any goroutine that doesn't receive this event is currently running? If so we'd have to detect which
			// goroutines receive neither EvGoWaiting or EvGoInSyscall, and which were already running.
			//
			// EvGoWaiting is emitted when we're in _Gwaiting, and EvGoInSyscall when we're in _Gsyscall. Critically
			// this doesn't cover _Gidle and _Grunnable, which means we don't know if it's running or waiting to run. If
			// there's another event then we can deduce it (we can't go from _Grunnable to _Gblocked, for example), but
			// if there are no more events, then we cannot tell if the goroutine was always running or always runnable.
			state = StateCreated
		case trace.EvGoStart:
			// ev.G starts running
			gid = ev.G
			pState = pRunG

			if _, ok := inMarkAssist[gid]; ok {
				state = StateGCMarkAssist
			} else {
				state = StateActive
			}
		case trace.EvGoStartLabel:
			// ev.G starts running
			// TODO(dh): make use of the label
			gid = ev.G
			pState = pRunG
			state = StateActive

			switch res.Strings[ev.Args[trace.ArgGoStartLabelLabelID]] {
			case "GC (dedicated)":
				state = StateGCDedicated
			case "GC (idle)":
				state = StateGCIdle
			}
		case trace.EvGoStop:
			// ev.G is stopping
			gid = ev.G
			pState = pStopG
			state = StateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			gid = ev.G
			pState = pStopG
			state = StateDone
		case trace.EvGoSched:
			// ev.G calls Gosched
			gid = ev.G
			pState = pStopG
			state = StateInactive
		case trace.EvGoSleep:
			// ev.G calls Sleep
			gid = ev.G
			pState = pStopG
			state = StateInactive
		case trace.EvGoPreempt:
			// ev.G got preempted
			gid = ev.G
			pState = pStopG
			state = StateReady
		case trace.EvGoBlockSend, trace.EvGoBlockRecv, trace.EvGoBlockSelect,
			trace.EvGoBlockSync, trace.EvGoBlockCond, trace.EvGoBlockNet,
			trace.EvGoBlockGC:
			// ev.G is blocking
			gid = ev.G
			pState = pStopG
			state = evTypeToState[ev.Type]
		case trace.EvGoBlock:
			// ev.G is blocking
			gid = ev.G
			pState = pStopG
			state = evTypeToState[ev.Type]

			if ev.Type == trace.EvGoBlock {
				if blockedIsInactive(gsByID[gid].Function) {
					state = StateInactive
				}
			}
		case trace.EvGoWaiting:
			// ev.G is blocked when tracing starts
			gid = ev.G
			state = StateBlocked
			if blockedIsInactive(gsByID[gid].Function) {
				state = StateInactive
			}
		case trace.EvGoUnblock:
			// ev.G is unblocking ev.Args[0]
			addEventToCurrentSpan(ev.G, EventID(evID))
			gid = ev.Args[trace.ArgGoUnblockG]
			state = StateReady
		case trace.EvGoSysCall:
			// From the runtime's documentation:
			//
			// Syscall tracing:
			// At the start of a syscall we emit traceGoSysCall to capture the stack trace.
			// If the syscall does not block, that is it, we do not emit any other events.
			// If the syscall blocks (that is, P is retaken), retaker emits traceGoSysBlock;
			// when syscall returns we emit traceGoSysExit and when the goroutine starts running
			// (potentially instantly, if exitsyscallfast returns true) we emit traceGoStart.

			// XXX guard against malformed trace
			lastSyscall[ev.G] = ev.StkID
			addEventToCurrentSpan(ev.G, EventID(evID))
			continue
		case trace.EvGoSysBlock:
			gid = ev.G
			pState = pStopG
			state = StateBlockedSyscall

			// EvGoSysblock will be followed by ProcStop. Leave a note for ProcStop to start a new span for the blocking
			// syscall. Also record enough data for EvGoSysExit to finish that span.
			blockingSyscallPerP[ev.P] = EventID(evID)
			blockingSyscallMPerG[ev.G] = lastMPerP[ev.P]

		case trace.EvGoInSyscall:
			gid = ev.G
			state = StateBlockedSyscall
		case trace.EvGoSysExit:
			gid = ev.G
			state = StateReady

			if supportMachineTimelines {
				if mid, ok := blockingSyscallMPerG[ev.G]; ok {
					delete(blockingSyscallMPerG, ev.G)
					m := getM(mid)
					span := &m.Spans[len(m.Spans)-1]
					switch span.State {
					case StateBlockedSyscall:
						// We didn't see an EvProcStart for this M
						span.End = ev.Ts
					case StateRunningP:
						// We saw a EvProcStart for this M before we saw the EvGoSysExit. The blocking syscall span will be
						// the second last span, and we'll have to slightly adjust its end time to not exceed the next
						// span's start time.

						// XXX guard against malformed traces
						pspan := &m.Spans[len(m.Spans)-2]
						if pspan.State != StateBlockedSyscall {
							return nil, errors.New("malformed trace: EvGoSysExit was preceeded by more than one EvProcStart or other events")
						}
						pspan.End = span.Start
					default:
						panic(fmt.Sprintf("unexpected state %d", span.State))
					}
				}
			}

		case trace.EvProcStart:
			if supportMachineTimelines {
				mid := ev.Args[0]
				m := getM(int32(mid))
				m.Spans = append(m.Spans, Span{Start: ev.Ts, End: -1, State: StateRunningP, Event: EventID(evID)})
				lastMPerP[ev.P] = m.ID
			}
			continue
		case trace.EvProcStop:
			if supportMachineTimelines {
				m := getM(lastMPerP[ev.P])
				span := &m.Spans[len(m.Spans)-1]
				span.End = ev.Ts

				if sevID, ok := blockingSyscallPerP[ev.P]; ok {
					delete(blockingSyscallPerP, ev.P)
					m.Spans = append(m.Spans, Span{Start: ev.Ts, End: -1, State: StateBlockedSyscall, Event: sevID})
				}
			}

			continue

		case trace.EvGCMarkAssistStart:
			// User goroutines may be asked to assist the GC's mark phase. This happens when the goroutine allocates
			// memory and some condition is true. When that happens, the tracer emits EvGCMarkAssistStart for that
			// goroutine.
			//
			// Note that this event is not preceeded by an EvGoBlock or similar. Similarly, EvGCMarkAssistDone is not
			// succeeded by an EvGoStart or similar. The mark assist events are laid over the normal goroutine
			// scheduling events.
			//
			// We instead turn these into proper goroutine states and split the current span in two to make room for
			// mark assist. This needs special care because mark assist can be preempted, so we might GoStart into mark
			// assist.

			gid = ev.G
			state = StateGCMarkAssist
			inMarkAssist[gid] = struct{}{}
		case trace.EvGCMarkAssistDone:
			// The counterpart to EvGCMarkAssistStop.

			gid = ev.G
			state = StateActive
			delete(inMarkAssist, gid)
		case trace.EvGCSweepStart:
			// This is similar to mark assist, but for sweeping spans. When a goroutine would need to allocate a new
			// span, it first sweeps other spans of the same size to find a free one.
			//
			// Unlike mark assist, sweeping cannot be preempted, simplifying our state tracking.

			if ev.G == 0 {
				// Sweeping can also happen on the system stack, for example when the allocator needs to allocate a new
				// span. We don't have a way to display this properly at the moment, so hide the information.
				continue
			}

			gid = ev.G
			state = StateGCSweep
		case trace.EvGCSweepDone:
			// The counterpart to EvGcSweepStart.

			if ev.G == 0 {
				// See EvGCSweepStart for why.
				continue
			}

			gid = ev.G
			state = StateActive

		case trace.EvGCStart:
			gc = append(gc, Span{Start: ev.Ts, State: StateActive, Event: EventID(evID)})
			continue

		case trace.EvGCSTWStart:
			stw = append(stw, Span{Start: ev.Ts, State: StateActive, Event: EventID(evID)})
			continue

		case trace.EvGCDone:
			// XXX verify that index isn't out of bounds
			gc[len(gc)-1].End = ev.Ts
			continue

		case trace.EvGCSTWDone:
			// Even though STW happens as part of GC, we can see EvGCSTWDone after EvGCDone.
			// XXX verify that index isn't out of bounds
			stw[len(stw)-1].End = ev.Ts
			continue

		case trace.EvHeapAlloc:
			// Instant measurement of currently allocated memory
			continue
		case trace.EvHeapGoal:
			// Instant measurement of new heap goal

			// TODO(dh): implement
			continue

		case trace.EvGomaxprocs:
			// TODO(dh): graph GOMAXPROCS
			continue

		case trace.EvUserTaskCreate:
			t := &Task{
				ID:    ev.Args[trace.ArgUserTaskCreateTaskID],
				Name:  res.Strings[ev.Args[trace.ArgUserTaskCreateTypeID]],
				Event: EventID(evID),
			}
			tasks = append(tasks, t)
			continue
		case trace.EvUserTaskEnd:
			continue

		case trace.EvUserRegion:
			const regionStart = 0
			gid := ev.G
			if mode := ev.Args[trace.ArgUserRegionMode]; mode == regionStart {
				endID := int(ev.Link[0]) | int(ev.Link[1])<<8 | int(ev.Link[2])<<16 | int(ev.Link[3])<<24 | int(ev.Link[4])<<32
				var end trace.Timestamp
				if endID != 0xFFFFFFFFFF {
					end = res.Events[endID].Ts
				} else {
					end = -1
				}
				s := Span{
					Start: ev.Ts,
					Event: EventID(evID),
					State: StateUserRegion,
					End:   end,
				}
				g := getG(ev.G)
				depth := userRegionDepths[gid]
				if depth >= len(g.UserRegions) {
					if depth < cap(g.UserRegions) {
						g.UserRegions = g.UserRegions[:depth+1]
					} else {
						s := make([]Spans, depth+1)
						copy(s, g.UserRegions)
						g.UserRegions = s
					}
				}
				g.UserRegions[depth] = append(g.UserRegions[depth], s)
				userRegionDepths[gid]++
			} else {
				d := userRegionDepths[gid] - 1
				if d > 0 {
					userRegionDepths[gid] = d
				} else {
					delete(userRegionDepths, gid)
				}
			}
			continue

		case trace.EvUserLog:
			// TODO(dh): incorporate logs in per-goroutine timeline
			addEventToCurrentSpan(ev.G, EventID(evID))
			continue

		case trace.EvCPUSample:
			cpuSamples[ev.G] = append(cpuSamples[ev.G], EventID(evID))
			hasCPUSamples = true
			continue

		default:
			return nil, fmt.Errorf("unsupported trace event %d", ev.Type)
		}

		if debug {
			if s := getG(gid).Spans; len(s) > 0 {
				if len(s) == 1 && ev.Type == trace.EvGoWaiting && s[0].State == StateInactive {
					// The execution trace emits GoCreate + GoWaiting for goroutines that already exist at the start of
					// tracing if they're in a blocked state. This causes a transition from inactive to blocked, which we
					// wouldn't normally permit.
				} else {
					prevState := s[len(s)-1].State
					if !legalStateTransitions[prevState][state] {
						panic(fmt.Sprintf("illegal state transition %d -> %d for goroutine %d, time %d", prevState, state, gid, ev.Ts))
					}
				}
			}
		}

		s := Span{Start: ev.Ts, State: state, Event: EventID(evID)}
		if ev.Type == trace.EvGoSysBlock {
			if debug && res.Events[s.Event].StkID != 0 {
				panic("expected zero stack ID")
			}
			res.Events[s.Event].StkID = lastSyscall[ev.G]
		}

		getG(gid).Spans = append(getG(gid).Spans, s)

		switch pState {
		case pRunG:
			p := getP(ev.P)
			p.Spans = append(p.Spans, Span{Start: ev.Ts, State: StateRunningG, Event: EventID(evID)})
			if supportMachineTimelines {
				mid := lastMPerP[p.ID]
				m := getM(mid)
				m.Goroutines = append(m.Goroutines, Span{Start: ev.Ts, Event: EventID(evID), State: StateRunningG})
			}
		case pStopG:
			// XXX guard against malformed traces
			p := getP(ev.P)
			p.Spans[len(p.Spans)-1].End = ev.Ts
			if supportMachineTimelines {
				mid := lastMPerP[p.ID]
				m := getM(mid)
				if len(m.Goroutines) == 0 {
					return nil, fmt.Errorf("malformed trace: g%d ran on m%d but M has no goroutine spans", ev.G, mid)
				}
				m.Goroutines[len(m.Goroutines)-1].End = ev.Ts
			}
		}
	}

	sem := make(chan struct{}, runtime.GOMAXPROCS(0))
	var wg sync.WaitGroup
	for _, g := range gsByID {
		sem <- struct{}{}
		g := g
		wg.Add(1)
		go func() {
			for i, s := range g.Spans {
				if i != len(g.Spans)-1 {
					s.End = g.Spans[i+1].Start
				}

				stack := res.Stacks[res.Events[s.Event].StkID]
				s = applyPatterns(s, res.PCs, stack)

				// move s.At out of the runtime
				for int(s.At+1) < len(stack) && s.At < 255 && strings.HasPrefix(res.PCs[stack[s.At]].Fn, "runtime.") {
					s.At++
				}

				g.Spans[i] = s
			}

			if len(g.Spans) != 0 {
				last := g.Spans[len(g.Spans)-1]
				if last.State == StateDone {
					// The goroutine has ended
					// XXX the event probably has a stack associated with it, which we shouldn't discard.
					g.Spans = g.Spans[:len(g.Spans)-1]
				} else {
					// XXX somehow encode open-ended traces
					g.Spans[len(g.Spans)-1].End = res.Events[len(res.Events)-1].Ts
				}
			}

			for depth, spans := range g.UserRegions {
				if len(spans) != 0 {
					if last := &spans[len(spans)-1]; last.End == -1 {
						// The user region wasn't closed before the trace ended; give it the maximum length possible,
						// that of the parent user region, or the goroutine if this is the top-most user region.

						if depth == 0 {
							last.End = g.Spans.End()
						} else {
							// OPT(dh): use binary search
							for _, parent := range g.UserRegions[depth-1] {
								// The first parent user region that ends after our region starts has to be our parent.
								if parent.End >= last.Start {
									last.End = parent.End
									break
								}
							}
						}
					}
				}
			}

			<-sem
			wg.Done()
		}()
	}
	wg.Wait()

	// Note: There is no point populating gs and ps in parallel, because ps only contains a handful of items.
	for _, g := range gsByID {
		if len(g.Spans) != 0 {
			// OPT(dh): preallocate gs
			gs = append(gs, g)
		}
	}

	sort.Slice(gs, func(i, j int) bool {
		return gs[i].ID < gs[j].ID
	})

	for i, g := range gs {
		g.SeqID = i
	}

	for _, p := range psByID {
		// OPT(dh): preallocate ps
		ps = append(ps, p)
	}

	sort.Slice(ps, func(i, j int) bool {
		return ps[i].ID < ps[j].ID
	})

	for i, p := range ps {
		p.SeqID = i
	}

	if supportMachineTimelines {
		for _, m := range msByID {
			// OPT(dh): preallocate ms
			ms = append(ms, m)
			if len(m.Spans) > 0 {
				if last := &m.Spans[len(m.Spans)-1]; last.End == -1 {
					last.End = res.Events[len(res.Events)-1].Ts
				}
			}
		}

		sort.Slice(ms, func(i, j int) bool {
			return ms[i].ID < ms[j].ID
		})

		for i, m := range ms {
			m.SeqID = i
		}
	}

	slices.SortFunc(tasks, func(a, b *Task) bool {
		return a.ID < b.ID
	})

	for i, t := range tasks {
		t.SeqID = i
	}

	tr := &Trace{
		Goroutines:    gs,
		gsByID:        gsByID,
		Processors:    ps,
		Machines:      ms,
		GC:            gc,
		STW:           stw,
		Tasks:         tasks,
		HasCPUSamples: hasCPUSamples,
		CPUSamples:    cpuSamples,
		ParseResult:   res,
	}
	for _, g := range tr.Goroutines {
		g.computeStatistics()
	}

	return tr, nil
}

//gcassert:inline
func (t *Trace) Event(ev EventID) *trace.Event {
	return &t.Events[ev]
}

func (t *Trace) Task(id uint64) *Task {
	idx, found := sort.Find(len(t.Tasks), func(i int) int {
		oid := t.Tasks[i].ID
		if id == oid {
			return 0
		} else if id < oid {
			return -1
		} else {
			return 1
		}
	})
	if !found {
		panic("couldn't find task")
	}
	return t.Tasks[idx]
}

func (tr *Trace) G(gid uint64) *Goroutine {
	// In a previous version we used binary search over Trace.gs. This didn't scale for traces with a lot of goroutines
	// because of how often getG has to be called during rendering. For example, for Sean's trace from hell, switching
	// from binary search to map lookups reduced frame times by 33%.

	g, found := tr.gsByID[gid]
	if !found {
		panic(fmt.Sprintf("couldn't find goroutine %d", gid))
	}
	return g
}

func (tr *Trace) P(pid int32) *Processor {
	// Unlike getG, getP doesn't get called every frame, and using binary search is fast enough.

	idx, found := sort.Find(len(tr.Processors), func(idx int) int {
		opid := tr.Processors[idx].ID
		if pid > opid {
			return 1
		} else if pid == opid {
			return 0
		} else {
			return -1
		}
	})
	if !found {
		panic(fmt.Sprintf("couldn't find processor %d", pid))
	}
	return tr.Processors[idx]
}

//gcassert:inline
func (s *Span) Duration() time.Duration {
	return time.Duration(s.End - s.Start)
}

func (s *Span) Events(all []EventID, tr *Trace) []EventID {
	// TODO(dh): this code is virtually identical to the code in MergedSpans.Events, but we cannot reuse that without
	// allocating.

	if len(all) == 0 {
		return nil
	}

	// The all argument contains all events in the span's container (a goroutine), sorted by timestamp, as indices into the
	// global list of events. Find the first and last event that overlaps with the span, and that is the set of events
	// belonging to this span.

	end := sort.Search(len(all), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= s.End
	})

	sTs := s.Start
	start := sort.Search(len(all[:end]), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= sTs
	})

	return all[start:end]
}

// Several background goroutines in the runtime go into a blocked state when they have no work to do. In all cases, this
// is more similar to a goroutine calling runtime.Gosched than to a goroutine really wishing it had work to do. Because
// of that we put those into the inactive state.
func blockedIsInactive(fn string) bool {
	if fn == "" {
		return false
	}
	switch fn {
	case "runtime.gcBgMarkWorker", "runtime.forcegchelper", "runtime.bgsweep", "runtime.bgscavenge", "runtime.runfinq":
		return true
	default:
		return false
	}
}

type Spans []Span

func (spans Spans) Start() trace.Timestamp {
	if len(spans) == 0 {
		return 0
	}
	return spans[0].Start
}

func (spans Spans) End() trace.Timestamp {
	if len(spans) == 0 {
		return 0
	}
	return spans[len(spans)-1].End
}

func (spans Spans) Duration() time.Duration {
	return time.Duration(spans.End() - spans.Start())
}

func (spans Spans) Events(all []EventID, tr *Trace) []EventID {
	if len(all) == 0 {
		return nil
	}

	end := sort.Search(len(all), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= spans.End()
	})

	sTs := spans.Start()

	start := sort.Search(len(all[:end]), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= sTs
	})

	return all[start:end]
}

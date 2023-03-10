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

	// Special states used by user regions and stack frames
	StateUserRegion
	StateStack
	StateCPUSample

	// Processor states
	StateRunningG

	// Machine states
	StateRunningP

	StateLast
)

type Point struct {
	When  trace.Timestamp
	Value uint64
}

type Trace struct {
	// OPT(dh): can we get rid of all these pointers?
	Goroutines []*Goroutine
	Processors []*Processor
	Machines   []*Machine
	Functions  map[string]*Function
	GC         Spans
	STW        Spans
	Tasks      []*Task
	HeapSize   []Point
	HeapGoal   []Point
	// Mapping from Goroutine ID to list of CPU sample events
	CPUSamples map[uint64][]EventID

	gsByID map[uint64]*Goroutine
	// psByID and msById will be unset after parsing finishes
	psByID        map[int32]*Processor
	msByID        map[int32]*Machine
	HasCPUSamples bool

	trace.Trace
}

type Statistics [StateLast]Statistic

func (stat *Statistics) Blocked() time.Duration {
	return stat[StateBlocked].Total +
		stat[StateBlockedSend].Total +
		stat[StateBlockedRecv].Total +
		stat[StateBlockedSelect].Total +
		stat[StateBlockedSync].Total +
		stat[StateBlockedSyncOnce].Total +
		stat[StateBlockedSyncTriggeringGC].Total +
		stat[StateBlockedCond].Total +
		stat[StateBlockedNet].Total +
		stat[StateBlockedGC].Total +
		stat[StateBlockedSyscall].Total +
		stat[StateStuck].Total
}

func (stat *Statistics) Running() time.Duration {
	return stat[StateActive].Total +
		stat[StateGCDedicated].Total +
		stat[StateGCIdle].Total
}

func (stat *Statistics) Inactive() time.Duration {
	return stat[StateInactive].Total +
		stat[StateReady].Total +
		stat[StateCreated].Total

}

func (stat *Statistics) GCAssist() time.Duration {
	return stat[StateGCMarkAssist].Total +
		stat[StateGCSweep].Total
}

type Statistic struct {
	Count           int
	Min, Max, Total time.Duration
	Average, Median float64
}

type Function struct {
	trace.Frame
	// Sequential ID of function in the trace
	SeqID      int
	Goroutines []*Goroutine
}

func (fn Function) String() string {
	return fn.Fn
}

type Goroutine struct {
	ID uint64
	// Sequential ID of goroutine in the trace
	SeqID       int
	Function    *Function
	Spans       Spans
	UserRegions []Spans
	Events      []EventID

	// Most goroutines are small enough that we can compute statistics on demand. For the rest, we compute them when
	// parsing the trace and cache them here.
	cachedStatistics *Statistics
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
	// collector won't have to scan any memory of our millions of events.
	//
	// Instead of pointers, fields like PC and Event are indices into slices.

	Start trace.Timestamp
	End   trace.Timestamp
	Event EventID
	// At is an offset from the top of the stack, skipping over uninteresting runtime frames.
	At uint8
	// We track the scheduling State explicitly, instead of mapping from trace.Event.Type, because we apply pattern
	// matching to stack traces that may result in more accurate states. For example, we can determine
	// stateBlockedSyncOnce from the stack trace, and we would otherwise use stateBlockedSync.
	State SchedulingState
	Tags  SpanTags
}

type EventID int32

func computeGoroutineStatistics(gs []*Goroutine, progress func(float64)) {
	// Compute cached statistics for large goroutines. We've measured an average of 168ns per span on an AMD 3950x. We
	// round that up to 400ns and set a target of 5ms to calculate statistics. By that calculation, goroutines with more
	// than 12500 spans need to have cached statistics.

	for i, g := range gs {
		if (i+1)%10_000 == 0 {
			progress(float64(i) / float64(len(gs)))
		}
		if len(g.Spans) < 12500 {
			continue
		}

		stats := g.Statistics()
		g.cachedStatistics = &stats
		progress(float64(i+1) / float64(len(gs)))
	}
}

func (g *Goroutine) Statistics() Statistics {
	// XXX only compute statistics for goroutines where it'd be too slow to do just in time. on this system we need
	// about 168ns per span. let's round that up to 400ns and go for a 5ms target. this would mean any goroutine with
	// >=12500 spans is too big.

	if g.cachedStatistics != nil {
		return *g.cachedStatistics
	}

	var values [StateLast][]time.Duration

	var stats Statistics

	for i := range values {
		values[i] = values[i][:0]
	}

	for i := range g.Spans {
		s := &g.Spans[i]
		stat := &stats[s.State]
		stat.Count++
		d := s.Duration()
		if d > stat.Max {
			stat.Max = d
		}
		if d < stat.Min || stat.Min == 0 {
			stat.Min = d
		}
		stat.Total += d
		values[s.State] = append(values[s.State], d)
	}

	for state := range stats {
		stat := &stats[state]

		if len(values[state]) == 0 {
			continue
		}

		stat.Average = float64(stat.Total) / float64(len(values[state]))

		sort.Slice(values[state], func(i, j int) bool {
			return values[state][i] < values[state][j]
		})

		if len(values[state])%2 == 0 {
			mid := len(values[state]) / 2
			stat.Median = float64(values[state][mid]+values[state][mid-1]) / 2
		} else {
			stat.Median = float64(values[state][len(values[state])/2])
		}
	}

	return stats
}

func Parse(res trace.Trace, progress func(float64)) (*Trace, error) {
	tr := &Trace{
		Trace:      res,
		Functions:  map[string]*Function{},
		gsByID:     map[uint64]*Goroutine{},
		psByID:     map[int32]*Processor{},
		msByID:     map[int32]*Machine{},
		CPUSamples: map[uint64][]EventID{},
	}

	makeProgresser := func(stage int, numStages int) func(float64) {
		return func(p float64) {
			if p > 1 {
				panic(p)
			}
			progress(float64(stage-1)/float64(numStages) + (p / float64(numStages)))
		}
	}

	if err := processEvents(res, tr, makeProgresser(1, 5)); err != nil {
		return nil, err
	}

	populateObjects(tr, makeProgresser(2, 5))
	postProcessSpans(tr, makeProgresser(3, 5))
	removeBogusCreatedSpans(tr)
	computeGoroutineStatistics(tr.Goroutines, makeProgresser(5, 5))

	tr.psByID = nil
	tr.msByID = nil

	return tr, nil
}

func processEvents(res trace.Trace, tr *Trace, progress func(float64)) error {
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

	getG := func(gid uint64) *Goroutine {
		g, ok := tr.gsByID[gid]
		if ok {
			return g
		}
		g = &Goroutine{ID: gid}
		tr.gsByID[gid] = g
		return g
	}
	getP := func(pid int32) *Processor {
		p, ok := tr.psByID[pid]
		if ok {
			return p
		}
		p = &Processor{
			ID: pid,
			// spanLabels: []string{local.Sprintf("p%d", pid)},
		}
		tr.psByID[pid] = p
		return p
	}
	getM := func(mid int32) *Machine {
		if !supportMachineTimelines {
			panic("getM was called despite supportmachineActivities == false")
		}
		m, ok := tr.msByID[mid]
		if ok {
			return m
		}
		m = &Machine{ID: mid}
		tr.msByID[mid] = m
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

	// FIXME(dh): rename function. or remove it outright
	addEventToCurrentSpan := func(gid uint64, ev EventID) {
		if gid == 0 {
			// Events such as the netpoller unblocking a goroutine will happen on g0.
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
		case trace.EvHeapAlloc:
			tr.HeapSize = append(tr.HeapSize, Point{
				ev.Ts,
				ev.Args[trace.ArgHeapAllocMem],
			})
		case trace.EvHeapGoal:
			tr.HeapGoal = append(tr.HeapGoal, Point{
				ev.Ts,
				ev.Args[trace.ArgHeapGoalMem],
			})
		case trace.EvGCStart, trace.EvGCSTWStart, trace.EvGCDone, trace.EvGCSTWDone,
			trace.EvGomaxprocs, trace.EvUserTaskCreate,
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
		if (evID+1)%10_000 == 0 {
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
					f := tr.function(res.PCs[stack[0]])
					g := getG(gid)
					f.Goroutines = append(f.Goroutines, g)
					g.Function = f
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
				if blockedIsInactive(tr.gsByID[gid].Function.Fn) {
					state = StateInactive
				}
			}
		case trace.EvGoWaiting:
			// ev.G is blocked when tracing starts
			gid = ev.G
			state = StateBlocked
			if blockedIsInactive(tr.gsByID[gid].Function.Fn) {
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
							return errors.New("malformed trace: EvGoSysExit was preceeded by more than one EvProcStart or other events")
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
			tr.GC = append(tr.GC, Span{Start: ev.Ts, State: StateActive, Event: EventID(evID)})
			continue

		case trace.EvGCSTWStart:
			tr.STW = append(tr.STW, Span{Start: ev.Ts, State: StateActive, Event: EventID(evID)})
			continue

		case trace.EvGCDone:
			// XXX verify that index isn't out of bounds
			tr.GC[len(tr.GC)-1].End = ev.Ts
			continue

		case trace.EvGCSTWDone:
			// Even though STW happens as part of GC, we can see EvGCSTWDone after EvGCDone.
			// XXX verify that index isn't out of bounds
			tr.STW[len(tr.STW)-1].End = ev.Ts
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
			tr.Tasks = append(tr.Tasks, t)
			continue
		case trace.EvUserTaskEnd:
			continue

		case trace.EvUserRegion:
			const regionStart = 0
			gid := ev.G
			if mode := ev.Args[trace.ArgUserRegionMode]; mode == regionStart {
				var end trace.Timestamp
				if ev.Link != -1 {
					end = res.Events[ev.Link].Ts
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
			tr.CPUSamples[ev.G] = append(tr.CPUSamples[ev.G], EventID(evID))
			tr.HasCPUSamples = true
			continue

		default:
			return fmt.Errorf("unsupported trace event %d", ev.Type)
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
					return fmt.Errorf("malformed trace: g%d ran on m%d but M has no goroutine spans", ev.G, mid)
				}
				m.Goroutines[len(m.Goroutines)-1].End = ev.Ts
			}
		}
	}

	return nil
}

func postProcessSpans(tr *Trace, progress func(float64)) {
	var wg sync.WaitGroup
	doG := func(g *Goroutine) {
		for i, s := range g.Spans {
			if i != len(g.Spans)-1 {
				s.End = g.Spans[i+1].Start
			}

			stack := tr.Stacks[tr.Events[s.Event].StkID]
			s = applyPatterns(s, tr.PCs, stack)

			// move s.At out of the runtime
			for int(s.At+1) < len(stack) && s.At < 255 && strings.HasPrefix(tr.PCs[stack[s.At]].Fn, "runtime.") {
				s.At++
			}

			g.Spans[i] = s
		}

		if len(g.Spans) != 0 {
			last := g.Spans[len(g.Spans)-1]
			if last.State == StateDone {
				// The goroutine has ended. We encode this as a zero length span.
				s := &g.Spans[len(g.Spans)-1]
				s.End = s.Start
			} else {
				g.Spans[len(g.Spans)-1].End = tr.Events[len(tr.Events)-1].Ts
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
	}

	step := len(tr.Goroutines) / runtime.GOMAXPROCS(0)
	if step == 0 {
		step = 1
	}

	var progressMu sync.Mutex
	var progressValue int
	for off := 0; off < len(tr.Goroutines); off += step {
		wg.Add(1)
		end := off + step
		if end > len(tr.Goroutines) {
			end = len(tr.Goroutines)
		}
		slice := tr.Goroutines[off:end]
		go func() {
			defer wg.Done()
			for i, g := range slice {
				doG(g)
				if (i+1)%1000 == 0 {
					progressValue += 1000
					progressMu.Lock()
					progress(float64(progressValue) / float64(len(tr.Goroutines)))
					progressMu.Unlock()
				}
			}
		}()
	}

	wg.Wait()
}

func removeBogusCreatedSpans(tr *Trace) {
evLoop:
	for _, ev := range tr.Events {
		switch ev.Type {
		case trace.EvGoCreate:
			// This goroutine already existed at the beginning of the trace. Merge the first two spans, of which the
			// first span is a GoCreate span that's not true, as the goroutine had already existed.
			g := tr.G(ev.Args[0])
			if len(g.Spans) > 1 {
				if g.Spans[0].State != StateCreated {
					panic(fmt.Sprintf("unexpected state %v", g.Spans[0].State))
				}
				// We set the second span's start time to 0, instead of the first span's start time, because the
				// timestamp of the GoCreate event for existing goroutines doesn't matter, it's only non-zero because
				// the tracer can't emit the state for all existing goroutines at once.
				g.Spans[1].Start = 0
				g.Spans = g.Spans[1:]
			}
		case trace.EvProcStart, trace.EvHeapAlloc, trace.EvGomaxprocs:
			// These are the events we know to occur at the end of the STW phase of starting a trace.
			break evLoop
		}
	}
}

func populateObjects(tr *Trace, progress func(float64)) {
	// Note: There is no point populating gs and ps in parallel, because ps only contains a handful of items.
	tr.Goroutines = make([]*Goroutine, 0, len(tr.gsByID))
	for _, g := range tr.gsByID {
		if len(g.Spans) != 0 {
			tr.Goroutines = append(tr.Goroutines, g)
		}
	}
	progress(1.0 / 5.0)

	for _, p := range tr.psByID {
		// OPT(dh): preallocate ps
		tr.Processors = append(tr.Processors, p)
	}
	progress(2.0 / 5.0)

	for _, m := range tr.msByID {
		// OPT(dh): preallocate ms
		tr.Machines = append(tr.Machines, m)
		if len(m.Spans) > 0 {
			if last := &m.Spans[len(m.Spans)-1]; last.End == -1 {
				last.End = tr.Events[len(tr.Events)-1].Ts
			}
		}
	}
	progress(3.0 / 5.0)

	sort.Slice(tr.Goroutines, func(i, j int) bool { return tr.Goroutines[i].ID < tr.Goroutines[j].ID })
	sort.Slice(tr.Processors, func(i, j int) bool { return tr.Processors[i].ID < tr.Processors[j].ID })
	sort.Slice(tr.Machines, func(i, j int) bool { return tr.Machines[i].ID < tr.Machines[j].ID })
	sort.Slice(tr.Tasks, func(i, j int) bool { return tr.Tasks[i].ID < tr.Tasks[j].ID })
	for _, f := range tr.Functions {
		slices.SortFunc(f.Goroutines, func(a, b *Goroutine) bool { return a.SeqID < b.SeqID })
	}
	progress(4.0 / 5.0)

	for i, g := range tr.Goroutines {
		g.SeqID = i
	}
	for i, p := range tr.Processors {
		p.SeqID = i
	}
	for i, m := range tr.Machines {
		m.SeqID = i
	}
	for i, t := range tr.Tasks {
		t.SeqID = i
	}
	progress(1)
}

func (t *Trace) function(frame trace.Frame) *Function {
	f, ok := t.Functions[frame.Fn]
	if ok {
		return f
	}
	f = &Function{
		Frame: frame,
		SeqID: len(t.Functions),
	}
	t.Functions[frame.Fn] = f
	return f
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

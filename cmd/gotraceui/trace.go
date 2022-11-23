package main

import (
	"fmt"
	"io"
	"runtime"
	"sort"
	"strings"
	"sync"
	"time"

	"golang.org/x/exp/slices"
	"honnef.co/go/gotraceui/trace"
)

type schedulingState uint8

const (
	stateNone schedulingState = iota

	// Goroutine states
	stateInactive
	stateActive
	stateGCIdle
	stateGCDedicated
	stateBlocked
	stateBlockedSend
	stateBlockedRecv
	stateBlockedSelect
	stateBlockedSync
	stateBlockedSyncOnce
	stateBlockedSyncTriggeringGC
	stateBlockedCond
	stateBlockedNet
	stateBlockedGC
	stateBlockedSyscall
	stateStuck
	stateReady
	stateCreated
	stateDone
	stateGCMarkAssist
	stateGCSweep

	// Special states used by user regions and CPU sampling
	stateUserRegion
	stateCPUSample

	// Processor states
	stateRunningG

	// Machine states
	stateRunningP

	stateLast
)

var legalStateTransitions = [256][stateLast]bool{
	stateInactive: {
		stateActive:         true,
		stateReady:          true,
		stateBlockedSyscall: true,

		// Starting back into preempted mark assist
		stateGCMarkAssist: true,
	},
	stateActive: {
		// active -> ready occurs on preemption
		stateReady:                   true,
		stateInactive:                true,
		stateBlocked:                 true,
		stateBlockedSend:             true,
		stateBlockedRecv:             true,
		stateBlockedSelect:           true,
		stateBlockedSync:             true,
		stateBlockedSyncOnce:         true,
		stateBlockedSyncTriggeringGC: true,
		stateBlockedCond:             true,
		stateBlockedNet:              true,
		stateBlockedGC:               true,
		stateBlockedSyscall:          true,
		stateStuck:                   true,
		stateDone:                    true,
		stateGCMarkAssist:            true,
		stateGCSweep:                 true,
	},
	stateGCIdle: {
		// active -> ready occurs on preemption
		stateReady:       true,
		stateInactive:    true,
		stateBlockedSync: true,
	},
	stateGCDedicated: {
		// active -> ready occurs on preemption
		stateReady:       true,
		stateInactive:    true,
		stateBlockedSync: true,
	},
	stateCreated: {
		stateActive: true,

		// FIXME(dh): These three transitions are only valid for goroutines that already existed when tracing started.
		// eventually we'll make it so those goroutines don't end up in stateReady, at which point we should remove
		// these entries.
		stateInactive:       true,
		stateBlocked:        true,
		stateBlockedSyscall: true,
	},
	stateReady: {
		stateActive:       true,
		stateGCMarkAssist: true,
		stateGCIdle:       true,
		stateGCDedicated:  true,
	},
	stateBlocked:                 {stateReady: true},
	stateBlockedSend:             {stateReady: true},
	stateBlockedRecv:             {stateReady: true},
	stateBlockedSelect:           {stateReady: true},
	stateBlockedSync:             {stateReady: true},
	stateBlockedSyncOnce:         {stateReady: true},
	stateBlockedSyncTriggeringGC: {stateReady: true},
	stateBlockedCond:             {stateReady: true},
	stateBlockedNet:              {stateReady: true},
	stateBlockedGC:               {stateReady: true},
	stateBlockedSyscall: {
		stateReady: true,
	},

	stateGCMarkAssist: {
		// active -> ready occurs on preemption
		stateReady:       true,
		stateActive:      true, // back to the goroutine's previous state
		stateInactive:    true, // mark assist can be preempted
		stateBlocked:     true,
		stateBlockedSync: true,
		stateBlockedGC:   true, // XXX what does this transition mean?
	},

	stateGCSweep: {
		stateActive: true, // back to the goroutine's previous state
	},
}

type Trace struct {
	// OPT(dh): can we get rid of all these pointers?
	gs     []*Goroutine
	gsByID map[uint64]*Goroutine
	ps     []*Processor
	ms     []*Machine
	gc     Spans
	stw    Spans
	tasks  []*Task
	trace.ParseResult
}

type reason uint8

const (
	reasonNone reason = iota
	reasonNewlyCreated
	reasonGosched
	reasonTimeSleep
	reasonPreempted
)

var reasonByEventType = [256]reason{
	trace.EvGoCreate:  reasonNewlyCreated,
	trace.EvGoSched:   reasonGosched,
	trace.EvGoSleep:   reasonTimeSleep,
	trace.EvGoPreempt: reasonPreempted,
}

//gcassert:inline
func (t *Trace) Reason(s *Span) reason {
	return reasonByEventType[t.Events[s.event()].Type]
}

//gcassert:inline
func (t *Trace) Event(ev EventID) *trace.Event {
	return &t.Events[ev]
}

// TODO(dh): remove this method. This was useful when Span didn't have a start field, but now it does. Move the method to Span.
//gcassert:inline
func (t *Trace) Duration(s *Span) time.Duration {
	return time.Duration(s.end - s.start)
}

func (t *Trace) Task(id uint64) *Task {
	idx, found := sort.Find(len(t.tasks), func(i int) int {
		oid := t.tasks[i].id
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
	return t.tasks[idx]
}

func (tr *Trace) getG(gid uint64) *Goroutine {
	// In a previous version we used binary search over Trace.gs. This didn't scale for traces with a lot of goroutines
	// because of how often getG has to be called during rendering. For example, for Sean's trace from hell, switching
	// from binary search to map lookups reduced frame times by 33%.

	g, found := tr.gsByID[gid]
	if !found {
		panic(fmt.Sprintf("couldn't find goroutine %d", gid))
	}
	return g
}

func (tr *Trace) getP(pid int32) *Processor {
	// Unlike getG, getP doesn't get called every frame, and using binary search is fast enough.

	idx, found := sort.Find(len(tr.ps), func(idx int) int {
		opid := tr.ps[idx].id
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
	return tr.ps[idx]
}

// MergedSpans and Spans have the same functionality. The two different types are used to make APIs easier to read, to
// be able to tell apart functions that operate on multiple spans as if they were individual items and functions that
// treat them as one unit, because they get merged during rendering.

// Spans represents a list of consecutive spans from a shared timeline.
type Spans []Span

// MergedSpans represents a list of consecutive spans from a shared timeline, which were merged during display.
//
// OPT(dh): we could theoretically save 8 bytes by storing the start and end indices instead of a slice, as merged
// spans have to be consecutive. It would also prevent potential misuse of MergedSpans, e.g. by creating an entirely
// new slice, instead of slicing an existing one. However, a slice is easier to access and iterate over.
type MergedSpans []Span

func (ms MergedSpans) Start(tr *Trace) trace.Timestamp           { return Spans(ms).Start(tr) }
func (ms MergedSpans) End() trace.Timestamp                      { return Spans(ms).End() }
func (ms MergedSpans) Duration(tr *Trace) time.Duration          { return Spans(ms).Duration(tr) }
func (ms MergedSpans) Events(all []EventID, tr *Trace) []EventID { return Spans(ms).Events(all, tr) }

func (spans Spans) Start(tr *Trace) trace.Timestamp {
	if len(spans) == 0 {
		return 0
	}
	return spans[0].start
}

func (spans Spans) End() trace.Timestamp {
	if len(spans) == 0 {
		return 0
	}
	return spans[len(spans)-1].end
}

func (spans Spans) Duration(tr *Trace) time.Duration {
	return time.Duration(spans.End() - spans.Start(tr))
}

func (spans Spans) Events(all []EventID, tr *Trace) []EventID {
	if len(all) == 0 {
		return nil
	}

	end := sort.Search(len(all), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= spans.End()
	})

	sTs := spans.Start(tr)

	start := sort.Search(len(all[:end]), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= sTs
	})

	return all[start:end]
}

type Span struct {
	// The Span type is carefully laid out to optimize its size and to avoid pointers, the latter so that the garbage
	// collector won't have to scan any memory of our millions of events. It is currently 32 bytes large, with no padding.
	//
	// Instead of pointers, fields like pc and event_ are indices into slices. event_ is a uint40, allowing for a total
	// of 1 trillion events, or 64 TiB worth of events. This size was chosen to eliminate padding.

	start trace.Timestamp
	end   trace.Timestamp
	// Stack frame this span represents, for sample tracks
	pc     uint64
	event_ [5]byte
	// at is an offset from the top of the stack, skipping over uninteresting runtime frames.
	at uint8
	// We track the scheduling state explicitly, instead of mapping from trace.Event.Type, because we apply pattern
	// matching to stack traces that may result in more accurate states. For example, we can determine
	// stateBlockedSyncOnce from the stack trace, and we would otherwise use stateBlockedSync.
	state schedulingState
	tags  spanTags
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
		return tr.Event(ev).Ts >= s.end
	})

	sTs := s.start
	start := sort.Search(len(all[:end]), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Ts >= sTs
	})

	return all[start:end]
}

//gcassert:inline
func (s *Span) event() EventID {
	return EventID(s.event_[0]) |
		EventID(s.event_[1])<<8 |
		EventID(s.event_[2])<<16 |
		EventID(s.event_[3])<<24 |
		EventID(s.event_[4])<<32
}

func fromUint40(n *[5]byte) int {
	if *n == ([5]byte{0xFF, 0xFF, 0xFF, 0xFF, 0xFF}) {
		return -1
	}

	return int(uint64(n[0]) |
		uint64(n[1])<<8 |
		uint64(n[2])<<16 |
		uint64(n[3])<<24 |
		uint64(n[4])<<32)
}

//gcassert:inline
func packEventID(id EventID) [5]byte {
	if debug && id >= 1<<40 {
		panic(fmt.Sprintf("id %d doesn't fit in uint40", id))
	}

	return [5]byte{
		byte(id),
		byte(id >> 8),
		byte(id >> 16),
		byte(id >> 24),
		byte(id >> 32),
	}
}

type Machine struct {
	id int32
	// OPT(dh): using Span for Ms is wasteful. We don't need tags, stacktrace offsets etc. We only care about what
	// processor is running at what time. The only benefit of reusing Span is that we can use the same code for
	// rendering Gs and Ms, but that doesn't seem worth the added cost.
	spans      Spans
	goroutines Spans
}

type Processor struct {
	id int32
	// OPT(dh): using Span for Ps is wasteful. We don't need tags, stacktrace offsets etc. We only care about what
	// goroutine is running at what time. The only benefit of reusing Span is that we can use the same code for
	// rendering Gs and Ps, but that doesn't seem worth the added cost.
	spans Spans
}

func (p *Processor) String() string {
	return fmt.Sprintf("processor %d", p.id)
}

// XXX goroutine 0 seems to be special and doesn't get (un)scheduled. look into that.

type Goroutine struct {
	id          uint64
	function    string
	spans       Spans
	userRegions []Spans
	events      []EventID
	cpuSamples  []EventID

	statistics struct {
		blocked, inactive, running, gcAssist             time.Duration
		blockedPct, inactivePct, runningPct, gcAssistPct float32
	}
}

func (g *Goroutine) computeStatistics(tr *Trace) {
	start := g.spans.Start(tr)
	end := g.spans.End()
	d := time.Duration(end - start)

	var blocked, inactive, running, gcAssist time.Duration
	for i := range g.spans {
		s := &g.spans[i]
		d := tr.Duration(s)
		switch s.state {
		case stateInactive:
			inactive += d
		case stateActive, stateGCDedicated, stateGCIdle:
			running += d
		case stateBlocked:
			blocked += d
		case stateBlockedSend:
			blocked += d
		case stateBlockedRecv:
			blocked += d
		case stateBlockedSelect:
			blocked += d
		case stateBlockedSync:
			blocked += d
		case stateBlockedSyncOnce:
			blocked += d
		case stateBlockedSyncTriggeringGC:
			blocked += d
		case stateBlockedCond:
			blocked += d
		case stateBlockedNet:
			blocked += d
		case stateBlockedGC:
			blocked += d
		case stateBlockedSyscall:
			blocked += d
		case stateStuck:
			blocked += d
		case stateReady:
			inactive += d
		case stateCreated:
			inactive += d
		case stateGCMarkAssist:
			gcAssist += d
		case stateGCSweep:
			gcAssist += d
		case stateDone:
		default:
			if debug {
				panic(fmt.Sprintf("unknown state %d", s.state))
			}
		}
	}

	g.statistics = struct {
		blocked     time.Duration
		inactive    time.Duration
		running     time.Duration
		gcAssist    time.Duration
		blockedPct  float32
		inactivePct float32
		runningPct  float32
		gcAssistPct float32
	}{
		blocked:  blocked,
		inactive: inactive,
		running:  running,
		gcAssist: gcAssist,

		blockedPct:  float32(blocked) / float32(d) * 100,
		inactivePct: float32(inactive) / float32(d) * 100,
		runningPct:  float32(running) / float32(d) * 100,
		gcAssistPct: float32(gcAssist) / float32(d) * 100,
	}
}

func (g *Goroutine) String() string {
	// OPT(dh): cache this. especially because it gets called a lot by the goroutine selector window.
	if g.function == "" {
		// At least GCSweepStart can happen on g0
		return local.Sprintf("goroutine %d", g.id)
	} else {
		return local.Sprintf("goroutine %d: %s", g.id, g.function)
	}
}

type Task struct {
	// OPT(dh): Technically we only need the EventID field, everything else can be extracted from the event on demand.
	// But there are probably not enough tasks to make that worth it.
	id    uint64
	name  string
	event EventID
}

type setProgresser interface {
	SetProgress(float32)
	SetProgressLossy(float32)
}

func loadTrace(f io.Reader, progresser setProgresser) (*Trace, error) {
	const ourStages = 1
	const totalStages = trace.Stages + ourStages

	var ms []*Machine
	var gs []*Goroutine
	var ps []*Processor
	var gc Spans
	var stw Spans
	var tasks []*Task

	p, err := trace.NewParser(f)
	if err != nil {
		return nil, err
	}
	p.Progress = func(stage, cur, total int) {
		progress := (float32(cur) / float32(total)) / totalStages
		progress += (1.0 / totalStages) * float32(stage)

		progresser.SetProgress(progress)
	}
	res, err := p.Parse()
	if err != nil {
		return nil, err
	}

	if exitAfterParsing {
		return nil, errExitAfterParsing
	}

	var evTypeToState = [...]schedulingState{
		trace.EvGoBlockSend:   stateBlockedSend,
		trace.EvGoBlockRecv:   stateBlockedRecv,
		trace.EvGoBlockSelect: stateBlockedSelect,
		trace.EvGoBlockSync:   stateBlockedSync,
		trace.EvGoBlockCond:   stateBlockedCond,
		trace.EvGoBlockNet:    stateBlockedNet,
		trace.EvGoBlockGC:     stateBlockedGC,
		trace.EvGoBlock:       stateBlocked,
	}

	gsByID := map[uint64]*Goroutine{}
	getG := func(gid uint64) *Goroutine {
		g, ok := gsByID[gid]
		if ok {
			return g
		}
		g = &Goroutine{id: gid}
		gsByID[gid] = g
		return g
	}
	psByID := map[int32]*Processor{}
	getP := func(pid int32) *Processor {
		p, ok := psByID[pid]
		if ok {
			return p
		}
		p = &Processor{id: pid}
		psByID[pid] = p
		return p
	}
	msByID := map[int32]*Machine{}
	getM := func(mid int32) *Machine {
		m, ok := msByID[mid]
		if ok {
			return m
		}
		m = &Machine{id: mid}
		msByID[mid] = m
		return m
	}

	// map from gid to stack ID
	lastSyscall := map[uint64]uint32{}
	// map from P to last M it ran on
	lastMPerP := map[int32]int32{}
	// set of gids currently in mark assist
	inMarkAssist := map[uint64]struct{}{}

	// FIXME(dh): rename function. or remove it alright
	addEventToCurrentSpan := func(gid uint64, ev EventID) {
		if gid == 0 {
			// FIXME(dh): figure out why we have events for g0 when there are no spans on g0.
			return
		}
		g := getG(gid)
		g.events = append(g.events, ev)
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
			eventsPerM[int32(ev.Args[0])]++
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
		getG(gid).spans = make(Spans, 0, n)
	}
	for pid, n := range eventsPerP {
		getP(pid).spans = make(Spans, 0, n)
	}
	for mid, n := range eventsPerM {
		getM(mid).spans = make(Spans, 0, n)
	}

	userRegionDepths := map[uint64]int{}
	for evID := range res.Events {
		ev := &res.Events[evID]
		if evID%10000 == 0 {
			progresser.SetProgressLossy(((1.0 / totalStages) * (trace.Stages + 0)) + (float32(evID)/float32(len(res.Events)))/totalStages)
		}
		var gid uint64
		var state schedulingState
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
					getG(gid).function = res.PCs[stack[0]].Fn
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
			state = stateCreated
		case trace.EvGoStart:
			// ev.G starts running
			gid = ev.G
			pState = pRunG

			if _, ok := inMarkAssist[gid]; ok {
				state = stateGCMarkAssist
			} else {
				state = stateActive
			}
		case trace.EvGoStartLabel:
			// ev.G starts running
			// TODO(dh): make use of the label
			gid = ev.G
			pState = pRunG
			state = stateActive

			switch res.Strings[ev.Args[trace.ArgGoStartLabelLabelID]] {
			case "GC (dedicated)":
				state = stateGCDedicated
			case "GC (idle)":
				state = stateGCIdle
			}
		case trace.EvGoStop:
			// ev.G is stopping
			gid = ev.G
			pState = pStopG
			state = stateStuck
		case trace.EvGoEnd:
			// ev.G is ending
			gid = ev.G
			pState = pStopG
			state = stateDone
		case trace.EvGoSched:
			// ev.G calls Gosched
			gid = ev.G
			pState = pStopG
			state = stateInactive
		case trace.EvGoSleep:
			// ev.G calls Sleep
			gid = ev.G
			pState = pStopG
			state = stateInactive
		case trace.EvGoPreempt:
			// ev.G got preempted
			gid = ev.G
			pState = pStopG
			state = stateReady
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
				if blockedIsInactive(gsByID[gid].function) {
					state = stateInactive
				}
			}
		case trace.EvGoWaiting:
			// ev.G is blocked when tracing starts
			gid = ev.G
			state = stateBlocked
			if blockedIsInactive(gsByID[gid].function) {
				state = stateInactive
			}
		case trace.EvGoUnblock:
			// ev.G is unblocking ev.Args[0]
			addEventToCurrentSpan(ev.G, EventID(evID))
			gid = ev.Args[trace.ArgGoUnblockG]
			state = stateReady
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
			state = stateBlockedSyscall

		case trace.EvGoInSyscall:
			gid = ev.G
			state = stateBlockedSyscall
		case trace.EvGoSysExit:
			gid = ev.G
			state = stateReady

		case trace.EvProcStart:
			mid := ev.Args[0]
			m := getM(int32(mid))
			m.spans = append(m.spans, Span{start: ev.Ts, end: -1, state: stateRunningP, event_: packEventID(EventID(evID))})
			lastMPerP[ev.P] = m.id
			continue
		case trace.EvProcStop:
			m := getM(lastMPerP[ev.P])
			span := &m.spans[len(m.spans)-1]
			span.end = ev.Ts
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
			state = stateGCMarkAssist
			inMarkAssist[gid] = struct{}{}
		case trace.EvGCMarkAssistDone:
			// The counterpart to EvGCMarkAssistStop.

			gid = ev.G
			state = stateActive
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
			state = stateGCSweep
		case trace.EvGCSweepDone:
			// The counterpart to EvGcSweepStart.

			if ev.G == 0 {
				// See EvGCSweepStart for why.
				continue
			}

			gid = ev.G
			state = stateActive

		case trace.EvGCStart:
			gc = append(gc, Span{start: ev.Ts, state: stateActive, event_: packEventID(EventID(evID))})
			continue

		case trace.EvGCSTWStart:
			stw = append(stw, Span{start: ev.Ts, state: stateActive, event_: packEventID(EventID(evID))})
			continue

		case trace.EvGCDone:
			// XXX verify that index isn't out of bounds
			gc[len(gc)-1].end = ev.Ts
			continue

		case trace.EvGCSTWDone:
			// Even though STW happens as part of GC, we can see EvGCSTWDone after EvGCDone.
			// XXX verify that index isn't out of bounds
			stw[len(stw)-1].end = ev.Ts
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
				id:    ev.Args[trace.ArgUserTaskCreateTaskID],
				name:  res.Strings[ev.Args[trace.ArgUserTaskCreateTypeID]],
				event: EventID(evID),
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
					start:  ev.Ts,
					event_: packEventID(EventID(evID)),
					state:  stateUserRegion,
					end:    end,
				}
				g := getG(ev.G)
				depth := userRegionDepths[gid]
				if depth >= len(g.userRegions) {
					if depth < cap(g.userRegions) {
						g.userRegions = g.userRegions[:depth+1]
					} else {
						s := make([]Spans, depth+1)
						copy(s, g.userRegions)
						g.userRegions = s
					}
				}
				g.userRegions[depth] = append(g.userRegions[depth], s)
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
			g := getG(ev.G)
			g.cpuSamples = append(g.cpuSamples, EventID(evID))
			continue

		default:
			return nil, fmt.Errorf("unsupported trace event %d", ev.Type)
		}

		if debug {
			if s := getG(gid).spans; len(s) > 0 {
				if len(s) == 1 && ev.Type == trace.EvGoWaiting && s[0].state == stateInactive {
					// The execution trace emits GoCreate + GoWaiting for goroutines that already exist at the start of
					// tracing if they're in a blocked state. This causes a transition from inactive to blocked, which we
					// wouldn't normally permit.
				} else {
					prevState := s[len(s)-1].state
					if !legalStateTransitions[prevState][state] {
						panic(fmt.Sprintf("illegal state transition %d -> %d for goroutine %d, time %d", prevState, state, gid, ev.Ts))
					}
				}
			}
		}

		s := Span{start: ev.Ts, state: state, event_: packEventID(EventID(evID))}
		if ev.Type == trace.EvGoSysBlock {
			if debug && res.Events[s.event()].StkID != 0 {
				panic("expected zero stack ID")
			}
			res.Events[s.event()].StkID = lastSyscall[ev.G]
		}

		getG(gid).spans = append(getG(gid).spans, s)

		switch pState {
		case pRunG:
			p := getP(ev.P)
			p.spans = append(p.spans, Span{start: ev.Ts, state: stateRunningG, event_: packEventID(EventID(evID))})
			mid := lastMPerP[p.id]
			m := getM(mid)
			m.goroutines = append(m.goroutines, Span{start: ev.Ts, event_: packEventID(EventID(evID)), state: stateRunningG})
		case pStopG:
			// XXX guard against malformed traces
			p := getP(ev.P)
			p.spans[len(p.spans)-1].end = ev.Ts
			mid := lastMPerP[p.id]
			m := getM(mid)
			m.goroutines[len(m.goroutines)-1].end = ev.Ts
		}
	}

	sem := make(chan struct{}, runtime.GOMAXPROCS(0))
	var wg sync.WaitGroup
	for _, g := range gsByID {
		sem <- struct{}{}
		g := g
		wg.Add(1)
		go func() {
			for i, s := range g.spans {
				if i != len(g.spans)-1 {
					s.end = g.spans[i+1].start
				}

				stack := res.Stacks[res.Events[s.event()].StkID]
				s = applyPatterns(s, res.PCs, stack)

				// move s.At out of the runtime
				for int(s.at+1) < len(stack) && s.at < 255 && strings.HasPrefix(res.PCs[stack[s.at]].Fn, "runtime.") {
					s.at++
				}

				g.spans[i] = s
			}

			if len(g.spans) != 0 {
				last := g.spans[len(g.spans)-1]
				if last.state == stateDone {
					// The goroutine has ended
					// XXX the event probably has a stack associated with it, which we shouldn't discard.
					g.spans = g.spans[:len(g.spans)-1]
				} else {
					// XXX somehow encode open-ended traces
					g.spans[len(g.spans)-1].end = res.Events[len(res.Events)-1].Ts
				}
			}

			for depth, spans := range g.userRegions {
				if len(spans) != 0 {
					if last := &spans[len(spans)-1]; last.end == -1 {
						// The user region wasn't closed before the trace ended; give it the maximum length possible,
						// that of the parent user region, or the goroutine if this is the top-most user region.

						if depth == 0 {
							last.end = g.spans.End()
						} else {
							// OPT(dh): use binary search
							for _, parent := range g.userRegions[depth-1] {
								// The first parent user region that ends after our region starts has to be our parent.
								if parent.end >= last.start {
									last.end = parent.end
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
		if len(g.spans) != 0 {
			// OPT(dh): preallocate gs
			gs = append(gs, g)
		}
	}

	sort.Slice(gs, func(i, j int) bool {
		return gs[i].id < gs[j].id
	})

	for _, p := range psByID {
		// OPT(dh): preallocate ps
		ps = append(ps, p)
	}

	sort.Slice(ps, func(i, j int) bool {
		return ps[i].id < ps[j].id
	})

	for _, m := range msByID {
		// OPT(dh): preallocate ms
		ms = append(ms, m)
		if len(m.spans) > 0 {
			if last := &m.spans[len(m.spans)-1]; last.end == -1 {
				last.end = res.Events[len(res.Events)-1].Ts
			}
		}
	}

	sort.Slice(ms, func(i, j int) bool {
		return ms[i].id < ms[j].id
	})

	if exitAfterLoading {
		return nil, errExitAfterLoading
	}

	slices.SortFunc(tasks, func(a, b *Task) bool {
		return a.id < b.id
	})

	tr := &Trace{
		gs:          gs,
		gsByID:      gsByID,
		ps:          ps,
		ms:          ms,
		gc:          gc,
		stw:         stw,
		tasks:       tasks,
		ParseResult: res,
	}
	for _, g := range tr.gs {
		g.computeStatistics(tr)
	}

	return tr, nil
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

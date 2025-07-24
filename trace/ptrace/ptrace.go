// Package ptrace processes a runtime trace and enriches it with additional information.
package ptrace

import (
	"cmp"
	"errors"
	"fmt"
	"io"
	"log"
	"reflect"
	"runtime"
	"slices"
	"sort"
	"strings"
	"sync"
	"time"

	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/stuff/container/maybe"

	exptrace "golang.org/x/exp/trace"
)

type SchedulingState uint8

const (
	StateNone SchedulingState = iota

	StateUndetermined

	// Goroutine states
	StateInactive
	StateActive
	StateGCIdle
	StateGCDedicated
	StateGCFractional
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
	StateWaitingPreempted
	StateStuck
	StateReady
	StateCreated
	StateDone
	StateGCMarkAssist
	StateGCSweep

	// Special states used by user regions and stack frames
	StateUserRegion
	StateTask
	StateStack
	StateCPUSample

	// Processor states
	// The proc is running but doesn't have a G
	StateProcRunningNoG
	// The proc is running and has a G
	StateProcRunningG
	// The proc is running and has a G, but the G is blocked
	StateProcRunningBlocked

	StateLast
)

type Metric struct {
	Timestamps []exptrace.Time
	Values     []uint64
}

type Trace struct {
	// OPT(dh): can we get rid of all these pointers?
	Goroutines    []*Goroutine
	Processors    []*Processor
	Machines      []*Machine
	Functions     map[string]*Function
	GC            []Span
	STW           []Span
	Tasks         []*Task
	Metrics       map[string]Metric
	CPUSamples    []EventID
	CPUSamplesByG map[exptrace.GoID][]EventID
	CPUSamplesByP map[exptrace.ProcID][]EventID
	Events        mem.LargeBucketSlice[exptrace.Event]
	PCs           map[uint64]exptrace.StackFrame
	Stacks        map[exptrace.Stack][]uint64

	gsByID map[exptrace.GoID]*Goroutine
	// psByID and msById will be unset after parsing finishes
	psByID map[exptrace.ProcID]*Processor
	msByID map[exptrace.ThreadID]*Machine
}

func (t *Trace) addStack(stk exptrace.Stack) {
	if stk == exptrace.NoStack {
		return
	}

	if _, ok := t.Stacks[stk]; ok {
		return
	}

	type dataTable struct {
		present []uint8
		dense   [][]uint64
		sparse  map[uint64][]uint64
	}

	get := func(d *dataTable, id uint64) ([]uint64, bool) {
		if id == 0 {
			return nil, true
		}
		if uint64(id) < uint64(len(d.dense)) {
			if d.present[id/8]&(uint8(1)<<(id%8)) != 0 {
				return d.dense[id], true
			}
		} else if d.sparse != nil {
			if data, ok := d.sparse[id]; ok {
				return data, true
			}
		}
		return nil, false
	}

	// Get PC slices directly from exptrace, instead of copying them.
	table := reflect.ValueOf(stk).FieldByName("table")
	stacks := (*dataTable)(table.Elem().FieldByName("stacks").Addr().UnsafePointer())
	pcs, _ := get(stacks, reflect.ValueOf(stk).FieldByName("id").Uint())
	t.Stacks[stk] = pcs

	for f := range stk.Frames() {
		t.PCs[f.PC] = f
	}
}

// Start returns the time of the first event in the trace.
func (t *Trace) Start() exptrace.Time {
	if t.Events.Len() == 0 {
		return 0
	}
	return t.Events.Ptr(0).Time()
}

// End returns the time of the last event in the trace.
func (t *Trace) End() exptrace.Time {
	if t.Events.Len() == 0 {
		return 0
	}

	return t.Events.Ptr(t.Events.Len() - 1).Time()
}

// Duration returns the time from the first to the last event in the trace.
func (t *Trace) Duration() time.Duration {
	return time.Duration(t.End() - t.Start())
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
	exptrace.StackFrame
	// Sequential ID of function in the trace
	SeqID      int
	Goroutines []*Goroutine
}

func (fn Function) String() string {
	return fn.Func
}

type Goroutine struct {
	ID     exptrace.GoID
	Parent exptrace.GoID
	// Sequential ID of goroutine in the trace
	SeqID    int
	Function *Function
	Spans    []Span
	Ranges   map[string][]Span
	// The actual Start and end times of the goroutine. While spans are bounded to 0 and the end of the trace, the
	// actual Start and end of the goroutine might be unknown.
	Start       maybe.Option[exptrace.Time]
	End         maybe.Option[exptrace.Time]
	UserRegions [][]Span
	Events      []EventID
}

func (g *Goroutine) EffectiveStart() exptrace.Time {
	if ts, ok := g.Start.Get(); ok {
		return ts
	} else {
		if len(g.Spans) != 0 {
			return g.Spans[0].Start
		} else {
			return 0
		}
	}
}

func (g *Goroutine) EffectiveEnd() exptrace.Time {
	if ts, ok := g.End.Get(); ok {
		return ts
	} else {
		if len(g.Spans) != 0 {
			return g.Spans[len(g.Spans)-1].End
		} else {
			return 0
		}
	}
}

type Machine struct {
	ID exptrace.ThreadID
	// Sequential ID of machine in the trace
	SeqID int
	// OPT(dh): using Span for Ms is wasteful. We don't need tags, stacktrace offsets etc. We only care about what
	// processor is running at what time. The only benefit of reusing Span is that we can use the same code for
	// rendering Gs and Ms, but that doesn't seem worth the added cost.
	Spans      []Span
	Goroutines []Span
}

type Processor struct {
	ID exptrace.ProcID
	// Sequential ID of processor in the trace
	SeqID  int
	Spans  []Span
	Ranges map[string][]Span

	// Labels used for spans representing this processor
	// spanLabels []string
	// Strings used for matching timeline filters
	// filterLabels []string
}

type Task struct {
	// OPT(dh): Technically we only need the StartEventID field, everything else can be extracted from the event on demand.
	// But there are probably not enough tasks to make that worth it.
	ID     exptrace.TaskID
	Parent exptrace.TaskID
	// Sequential ID of task in the trace
	SeqID      int
	Name       string
	Start      maybe.Option[exptrace.Time]
	End        maybe.Option[exptrace.Time]
	StartEvent EventID
	EndEvent   EventID
	Spans      []Span
	Events     []EventID
}

func (t *Task) Stub() bool {
	return t.StartEvent == 0
}

func (t *Task) EffectiveStart() exptrace.Time {
	if ts, ok := t.Start.Get(); ok {
		return ts
	} else {
		if len(t.Spans) != 0 {
			return t.Spans[0].Start
		} else {
			return 0
		}
	}
}

func (t *Task) EffectiveEnd() exptrace.Time {
	if ts, ok := t.End.Get(); ok {
		return ts
	} else {
		if len(t.Spans) != 0 {
			return t.Spans[len(t.Spans)-1].End
		} else {
			return 0
		}
	}
}

func (s *Span) StartedBeforeTrace(tr *Trace) bool {
	if s.State == StateUndetermined {
		return true
	}
	if s.StartEvent != NoEvent {
		ev := tr.Event(s.StartEvent)
		if ev.Kind() == exptrace.EventStateTransition {
			trans := ev.StateTransition()
			switch trans.Resource.Kind {
			case exptrace.ResourceNone:
			case exptrace.ResourceThread:
			case exptrace.ResourceProc:
				from, _ := trans.Proc()
				if from == exptrace.ProcUndetermined {
					return true
				}
			case exptrace.ResourceGoroutine:
				from, _ := trans.Goroutine()
				if from == exptrace.GoUndetermined {
					return true
				}
			default:
				panic(fmt.Sprintf("unhandled kind %s", trans.Resource.Kind))
			}
		}
	}
	return false
}

type Span struct {
	// The Span type is carefully laid out to optimize its size and to avoid pointers, the latter so that the garbage
	// collector won't have to scan any memory of our millions of events.
	//
	// Instead of pointers, fields like PC and StartEvent are indices into slices.

	Start exptrace.Time
	End   exptrace.Time
	// The event that started the span.
	StartEvent EventID
	// The event that ended the span. Usually this is the start event of the next span.
	EndEvent EventID
	// At is an offset from the top of the stack, skipping over uninteresting runtime frames.
	At uint8
	// We track the scheduling State explicitly, instead of mapping from trace.StartEvent.Type, because we apply pattern
	// matching to stack traces that may result in more accurate states. For example, we can determine
	// stateBlockedSyncOnce from the stack trace, and we would otherwise use stateBlockedSync.
	State SchedulingState
	Tags  SpanTags
	// The kind of span. This primarily affects the kinds of events you can find in StartEvent and EndEvent.
	Kind SpanKind
}

type SpanKind uint8

const (
	SpanKindNone SpanKind = iota
	// The span encompasses a state transition into and a state transition out of the span.
	SpanKindStateTransition
	SpanKindCustom
)

// The ID of an event. The zero ID indicates the lack of an event.
type EventID int32

const NoEvent EventID = -1

func Parse(r *exptrace.Reader, progress func(float64)) (*Trace, error) {
	tr := &Trace{
		Functions:     map[string]*Function{},
		gsByID:        map[exptrace.GoID]*Goroutine{},
		psByID:        map[exptrace.ProcID]*Processor{},
		msByID:        map[exptrace.ThreadID]*Machine{},
		CPUSamplesByG: map[exptrace.GoID][]EventID{},
		CPUSamplesByP: map[exptrace.ProcID][]EventID{},
		Metrics:       map[string]Metric{},
		GC:            make(spansSlice, 0),
		STW:           make(spansSlice, 0),
		PCs:           make(map[uint64]exptrace.StackFrame),
		Stacks:        make(map[exptrace.Stack][]uint64),
	}

	makeProgresser := func(stage int, numStages int) func(float64) {
		return func(p float64) {
			if p > 1 {
				panic(p)
			}
			progress(float64(stage-1)/float64(numStages) + p/float64(numStages))
		}
	}

	if err := processEvents(r, tr, makeProgresser(1, 4)); err != nil {
		return nil, err
	}

	populateObjects(tr, makeProgresser(2, 4))
	postProcessSpans(tr, makeProgresser(3, 4))

	tr.psByID = nil
	tr.msByID = nil

	return tr, nil
}

type rangeScope uint8

const (
	rangeScopeUnknown = iota
	rangeScopeSTW
	rangeScopeGC
	rangeScopeGlobal
	rangeScopeThread
	rangeScopeProc
	rangeScopeGoroutine
)

func rangeActualScope(r exptrace.Range) rangeScope {
	if strings.HasPrefix(r.Name, "stop-the-world") {
		return rangeScopeSTW
	}
	switch r.Name {
	case "GC mark assist":
		// User goroutines may be asked to assist the GC's mark phase. This happens when the goroutine allocates
		// memory and some condition is true. When that happens, the tracer starts a "GC mark assist" range for that
		// goroutine.
		return rangeScopeGoroutine
	case "GC concurrent mark phase":
		return rangeScopeGC
	case "GC incremental sweep":
		// XXX
	}

	switch r.Scope.Kind {
	case exptrace.ResourceGoroutine:
		return rangeScopeGoroutine
	case exptrace.ResourceProc:
		return rangeScopeProc
	case exptrace.ResourceThread:
		return rangeScopeThread
	case exptrace.ResourceNone:
		return rangeScopeGlobal
	default:
		return rangeScopeUnknown
	}
}

// runningGauge builds up a gauge over time.
type runningGauge struct {
	current    int64
	timestamps []exptrace.Time
	values     []uint64
}

// add adds a value to the gauge at time t.
// It assumes that t will not decrease with subsequent calls.
func (rg *runningGauge) add(t exptrace.Time, v int64) {
	rg.current += v
	if idx := len(rg.timestamps) - 1; idx >= 0 && rg.timestamps[idx] == t {
		rg.values[idx] = uint64(rg.current)
	} else {
		rg.timestamps = append(rg.timestamps, t)
		rg.values = append(rg.values, uint64(rg.current))
	}
}

type goroutineMetrics struct {
	runnableGoroutines runningGauge
	runningGoroutines  runningGauge
	blockedGoroutines  runningGauge
}

func processEvents(r *exptrace.Reader, tr *Trace, progress func(float64)) error {
	// OPT(dh): evaluate reading all events in one pass, then preallocating []Span slices based on the number
	// of events we saw for Ps and Gs.
	getG := func(gid exptrace.GoID) *Goroutine {
		g, ok := tr.gsByID[gid]
		if ok {
			return g
		}
		g = &Goroutine{
			ID: gid,
			// XXX only allocate map once we have a range to store
			Ranges: map[string][]Span{},
		}
		tr.gsByID[gid] = g
		return g
	}
	getP := func(pid exptrace.ProcID) *Processor {
		p, ok := tr.psByID[pid]
		if ok {
			return p
		}
		p = &Processor{
			ID: pid,
			// XXX only allocate map once we have a range to store
			Ranges: map[string][]Span{},
			// spanLabels: []string{local.Sprintf("p%d", pid)},
		}
		tr.psByID[pid] = p
		return p
	}
	addEventToCurrentSpan := func(gid exptrace.GoID, ev EventID) {
		g := getG(gid)
		g.Events = append(g.Events, ev)
	}
	getTask := func(taskID exptrace.TaskID) *Task {
		idx, ok := tr.task(taskID)
		if ok {
			return tr.Tasks[idx]
		}

		// The task with the given ID doesn't exist. This can happen in well-formed traces when the task
		// was created before tracing began.
		task := &Task{
			ID: taskID,
		}
		tr.Tasks = slices.Insert(tr.Tasks, idx, task)
		return task
	}
	addEventToTask := func(taskID exptrace.TaskID, ev EventID) {
		t := getTask(taskID)
		t.Events = append(t.Events, ev)
	}

	synced := false
	userRegionDepths := map[exptrace.GoID]int{}
	var traceStart exptrace.Time
	var gm goroutineMetrics
	for {
		ev, err := r.ReadEvent()
		if err != nil {
			if err == io.EOF {
				break
			}
			return err
		}
		// fmt.Println(ev)

		// TODO(dh): call progress

		if tr.Events.Len() == 0 {
			traceStart = ev.Time()
			gm.runningGoroutines.add(traceStart, 0)
			gm.runnableGoroutines.add(traceStart, 0)
			gm.blockedGoroutines.add(traceStart, 0)
		}

		evID := EventID(tr.Events.Len())
		tr.Events.Append(ev)

		// Cache all stacks
		tr.addStack(ev.Stack())
		if ev.Kind() == exptrace.EventStateTransition {
			tr.addStack(ev.StateTransition().Stack)
		}

		switch ev.Kind() {
		case exptrace.EventSync:
			synced = true
		case exptrace.EventLabel:
			l := ev.Label()
			switch l.Resource.Kind {
			case exptrace.ResourceGoroutine:
				g := getG(l.Resource.Goroutine())
				if len(g.Spans) == 0 {
					return fmt.Errorf("got label for goroutine %d but it has no spans", g.ID)
				}
				span := &g.Spans[len(g.Spans)-1]
				if tr.Events.Ptr(int(span.StartEvent)).Kind() != exptrace.EventStateTransition {
					return fmt.Errorf("got label for goroutine %d but last span isn't a state transition", g.ID)
				}
				switch l.Label {
				case "GC (dedicated)":
					span.State = StateGCDedicated
				case "GC (idle)":
					span.State = StateGCIdle
				case "GC (fractional)":
					span.State = StateGCFractional
				default:
					log.Printf("unhandled label %q", l.Label)
				}
			default:
				panic(fmt.Sprintf("unhandled kind %s", l.Resource.Kind))
			}
		case exptrace.EventLog:
			addEventToCurrentSpan(ev.Goroutine(), evID)
			l := ev.Log()
			if l.Task != exptrace.NoTask {
				addEventToTask(l.Task, evID)
			}
		case exptrace.EventMetric:
			m := ev.Metric()
			mm := tr.Metrics[m.Name]
			mm.Timestamps = append(mm.Timestamps, ev.Time())
			mm.Values = append(mm.Values, m.Value.Uint64())
			tr.Metrics[m.Name] = mm
		case exptrace.EventRangeActive:
			if synced {
				// We're being told about a range we must've already seen a RangeBegin for
				continue
			}
			fallthrough
		case exptrace.EventRangeBegin:
			r := ev.Range()
			var s Span
			if ev.Kind() == exptrace.EventRangeActive {
				s = Span{StartEvent: evID, Start: tr.Events.Ptr(0).Time(), EndEvent: -1}
			} else {
				s = Span{Start: ev.Time(), StartEvent: evID, EndEvent: -1}
			}

			switch scope := rangeActualScope(r); scope {
			case rangeScopeUnknown:
			case rangeScopeGC:
				tr.GC = append(tr.GC, s)
			case rangeScopeSTW:
				tr.STW = append(tr.STW, s)
			case rangeScopeGoroutine:
				g := getG(r.Scope.Goroutine())
				g.Ranges[r.Name] = append(g.Ranges[r.Name], s)
			case rangeScopeProc:
				p := getP(r.Scope.Proc())
				p.Ranges[r.Name] = append(p.Ranges[r.Name], s)
			case rangeScopeThread:
				// XXX implement
			case rangeScopeGlobal:
				// XXX implement
			default:
				panic(fmt.Sprintf("unhandled range scope %d for range %q", scope, r.Name))
			}
		case exptrace.EventRangeEnd:
			r := ev.Range()
			var prev *Span
			switch scope := rangeActualScope(r); scope {
			case rangeScopeUnknown:
				continue
			case rangeScopeGC:
				prev = &tr.GC[len(tr.GC)-1]
			case rangeScopeSTW:
				prev = &tr.STW[len(tr.STW)-1]
			case rangeScopeGoroutine:
				g := getG(r.Scope.Goroutine())
				prev = &g.Ranges[r.Name][len(g.Ranges[r.Name])-1]
			case rangeScopeProc:
				p := getP(r.Scope.Proc())
				prev = &p.Ranges[r.Name][len(p.Ranges[r.Name])-1]
			case rangeScopeThread:
				// XXX implement
			case rangeScopeGlobal:
				// XXX implement
			default:
				panic(fmt.Sprintf("unhandled range scope %d", scope))
			}
			prev.End = ev.Time()
			prev.EndEvent = evID
		case exptrace.EventRegionBegin:
			s := Span{
				Start:      ev.Time(),
				StartEvent: evID,
				State:      StateUserRegion,
				EndEvent:   -1,
			}
			gid := ev.Goroutine()
			g := getG(gid)
			depth := userRegionDepths[gid]
			if depth >= len(g.UserRegions) {
				if depth < cap(g.UserRegions) {
					g.UserRegions = g.UserRegions[:depth+1]
				} else {
					s := make([][]Span, depth+1)
					copy(s, g.UserRegions)
					g.UserRegions = s
				}
			}
			if g.UserRegions[depth] == nil {
				g.UserRegions[depth] = make([]Span, 0)
			}
			g.UserRegions[depth] = append(g.UserRegions[depth], s)
			userRegionDepths[gid]++

			r := ev.Region()
			// XXX add a default background task with ID 0
			if r.Task != 0 && r.Task != exptrace.NoTask {
				// ensure the task exists
				getTask(r.Task)
			}
			continue
		case exptrace.EventRegionEnd:
			gid := ev.Goroutine()
			g := getG(gid)
			d := userRegionDepths[gid] - 1
			// We can see a region end without a region start if the two occured in different traces.
			if d >= 0 {
				ss := g.UserRegions[d]
				ss[len(ss)-1].EndEvent = evID
				ss[len(ss)-1].End = ev.Time()
				if d > 0 {
					userRegionDepths[gid] = d
				} else {
					delete(userRegionDepths, gid)
				}
			}
		case exptrace.EventStackSample:
			tr.CPUSamples = append(tr.CPUSamples, evID)
			if gid := ev.Goroutine(); gid != exptrace.NoGoroutine {
				tr.CPUSamplesByG[gid] = append(tr.CPUSamplesByG[gid], evID)
			}
			if pid := ev.Proc(); pid != exptrace.NoProc {
				tr.CPUSamplesByP[pid] = append(tr.CPUSamplesByP[pid], evID)
			}
		case exptrace.EventStateTransition:
			trans := ev.StateTransition()
			res := trans.Resource
			switch res.Kind {
			case exptrace.ResourceThread:
				// TODO(dh): support threads
			case exptrace.ResourceProc:
				from, to := trans.Proc()
				if from == to {
					// Enumeration during a generation
					continue
				}
				p := getP(trans.Resource.Proc())
				s := Span{
					Start:      ev.Time(),
					StartEvent: evID,
					Kind:       SpanKindStateTransition,
					EndEvent:   -1,
				}

				if from == exptrace.ProcUndetermined {
					s.Start = traceStart
				}

				if from != exptrace.ProcUndetermined && from != exptrace.ProcNotExist && from != exptrace.ProcIdle {
					prevSpan := &p.Spans[len(p.Spans)-1]
					prevSpan.End = ev.Time()
					prevSpan.EndEvent = evID
				}
				switch to {
				case exptrace.ProcRunning:
					s.State = StateProcRunningNoG
				case exptrace.ProcIdle:
					continue
				default:
					panic(fmt.Sprintf("unhandled state %s", to))
				}
				p.Spans = append(p.Spans, s)
			case exptrace.ResourceGoroutine:
				from, to := trans.Goroutine()
				if from == to {
					// Enumeration during a generation
					continue
				}
				g := getG(trans.Resource.Goroutine())
				s := Span{
					Start:      ev.Time(),
					StartEvent: evID,
					Kind:       SpanKindStateTransition,
					EndEvent:   -1,
				}

				if from == exptrace.GoUndetermined {
					s.Start = traceStart
				}
				switch {
				case from == exptrace.GoRunnable:
					gm.runnableGoroutines.add(ev.Time(), -1)
				case to == exptrace.GoRunnable:
					gm.runnableGoroutines.add(ev.Time(), 1)
				}
				switch {
				case from == exptrace.GoRunning:
					gm.runningGoroutines.add(ev.Time(), -1)
				case to == exptrace.GoRunning:
					gm.runningGoroutines.add(ev.Time(), 1)
				}
				fromIsBlocked := from == exptrace.GoSyscall || from == exptrace.GoWaiting
				toIsBlocked := to == exptrace.GoSyscall || to == exptrace.GoWaiting
				switch {
				case fromIsBlocked && !toIsBlocked:
					gm.blockedGoroutines.add(ev.Time(), -1)
				case !fromIsBlocked && toIsBlocked:
					gm.blockedGoroutines.add(ev.Time(), 1)
				}

				// XXX actually, for from == exptrace.GoUndetermined, we still need to do omst of the work to update P
				// spans. a proc may start, followed by a goroutine going from undetermined->running on that proc, and
				// we need to update the "running without G" span in the P.
				if from != exptrace.GoUndetermined && from != exptrace.GoNotExist {
					prevSpan := &g.Spans[len(g.Spans)-1]
					prevSpan.End = ev.Time()
					prevSpan.EndEvent = evID

					if ev.Proc() != exptrace.NoProc {
						switch to {
						case exptrace.GoNotExist:
							p := getP(ev.Proc())
							if len(p.Spans) == 0 {
								return errors.New("transitioning to GoNotExist but don't have existing span")
							}
							last := &p.Spans[len(p.Spans)-1]
							last.End = ev.Time()
							s := Span{
								Start:      ev.Time(),
								StartEvent: evID,
								State:      StateProcRunningNoG,
								EndEvent:   -1,
							}
							p.Spans = append(p.Spans, s)
						case exptrace.GoSyscall, exptrace.GoWaiting:
							p := getP(ev.Proc())
							last := &p.Spans[len(p.Spans)-1]
							last.End = ev.Time()
							s := Span{
								Start:      ev.Time(),
								StartEvent: evID,
								State:      StateProcRunningBlocked,
								EndEvent:   -1,
							}
							p.Spans = append(p.Spans, s)
						case exptrace.GoRunning:
							p := getP(ev.Proc())
							last := &p.Spans[len(p.Spans)-1]
							last.End = ev.Time()
							s := Span{
								Start:      ev.Time(),
								StartEvent: evID,
								State:      StateProcRunningG,
								EndEvent:   -1,
							}
							p.Spans = append(p.Spans, s)
						case exptrace.GoRunnable:
							// Nothing to do
						default:
							panic(fmt.Sprintf("unhandled state %s", to))
						}
					}
				}
				switch to {
				case exptrace.GoNotExist:
					g.End = maybe.Some(ev.Time())
					continue
				case exptrace.GoRunnable:
					if from == exptrace.GoNotExist {
						// Goroutine creation
						s.State = StateCreated
						g.Start = maybe.Some(ev.Time())
						g.Parent = ev.Goroutine()
						// ev.Goroutine is the goroutine that's creating us, versus g, which is the
						// created goroutine.
						addEventToCurrentSpan(ev.Goroutine(), evID)
					} else {
						if trans.Reason == "runtime.GoSched" || trans.Reason == "runtime.Gosched" {
							s.State = StateInactive
						} else {
							s.State = StateReady
						}
						if from == exptrace.GoWaiting {
							// ev.Goroutine is the goroutine that's unblocking us, versus g, which is the
							// unblocked goroutine.
							if ev.Goroutine() != exptrace.NoGoroutine {
								addEventToCurrentSpan(ev.Goroutine(), evID)
							}
						}
					}
				case exptrace.GoRunning:
					s.State = StateActive
				case exptrace.GoSyscall:
					s.State = StateBlockedSyscall
				case exptrace.GoWaiting:
					switch trans.Reason {
					case "chan send":
						s.State = StateBlockedSend
					case "chan receive":
						s.State = StateBlockedRecv
					case "network":
						s.State = StateBlockedNet
					case "runtime.GoSched", "runtime.Gosched":
						s.State = StateInactive
					case "select":
						s.State = StateBlockedSelect
					case "sleep":
						s.State = StateInactive
					case "sync":
						s.State = StateBlockedSync
					case "sync.(*Cond).Wait":
						s.State = StateBlockedCond
					case "system goroutine wait":
						s.State = StateInactive
					case "GC mark assist wait for work":
						s.State = StateInactive
					case "GC background sweeper wait":
						s.State = StateInactive
					case "preempted":
						s.State = StateWaitingPreempted
					case "forever":
						s.State = StateStuck
					case "wait for debug call":
						s.State = StateBlocked
					case "wait until GC ends":
						s.State = StateBlockedGC
					case "":
						s.State = StateBlocked
					default:
						log.Printf("unhandled reason %q", trans.Reason)
					}
				default:
					panic(fmt.Sprintf("unhandled state %s", to))
				}
				g.Spans = append(g.Spans, s)
			default:
				return fmt.Errorf("invalid resource kind %s", res.Kind)
			}
		case exptrace.EventTaskBegin:
			t := ev.Task()
			idx, ok := tr.task(t.ID)
			if ok {
				panic("task already exists")
			}
			task := &Task{
				ID:         t.ID,
				Parent:     t.Parent,
				Start:      maybe.Some(ev.Time()),
				StartEvent: evID,
				Name:       t.Type,
			}
			// Tasks may not be sorted in the trace, so we need to insert them at the correct position.
			// This will translate to an append in most cases.
			tr.Tasks = slices.Insert(tr.Tasks, idx, task)
			addEventToCurrentSpan(ev.Goroutine(), evID)
			if t.Parent != exptrace.NoTask {
				addEventToTask(t.Parent, evID)
			}
		case exptrace.EventTaskEnd:
			t := ev.Task()
			idx, ok := tr.task(t.ID)
			if !ok {
				// The task with the given ID doesn't exist. This can happen in well-formed traces when the task
				// was created before tracing began.
				task := &Task{
					ID:       t.ID,
					Parent:   t.Parent,
					End:      maybe.Some(ev.Time()),
					EndEvent: evID,
					Name:     t.Type,
				}
				tr.Tasks = slices.Insert(tr.Tasks, idx, task)
			} else {
				if tr.Tasks[idx].Stub() {
					// Fill the missing information
					tr.Tasks[idx].Parent = t.Parent
					tr.Tasks[idx].Name = t.Type
				}
				tr.Tasks[idx].End = maybe.Some(ev.Time())
				tr.Tasks[idx].EndEvent = evID
			}
			if t.Parent != exptrace.NoTask {
				addEventToTask(t.Parent, evID)
			}
		case exptrace.EventExperimental:
			// TODO(dh): do something useful with these
		default:
			panic(fmt.Sprintf("unhandled kind %s", ev.Kind()))
		}
	}

	// TODO(dh): try harder to figure out goroutines' functions
	for _, g := range tr.gsByID {
		for i := range g.Spans {
			s := &g.Spans[i]
			if s.StartEvent != 0 {
				if stk := tr.Event(s.StartEvent).StateTransition().Stack; stk != exptrace.NoStack {
					pcs := tr.Stacks[stk]
					if len(pcs) > 0 {
						f := tr.function(pcs[len(pcs)-1])
						g.Function = f
						f.Goroutines = append(f.Goroutines, g)
						break
					}
				}
			}
		}
	}

	tr.Metrics["/gotraceui/sched/goroutines/runnable:goroutines"] = Metric{
		Timestamps: gm.runnableGoroutines.timestamps,
		Values:     gm.runnableGoroutines.values,
	}
	tr.Metrics["/gotraceui/sched/goroutines/running:goroutines"] = Metric{
		Timestamps: gm.runningGoroutines.timestamps,
		Values:     gm.runningGoroutines.values,
	}
	tr.Metrics["/gotraceui/sched/goroutines/waiting:goroutines"] = Metric{
		Timestamps: gm.blockedGoroutines.timestamps,
		Values:     gm.blockedGoroutines.values,
	}

	return nil
}

func postProcessSpans(tr *Trace, progress func(float64)) {
	var wg sync.WaitGroup
	doG := func(g *Goroutine) {
		for i := range len(g.Spans) {
			s := g.Spans[i]
			pcs := tr.Stacks[tr.Events.Ptr(int(s.StartEvent)).Stack()]
			s = applyPatterns(tr, s, pcs)

			// move s.At out of the runtime
			for int(s.At+1) < len(pcs) && s.At < 255 && strings.HasPrefix(tr.PCs[pcs[s.At]].Func, "runtime.") {
				s.At++
			}

			g.Spans[i] = s
		}

		if len(g.Spans) != 0 {
			last := g.Spans[len(g.Spans)-1]
			if last.State == StateDone {
				// The goroutine has ended. We encode this as a zero length span.
				s := &g.Spans[len(g.Spans)-1]
				g.Spans = g.Spans[:len(g.Spans)-1]
				g.End = maybe.Some(s.Start)
			}
		}

		for depth, spans := range g.UserRegions {
			if len(spans) != 0 {
				if last := &spans[len(spans)-1]; last.End == -1 {
					// The user region wasn't closed before the trace ended; give it the maximum length possible,
					// that of the parent user region, or the goroutine if this is the top-most user region.

					if depth == 0 {
						last.End = g.Spans[len(g.Spans)-1].End
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

	fixEnds := func(spans []Span) {
		if len(spans) == 0 {
			return
		}
		s := &spans[len(spans)-1]
		if s.End == 0 {
			s.End = tr.Events.Ptr(tr.Events.Len() - 1).Time()
		}
	}

	step := len(tr.Goroutines) / runtime.GOMAXPROCS(0)
	if step == 0 {
		step = 1
	}

	var progressMu sync.Mutex
	var progressValue int
	for slice := range slices.Chunk(tr.Goroutines, step) {
		wg.Add(1)
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

	for _, g := range tr.Goroutines {
		fixEnds(g.Spans)
		for _, ranges := range g.Ranges {
			fixEnds(ranges)
		}
		for _, u := range g.UserRegions {
			fixEnds(u)
		}
	}
	for _, p := range tr.Processors {
		fixEnds(p.Spans)
		for _, ranges := range p.Ranges {
			fixEnds(ranges)
		}
	}
	for i := range tr.Tasks {
		tr.Tasks[i].Spans = []Span{
			{
				Start:      tr.Tasks[i].EffectiveStart(),
				End:        tr.Tasks[i].EffectiveEnd(),
				StartEvent: tr.Tasks[i].StartEvent,
				EndEvent:   tr.Tasks[i].EndEvent,
				State:      StateTask,
			},
		}
		fixEnds(tr.Tasks[i].Spans)
	}
	fixEnds(tr.GC)
	fixEnds(tr.STW)

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
		if len(p.Spans) != 0 {
			tr.Processors = append(tr.Processors, p)
		}
	}
	progress(2.0 / 5.0)

	for _, m := range tr.msByID {
		// OPT(dh): preallocate ms
		tr.Machines = append(tr.Machines, m)
		if len(m.Spans) > 0 {
			if last := &m.Spans[len(m.Spans)-1]; last.End == -1 {
				last.End = tr.Events.Ptr(tr.Events.Len() - 1).Time()
			}
		}
	}
	progress(3.0 / 5.0)

	sort.Slice(tr.Goroutines, func(i, j int) bool { return tr.Goroutines[i].ID < tr.Goroutines[j].ID })
	sort.Slice(tr.Processors, func(i, j int) bool { return tr.Processors[i].ID < tr.Processors[j].ID })
	sort.Slice(tr.Machines, func(i, j int) bool { return tr.Machines[i].ID < tr.Machines[j].ID })
	sort.Slice(tr.Tasks, func(i, j int) bool { return tr.Tasks[i].ID < tr.Tasks[j].ID })
	for _, f := range tr.Functions {
		slices.SortFunc(f.Goroutines, func(a, b *Goroutine) int { return cmp.Compare(a.SeqID, b.SeqID) })
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

func (t *Trace) function(pc uint64) *Function {
	frame := t.PCs[pc]
	f, ok := t.Functions[frame.Func]
	if ok {
		return f
	}
	f = &Function{
		StackFrame: frame,
		SeqID:      len(t.Functions),
	}
	t.Functions[frame.Func] = f
	return f
}

//gcassert:inline
func (t *Trace) Event(ev EventID) *exptrace.Event {
	return t.Events.Ptr(int(ev))
}

func (t *Trace) Task(id exptrace.TaskID) *Task {
	idx, ok := t.task(id)
	if !ok {
		panic(fmt.Sprintf("couldn't find task %d", id))
	}
	return t.Tasks[idx]
}

func (t *Trace) task(id exptrace.TaskID) (int, bool) {
	return sort.Find(len(t.Tasks), func(i int) int {
		oid := t.Tasks[i].ID
		if id == oid {
			return 0
		} else if id < oid {
			return -1
		} else {
			return 1
		}
	})
}

func (tr *Trace) G(gid exptrace.GoID) *Goroutine {
	// In a previous version we used binary search over Trace.gs. This didn't scale for traces with a lot of goroutines
	// because of how often getG has to be called during rendering. For example, for Sean's trace from hell, switching
	// from binary search to map lookups reduced frame times by 33%.

	g, found := tr.gsByID[gid]
	if !found {
		panic(fmt.Sprintf("couldn't find goroutine %d", gid))
	}
	return g
}

func (tr *Trace) P(pid exptrace.ProcID) *Processor {
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
	if len(all) == 0 {
		return nil
	}

	// The all argument contains all events in the span's container (e.g. a goroutine), sorted by timestamp,
	// as indices into the global list of events. Find the first and last event that overlaps with the span,
	// and that is the set of events belonging to this span.

	end := sort.Search(len(all), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Time() >= s.End
	})

	sTs := s.Start
	start := sort.Search(len(all[:end]), func(i int) bool {
		ev := all[i]
		return tr.Event(ev).Time() >= sTs
	})

	return all[start:end]
}

func IsGoroutineCreation(trans *exptrace.StateTransition) bool {
	if trans.Resource.Kind != exptrace.ResourceGoroutine {
		return false
	}
	from, to := trans.Goroutine()
	return from == exptrace.GoNotExist && to == exptrace.GoRunnable
}

func IsGoroutineUnblock(trans *exptrace.StateTransition) bool {
	if trans.Resource.Kind != exptrace.ResourceGoroutine {
		return false
	}
	from, to := trans.Goroutine()
	return from == exptrace.GoWaiting && to == exptrace.GoRunnable
}

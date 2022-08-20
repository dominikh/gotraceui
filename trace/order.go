// Copyright 2016 The Go Authors. All rights reserved.

package trace

type orderEvent struct {
	ev   Event
	pid  int32
	g    uint64
	init gState
	next gState
}

type gStatus int

type gState struct {
	seq    uint64
	status gStatus
}

const (
	gDead gStatus = iota
	gRunnable
	gRunning
	gWaiting

	unordered = ^uint64(0)
	garbage   = ^uint64(0) - 1
	noseq     = ^uint64(0)
	seqinc    = ^uint64(0) - 1
)

// stateTransition returns goroutine state (sequence and status) when the event
// becomes ready for merging (init) and the goroutine state after the event (next).
func stateTransition(ev Event) (g uint64, init, next gState) {
	switch ev.Type {
	case EvGoCreate:
		g = ev.Args[0]
		init = gState{0, gDead}
		next = gState{1, gRunnable}
	case EvGoWaiting, EvGoInSyscall:
		g = ev.G
		init = gState{1, gRunnable}
		next = gState{2, gWaiting}
	case EvGoStart, EvGoStartLabel:
		g = ev.G
		init = gState{ev.Args[1], gRunnable}
		next = gState{ev.Args[1] + 1, gRunning}
	case EvGoStartLocal:
		// noseq means that this event is ready for merging as soon as
		// frontier reaches it (EvGoStartLocal is emitted on the same P
		// as the corresponding EvGoCreate/EvGoUnblock, and thus the latter
		// is already merged).
		// seqinc is a stub for cases when event increments g sequence,
		// but since we don't know current seq we also don't know next seq.
		g = ev.G
		init = gState{noseq, gRunnable}
		next = gState{seqinc, gRunning}
	case EvGoBlock, EvGoBlockSend, EvGoBlockRecv, EvGoBlockSelect,
		EvGoBlockSync, EvGoBlockCond, EvGoBlockNet, EvGoSleep,
		EvGoSysBlock, EvGoBlockGC:
		g = ev.G
		init = gState{noseq, gRunning}
		next = gState{noseq, gWaiting}
	case EvGoSched, EvGoPreempt:
		g = ev.G
		init = gState{noseq, gRunning}
		next = gState{noseq, gRunnable}
	case EvGoUnblock, EvGoSysExit:
		g = ev.Args[0]
		init = gState{ev.Args[1], gWaiting}
		next = gState{ev.Args[1] + 1, gRunnable}
	case EvGoUnblockLocal, EvGoSysExitLocal:
		g = ev.Args[0]
		init = gState{noseq, gWaiting}
		next = gState{seqinc, gRunnable}
	case EvGCStart:
		g = garbage
		init = gState{ev.Args[0], gDead}
		next = gState{ev.Args[0] + 1, gDead}
	default:
		// no ordering requirements
		g = unordered
	}
	return
}

func transitionReady(g uint64, curr, init gState) bool {
	return g == unordered || (init.seq == noseq || init.seq == curr.seq) && init.status == curr.status
}

func transition(gs map[uint64]gState, g uint64, init, next gState) {
	if g == unordered {
		return
	}
	curr := gs[g]
	if !transitionReady(g, curr, init) {
		panic("event sequences are broken")
	}
	switch next.seq {
	case noseq:
		next.seq = curr.seq
	case seqinc:
		next.seq = curr.seq + 1
	}
	gs[g] = next
}

type orderEventList []orderEvent

func (l *orderEventList) Less(i, j int) bool {
	return (*l)[i].ev.Ts < (*l)[j].ev.Ts
}

type eventList []Event

func (l *eventList) Len() int {
	return len(*l)
}

func (l *eventList) Less(i, j int) bool {
	return (*l)[i].Ts < (*l)[j].Ts
}

func (l *eventList) Swap(i, j int) {
	(*l)[i], (*l)[j] = (*l)[j], (*l)[i]
}

func (h *orderEventList) Push(x orderEvent) {
	*h = append(*h, x)
	heapUp(h, len(*h)-1)
}

func (h *orderEventList) Pop() orderEvent {
	n := len(*h) - 1
	(*h)[0], (*h)[n] = (*h)[n], (*h)[0]
	heapDown(h, 0, n)
	x := (*h)[len(*h)-1]
	*h = (*h)[:len(*h)-1]
	return x
}

func heapUp(h *orderEventList, j int) {
	for {
		i := (j - 1) / 2 // parent
		if i == j || !h.Less(j, i) {
			break
		}
		(*h)[i], (*h)[j] = (*h)[j], (*h)[i]
		j = i
	}
}

func heapDown(h *orderEventList, i0, n int) bool {
	i := i0
	for {
		j1 := 2*i + 1
		if j1 >= n || j1 < 0 { // j1 < 0 after int overflow
			break
		}
		j := j1 // left child
		if j2 := j1 + 1; j2 < n && h.Less(j2, j1) {
			j = j2 // = 2*i + 2  // right child
		}
		if !h.Less(j, i) {
			break
		}
		(*h)[i], (*h)[j] = (*h)[j], (*h)[i]
		i = j
	}
	return i > i0
}

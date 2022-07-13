// Copyright 2014 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package trace

import (
	"bytes"
	"encoding/binary"
	"fmt"
	"io"
	"math"
)

// Event describes one event in the trace.
type Event struct {
	Ts    int64     // timestamp in nanoseconds
	G     uint64    // G on which the event happened
	StkID uint64    // unique stack ID
	Args  [3]uint64 // event-type-specific arguments
	// linked event (can be nil), depends on event type:
	// for GCStart: the GCStop
	// for GCSTWStart: the GCSTWDone
	// for GCSweepStart: the GCSweepDone
	// for GoCreate: first GoStart of the created goroutine
	// for GoStart/GoStartLabel: the associated GoEnd, GoBlock or other blocking event
	// for GoSched/GoPreempt: the next GoStart
	// for GoBlock and other blocking events: the unblock event
	// for GoUnblock: the associated GoStart
	// for blocking GoSysCall: the associated GoSysExit
	// for GoSysExit: the next GoStart
	// for GCMarkAssistStart: the associated GCMarkAssistDone
	// for UserTaskCreate: the UserTaskEnd
	// for UserRegion: if the start region, the corresponding UserRegion end event
	Link int
	P    uint32 // P on which the event happened (can be one of TimerP, NetpollP, SyscallP)
	Type byte   // one of Ev*
}

type Stack []byte

func (s Stack) Decode() []uint64 {
	if len(s) == 0 {
		return nil
	}

	// First byte indicates format, followed by a uvarint of the number of PCs, followed by a uvarint of the first PC,
	// followed by either uvarints of PCs, or varints of deltas of PCs.

	off := 1
	num, n := binary.Uvarint(s[1:])
	off += n
	out := make([]uint64, num)
	f, n := binary.Uvarint(s[off:])
	off += n
	out[0] = f

	if s[0] == 0 {
		// uvarint encoding
		for i := 1; off < len(s); i++ {
			f, n := binary.Uvarint(s[off:])
			off += n
			out[i] = f
		}
	} else {
		// varint + delta encoding
		prev := out[0]
		for i := 1; off < len(s); i++ {
			d, n := binary.Varint(s[off:])
			off += n
			f := prev + uint64(d)
			out[i] = f
			prev = f
		}
	}

	return out
}

func (p *parser) toStack(pcs []uint64) Stack {
	if len(pcs) == 0 {
		return nil
	}

	// Stacks are plentiful but small. For our "Staticcheck on std" trace with 11e6 events, we have roughly 500,000
	// stacks, using 41 MiB of memory. To avoid making 500,000 small allocations we allocate backing arrays 1 MiB at a
	// time. We have to be careful not to ever grow the backing array, because existing slices will continue referring
	// to the old array.
	out := p.stacksData[len(p.stacksData):]
	if cap(out) < len(pcs)*binary.MaxVarintLen64+1 {
		// We cannot guarantee that this slice will have enough capacity, so allocate a new slice. Otherwise we would
		// allocate a new array while existing slices continued referring to the old one.
		//
		// It doesn't matter if the new slice's size is slightly too small, because we'll only grow it once (in this
		// very invocation of the function) and thus not return stale handles.
		out = make([]byte, 0, 1024*1024)
	}

	var buf [binary.MaxVarintLen64]byte
	out = append(out, 1)
	n := binary.PutUvarint(buf[:], uint64(len(pcs)))
	out = append(out, buf[:n]...)
	prev := pcs[0]
	n = binary.PutUvarint(buf[:], prev)
	out = append(out, buf[:n]...)

	for _, pc := range pcs[1:] {
		var sd int64
		if pc > prev {
			ud := pc - prev
			if ud > math.MaxInt64 {
				out = toStackUvarint(pcs, out[:0])
				p.stacksData = out
				return out
			}
			sd = int64(ud)
		} else {
			ud := prev - pc
			if ud > math.MaxInt64 {
				out = toStackUvarint(pcs, out[:0])
				p.stacksData = out
				return out
			}
			sd = int64(-ud)
		}

		n := binary.PutVarint(buf[:], sd)
		out = append(out, buf[:n]...)
		prev = pc
	}

	p.stacksData = out
	return out
}

func toStackUvarint(pcs []uint64, out []byte) Stack {
	out = append(out, 0)
	var buf [binary.MaxVarintLen64]byte
	n := binary.PutUvarint(buf[:], uint64(len(pcs)))
	out = append(out, buf[:n]...)

	for _, pc := range pcs {
		n := binary.PutUvarint(buf[:], pc)
		out = append(out, buf[:n]...)
	}
	return out
}

// Frame is a frame in stack traces.
type Frame struct {
	PC   uint64
	Fn   string
	File string
	Line int
}

const (
	// Special P identifiers:
	FakeP    = 1000000 + iota
	TimerP   // depicts timer unblocks
	NetpollP // depicts network unblocks
	SyscallP // depicts returns from syscalls
	GCP      // depicts GC state
	ProfileP // depicts recording of CPU profile samples
)

// ParseResult is the result of Parse.
type ParseResult struct {
	// Events is the sorted list of Events in the trace.
	Events []Event
	// Stacks is the stack traces keyed by stack IDs from the trace.
	Stacks  map[uint64]Stack
	PCs     map[uint64]Frame
	Strings map[uint64]string
}

type parser struct {
	byte [1]byte

	strings     map[uint64]string
	batches     map[uint32]*batch // events by P
	stacks      map[uint64]Stack
	stacksData  []byte
	timerGoids  map[uint64]bool
	ticksPerSec int64
	pcs         map[uint64]Frame

	// state for parseEvent
	lastTs int64
	lastG  uint64
	lastP  uint32
	lastGs map[uint32]uint64 // last goroutine running on P
	stk    []uint64          // scratch space for building stacks
}

type batch struct {
	events [][]Event
}

func (b *batch) index(idx int) (int, int) {
	for major, events := range b.events {
		if len(events) <= idx {
			idx -= len(events)
			continue
		} else {
			return major, idx
		}
	}
	panic(fmt.Sprintf("index %d is out of bounds", idx))
}

func (b *batch) len() int {
	n := 0
	for _, evs := range b.events {
		n += len(evs)
	}
	return n
}

func (b *batch) at(idx int) Event {
	maj, min := b.index(idx)
	return b.events[maj][min]
}

func (b *batch) set(idx int, ev Event) {
	maj, min := b.index(idx)
	b.events[maj][min] = ev
}

func (b *batch) add(ev Event) {
	evs := b.events
	last := len(evs) - 1
	if last == -1 || len(evs[last]) == cap(evs[last]) {
		evs = append(evs, make([]Event, 0, (len(b.events)+1)*128))
		last = len(evs) - 1
	}
	evs[last] = append(evs[last], ev)
	b.events = evs
}

func (b *batch) popFront() {
	b.events[0] = b.events[0][1:]
	if len(b.events[0]) == 0 {
		b.events = b.events[1:]
	}
}

// Parse parses, post-processes and verifies the trace.
func Parse(r io.Reader, bin string) (ParseResult, error) {
	p := parser{}
	_, res, err := p.parse(r, bin)
	if err != nil {
		return ParseResult{}, err
	}
	return res, nil
}

// parse parses, post-processes and verifies the trace. It returns the
// trace version and the list of events.
func (p *parser) parse(r io.Reader, bin string) (int, ParseResult, error) {
	p.strings = make(map[uint64]string)
	p.batches = make(map[uint32]*batch)
	p.stacks = make(map[uint64]Stack)
	p.timerGoids = make(map[uint64]bool)
	p.lastGs = make(map[uint32]uint64)
	p.pcs = make(map[uint64]Frame)

	ver, err := p.readHeader(r)
	if err != nil {
		return 0, ParseResult{}, err
	}

	err = p.readTrace(r, ver)
	if err != nil {
		return 0, ParseResult{}, err
	}

	events, err := p.finalize()
	if err != nil {
		return 0, ParseResult{}, err
	}

	events = removeFutile(events)
	err = postProcessTrace(ver, events)
	if err != nil {
		return 0, ParseResult{}, err
	}
	// Attach stack traces.
	return ver, ParseResult{Events: events, Stacks: p.stacks, Strings: p.strings, PCs: p.pcs}, nil
}

// rawEvent is a helper type used during parsing.
type rawEvent struct {
	off   int
	typ   byte
	args  []uint64
	sargs []string
}

func (p *parser) readHeader(r io.Reader) (ver int, err error) {
	// Read and validate trace header.
	var buf [16]byte
	off, err := io.ReadFull(r, buf[:])
	if err != nil {
		return 0, fmt.Errorf("failed to read header: read %v, err %v", off, err)
	}
	ver, err = parseHeader(buf[:])
	if err != nil {
		return 0, err
	}
	switch ver {
	case 1011, 1019:
		// Note: When adding a new version, add canned traces
		// from the old version to the test suite using mkcanned.bash.
	default:
		return 0, fmt.Errorf("unsupported trace file version %v.%v", ver/1000, ver%1000)
	}

	return ver, err
}

func (p *parser) readTrace(r io.Reader, ver int) (err error) {
	var buf [16]byte
	var off int

	// space for event args, reused between events
	var args []uint64

	// Read events.
	for {
		// Read event type and number of arguments (1 byte).
		var off0 int
		var n int
		n, err = r.Read(buf[:1])
		if err == io.EOF {
			err = nil
			break
		}
		if err != nil || n != 1 {
			err = fmt.Errorf("failed to read trace at offset 0x%x: n=%v err=%v", off0, n, err)
			return
		}
		off += n
		typ := buf[0] << 2 >> 2
		narg := buf[0]>>6 + 1
		inlineArgs := byte(4)
		if typ == EvNone || typ >= EvCount || EventDescriptions[typ].minVersion > ver {
			err = fmt.Errorf("unknown event type %v at offset 0x%x", typ, off0)
			return
		}
		if typ == EvString {
			// String dictionary entry [ID, length, string].
			var id uint64
			id, off, err = p.readVal(r, off)
			if err != nil {
				return
			}
			if id == 0 {
				err = fmt.Errorf("string at offset %d has invalid id 0", off)
				return
			}
			if p.strings[id] != "" {
				err = fmt.Errorf("string at offset %d has duplicate id %v", off, id)
				return
			}
			var ln uint64
			ln, off, err = p.readVal(r, off)
			if err != nil {
				return
			}
			if ln == 0 {
				err = fmt.Errorf("string at offset %d has invalid length 0", off)
				return
			}
			if ln > 1e6 {
				err = fmt.Errorf("string at offset %d has too large length %v", off, ln)
				return
			}
			buf := make([]byte, ln)
			var n int
			n, err = io.ReadFull(r, buf)
			if err != nil {
				err = fmt.Errorf("failed to read trace at offset %d: read %v, want %v, error %v", off, n, ln, err)
				return
			}
			off += n
			p.strings[id] = string(buf)
			continue
		}
		ev := rawEvent{typ: typ, off: off0, args: args[:0]}
		if narg < inlineArgs {
			for i := 0; i < int(narg); i++ {
				var v uint64
				v, off, err = p.readVal(r, off)
				if err != nil {
					err = fmt.Errorf("failed to read event %v argument at offset %v (%v)", typ, off, err)
					return
				}
				ev.args = append(ev.args, v)
			}
		} else {
			// More than inlineArgs args, the first value is length of the event in bytes.
			var v uint64
			v, off, err = p.readVal(r, off)
			if err != nil {
				err = fmt.Errorf("failed to read event %v argument at offset %v (%v)", typ, off, err)
				return
			}
			evLen := v
			off1 := off
			for evLen > uint64(off-off1) {
				v, off, err = p.readVal(r, off)
				if err != nil {
					err = fmt.Errorf("failed to read event %v argument at offset %v (%v)", typ, off, err)
					return
				}
				ev.args = append(ev.args, v)
			}
			if evLen != uint64(off-off1) {
				err = fmt.Errorf("event has wrong length at offset 0x%x: want %v, got %v", off0, evLen, off-off1)
				return
			}
		}
		switch ev.typ {
		case EvUserLog: // EvUserLog records are followed by a value string of length ev.args[len(ev.args)-1]
			var s string
			s, off, err = p.readStr(r, off)
			ev.sargs = append(ev.sargs, s)
		}
		p.parseEvent(ver, ev)

		args = ev.args[:0]
	}
	return
}

func (p *parser) readStr(r io.Reader, off0 int) (s string, off int, err error) {
	var sz uint64
	sz, off, err = p.readVal(r, off0)
	if err != nil || sz == 0 {
		return "", off, err
	}
	if sz > 1e6 {
		return "", off, fmt.Errorf("string at offset %d is too large (len=%d)", off, sz)
	}
	buf := make([]byte, sz)
	n, err := io.ReadFull(r, buf)
	if err != nil || sz != uint64(n) {
		return "", off + n, fmt.Errorf("failed to read trace at offset %d: read %v, want %v, error %v", off, n, sz, err)
	}
	return string(buf), off + n, nil
}

// parseHeader parses trace header of the form "go 1.7 trace\x00\x00\x00\x00"
// and returns parsed version as 1007.
func parseHeader(buf []byte) (int, error) {
	if len(buf) != 16 {
		return 0, fmt.Errorf("bad header length")
	}
	if buf[0] != 'g' || buf[1] != 'o' || buf[2] != ' ' ||
		buf[3] < '1' || buf[3] > '9' ||
		buf[4] != '.' ||
		buf[5] < '1' || buf[5] > '9' {
		return 0, fmt.Errorf("not a trace file")
	}
	ver := int(buf[5] - '0')
	i := 0
	for ; buf[6+i] >= '0' && buf[6+i] <= '9' && i < 2; i++ {
		ver = ver*10 + int(buf[6+i]-'0')
	}
	ver += int(buf[3]-'0') * 1000
	if !bytes.Equal(buf[6+i:], []byte(" trace\x00\x00\x00\x00")[:10-i]) {
		return 0, fmt.Errorf("not a trace file")
	}
	return ver, nil
}

// parseEvent transforms raw events into events.
// It does analyze and verify per-event-type arguments.
func (p *parser) parseEvent(ver int, raw rawEvent) error {
	desc := EventDescriptions[raw.typ]
	if desc.Name == "" {
		return fmt.Errorf("missing description for event type %v", raw.typ)
	}
	narg := argNum(raw, ver)
	if len(raw.args) != narg {
		return fmt.Errorf("%v has wrong number of arguments at offset 0x%x: want %v, got %v",
			desc.Name, raw.off, narg, len(raw.args))
	}
	switch raw.typ {
	case EvBatch:
		p.lastGs[p.lastP] = p.lastG
		if raw.args[0] > math.MaxUint {
			return fmt.Errorf("processor ID %d is larger than maximum of %d", raw.args[0], uint64(math.MaxUint))
		}
		p.lastP = uint32(raw.args[0])
		p.lastG = p.lastGs[p.lastP]
		p.lastTs = int64(raw.args[1])
	case EvFrequency:
		p.ticksPerSec = int64(raw.args[0])
		if p.ticksPerSec <= 0 {
			// The most likely cause for this is tick skew on different CPUs.
			// For example, solaris/amd64 seems to have wildly different
			// ticks on different CPUs.
			return ErrTimeOrder
		}
	case EvTimerGoroutine:
		p.timerGoids[raw.args[0]] = true
	case EvStack:
		if len(raw.args) < 2 {
			return fmt.Errorf("EvStack has wrong number of arguments at offset 0x%x: want at least 2, got %v",
				raw.off, len(raw.args))
		}
		size := raw.args[1]
		if size > 1000 {
			return fmt.Errorf("EvStack has bad number of frames at offset 0x%x: %v",
				raw.off, size)
		}
		want := 2 + 4*size
		if uint64(len(raw.args)) != want {
			return fmt.Errorf("EvStack has wrong number of arguments at offset 0x%x: want %v, got %v",
				raw.off, want, len(raw.args))
		}
		id := raw.args[0]
		if id != 0 && size > 0 {
			stk := p.stk
			if size <= uint64(cap(stk)) {
				stk = stk[:size]
			} else {
				stk = make([]uint64, size)
				p.stk = stk[:0]
			}
			for i := 0; i < int(size); i++ {
				pc := raw.args[2+i*4+0]
				fn := raw.args[2+i*4+1]
				file := raw.args[2+i*4+2]
				line := raw.args[2+i*4+3]
				stk[i] = pc

				if _, ok := p.pcs[pc]; !ok {
					p.pcs[pc] = Frame{PC: pc, Fn: p.strings[fn], File: p.strings[file], Line: int(line)}
				}
			}
			p.stacks[id] = p.toStack(stk)
		}
	default:
		e := Event{Type: raw.typ, P: p.lastP, G: p.lastG, Link: -1}
		var argOffset int
		e.Ts = p.lastTs + int64(raw.args[0])
		argOffset = 1
		p.lastTs = e.Ts
		for i := argOffset; i < narg; i++ {
			if i == narg-1 && desc.Stack {
				e.StkID = raw.args[i]
			} else {
				e.Args[i-argOffset] = raw.args[i]
			}
		}
		switch raw.typ {
		case EvGoStart, EvGoStartLocal, EvGoStartLabel:
			p.lastG = e.Args[0]
			e.G = p.lastG
		case EvGCSTWStart:
			e.G = 0
		case EvGCStart, EvGCDone, EvGCSTWDone:
			e.G = 0
		case EvGoEnd, EvGoStop, EvGoSched, EvGoPreempt,
			EvGoSleep, EvGoBlock, EvGoBlockSend, EvGoBlockRecv,
			EvGoBlockSelect, EvGoBlockSync, EvGoBlockCond, EvGoBlockNet,
			EvGoSysBlock, EvGoBlockGC:
			p.lastG = 0
		case EvGoSysExit, EvGoWaiting, EvGoInSyscall:
			e.G = e.Args[0]
		case EvUserTaskCreate:
			// e.Args 0: taskID, 1:parentID, 2:nameID
		case EvUserRegion:
			// e.Args 0: taskID, 1: mode, 2:nameID
		case EvUserLog:
			// e.Args 0: taskID, 1:keyID, 2: stackID
		case EvCPUSample:
			e.Ts = int64(e.Args[0])
			e.P = uint32(e.Args[1])
			e.G = e.Args[2]
			e.Args[0] = 0
		}
		var b *batch
		switch raw.typ {
		case EvCPUSample:
			// Most events are written out by the active P at the exact
			// moment they describe. CPU profile samples are different
			// because they're written to the tracing log after some delay,
			// by a separate worker goroutine, into a separate buffer.
			//
			// We keep these in their own batch until all of the batches are
			// merged in timestamp order. We also (right before the merge)
			// re-sort these events by the timestamp captured in the
			// profiling signal handler.
			b = p.batches[ProfileP]
			if b == nil {
				b = &batch{}
				p.batches[ProfileP] = b
			}
		default:
			b = p.batches[p.lastP]
			if b == nil {
				b = &batch{}
				p.batches[p.lastP] = b
			}
		}
		b.add(e)
	}

	return nil
}

const (
	STWKindMark  = 0
	STWKindSweep = 1
)

func (p *parser) finalize() ([]Event, error) {
	if len(p.batches) == 0 {
		return nil, fmt.Errorf("trace is empty")
	}
	if p.ticksPerSec == 0 {
		return nil, fmt.Errorf("no EvFrequency event")
	}

	events, err := order1007(p.batches)
	if err != nil {
		return nil, err
	}

	// Translate cpu ticks to real time.
	minTs := events[0].Ts
	// Use floating point to avoid integer overflows.
	freq := 1e9 / float64(p.ticksPerSec)
	for i := range events {
		ev := &events[i]
		ev.Ts = int64(float64(ev.Ts-minTs) * freq)
		// Move timers and syscalls to separate fake Ps.
		if p.timerGoids[ev.G] && ev.Type == EvGoUnblock {
			ev.P = TimerP
		}
		if ev.Type == EvGoSysExit {
			ev.P = SyscallP
		}
	}

	return events, nil
}

// removeFutile removes all constituents of futile wakeups (block, unblock, start).
// For example, a goroutine was unblocked on a mutex, but another goroutine got
// ahead and acquired the mutex before the first goroutine is scheduled,
// so the first goroutine has to block again. Such wakeups happen on buffered
// channels and sync.Mutex, but are generally not interesting for end user.
func removeFutile(events []Event) []Event {
	// Two non-trivial aspects:
	// 1. A goroutine can be preempted during a futile wakeup and migrate to another P.
	//	We want to remove all of that.
	// 2. Tracing can start in the middle of a futile wakeup.
	//	That is, we can see a futile wakeup event w/o the actual wakeup before it.
	// postProcessTrace runs after us and ensures that we leave the trace in a consistent state.

	// Phase 1: determine futile wakeup sequences.
	type G struct {
		futile    bool
		wakeupArr [2]int
		wakeup    []int // wakeup sequence (subject for removal)
	}
	gs := make(map[uint64]*G)
	getG := func(gid uint64) *G {
		g := gs[gid]
		if g != nil {
			return g
		}
		g = &G{}
		g.wakeup = g.wakeupArr[:0]
		gs[gid] = g
		return g
	}
	futile := make(map[int]struct{})
	for i := range events {
		ev := &events[i]
		switch ev.Type {
		case EvGoUnblock:
			g := getG(ev.Args[0])
			g.wakeup = g.wakeupArr[:1]
			g.wakeup[0] = i
		case EvGoStart, EvGoPreempt, EvFutileWakeup:
			g := getG(ev.G)
			if g.wakeup == nil {
				g.wakeup = g.wakeupArr[:0]
			}
			g.wakeup = append(g.wakeup, i)
			if ev.Type == EvFutileWakeup {
				g.futile = true
			}
		case EvGoBlock, EvGoBlockSend, EvGoBlockRecv, EvGoBlockSelect, EvGoBlockSync, EvGoBlockCond:
			g := getG(ev.G)
			if g.futile {
				futile[i] = struct{}{}
				for _, ev1 := range g.wakeup {
					futile[ev1] = struct{}{}
				}
			}
			g.wakeup = g.wakeup[:0]
		}
	}

	// Phase 2: remove futile wakeup sequences.
	newEvents := events[:0] // overwrite the original slice
	for i, ev := range events {
		if _, ok := futile[i]; !ok {
			newEvents = append(newEvents, ev)
		}
	}
	return newEvents
}

// ErrTimeOrder is returned by Parse when the trace contains
// time stamps that do not respect actual event ordering.
var ErrTimeOrder = fmt.Errorf("time stamps out of order")

// postProcessTrace does inter-event verification and information restoration.
// The resulting trace is guaranteed to be consistent
// (for example, a P does not run two Gs at the same time, or a G is indeed
// blocked before an unblock event).
func postProcessTrace(ver int, events []Event) error {
	const (
		gDead = iota
		gRunnable
		gRunning
		gWaiting
	)
	type gdesc struct {
		state        int
		ev           *Event
		evStart      *Event
		evCreate     *Event
		evMarkAssist *Event
	}
	type pdesc struct {
		running bool
		g       uint64
		evSweep *Event
	}

	gs := make(map[uint64]gdesc)
	ps := make(map[uint32]pdesc)
	tasks := make(map[uint64]*Event)           // task id to task creation events
	activeRegions := make(map[uint64][]*Event) // goroutine id to stack of regions
	gs[0] = gdesc{state: gRunning}
	var evGC, evSTW *Event

	checkRunning := func(p pdesc, g gdesc, ev *Event, allowG0 bool) error {
		name := EventDescriptions[ev.Type].Name
		if g.state != gRunning {
			return fmt.Errorf("g %v is not running while %v (time %v)", ev.G, name, ev.Ts)
		}
		if p.g != ev.G {
			return fmt.Errorf("p %v is not running g %v while %v (time %v)", ev.P, ev.G, name, ev.Ts)
		}
		if !allowG0 && ev.G == 0 {
			return fmt.Errorf("g 0 did %v (time %v)", EventDescriptions[ev.Type].Name, ev.Ts)
		}
		return nil
	}

	for evIdx := range events {
		ev := &events[evIdx]
		g := gs[ev.G]
		p := ps[ev.P]

		switch ev.Type {
		case EvProcStart:
			if p.running {
				return fmt.Errorf("p %v is running before start (time %v)", ev.P, ev.Ts)
			}
			p.running = true
		case EvProcStop:
			if !p.running {
				return fmt.Errorf("p %v is not running before stop (time %v)", ev.P, ev.Ts)
			}
			if p.g != 0 {
				return fmt.Errorf("p %v is running a goroutine %v during stop (time %v)", ev.P, p.g, ev.Ts)
			}
			p.running = false
		case EvGCStart:
			if evGC != nil {
				return fmt.Errorf("previous GC is not ended before a new one (time %v)", ev.Ts)
			}
			evGC = ev
			// Attribute this to the global GC state.
			ev.P = GCP
		case EvGCDone:
			if evGC == nil {
				return fmt.Errorf("bogus GC end (time %v)", ev.Ts)
			}
			evGC.Link = evIdx
			evGC = nil
		case EvGCSTWStart:
			evp := &evSTW
			if *evp != nil {
				return fmt.Errorf("previous STW is not ended before a new one (time %v)", ev.Ts)
			}
			*evp = ev
		case EvGCSTWDone:
			evp := &evSTW
			if *evp == nil {
				return fmt.Errorf("bogus STW end (time %v)", ev.Ts)
			}
			(*evp).Link = evIdx
			*evp = nil
		case EvGCSweepStart:
			if p.evSweep != nil {
				return fmt.Errorf("previous sweeping is not ended before a new one (time %v)", ev.Ts)
			}
			p.evSweep = ev
		case EvGCMarkAssistStart:
			if g.evMarkAssist != nil {
				return fmt.Errorf("previous mark assist is not ended before a new one (time %v)", ev.Ts)
			}
			g.evMarkAssist = ev
		case EvGCMarkAssistDone:
			// Unlike most events, mark assists can be in progress when a
			// goroutine starts tracing, so we can't report an error here.
			if g.evMarkAssist != nil {
				g.evMarkAssist.Link = evIdx
				g.evMarkAssist = nil
			}
		case EvGCSweepDone:
			if p.evSweep == nil {
				return fmt.Errorf("bogus sweeping end (time %v)", ev.Ts)
			}
			p.evSweep.Link = evIdx
			p.evSweep = nil
		case EvGoWaiting:
			if g.state != gRunnable {
				return fmt.Errorf("g %v is not runnable before EvGoWaiting (time %v)", ev.G, ev.Ts)
			}
			g.state = gWaiting
			g.ev = ev
		case EvGoInSyscall:
			if g.state != gRunnable {
				return fmt.Errorf("g %v is not runnable before EvGoInSyscall (time %v)", ev.G, ev.Ts)
			}
			g.state = gWaiting
			g.ev = ev
		case EvGoCreate:
			if err := checkRunning(p, g, ev, true); err != nil {
				return err
			}
			if _, ok := gs[ev.Args[0]]; ok {
				return fmt.Errorf("g %v already exists (time %v)", ev.Args[0], ev.Ts)
			}
			gs[ev.Args[0]] = gdesc{state: gRunnable, ev: ev, evCreate: ev}
		case EvGoStart, EvGoStartLabel:
			if g.state != gRunnable {
				return fmt.Errorf("g %v is not runnable before start (time %v)", ev.G, ev.Ts)
			}
			if p.g != 0 {
				return fmt.Errorf("p %v is already running g %v while start g %v (time %v)", ev.P, p.g, ev.G, ev.Ts)
			}
			g.state = gRunning
			g.evStart = ev
			p.g = ev.G
			if g.evCreate != nil {
				ev.StkID = g.evCreate.Args[1]
				g.evCreate = nil
			}

			if g.ev != nil {
				g.ev.Link = evIdx
				g.ev = nil
			}
		case EvGoEnd, EvGoStop:
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.evStart.Link = evIdx
			g.evStart = nil
			g.state = gDead
			p.g = 0

			if ev.Type == EvGoEnd { // flush all active regions
				regions := activeRegions[ev.G]
				for _, s := range regions {
					s.Link = evIdx
				}
				delete(activeRegions, ev.G)
			}

		case EvGoSched, EvGoPreempt:
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gRunnable
			g.evStart.Link = evIdx
			g.evStart = nil
			p.g = 0
			g.ev = ev
		case EvGoUnblock:
			if g.state != gRunning {
				return fmt.Errorf("g %v is not running while unpark (time %v)", ev.G, ev.Ts)
			}
			if ev.P != TimerP && p.g != ev.G {
				return fmt.Errorf("p %v is not running g %v while unpark (time %v)", ev.P, ev.G, ev.Ts)
			}
			g1 := gs[ev.Args[0]]
			if g1.state != gWaiting {
				return fmt.Errorf("g %v is not waiting before unpark (time %v)", ev.Args[0], ev.Ts)
			}
			if g1.ev != nil && g1.ev.Type == EvGoBlockNet && ev.P != TimerP {
				ev.P = NetpollP
			}
			if g1.ev != nil {
				g1.ev.Link = evIdx
			}
			g1.state = gRunnable
			g1.ev = ev
			gs[ev.Args[0]] = g1
		case EvGoSysCall:
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.ev = ev
		case EvGoSysBlock:
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gWaiting
			g.evStart.Link = evIdx
			g.evStart = nil
			p.g = 0
		case EvGoSysExit:
			if g.state != gWaiting {
				return fmt.Errorf("g %v is not waiting during syscall exit (time %v)", ev.G, ev.Ts)
			}
			if g.ev != nil && g.ev.Type == EvGoSysCall {
				g.ev.Link = evIdx
			}
			g.state = gRunnable
			g.ev = ev
		case EvGoSleep, EvGoBlock, EvGoBlockSend, EvGoBlockRecv,
			EvGoBlockSelect, EvGoBlockSync, EvGoBlockCond, EvGoBlockNet, EvGoBlockGC:
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gWaiting
			g.ev = ev
			g.evStart.Link = evIdx
			g.evStart = nil
			p.g = 0
		case EvUserTaskCreate:
			taskid := ev.Args[0]
			if prevEv, ok := tasks[taskid]; ok {
				return fmt.Errorf("task id conflicts (id:%d), %q vs %q", taskid, ev, prevEv)
			}
			tasks[ev.Args[0]] = ev
		case EvUserTaskEnd:
			taskid := ev.Args[0]
			if taskCreateEv, ok := tasks[taskid]; ok {
				taskCreateEv.Link = evIdx
				delete(tasks, taskid)
			}
		case EvUserRegion:
			mode := ev.Args[1]
			regions := activeRegions[ev.G]
			if mode == 0 { // region start
				activeRegions[ev.G] = append(regions, ev) // push
			} else if mode == 1 { // region end
				n := len(regions)
				if n > 0 { // matching region start event is in the trace.
					s := regions[n-1]
					if s.Args[0] != ev.Args[0] || s.Args[2] != ev.Args[2] { // task id, region name mismatch
						return fmt.Errorf("misuse of region in goroutine %d: span end %q when the inner-most active span start event is %q", ev.G, ev, s)
					}
					// Link region start event with span end event
					s.Link = evIdx

					if n > 1 {
						activeRegions[ev.G] = regions[:n-1]
					} else {
						delete(activeRegions, ev.G)
					}
				}
			} else {
				return fmt.Errorf("invalid user region mode: %q", ev)
			}
		}

		gs[ev.G] = g
		ps[ev.P] = p
	}

	// TODO(dvyukov): restore stacks for EvGoStart events.
	// TODO(dvyukov): test that all EvGoStart events has non-nil Link.

	return nil
}

// readVal reads unsigned base-128 value from r.
func (p *parser) readVal(r io.Reader, off0 int) (v uint64, off int, err error) {
	off = off0
	for i := 0; i < 10; i++ {
		var n int
		n, err = r.Read(p.byte[:])
		if err != nil || n != 1 {
			return 0, 0, fmt.Errorf("failed to read trace at offset %d: read %v, error %v", off0, n, err)
		}
		off++
		v |= uint64(p.byte[0]&0x7f) << (uint(i) * 7)
		if p.byte[0]&0x80 == 0 {
			return
		}
	}
	return 0, 0, fmt.Errorf("bad value at offset 0x%x", off0)
}

// Print dumps events to stdout. For debugging.
func Print(events []*Event) {
	for _, ev := range events {
		PrintEvent(ev)
	}
}

// PrintEvent dumps the event to stdout. For debugging.
func PrintEvent(ev *Event) {
	fmt.Printf("%s\n", ev)
}

func (ev *Event) String() string {
	desc := EventDescriptions[ev.Type]
	w := new(bytes.Buffer)
	fmt.Fprintf(w, "%v %v p=%v g=%v", ev.Ts, desc.Name, ev.P, ev.G)
	for i, a := range desc.Args {
		fmt.Fprintf(w, " %v=%v", a, ev.Args[i])
	}
	return w.String()
}

// argNum returns total number of args for the event accounting for timestamps,
// sequence numbers and differences between trace format versions.
func argNum(raw rawEvent, ver int) int {
	desc := EventDescriptions[raw.typ]
	if raw.typ == EvStack {
		return len(raw.args)
	}
	narg := len(desc.Args)
	if desc.Stack {
		narg++
	}
	switch raw.typ {
	case EvBatch, EvFrequency, EvTimerGoroutine:
		return narg
	}
	narg++ // timestamp
	return narg
}

// Event types in the trace.
// Verbatim copy from src/runtime/trace.go with the "trace" prefix removed.
const (
	EvNone              = 0  // unused
	EvBatch             = 1  // start of per-P batch of events [pid, timestamp]
	EvFrequency         = 2  // contains tracer timer frequency [frequency (ticks per second)]
	EvStack             = 3  // stack [stack id, number of PCs, array of {PC, func string ID, file string ID, line}]
	EvGomaxprocs        = 4  // current value of GOMAXPROCS [timestamp, GOMAXPROCS, stack id]
	EvProcStart         = 5  // start of P [timestamp, thread id]
	EvProcStop          = 6  // stop of P [timestamp]
	EvGCStart           = 7  // GC start [timestamp, seq, stack id]
	EvGCDone            = 8  // GC done [timestamp]
	EvGCSTWStart        = 9  // GC mark termination start [timestamp, kind]
	EvGCSTWDone         = 10 // GC mark termination done [timestamp]
	EvGCSweepStart      = 11 // GC sweep start [timestamp, stack id]
	EvGCSweepDone       = 12 // GC sweep done [timestamp, swept, reclaimed]
	EvGoCreate          = 13 // goroutine creation [timestamp, new goroutine id, new stack id, stack id]
	EvGoStart           = 14 // goroutine starts running [timestamp, goroutine id, seq]
	EvGoEnd             = 15 // goroutine ends [timestamp]
	EvGoStop            = 16 // goroutine stops (like in select{}) [timestamp, stack]
	EvGoSched           = 17 // goroutine calls Gosched [timestamp, stack]
	EvGoPreempt         = 18 // goroutine is preempted [timestamp, stack]
	EvGoSleep           = 19 // goroutine calls Sleep [timestamp, stack]
	EvGoBlock           = 20 // goroutine blocks [timestamp, stack]
	EvGoUnblock         = 21 // goroutine is unblocked [timestamp, goroutine id, seq, stack]
	EvGoBlockSend       = 22 // goroutine blocks on chan send [timestamp, stack]
	EvGoBlockRecv       = 23 // goroutine blocks on chan recv [timestamp, stack]
	EvGoBlockSelect     = 24 // goroutine blocks on select [timestamp, stack]
	EvGoBlockSync       = 25 // goroutine blocks on Mutex/RWMutex [timestamp, stack]
	EvGoBlockCond       = 26 // goroutine blocks on Cond [timestamp, stack]
	EvGoBlockNet        = 27 // goroutine blocks on network [timestamp, stack]
	EvGoSysCall         = 28 // syscall enter [timestamp, stack]
	EvGoSysExit         = 29 // syscall exit [timestamp, goroutine id, seq, real timestamp]
	EvGoSysBlock        = 30 // syscall blocks [timestamp]
	EvGoWaiting         = 31 // denotes that goroutine is blocked when tracing starts [timestamp, goroutine id]
	EvGoInSyscall       = 32 // denotes that goroutine is in syscall when tracing starts [timestamp, goroutine id]
	EvHeapAlloc         = 33 // gcController.heapLive change [timestamp, heap live bytes]
	EvHeapGoal          = 34 // gcController.heapGoal change [timestamp, heap goal bytes]
	EvTimerGoroutine    = 35 // denotes timer goroutine [timer goroutine id]
	EvFutileWakeup      = 36 // denotes that the previous wakeup of this goroutine was futile [timestamp]
	EvString            = 37 // string dictionary entry [ID, length, string]
	EvGoStartLocal      = 38 // goroutine starts running on the same P as the last event [timestamp, goroutine id]
	EvGoUnblockLocal    = 39 // goroutine is unblocked on the same P as the last event [timestamp, goroutine id, stack]
	EvGoSysExitLocal    = 40 // syscall exit on the same P as the last event [timestamp, goroutine id, real timestamp]
	EvGoStartLabel      = 41 // goroutine starts running with label [timestamp, goroutine id, seq, label string id]
	EvGoBlockGC         = 42 // goroutine blocks on GC assist [timestamp, stack]
	EvGCMarkAssistStart = 43 // GC mark assist start [timestamp, stack]
	EvGCMarkAssistDone  = 44 // GC mark assist done [timestamp]
	EvUserTaskCreate    = 45 // trace.NewContext [timestamp, internal task id, internal parent id, stack, name string]
	EvUserTaskEnd       = 46 // end of task [timestamp, internal task id, stack]
	EvUserRegion        = 47 // trace.WithRegion [timestamp, internal task id, mode(0:start, 1:end), stack, name string]
	EvUserLog           = 48 // trace.Log [timestamp, internal id, key string id, stack, value string]
	EvCPUSample         = 49 // CPU profiling sample [timestamp, stack, real timestamp, real P id (-1 when absent), goroutine id]
	EvCount             = 50
)

var EventDescriptions = [EvCount]struct {
	Name       string
	minVersion int
	Stack      bool
	Args       []string
	SArgs      []string // string arguments
}{
	EvNone:              {"None", 1005, false, []string{}, nil},
	EvBatch:             {"Batch", 1005, false, []string{"p", "ticks"}, nil}, // in 1.5 format it was {"p", "seq", "ticks"}
	EvFrequency:         {"Frequency", 1005, false, []string{"freq"}, nil},   // in 1.5 format it was {"freq", "unused"}
	EvStack:             {"Stack", 1005, false, []string{"id", "siz"}, nil},
	EvGomaxprocs:        {"Gomaxprocs", 1005, true, []string{"procs"}, nil},
	EvProcStart:         {"ProcStart", 1005, false, []string{"thread"}, nil},
	EvProcStop:          {"ProcStop", 1005, false, []string{}, nil},
	EvGCStart:           {"GCStart", 1005, true, []string{"seq"}, nil}, // in 1.5 format it was {}
	EvGCDone:            {"GCDone", 1005, false, []string{}, nil},
	EvGCSTWStart:        {"GCSTWStart", 1005, false, []string{"kindid"}, []string{"kind"}}, // <= 1.9, args was {} (implicitly {0})
	EvGCSTWDone:         {"GCSTWDone", 1005, false, []string{}, nil},
	EvGCSweepStart:      {"GCSweepStart", 1005, true, []string{}, nil},
	EvGCSweepDone:       {"GCSweepDone", 1005, false, []string{"swept", "reclaimed"}, nil}, // before 1.9, format was {}
	EvGoCreate:          {"GoCreate", 1005, true, []string{"g", "stack"}, nil},
	EvGoStart:           {"GoStart", 1005, false, []string{"g", "seq"}, nil}, // in 1.5 format it was {"g"}
	EvGoEnd:             {"GoEnd", 1005, false, []string{}, nil},
	EvGoStop:            {"GoStop", 1005, true, []string{}, nil},
	EvGoSched:           {"GoSched", 1005, true, []string{}, nil},
	EvGoPreempt:         {"GoPreempt", 1005, true, []string{}, nil},
	EvGoSleep:           {"GoSleep", 1005, true, []string{}, nil},
	EvGoBlock:           {"GoBlock", 1005, true, []string{}, nil},
	EvGoUnblock:         {"GoUnblock", 1005, true, []string{"g", "seq"}, nil}, // in 1.5 format it was {"g"}
	EvGoBlockSend:       {"GoBlockSend", 1005, true, []string{}, nil},
	EvGoBlockRecv:       {"GoBlockRecv", 1005, true, []string{}, nil},
	EvGoBlockSelect:     {"GoBlockSelect", 1005, true, []string{}, nil},
	EvGoBlockSync:       {"GoBlockSync", 1005, true, []string{}, nil},
	EvGoBlockCond:       {"GoBlockCond", 1005, true, []string{}, nil},
	EvGoBlockNet:        {"GoBlockNet", 1005, true, []string{}, nil},
	EvGoSysCall:         {"GoSysCall", 1005, true, []string{}, nil},
	EvGoSysExit:         {"GoSysExit", 1005, false, []string{"g", "seq", "ts"}, nil},
	EvGoSysBlock:        {"GoSysBlock", 1005, false, []string{}, nil},
	EvGoWaiting:         {"GoWaiting", 1005, false, []string{"g"}, nil},
	EvGoInSyscall:       {"GoInSyscall", 1005, false, []string{"g"}, nil},
	EvHeapAlloc:         {"HeapAlloc", 1005, false, []string{"mem"}, nil},
	EvHeapGoal:          {"HeapGoal", 1005, false, []string{"mem"}, nil},
	EvTimerGoroutine:    {"TimerGoroutine", 1005, false, []string{"g"}, nil}, // in 1.5 format it was {"g", "unused"}
	EvFutileWakeup:      {"FutileWakeup", 1005, false, []string{}, nil},
	EvString:            {"String", 1007, false, []string{}, nil},
	EvGoStartLocal:      {"GoStartLocal", 1007, false, []string{"g"}, nil},
	EvGoUnblockLocal:    {"GoUnblockLocal", 1007, true, []string{"g"}, nil},
	EvGoSysExitLocal:    {"GoSysExitLocal", 1007, false, []string{"g", "ts"}, nil},
	EvGoStartLabel:      {"GoStartLabel", 1008, false, []string{"g", "seq", "labelid"}, []string{"label"}},
	EvGoBlockGC:         {"GoBlockGC", 1008, true, []string{}, nil},
	EvGCMarkAssistStart: {"GCMarkAssistStart", 1009, true, []string{}, nil},
	EvGCMarkAssistDone:  {"GCMarkAssistDone", 1009, false, []string{}, nil},
	EvUserTaskCreate:    {"UserTaskCreate", 1011, true, []string{"taskid", "pid", "typeid"}, []string{"name"}},
	EvUserTaskEnd:       {"UserTaskEnd", 1011, true, []string{"taskid"}, nil},
	EvUserRegion:        {"UserRegion", 1011, true, []string{"taskid", "mode", "typeid"}, []string{"name"}},
	EvUserLog:           {"UserLog", 1011, true, []string{"id", "keyid"}, []string{"category", "message"}},
	EvCPUSample:         {"CPUSample", 1019, true, []string{"ts", "p", "g"}, nil},
}

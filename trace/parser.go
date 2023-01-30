// Copyright 2014 The Go Authors. All rights reserved.
package trace

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"sort"
)

var ErrTooManyEvents = fmt.Errorf("trace contains more than %d events", math.MaxInt32)

type Timestamp int64

// Event describes one event in the trace.
type Event struct {
	// The Event type is carefully laid out to optimize its size and to avoid pointers, the latter so that the garbage
	// collector won't have to scan any memory of our millions of events.
	//
	// Instead of pointers, fields like StkID and Link are indices into slices.

	Ts    Timestamp // timestamp in nanoseconds
	G     uint64    // G on which the event happened
	Args  [4]uint64 // event-type-specific arguments
	StkID uint32    // unique stack ID
	P     int32     // P on which the event happened (can be one of TimerP, NetpollP, SyscallP)
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
	Link int32
	Type byte // one of Ev*
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
	NetpollP // depicts network unblocks
	SyscallP // depicts returns from syscalls
	GCP      // depicts GC state
	ProfileP // depicts recording of CPU profile samples
)

const headerLength = 16

// Trace is the result of Parse.
type Trace struct {
	// Events is the sorted list of Events in the trace.
	Events []Event
	// Stacks is the stack traces keyed by stack IDs from the trace.
	//
	// OPT(dh): we could renumber stacks, PCs and Strings densely and store them in slices instead of maps. I don't know
	// if the cost of accesses will outweigh the cost of renumbering.
	Stacks  map[uint32][]uint64
	PCs     map[uint64]Frame
	Strings map[uint64]string
}

type batch struct {
	offset    int
	numEvents int
}

type pState struct {
	// list of batch offsets and sizes
	batches []batch
	// last goroutine running on P
	lastG uint64

	slice []Event
}

// The number of parsing stages. as reported to Parser.Progress. Each stage has its own total, and the current progress
// resets to 0 at the start of each stage.

type Parser struct {
	progress func(p float64)

	ver  int
	data []byte
	off  int

	bigArgsBuf []byte

	strings map[uint64]string
	// OPT(dh): pStates doesn't need to be a map, as processor IDs are gapless and start at 0. We just have to change
	// how we track fake Ps. Instead of starting them at a specific offset, give them the next free IDs.
	pStates     map[int32]*pState
	stacks      map[uint32][]uint64
	stacksData  []uint64
	ticksPerSec int64
	pcs         map[uint64]Frame
	cpuSamples  []Event

	// state for indexing
	curP int32

	// state for readRawEvent
	args []uint64

	// state for parseEvent
	lastTs       Timestamp
	lastG        uint64
	lastP        int32
	logMessageID uint64
}

//gcassert:inline
func (p *Parser) pState(pid int32) *pState {
	ps, ok := p.pStates[pid]
	if ok {
		return ps
	}
	ps = &pState{}
	p.pStates[pid] = ps
	return ps
}

//gcassert:inline
func (p *Parser) discard(n uint64) bool {
	if n > math.MaxInt {
		return false
	}
	if noff := p.off + int(n); noff < p.off || noff > len(p.data) {
		return false
	} else {
		p.off = noff
	}
	return true
}

func NewParser(r io.Reader) (*Parser, error) {
	var buf []byte
	if seeker, ok := r.(io.Seeker); ok {
		cur, err := seeker.Seek(0, io.SeekCurrent)
		if err != nil {
			return nil, err
		}
		end, err := seeker.Seek(0, io.SeekEnd)
		if err != nil {
			return nil, err
		}
		_, err = seeker.Seek(cur, io.SeekStart)
		if err != nil {
			return nil, err
		}

		buf = make([]byte, end-cur)
		_, err = io.ReadFull(r, buf)
		if err != nil {
			return nil, err
		}
	} else {
		var err error
		buf, err = io.ReadAll(r)
		if err != nil {
			return nil, err
		}
	}
	return &Parser{data: buf}, nil
}

func Parse(r io.Reader, progress func(float64)) (Trace, error) {
	p, err := NewParser(r)
	if err != nil {
		return Trace{}, err
	}
	p.progress = progress
	return p.Parse()
}

func (p *Parser) Parse() (Trace, error) {
	_, res, err := p.parse()
	p.data = nil
	return res, err
}

// parse parses, post-processes and verifies the trace. It returns the
// trace version and the list of events.
func (p *Parser) parse() (int, Trace, error) {
	p.strings = make(map[uint64]string)
	p.pStates = make(map[int32]*pState)
	p.stacks = make(map[uint32][]uint64)
	p.pcs = make(map[uint64]Frame)

	ver, err := p.readHeader()
	if err != nil {
		return 0, Trace{}, err
	}

	p.ver = ver

	if err := p.indexAndPartiallyParse(); err != nil {
		return 0, Trace{}, err
	}

	events, err := p.parseRest()
	if err != nil {
		return 0, Trace{}, err
	}

	if p.ticksPerSec == 0 {
		return 0, Trace{}, errors.New("no EvFrequency event")
	}

	if len(events) > 0 {
		// Translate cpu ticks to real time.
		minTs := events[0].Ts
		// Use floating point to avoid integer overflows.
		freq := 1e9 / float64(p.ticksPerSec)
		for i := range events {
			ev := &events[i]
			ev.Ts = Timestamp(float64(ev.Ts-minTs) * freq)
			// Move syscalls to separate fake Ps.
			if ev.Type == EvGoSysExit {
				ev.P = SyscallP
			}
		}
	}

	if err := p.postProcessTrace(events); err != nil {
		return 0, Trace{}, err
	}

	res := Trace{
		Events:  events,
		Stacks:  p.stacks,
		Strings: p.strings,
		PCs:     p.pcs,
	}
	return ver, res, nil
}

// rawEvent is a helper type used during parsing.
type rawEvent struct {
	typ   byte
	args  []uint64
	sargs []string
}

func (p *Parser) readHeader() (ver int, err error) {
	// Read and validate trace header.
	if len(p.data) < headerLength {
		return 0, errors.New("trace too short")
	}
	ver, err = parseHeader(p.data[:headerLength])
	if err != nil {
		return 0, err
	}
	p.off += headerLength
	switch ver {
	case 1011, 1019:
		// Note: When adding a new version, add canned traces
		// from the old version to the test suite using mkcanned.bash.
	default:
		return 0, fmt.Errorf("unsupported trace file version %d.%d", ver/1000, ver%1000)
	}

	return ver, err
}

type proc struct {
	pid    int32
	events []Event

	// there are no more batches left
	done bool
}

// parseRest reads per-P event batches and merges them into a single, consistent stream.
// The high level idea is as follows. Events within an individual batch are in
// correct order, because they are emitted by a single P. So we need to produce
// a correct interleaving of the batches. To do this we take first unmerged event
// from each batch (frontier). Then choose subset that is "ready" to be merged,
// that is, events for which all dependencies are already merged. Then we choose
// event with the lowest timestamp from the subset, merge it and repeat.
// This approach ensures that we form a consistent stream even if timestamps are
// incorrect (condition observed on some machines).
func (p *Parser) parseRest() ([]Event, error) {
	// The ordering of CPU profile sample events in the data stream is based on
	// when each run of the signal handler was able to acquire the spinlock,
	// with original timestamps corresponding to when ReadTrace pulled the data
	// off of the profBuf queue. Re-sort them by the timestamp we captured
	// inside the signal handler.
	sort.Sort((*eventList)(&p.cpuSamples))

	var totalEvents uint64
	allProcs := make([]proc, 0, len(p.pStates))
	for m, pState := range p.pStates {
		allProcs = append(allProcs, proc{pid: m})

		for _, b := range pState.batches {
			totalEvents += uint64(b.numEvents)
		}
	}
	allProcs = append(allProcs, proc{pid: ProfileP, events: p.cpuSamples})
	totalEvents += uint64(len(p.cpuSamples))

	if totalEvents > math.MaxInt32 {
		return nil, ErrTooManyEvents
	}

	events := make([]Event, 0, totalEvents)

	// Merge events as long as at least one P has more events
	gs := make(map[uint64]gState)
	// Note: technically we don't need a priority queue here. We're only ever interested in the earliest elligible
	// event, which means we just have to track the smallest element. However, in practice, the priority queue performs
	// better, because for each event we only have to compute its state transition once, not on each iteration. If it
	// was elligible before, it'll already be in the queue. Furthermore, on average, we only have one P to look at in
	// each iteration, because all other Ps are already in the queue.
	var frontier orderEventList

	availableProcs := make([]*proc, len(allProcs))
	for i := range allProcs {
		availableProcs[i] = &allProcs[i]
	}
	for {
		if p.progress != nil && len(events)%100_000 == 0 {
			p.progress(0.5 + 0.5*(float64(len(events))/float64(cap(events))))
		}
	pidLoop:
		for i := 0; i < len(availableProcs); i++ {
			proc := availableProcs[i]

			for len(proc.events) == 0 {
				// Call loadBatch in a loop because sometimes batches are empty
				evs, err := p.loadBatch(proc.pid)
				if err == io.EOF {
					// This P has no more events
					proc.done = true
					availableProcs[i], availableProcs[len(availableProcs)-1] = availableProcs[len(availableProcs)-1], availableProcs[i]
					availableProcs = availableProcs[:len(availableProcs)-1]
					// We swapped the element at i with another proc, so look at the index again
					i--
					continue pidLoop
				} else if err != nil {
					return nil, err
				} else {
					proc.events = evs
				}
			}

			ev := &proc.events[0]
			g, init, _ := stateTransition(ev)

			// TODO(dh): This implementation matches the behavior of the upstream 'go tool trace', and works in
			// practice, but has run into the following inconsistency during fuzzing: what happens if multiple Ps have
			// events for the same G? While building the frontier we will check all of the events against the current
			// state of the G. However, when we process the frontier, the state of the G changes, and a transition that
			// was valid while building the frontier may no longer be valid when processing the frontier. Is this
			// something that can happen for real, valid traces, or is this only possible with corrupt data?
			if !transitionReady(g, gs[g], init) {
				continue
			}
			proc.events = proc.events[1:]
			availableProcs[i], availableProcs[len(availableProcs)-1] = availableProcs[len(availableProcs)-1], availableProcs[i]
			availableProcs = availableProcs[:len(availableProcs)-1]
			frontier.Push(orderEvent{*ev, proc})

			// We swapped the element at i with another proc, so look at the index again
			i--
		}

		if len(frontier) == 0 {
			for i := range allProcs {
				if !allProcs[i].done {
					return nil, fmt.Errorf("no consistent ordering of events possible")
				}
			}
			break
		}
		f := frontier.Pop()

		// We're computing the state transition twice, once when computing the frontier, and now to apply the
		// transition. This is fine because stateTransition is a pure function. Computing it again is cheaper than
		// storing large items in the frontier.
		g, init, next := stateTransition(&f.ev)

		// Get rid of "Local" events, they are intended merely for ordering.
		switch f.ev.Type {
		case EvGoStartLocal:
			f.ev.Type = EvGoStart
		case EvGoUnblockLocal:
			f.ev.Type = EvGoUnblock
		case EvGoSysExitLocal:
			f.ev.Type = EvGoSysExit
		}
		events = append(events, f.ev)

		if err := transition(gs, g, init, next); err != nil {
			return nil, err
		}
		availableProcs = append(availableProcs, f.proc)
	}

	// At this point we have a consistent stream of events.
	// Make sure time stamps respect the ordering.
	// The tests will skip (not fail) the test case if they see this error.
	if !sort.IsSorted((*eventList)(&events)) {
		return nil, ErrTimeOrder
	}

	// The last part is giving correct timestamps to EvGoSysExit events.
	// The problem with EvGoSysExit is that actual syscall exit timestamp (ev.Args[2])
	// is potentially acquired long before event emission. So far we've used
	// timestamp of event emission (ev.Ts).
	// We could not set ev.Ts = ev.Args[2] earlier, because it would produce
	// seemingly broken timestamps (misplaced event).
	// We also can't simply update the timestamp and resort events, because
	// if timestamps are broken we will misplace the event and later report
	// logically broken trace (instead of reporting broken timestamps).
	lastSysBlock := make(map[uint64]Timestamp)
	for _, ev := range events {
		switch ev.Type {
		case EvGoSysBlock, EvGoInSyscall:
			lastSysBlock[ev.G] = ev.Ts
		case EvGoSysExit:
			ts := Timestamp(ev.Args[2])
			if ts == 0 {
				continue
			}
			block := lastSysBlock[ev.G]
			if block == 0 {
				return nil, fmt.Errorf("stray syscall exit")
			}
			if ts < block {
				return nil, ErrTimeOrder
			}
			ev.Ts = ts
		}
	}
	sort.Stable((*eventList)(&events))

	return events, nil
}

// indexAndPartiallyParse records the offsets of batches and parses strings and CPU samples.
func (p *Parser) indexAndPartiallyParse() error {
	// Read events.
	var raw rawEvent
	for n := uint64(0); ; n++ {
		if p.progress != nil && n%1_000_000 == 0 {
			p.progress(0.5 * (float64(p.off) / float64(len(p.data))))
		}
		err := p.readRawEvent(skipArgs|skipStrings|trackBatches, &raw)
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		if raw.typ == EvNone {
			continue
		}

		if raw.typ == EvCPUSample {
			e := Event{Type: raw.typ}

			argOffset := 1
			narg := argNum(&raw)
			if len(raw.args) != narg {
				return fmt.Errorf("CPU sample has wrong number of arguments: want %d, got %d", narg, len(raw.args))
			}
			for i := argOffset; i < narg; i++ {
				if i == narg-1 {
					e.StkID = uint32(raw.args[i])
				} else {
					e.Args[i-argOffset] = raw.args[i]
				}
			}

			e.Ts = Timestamp(e.Args[0])
			e.P = int32(e.Args[1])
			e.G = e.Args[2]
			e.Args[0] = 0

			// Most events are written out by the active P at the exact
			// moment they describe. CPU profile samples are different
			// because they're written to the tracing log after some delay,
			// by a separate worker goroutine, into a separate buffer.
			//
			// We keep these in their own batch until all of the batches are
			// merged in timestamp order. We also (right before the merge)
			// re-sort these events by the timestamp captured in the
			// profiling signal handler.
			//
			// Note that we're not concerned about the memory usage of storing all CPU samples during the indexing
			// phase. There are orders of magnitude fewer CPU samples than runtime events.
			p.cpuSamples = append(p.cpuSamples, e)
		}
	}

	if p.progress != nil {
		p.progress(0.5)
	}

	return nil
}

const (
	skipArgs = 1 << iota
	skipStrings
	trackBatches
)

//gcassert:inline
func (p *Parser) readByte() (byte, bool) {
	if p.off < len(p.data) && p.off >= 0 {
		b := p.data[p.off]
		p.off++
		return b, true
	} else {
		return 0, false
	}
}

//gcassert:inline
func (p *Parser) readFull(b []byte) bool {
	if p.off >= len(p.data) || p.off < 0 || p.off+len(b) > len(p.data) {
		// p.off < 0 is impossible but makes BCE happy.
		// We do fail outright if there's not enough data, we don't care about partial results.
		return false
	}
	copy(b, p.data[p.off:])
	p.off += len(b)
	return true
}

func (p *Parser) readRawEvent(flags uint, ev *rawEvent) error {
	// The number of arguments is encoded using two bits and can thus only represent the values 0–3. The value 3 (on the
	// wire) indicates that arguments are prefixed by their byte length, to encode >=3 arguments.
	const inlineArgs = 3

	// Read event type and number of arguments (1 byte).
	b, ok := p.readByte()
	if !ok {
		return io.EOF
	}
	typ := b << 2 >> 2
	// Most events have a timestamp before the actual arguments, so we add 1 and parse it like it's the first argument.
	// EvString has a special format and the number of arguments doesn't matter. EvBatch writes '1' as the number of
	// arguments, but actually has two: a pid and a timestamp, but here the timestamp is the second argument, not the
	// first; adding 1 happens to come up with the correct number, but it doesn't matter, because EvBatch has custom
	// logic for parsing.
	//
	// Note that because we're adding 1, inlineArgs == 3 describes the largest number of logical arguments that isn't
	// length-prefixed, even though the value 3 on the wire indicates length-prefixing. For us, that becomes narg == 4.
	narg := b>>6 + 1
	if typ == EvNone || typ >= EvCount || EventDescriptions[typ].minVersion > p.ver {
		return fmt.Errorf("unknown event type %d", typ)
	}

	switch typ {
	case EvString:
		if flags&skipStrings != 0 {
			// String dictionary entry [ID, length, string].
			if !p.discardVal() {
				return errMalformedVarint
			}
			ln, ok := p.readVal()
			if !ok {
				return errMalformedVarint
			}
			if !p.discard(ln) {
				return fmt.Errorf("failed to read trace: %w", io.EOF)
			}
		} else {
			// String dictionary entry [ID, length, string].
			id, ok := p.readVal()
			if !ok {
				return errMalformedVarint
			}
			if id == 0 {
				return errors.New("string has invalid id 0")
			}
			if p.strings[id] != "" {
				return fmt.Errorf("string has duplicate id %d", id)
			}
			var ln uint64
			ln, ok = p.readVal()
			if !ok {
				return errMalformedVarint
			}
			if ln == 0 {
				return errors.New("string has invalid length 0")
			}
			if ln > 1e6 {
				return fmt.Errorf("string has too large length %d", ln)
			}
			buf := make([]byte, ln)
			if !p.readFull(buf) {
				return fmt.Errorf("failed to read trace: %w", io.ErrUnexpectedEOF)
			}
			p.strings[id] = string(buf)
		}

		ev.typ = EvNone
		return nil
	case EvBatch:
		if want := byte(2); narg != want {
			return fmt.Errorf("EvBatch has wrong number of arguments: got %d, want %d", narg, want)
		}

		// -1 because we've already read the first byte of the batch
		off := p.off - 1

		pid, ok := p.readVal()
		if !ok {
			return errMalformedVarint
		}
		if pid != math.MaxUint64 && pid > math.MaxInt32 {
			return fmt.Errorf("processor ID %d is larger than maximum of %d", pid, uint64(math.MaxUint))
		}

		var pid32 int32
		if pid == math.MaxUint64 {
			pid32 = -1
		} else {
			pid32 = int32(pid)
		}

		if flags&trackBatches != 0 {
			p.pState(pid32).batches = append(p.pState(pid32).batches, batch{offset: off})
			p.curP = pid32
		}

		v, ok := p.readVal()
		if !ok {
			return errMalformedVarint
		}

		*ev = rawEvent{typ: EvBatch, args: p.args[:0]}
		ev.args = append(ev.args, pid, v)
		return nil
	default:
		if flags&trackBatches != 0 {
			batches := p.pState(p.curP).batches
			if len(batches) == 0 {
				return fmt.Errorf("read event %d with current P of %d, but P has no batches yet", typ, p.curP)
			}
			batches[len(batches)-1].numEvents++
		}

		*ev = rawEvent{typ: typ, args: p.args[:0]}
		if narg <= inlineArgs {
			if flags&skipArgs == 0 {
				for i := 0; i < int(narg); i++ {
					v, ok := p.readVal()
					if !ok {
						return fmt.Errorf("failed to read event %d argument: %w", typ, errMalformedVarint)
					}
					ev.args = append(ev.args, v)
				}
			} else {
				for i := 0; i < int(narg); i++ {
					if !p.discardVal() {
						return fmt.Errorf("failed to read event %d argument: %w", typ, errMalformedVarint)
					}
				}
			}
		} else {
			// More than inlineArgs args, the first value is length of the event in bytes.
			//
			// OPT(dh): looking at the runtime code, the length seems to be limited to < 128, i.e. a single byte. we
			// don't have to use readVal. However, there are so few events with more than 4 arguments that calling
			// readVal is barely noticeable.
			v, ok := p.readVal()
			if !ok {
				return fmt.Errorf("failed to read event %d argument: %w", typ, errMalformedVarint)
			}

			if limit := uint64(2048); v > limit {
				// At the time of Go 1.19, v seems to be at most 128. Set 2048 as a generous upper limit and guard
				// against malformed traces.
				return fmt.Errorf("failed to read event %d argument: length-prefixed argument too big: %d bytes, limit is %d", typ, v, limit)
			}

			if flags&skipArgs == 0 || typ == EvCPUSample {
				buf := p.bigArgsBuf
				if uint64(cap(buf)) >= v {
					buf = buf[:v]
				} else {
					buf = make([]byte, v)
					p.bigArgsBuf = buf[:0]
				}
				if !p.readFull(buf) {
					return fmt.Errorf("failed to read trace: %w", io.ErrUnexpectedEOF)
				}
				for len(buf) > 0 {
					var v uint64
					var ok bool
					v, buf, ok = readValFrom(buf)
					if !ok {
						return errMalformedVarint
					}
					ev.args = append(ev.args, v)
				}
			} else {
				// Skip over arguments
				if !p.discard(v) {
					return fmt.Errorf("failed to read trace: %w", io.EOF)
				}
			}
			if typ == EvUserLog {
				// EvUserLog records are followed by a value string
				if flags&skipArgs == 0 {
					// Read string
					s, err := p.readStr()
					if err != nil {
						return err
					}
					ev.sargs = append(ev.sargs, s)
				} else {
					// Skip string
					v, ok := p.readVal()
					if !ok {
						return errMalformedVarint
					}
					if !p.discard(v) {
						return io.EOF
					}
				}
			}
		}

		p.args = ev.args[:0]
		return nil
	}
}

func (p *Parser) loadBatch(pid int32) ([]Event, error) {
	pState := p.pState(pid)

	offsets := pState.batches
	if len(offsets) == 0 {
		return nil, io.EOF
	}
	n := offsets[0].numEvents
	offset := offsets[0].offset
	offsets = offsets[1:]
	pState.batches = offsets

	p.off = offset

	events := pState.slice[:0]
	if cap(events) < n {
		events = make([]Event, 0, n)
		pState.slice = events
	}

	gotHeader := false
	var raw rawEvent
	var ev Event
	for {
		err := p.readRawEvent(0, &raw)
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		if raw.typ == EvNone || raw.typ == EvCPUSample {
			continue
		}
		if raw.typ == EvBatch {
			if gotHeader {
				break
			} else {
				gotHeader = true
			}
		}

		err = p.parseEvent(&raw, &ev)
		if err != nil {
			return nil, err
		}
		if ev.Type != EvNone {
			events = append(events, ev)
		}
	}

	return events, nil
}

func (p *Parser) readStr() (s string, err error) {
	sz, ok := p.readVal()
	if !ok {
		return "", errMalformedVarint
	}
	if sz == 0 {
		return "", nil
	}
	if sz > 1e6 {
		return "", fmt.Errorf("string is too large (len=%d)", sz)
	}
	buf := make([]byte, sz)
	if !p.readFull(buf) {
		return "", fmt.Errorf("failed to read trace: %w", io.ErrUnexpectedEOF)
	}
	return string(buf), nil
}

// parseHeader parses trace header of the form "go 1.7 trace\x00\x00\x00\x00"
// and returns parsed version as 1007.
func parseHeader(buf []byte) (int, error) {
	if len(buf) != headerLength {
		return 0, errors.New("bad header length")
	}
	if buf[0] != 'g' || buf[1] != 'o' || buf[2] != ' ' ||
		buf[3] < '1' || buf[3] > '9' ||
		buf[4] != '.' ||
		buf[5] < '1' || buf[5] > '9' {
		return 0, errors.New("not a trace file")
	}
	ver := int(buf[5] - '0')
	i := 0
	for ; buf[6+i] >= '0' && buf[6+i] <= '9' && i < 2; i++ {
		ver = ver*10 + int(buf[6+i]-'0')
	}
	ver += int(buf[3]-'0') * 1000
	if !bytes.Equal(buf[6+i:], []byte(" trace\x00\x00\x00\x00")[:10-i]) {
		return 0, errors.New("not a trace file")
	}
	return ver, nil
}

// parseEvent transforms raw events into events.
// It does analyze and verify per-event-type arguments.
func (p *Parser) parseEvent(raw *rawEvent, ev *Event) error {
	desc := &EventDescriptions[raw.typ]
	if desc.Name == "" {
		return fmt.Errorf("missing description for event type %d", raw.typ)
	}
	narg := argNum(raw)
	if len(raw.args) != narg {
		return fmt.Errorf("%s has wrong number of arguments: want %d, got %d", desc.Name, narg, len(raw.args))
	}
	switch raw.typ {
	case EvBatch:
		p.pState(p.lastP).lastG = p.lastG
		if raw.args[0] != math.MaxUint64 && raw.args[0] > math.MaxInt32 {
			return fmt.Errorf("processor ID %d is larger than maximum of %d", raw.args[0], uint64(math.MaxInt32))
		}
		if raw.args[0] == math.MaxUint64 {
			p.lastP = -1
		} else {
			p.lastP = int32(raw.args[0])
		}
		p.lastG = p.pState(p.lastP).lastG
		p.lastTs = Timestamp(raw.args[1])
	case EvFrequency:
		p.ticksPerSec = int64(raw.args[0])
		if p.ticksPerSec <= 0 {
			// The most likely cause for this is tick skew on different CPUs.
			// For example, solaris/amd64 seems to have wildly different
			// ticks on different CPUs.
			return ErrTimeOrder
		}
	case EvTimerGoroutine:
		// Timer goroutines haven't been used since 2019, see https://go.dev/cl/171884
		return errors.New("unsupported event EvTimerGoroutine")
	case EvStack:
		if len(raw.args) < 2 {
			return fmt.Errorf("EvStack has wrong number of arguments: want at least 2, got %d", len(raw.args))
		}
		size := raw.args[1]
		if size > 1000 {
			return fmt.Errorf("EvStack has bad number of frames: %d", size)
		}
		want := 2 + 4*size
		if uint64(len(raw.args)) != want {
			return fmt.Errorf("EvStack has wrong number of arguments: want %d, got %d", want, len(raw.args))
		}
		id := uint32(raw.args[0])
		if id != 0 && size > 0 {
			stk := p.allocateStack(size)
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
			p.stacks[id] = stk
		}
	case EvCPUSample:
		// These events get parsed during the indexing step and don't strictly belong to the batch.
	default:
		*ev = Event{Type: raw.typ, P: p.lastP, G: p.lastG, Link: -1}
		var argOffset int
		ev.Ts = p.lastTs + Timestamp(raw.args[0])
		argOffset = 1
		p.lastTs = ev.Ts
		for i := argOffset; i < narg; i++ {
			if i == narg-1 && desc.Stack {
				ev.StkID = uint32(raw.args[i])
			} else {
				ev.Args[i-argOffset] = raw.args[i]
			}
		}
		switch raw.typ {
		case EvGoStart, EvGoStartLocal, EvGoStartLabel:
			p.lastG = ev.Args[0]
			ev.G = p.lastG
		case EvGoEnd, EvGoStop, EvGoSched, EvGoPreempt,
			EvGoSleep, EvGoBlock, EvGoBlockSend, EvGoBlockRecv,
			EvGoBlockSelect, EvGoBlockSync, EvGoBlockCond, EvGoBlockNet,
			EvGoSysBlock, EvGoBlockGC:
			p.lastG = 0
		case EvGoSysExit, EvGoWaiting, EvGoInSyscall:
			ev.G = ev.Args[0]
		case EvUserTaskCreate:
			// e.Args 0: taskID, 1:parentID, 2:nameID
		case EvUserRegion:
			// e.Args 0: taskID, 1: mode, 2:nameID
		case EvUserLog:
			// e.Args 0: taskID, 1:keyID, 2: stackID, 3: messageID
			// raw.sargs 0: message

			// EvUserLog contains the message inline, not as a string ID. We turn it into an ID. String IDs are
			// (currently) sequentially allocated and start from zero, so we count backwards starting from MaxUint64,
			// hoping runtime IDs and our IDs will never meet.
			p.logMessageID--
			p.strings[p.logMessageID] = raw.sargs[0]
			ev.Args[3] = p.logMessageID
		}

		return nil
	}

	ev.Type = EvNone
	return nil
}

// ErrTimeOrder is returned by Parse when the trace contains
// time stamps that do not respect actual event ordering.
var ErrTimeOrder = errors.New("time stamps out of order")

// postProcessTrace does inter-event verification and information restoration.
// The resulting trace is guaranteed to be consistent
// (for example, a P does not run two Gs at the same time, or a G is indeed
// blocked before an unblock event).
func (p *Parser) postProcessTrace(events []Event) error {
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
	ps := make(map[int32]pdesc)
	tasks := make(map[uint64]*Event)           // task id to task creation events
	activeRegions := make(map[uint64][]*Event) // goroutine id to stack of regions
	gs[0] = gdesc{state: gRunning}
	var evGC, evSTW *Event

	checkRunning := func(p pdesc, g gdesc, ev *Event, allowG0 bool) error {
		name := EventDescriptions[ev.Type].Name
		if g.state != gRunning {
			return fmt.Errorf("g %d is not running while %s (time %d)", ev.G, name, ev.Ts)
		}
		if p.g != ev.G {
			return fmt.Errorf("p %d is not running g %d while %s (time %d)", ev.P, ev.G, name, ev.Ts)
		}
		if !allowG0 && ev.G == 0 {
			return fmt.Errorf("g 0 did %s (time %d)", name, ev.Ts)
		}
		return nil
	}

	for evIdx := range events {
		ev := &events[evIdx]

		// Note: each branch is responsible for retrieving P and G descriptions and writing back modifications to the
		// maps. Deduplicating this step and pulling it outside the switch is too expensive.
		switch ev.Type {
		case EvProcStart:
			p := ps[ev.P]
			if p.running {
				return fmt.Errorf("p %d is running before start (time %d)", ev.P, ev.Ts)
			}
			p.running = true

			ps[ev.P] = p
		case EvProcStop:
			p := ps[ev.P]
			if !p.running {
				return fmt.Errorf("p %d is not running before stop (time %d)", ev.P, ev.Ts)
			}
			if p.g != 0 {
				return fmt.Errorf("p %d is running a goroutine %d during stop (time %d)", ev.P, p.g, ev.Ts)
			}
			p.running = false

			ps[ev.P] = p
		case EvGCStart:
			if evGC != nil {
				return fmt.Errorf("previous GC is not ended before a new one (time %d)", ev.Ts)
			}
			evGC = ev
			// Attribute this to the global GC state.
			ev.P = GCP
		case EvGCDone:
			if evGC == nil {
				return fmt.Errorf("bogus GC end (time %d)", ev.Ts)
			}
			evGC.Link = int32(evIdx)
			evGC = nil
		case EvGCSTWStart:
			evp := &evSTW
			if *evp != nil {
				return fmt.Errorf("previous STW is not ended before a new one (time %d)", ev.Ts)
			}
			*evp = ev
		case EvGCSTWDone:
			evp := &evSTW
			if *evp == nil {
				return fmt.Errorf("bogus STW end (time %d)", ev.Ts)
			}
			(*evp).Link = int32(evIdx)
			*evp = nil
		case EvGCSweepStart:
			p := ps[ev.P]
			if p.evSweep != nil {
				return fmt.Errorf("previous sweeping is not ended before a new one (time %d)", ev.Ts)
			}
			p.evSweep = ev

			ps[ev.P] = p
		case EvGCMarkAssistStart:
			g := gs[ev.G]
			if g.evMarkAssist != nil {
				return fmt.Errorf("previous mark assist is not ended before a new one (time %d)", ev.Ts)
			}
			g.evMarkAssist = ev

			gs[ev.G] = g
		case EvGCMarkAssistDone:
			// Unlike most events, mark assists can be in progress when a
			// goroutine starts tracing, so we can't report an error here.
			g := gs[ev.G]
			if g.evMarkAssist != nil {
				g.evMarkAssist.Link = int32(evIdx)
				g.evMarkAssist = nil
			}

			gs[ev.G] = g
		case EvGCSweepDone:
			p := ps[ev.P]
			if p.evSweep == nil {
				return fmt.Errorf("bogus sweeping end (time %d)", ev.Ts)
			}
			p.evSweep.Link = int32(evIdx)
			p.evSweep = nil

			ps[ev.P] = p
		case EvGoWaiting:
			g := gs[ev.G]
			if g.state != gRunnable {
				return fmt.Errorf("g %d is not runnable before EvGoWaiting (time %d)", ev.G, ev.Ts)
			}
			g.state = gWaiting
			g.ev = ev

			gs[ev.G] = g
		case EvGoInSyscall:
			g := gs[ev.G]
			if g.state != gRunnable {
				return fmt.Errorf("g %d is not runnable before EvGoInSyscall (time %d)", ev.G, ev.Ts)
			}
			g.state = gWaiting
			g.ev = ev

			gs[ev.G] = g
		case EvGoCreate:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, true); err != nil {
				return err
			}
			if _, ok := gs[ev.Args[0]]; ok {
				return fmt.Errorf("g %d already exists (time %d)", ev.Args[0], ev.Ts)
			}
			gs[ev.Args[0]] = gdesc{state: gRunnable, ev: ev, evCreate: ev}

		case EvGoStart, EvGoStartLabel:
			g := gs[ev.G]
			p := ps[ev.P]
			if g.state != gRunnable {
				return fmt.Errorf("g %d is not runnable before start (time %d)", ev.G, ev.Ts)
			}
			if p.g != 0 {
				return fmt.Errorf("p %d is already running g %d while start g %d (time %d)", ev.P, p.g, ev.G, ev.Ts)
			}
			g.state = gRunning
			g.evStart = ev
			p.g = ev.G
			if g.evCreate != nil {
				ev.StkID = uint32(g.evCreate.Args[1])
				g.evCreate = nil
			}

			if g.ev != nil {
				g.ev.Link = int32(evIdx)
				g.ev = nil
			}

			gs[ev.G] = g
			ps[ev.P] = p
		case EvGoEnd, EvGoStop:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.evStart.Link = int32(evIdx)
			g.evStart = nil
			g.state = gDead
			p.g = 0

			if ev.Type == EvGoEnd { // flush all active regions
				regions := activeRegions[ev.G]
				for _, s := range regions {
					s.Link = int32(evIdx)
				}
				delete(activeRegions, ev.G)
			}

			gs[ev.G] = g
			ps[ev.P] = p
		case EvGoSched, EvGoPreempt:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gRunnable
			g.evStart.Link = int32(evIdx)
			g.evStart = nil
			p.g = 0
			g.ev = ev

			gs[ev.G] = g
			ps[ev.P] = p
		case EvGoUnblock:
			g := gs[ev.G]
			p := ps[ev.P]
			if g.state != gRunning {
				return fmt.Errorf("g %d is not running while unpark (time %d)", ev.G, ev.Ts)
			}
			if p.g != ev.G {
				return fmt.Errorf("p %d is not running g %d while unpark (time %d)", ev.P, ev.G, ev.Ts)
			}
			g1 := gs[ev.Args[0]]
			if g1.state != gWaiting {
				return fmt.Errorf("g %d is not waiting before unpark (time %d)", ev.Args[0], ev.Ts)
			}
			if g1.ev != nil && g1.ev.Type == EvGoBlockNet {
				ev.P = NetpollP
			}
			if g1.ev != nil {
				g1.ev.Link = int32(evIdx)
			}
			g1.state = gRunnable
			g1.ev = ev
			gs[ev.Args[0]] = g1

		case EvGoSysCall:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.ev = ev

			gs[ev.G] = g
		case EvGoSysBlock:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gWaiting
			g.evStart.Link = int32(evIdx)
			g.evStart = nil
			p.g = 0

			gs[ev.G] = g
			ps[ev.P] = p
		case EvGoSysExit:
			g := gs[ev.G]
			if g.state != gWaiting {
				return fmt.Errorf("g %d is not waiting during syscall exit (time %d)", ev.G, ev.Ts)
			}
			if g.ev != nil && g.ev.Type == EvGoSysCall {
				g.ev.Link = int32(evIdx)
			}
			g.state = gRunnable
			g.ev = ev

			gs[ev.G] = g
		case EvGoSleep, EvGoBlock, EvGoBlockSend, EvGoBlockRecv,
			EvGoBlockSelect, EvGoBlockSync, EvGoBlockCond, EvGoBlockNet, EvGoBlockGC:
			g := gs[ev.G]
			p := ps[ev.P]
			if err := checkRunning(p, g, ev, false); err != nil {
				return err
			}
			g.state = gWaiting
			g.ev = ev
			g.evStart.Link = int32(evIdx)
			g.evStart = nil
			p.g = 0

			gs[ev.G] = g
			ps[ev.P] = p
		case EvUserTaskCreate:
			taskid := ev.Args[0]
			if prevEv, ok := tasks[taskid]; ok {
				return fmt.Errorf("task id conflicts (id:%d), %q vs %q", taskid, ev, prevEv)
			}
			tasks[ev.Args[0]] = ev

		case EvUserTaskEnd:
			taskid := ev.Args[0]
			if taskCreateEv, ok := tasks[taskid]; ok {
				taskCreateEv.Link = int32(evIdx)
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
					s.Link = int32(evIdx)

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

		if ev.StkID != 0 && len(p.stacks[ev.StkID]) == 0 {
			// Make sure events don't refer to stacks that don't exist or to stacks with zero frames. Neither of these
			// should be possible, but better be safe than sorry.

			ev.StkID = 0
		}
	}

	// TODO(dvyukov): restore stacks for EvGoStart events.
	// TODO(dvyukov): test that all EvGoStart events has non-nil Link.

	return nil
}

var errMalformedVarint = errors.New("malformatted base-128 varint")

// readVal reads unsigned base-128 value from r.
//
//gcassert:inline
func (p *Parser) readVal() (uint64, bool) {
	var v uint64
	for i := 0; i < 10; i++ {
		b, ok := p.readByte()
		if !ok {
			return 0, false
		}
		v |= uint64(b&0x7f) << (uint(i) * 7)
		if b < 0x80 {
			return v, true
		}
	}
	return 0, false
}

//gcassert:inline
func (p *Parser) discardVal() bool {
	for i := 0; i < 10; i++ {
		b, ok := p.readByte()
		if !ok {
			return false
		}
		if b < 0x80 {
			return true
		}
	}
	return false
}

//gcassert:inline
func readValFrom(buf []byte) (v uint64, rem []byte, ok bool) {
	for i := 0; i < 10; i++ {
		if i >= len(buf) {
			return 0, nil, false
		}
		b := buf[i]
		v |= uint64(b&0x7f) << (uint(i) * 7)
		if b&0x80 == 0 {
			return v, buf[i+1:], true
		}
	}
	return 0, nil, false
}

func (ev *Event) String() string {
	desc := &EventDescriptions[ev.Type]
	w := new(bytes.Buffer)
	fmt.Fprintf(w, "%d %s p=%d g=%d", ev.Ts, desc.Name, ev.P, ev.G)
	for i, a := range desc.Args {
		fmt.Fprintf(w, " %s=%d", a, ev.Args[i])
	}
	return w.String()
}

// argNum returns total number of args for the event accounting for timestamps,
// sequence numbers and differences between trace format versions.
//
//gcassert:inline
func argNum(raw *rawEvent) int {
	desc := &EventDescriptions[raw.typ]
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
	EvUserTaskCreate    = 45 // trace.NewTask [timestamp, internal task id, internal parent id, stack, name string]
	EvUserTaskEnd       = 46 // end of task [timestamp, internal task id, stack]
	EvUserRegion        = 47 // trace.WithRegion [timestamp, internal task id, mode(0:start, 1:end), stack, name string]
	EvUserLog           = 48 // trace.Log [timestamp, internal id, key string id, stack, value string]
	EvCPUSample         = 49 // CPU profiling sample [timestamp, stack, real timestamp, real P id (-1 when absent), goroutine id]
	EvCount             = 50
)

var EventDescriptions = [256]struct {
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

//gcassert:inline
func (p *Parser) allocateStack(size uint64) []uint64 {
	if size == 0 {
		return nil
	}

	// Stacks are plentiful but small. For our "Staticcheck on std" trace with 11e6 events, we have roughly 500,000
	// stacks, using 200 MiB of memory. To avoid making 500,000 small allocations we allocate backing arrays 1 MiB at a
	// time.
	out := p.stacksData
	if uint64(len(out)) < size {
		out = make([]uint64, 1024*128)
	}
	p.stacksData = out[size:]
	return out[:size:size]
}

const (
	ArgGCSweepDoneReclaimed = 1
	ArgGCSweepDoneSwept     = 0
	ArgGoCreateG            = 0
	ArgGoCreateStack        = 1
	ArgGoStartLabelLabelID  = 2
	ArgGoUnblockG           = 0
	ArgUserLogKeyID         = 1
	ArgUserLogMessage       = 3
	ArgUserRegionMode       = 1
	ArgUserRegionTaskID     = 0
	ArgUserRegionTypeID     = 2
	ArgUserTaskCreateTaskID = 0
	ArgUserTaskCreateTypeID = 2
)

package ptrace

type SpanTags uint8

const (
	SpanTagNetwork SpanTags = 1 << iota
	SpanTagTCP
	SpanTagTLS
	SpanTagRead
	SpanTagAccept
	SpanTagDial
	SpanTagHTTP

	// Used for spans of GC goroutines, used when choosing span colors for processor timelines.
	SpanTagGC
)

type pattern struct {
	state SchedulingState
	// fns looks for functions in the stack at absolute offsets
	fns []string

	// relFns looks for runs of functions in the stack, at no particular offsets
	relFns [][]string

	newState SchedulingState
	at       uint8
	tags     SpanTags
}

// TODO(dh): implement a pattern language similar to that used in Staticcheck so that we can express ANDs and ORs
// without having to write a bunch of Go.
var patterns = [256][]pattern{
	// XXX add a pattern for "GC incremental sweep" range, to skip some amount of frames
	StateBlocked: {
		{
			state: StateBlocked,
			fns: []string{
				0: "runtime.ReadTrace",
			},
			newState: StateInactive,
		},
	},

	StateBlockedRecv: {
		{
			state: StateBlockedRecv,
			fns: []string{
				0: "runtime.chanrecv1",
			},
			at: 1,
		},
		{
			state: StateBlockedRecv,
			fns: []string{
				0: "runtime.chanrecv2",
			},
			at: 1,
		},
	},

	StateBlockedSend: {
		{
			state: StateBlockedSend,
			fns: []string{
				0: "runtime.chansend1",
			},
			at: 1,
		},
	},

	StateBlockedSync: {
		{
			state: StateBlockedSync,
			fns: []string{
				0: "runtime.gcStart",
			},
			newState: StateBlockedSyncTriggeringGC,
		},
		{
			state: StateBlockedSync,
			fns: []string{
				0: "sync.(*Mutex).Lock",
				1: "sync.(*Once).doSlow",
				2: "sync.(*Once).Do",
			},
			newState: StateBlockedSyncOnce,
			at:       3,
		},
	},

	StateBlockedCond: {
		{
			state: StateBlockedCond,
			fns: []string{
				0: "sync.(*Cond).Wait",
			},
			at: 1,
		},
	},

	StateBlockedNet: {
		{
			state: StateBlockedNet,
			fns: []string{
				0: "internal/poll.(*FD).Read",
			},
			tags: SpanTagRead,
			at:   1,
		},
		{
			state: StateBlockedNet,
			fns: []string{
				0: "internal/poll.(*FD).Read",
				1: "net.(*netFD).Read",
			},
			tags: SpanTagNetwork,
			at:   2,
		},
		{
			state: StateBlockedNet,
			fns: []string{
				0: "internal/poll.(*FD).Accept",
			},
			tags: SpanTagAccept,
			at:   1,
		},
		{
			state: StateBlockedNet,
			fns: []string{
				0: "internal/poll.(*FD).Accept",
				1: "net.(*netFD).accept",
			},
			at:   2,
			tags: SpanTagNetwork,
		},
		{
			state: StateBlockedNet,
			fns: []string{
				0: "internal/poll.(*FD).Accept",
				2: "net.(*TCPListener).accept",
				3: "net.(*TCPListener).Accept",
			},
			at:   4,
			tags: SpanTagTCP,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net.(*sysDialer).dialSingle"},
			},
			tags: SpanTagDial,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net.(*sysDialer).dialTCP"},
			},
			tags: SpanTagTCP,
		},

		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"crypto/tls.(*Conn).readFromUntil"},
			},
			tags: SpanTagTLS,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"crypto/tls.(*listener).Accept"},
			},
			tags: SpanTagTLS,
		},

		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net/http.(*connReader).Read"},
			},
			tags: SpanTagHTTP,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net/http.(*persistConn).Read"},
			},
			tags: SpanTagHTTP,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net/http.(*http2clientConnReadLoop).run"},
			},
			tags: SpanTagHTTP,
		},
		{
			state: StateBlockedNet,
			relFns: [][]string{
				{"net/http.(*Server).Serve"},
			},
			tags: SpanTagHTTP,
		},
	},

	StateBlockedSelect: {
		{
			state: StateBlockedSelect,
			at:    1,
		},
	},
}

func applyPatterns(tr *Trace, s Span, pcs []uint64) Span {
	// OPT(dh): be better than O(n)

patternLoop:
	for _, p := range patterns[s.State] {
		if len(pcs) < len(p.fns) {
			continue
		}

		for i, fn := range p.fns {
			if fn == "" {
				continue
			}
			if tr.PCs[pcs[i]].Func != fn {
				continue patternLoop
			}
		}

		for _, relFns := range p.relFns {
			matched := false

			// OPT(dh): be better than O(n²)
		offsetLoop:
			for start := 0; start < len(pcs); start++ {
				if len(pcs)-start < len(relFns) {
					break
				}

				for i, fn := range relFns {
					if fn == "" {
						continue
					}
					if tr.PCs[pcs[i+start]].Func != fn {
						continue offsetLoop
					}
				}

				matched = true
				break
			}

			if !matched {
				continue patternLoop
			}
		}

		if p.at != 0 && int(p.at) >= len(pcs) {
			continue
		}

		if p.at != 0 {
			if int(p.at) < len(pcs) {
				s.At = p.at
			} else {
				continue
			}
		}

		if p.newState != StateNone {
			s.State = p.newState
		}

		s.Tags |= p.tags
	}

	return s
}

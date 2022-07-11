package main

import "honnef.co/go/gotraceui/trace"

type spanTags uint16

const (
	spanTagNetwork spanTags = 1 << iota
	spanTagTCP
	spanTagTLS
	spanTagRead
	spanTagAccept
	spanTagDial
	spanTagHTTP
)

type pattern struct {
	state schedulingState
	// fns looks for functions in the stack at absolute offsets
	fns []string

	// relFns looks for runs of functions in the stack, at no particular offsets
	relFns [][]string

	newState schedulingState
	at       int
	tags     spanTags
}

// TODO(dh): implement a pattern language similar to that used in Staticcheck so that we can express ANDs and ORs
// without having to write a bunch of Go.
var patterns = []pattern{
	{
		state: stateBlocked,
		fns: []string{
			0: "runtime.ReadTrace",
		},
		newState: stateBlockedWaitingForTraceData,
	},

	{
		state: stateBlockedRecv,
		fns: []string{
			0: "runtime.chanrecv1",
		},
		at: 1,
	},
	{
		state: stateBlockedRecv,
		fns: []string{
			0: "runtime.chanrecv2",
		},
		at: 1,
	},
	{
		state: stateBlockedSend,
		fns: []string{
			0: "runtime.chansend1",
		},
		at: 1,
	},

	{
		state: stateBlockedSync,
		fns: []string{
			0: "runtime.gcStart",
		},
		newState: stateBlockedSyncTriggeringGC,
	},
	{
		state: stateBlockedSync,
		fns: []string{
			0: "sync.(*Mutex).Lock",
			1: "sync.(*Once).doSlow",
			2: "sync.(*Once).Do",
		},
		newState: stateBlockedSyncOnce,
		at:       3,
	},

	{
		state: stateBlockedCond,
		fns: []string{
			0: "sync.(*Cond).Wait",
		},
		at: 1,
	},

	{
		state: stateBlockedNet,
		fns: []string{
			0: "internal/poll.(*FD).Read",
		},
		tags: spanTagRead,
		at:   1,
	},
	{
		state: stateBlockedNet,
		fns: []string{
			0: "internal/poll.(*FD).Read",
			1: "net.(*netFD).Read",
		},
		tags: spanTagNetwork,
		at:   2,
	},

	{
		state: stateBlockedNet,
		fns: []string{
			0: "internal/poll.(*FD).Accept",
		},
		tags: spanTagAccept,
		at:   1,
	},
	{
		state: stateBlockedNet,
		fns: []string{
			0: "internal/poll.(*FD).Accept",
			1: "net.(*netFD).accept",
		},
		at:   2,
		tags: spanTagNetwork,
	},
	{
		state: stateBlockedNet,
		fns: []string{
			0: "internal/poll.(*FD).Accept",
			2: "net.(*TCPListener).accept",
			3: "net.(*TCPListener).Accept",
		},
		at:   4,
		tags: spanTagTCP,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net.(*sysDialer).dialSingle"},
		},
		tags: spanTagDial,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net.(*sysDialer).dialTCP"},
		},
		tags: spanTagTCP,
	},

	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"crypto/tls.(*Conn).readFromUntil"},
		},
		tags: spanTagTLS,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"crypto/tls.(*listener).Accept"},
		},
		tags: spanTagTLS,
	},

	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net/http.(*connReader).Read"},
		},
		tags: spanTagHTTP,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net/http.(*persistConn).Read"},
		},
		tags: spanTagHTTP,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net/http.(*http2clientConnReadLoop).run"},
		},
		tags: spanTagHTTP,
	},
	{
		state: stateBlockedNet,
		relFns: [][]string{
			{"net/http.(*Server).Serve"},
		},
		tags: spanTagHTTP,
	},

	{
		state: stateBlockedSelect,
		at:    1,
	},
}

func applyPatterns(s Span, pcs map[uint64]trace.Frame, stacks map[uint64]trace.Stack) Span {
	// OPT(dh): be better than O(n)

	stack := stacks[s.Stack].Decode()
patternLoop:
	for _, p := range patterns {
		if s.State != p.state {
			continue
		}
		if len(stack) < len(p.fns) {
			continue
		}

		for i, fn := range p.fns {
			if fn == "" {
				continue
			}
			if pcs[stack[i]].Fn != fn {
				continue patternLoop
			}
		}

		for _, relFns := range p.relFns {
			matched := false

			// OPT(dh): be better than O(n²)
		offsetLoop:
			for start := range stack {
				if len(stack[start:]) < len(relFns) {
					break
				}

				for i, fn := range relFns {
					if fn == "" {
						continue
					}
					if pcs[stack[start:][i]].Fn != fn {
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

		if p.at != 0 && p.at >= len(stack) {
			continue
		}

		if p.at != 0 {
			if p.at < len(stack) {
				s.At = p.at
			} else {
				continue
			}
		}

		if p.newState != stateNone {
			s.State = p.newState
		}

		s.Tags |= p.tags
	}

	return s
}

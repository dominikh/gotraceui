package ptrace

var legalStateTransitions = [256][StateLast]bool{
	StateInactive: {
		StateActive:         true,
		StateReady:          true,
		StateBlockedSyscall: true,

		// Starting back into preempted mark assist
		StateGCMarkAssist: true,
	},
	StateActive: {
		// active -> ready occurs on preemption
		StateReady:                   true,
		StateInactive:                true,
		StateBlocked:                 true,
		StateBlockedSend:             true,
		StateBlockedRecv:             true,
		StateBlockedSelect:           true,
		StateBlockedSync:             true,
		StateBlockedSyncOnce:         true,
		StateBlockedSyncTriggeringGC: true,
		StateBlockedCond:             true,
		StateBlockedNet:              true,
		StateBlockedGC:               true,
		StateBlockedSyscall:          true,
		StateStuck:                   true,
		StateDone:                    true,
		StateGCMarkAssist:            true,
		StateGCSweep:                 true,
	},
	StateGCIdle: {
		// active -> ready occurs on preemption
		StateReady:       true,
		StateInactive:    true,
		StateBlockedSync: true,
	},
	StateGCDedicated: {
		// active -> ready occurs on preemption
		StateReady:       true,
		StateInactive:    true,
		StateBlockedSync: true,
	},
	StateCreated: {
		StateActive: true,

		// FIXME(dh): These three transitions are only valid for goroutines that already existed when tracing started.
		// eventually we'll make it so those goroutines don't end up in stateReady, at which point we should remove
		// these entries.
		StateInactive:       true,
		StateBlocked:        true,
		StateBlockedSyscall: true,
	},
	StateReady: {
		StateActive:       true,
		StateGCMarkAssist: true,
		StateGCIdle:       true,
		StateGCDedicated:  true,
	},
	StateBlocked:                 {StateReady: true},
	StateBlockedSend:             {StateReady: true},
	StateBlockedRecv:             {StateReady: true},
	StateBlockedSelect:           {StateReady: true},
	StateBlockedSync:             {StateReady: true},
	StateBlockedSyncOnce:         {StateReady: true},
	StateBlockedSyncTriggeringGC: {StateReady: true},
	StateBlockedCond:             {StateReady: true},
	StateBlockedNet:              {StateReady: true},
	StateBlockedGC:               {StateReady: true},
	StateBlockedSyscall: {
		StateReady: true,
	},

	StateGCMarkAssist: {
		// active -> ready occurs on preemption
		StateReady:       true,
		StateActive:      true, // back to the goroutine's previous state
		StateInactive:    true, // mark assist can be preempted
		StateBlocked:     true,
		StateBlockedSync: true,
		StateBlockedGC:   true, // XXX what does this transition mean?
	},

	StateGCSweep: {
		StateActive: true, // back to the goroutine's previous state
	},
}

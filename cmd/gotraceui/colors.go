package main

import "image/color"

var colors = [...]color.NRGBA{
	colorStateInactive: toColor(0x888888FF),
	colorStateActive:   toColor(0x448844FF),

	colorStateBlocked:              toColor(0xBA4141FF),
	colorStateBlockedHappensBefore: toColor(0xBB6363FF),
	colorStateBlockedNet:           toColor(0xBB5D5DFF),
	colorStateBlockedGC:            toColor(0x9C6FD6FF),
	colorStateBlockedSyscall:       toColor(0xBA4F41FF),
	colorStateGC:                   toColor(0x9C6FD6FF),

	colorStateReady:      toColor(0x4BACB8FF),
	colorStateStuck:      toColor(0x000000FF),
	colorStateMerged:     toColor(0xB9BB63FF),
	colorStateUnknown:    toColor(0xFFFF00FF),
	colorStateUserRegion: toColor(0xC1CF94FF),

	colorBackground:    toColor(0xffffeaFF),
	colorZoomSelection: toColor(0xeeee9e99),
	colorCursor:        toColor(0x000000FF),
	colorTick:          toColor(0x000000FF),
	colorTickLabel:     toColor(0x000000FF),

	colorActivityLabel:  toColor(0x888888FF),
	colorActivityBorder: toColor(0xDDDDDDFF),

	// TODO(dh): find a nice color for this
	colorSpanWithEvents: toColor(0xFF00FFFF),
	colorSpanOutline:    toColor(0x000000FF),
}

type colorIndex uint8

const (
	colorStateUnknown colorIndex = iota

	colorStateInactive
	colorStateActive

	colorStateBlocked
	colorStateBlockedHappensBefore
	colorStateBlockedNet
	colorStateBlockedGC
	colorStateBlockedSyscall
	colorStateGC

	colorStateReady
	colorStateStuck
	colorStateMerged
	colorStateUserRegion

	colorStateLast

	colorBackground
	colorZoomSelection
	colorCursor
	colorTick
	colorTickLabel

	colorActivityLabel
	colorActivityBorder

	colorSpanWithEvents
	colorSpanOutline
)

var stateColors = [256]colorIndex{
	// per-G states
	stateInactive:                colorStateInactive,
	stateActive:                  colorStateActive,
	stateBlocked:                 colorStateBlocked,
	stateBlockedSend:             colorStateBlockedHappensBefore,
	stateBlockedRecv:             colorStateBlockedHappensBefore,
	stateBlockedSelect:           colorStateBlockedHappensBefore,
	stateBlockedSync:             colorStateBlockedHappensBefore,
	stateBlockedCond:             colorStateBlockedHappensBefore,
	stateBlockedNet:              colorStateBlockedNet,
	stateBlockedGC:               colorStateBlockedGC,
	stateBlockedSyscall:          colorStateBlockedSyscall,
	stateStuck:                   colorStateStuck,
	stateReady:                   colorStateReady,
	stateCreated:                 colorStateReady,
	stateGCMarkAssist:            colorStateGC,
	stateGCSweep:                 colorStateGC,
	stateGCIdle:                  colorStateGC,
	stateGCDedicated:             colorStateGC,
	stateBlockedSyncOnce:         colorStateBlockedHappensBefore,
	stateBlockedSyncTriggeringGC: colorStateGC,
	stateUserRegion:              colorStateUserRegion,
	stateDone:                    colorStateUnknown, // no span with this state should be rendered

	// per-P states
	stateRunningG: colorStateActive,
}

func toColor(c uint32) color.NRGBA {
	// XXX does endianness matter?
	return color.NRGBA{
		A: uint8(c & 0xFF),
		B: uint8(c >> 8 & 0xFF),
		G: uint8(c >> 16 & 0xFF),
		R: uint8(c >> 24 & 0xFF),
	}
}

package main

import "image/color"

var colors = [...]color.NRGBA{
	colorStateInactive: rgba(0x888888FF),
	colorStateActive:   rgba(0x448844FF),

	colorStateBlocked:              rgba(0xBA4141FF),
	colorStateBlockedHappensBefore: rgba(0xBB6363FF),
	colorStateBlockedNet:           rgba(0xBB5D5DFF),
	colorStateBlockedGC:            rgba(0x9C6FD6FF),
	colorStateBlockedSyscall:       rgba(0xBA4F41FF),
	colorStateGC:                   rgba(0x9C6FD6FF),

	colorStateReady:      rgba(0x4BACB8FF),
	colorStateStuck:      rgba(0x000000FF),
	colorStateMerged:     rgba(0xB9BB63FF),
	colorStateUnknown:    rgba(0xFFFF00FF),
	colorStateUserRegion: rgba(0xF2A2E8FF),
	colorStateSample:     rgba(0x79B579FF),

	colorBackground:    rgba(0xffffeaFF),
	colorZoomSelection: rgba(0xeeee9e99),
	colorCursor:        rgba(0x000000FF),
	colorTick:          rgba(0x000000FF),
	colorTickLabel:     rgba(0x000000FF),

	colorTimelineLabel:  rgba(0x888888FF),
	colorTimelineBorder: rgba(0xDDDDDDFF),

	// TODO(dh): find a nice color for this
	colorSpanWithEvents:                  rgba(0xFF00FFFF),
	colorSpanOutline:                     rgba(0x000000FF),
	colorSpanHighlightedPrimaryOutline:   rgba(0xFF00FFFF),
	colorSpanHighlightedSecondaryOutline: rgba(0x6FFF00FF),
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
	colorStateSample

	colorStateLast

	colorBackground
	colorZoomSelection
	colorCursor
	colorTick
	colorTickLabel

	colorTimelineLabel
	colorTimelineBorder

	colorSpanWithEvents
	colorSpanOutline
	colorSpanHighlightedPrimaryOutline
	colorSpanHighlightedSecondaryOutline
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

	// per-M states
	stateRunningP: colorStateActive,
}

func rgba(c uint32) color.NRGBA {
	// XXX does endianness matter?
	return color.NRGBA{
		A: uint8(c & 0xFF),
		B: uint8(c >> 8 & 0xFF),
		G: uint8(c >> 16 & 0xFF),
		R: uint8(c >> 24 & 0xFF),
	}
}

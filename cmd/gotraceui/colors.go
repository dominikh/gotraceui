package main

import (
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/trace/ptrace"
)

const (
	colorsLightBase  = 58.51
	colorsChromaBase = 0.122
	colorLightStep1  = 15
	colorLightStep2  = 10
)

var colors = [colorLast]color.Oklch{
	colorStateUndetermined: oklch(colorsLightBase+colorLightStep1, colorsChromaBase, 70.54), // Manually chosen

	colorStateActive:             oklch(colorsLightBase, colorsChromaBase, 143.74),  // Manually chosen
	colorStateProcRunningNoG:     oklch(colorsLightBase, colorsChromaBase, 206.35),  // Manually chosen, same as colorStateReady
	colorStateProcRunningBlocked: oklch(colorsLightBase-5, colorsChromaBase, 23.89), // Manually chosen, same as colorStateBlocked
	colorStateProcRunningG:       oklch(colorsLightBase, colorsChromaBase, 143.74),  // Manually chosen, same as colorStateActive
	colorStateStack:              oklchDelta(oklch(colorsLightBase, colorsChromaBase, 143.74), colorLightStep1, -0.01, 0),
	colorStateCPUSample:          oklchDelta(oklchDelta(oklch(colorsLightBase, colorsChromaBase, 143.74), colorLightStep1, -0.01, 0), colorLightStep2, -0.01, 0),

	colorStateReady:            oklch(colorsLightBase, colorsChromaBase, 206.35), // Manually chosen
	colorStateWaitingPreempted: oklch(colorsLightBase, colorsChromaBase, 206.35), // Manually chosen
	colorStateInactive:         oklch(colorsLightBase, 0, 0),

	colorStateUserRegion: oklch(colorsLightBase+colorLightStep1+colorLightStep2, colorsChromaBase, 331.18), // Manually chosen

	// Manually chosen. This is the rarest blocked state, so we darken it to have more range for the other states.
	colorStateBlocked:              oklch(colorsLightBase-5, colorsChromaBase, 23.89),
	colorStateBlockedSyscall:       oklch(colorsLightBase, colorsChromaBase, 23.89),
	colorStateBlockedNet:           oklch(colorsLightBase+6, colorsChromaBase-0.01, 23.89),
	colorStateBlockedHappensBefore: oklch(colorsLightBase+colorLightStep2, colorsChromaBase, 23.89),
	colorStateBlockedGC:            oklch(colorsLightBase, colorsChromaBase, 0), // a blend of colorStateGC and red

	colorStateGC:  oklch(colorsLightBase, colorsChromaBase, 302.36),
	colorStateSTW: oklch(colorsLightBase, colorsChromaBase+0.072, 23.89), // STW is the most severe form of blocking, hence the increased chroma

	colorTimelineLabel:  oklch(62.68, 0, 0),
	colorTimelineBorder: oklch(89.75, 0, 0),

	// 	// TODO(dh): find a nice color for this
	// We don't use the l constant for thse colors because they're independent from the span colors
	colorSpanHighlightedPrimaryOutline:   oklch(70.71, 0.322, 328.36),
	colorSpanHighlightedSecondaryOutline: oklch(88.44, 0.27, 137.68),

	colorStateMerged: oklch(colorsLightBase+colorLightStep1, colorsChromaBase, 109.91), // Manually chosen, made brighter so it stands out in gradients

	colorStateStuck:   oklch(0, 0, 0),
	colorStateDone:    oklch(0, 0, 0),
	colorEvent:        oklch(colorsLightBase, colorsChromaBase, 0),
	colorMergedEvents: oklch(colorsLightBase+colorLightStep1, colorsChromaBase, 284.44),

	colorStateUnknown:              oklch(96.8, 0.211, 109.77),
	colorStatePlaceholderStackSpan: oklch(92.59, 0.025, 106.88),
}

var mappedColors [len(colors)]color.LinearSRGB

func init() {
	for i, c := range colors {
		mappedColors[i] = c.MapToSRGBGamut()
	}
}

type colorIndex uint8

const (
	colorStateUnknown colorIndex = iota

	colorStateUndetermined

	colorStateInactive
	colorStateActive

	colorStateBlocked
	colorStateBlockedHappensBefore
	colorStateBlockedNet
	colorStateBlockedGC
	colorStateBlockedSyscall
	colorStateGC
	colorStateSTW

	colorStateReady
	colorStateWaitingPreempted
	colorStateStuck
	colorStateMerged
	colorStateUserRegion
	colorStateStack
	colorStateCPUSample
	colorStateDone
	colorStatePlaceholderStackSpan

	colorStateProcRunningG
	colorStateProcRunningNoG
	colorStateProcRunningBlocked

	colorStateLast

	colorTimelineLabel
	colorTimelineBorder

	colorSpanHighlightedPrimaryOutline
	colorSpanHighlightedSecondaryOutline

	colorEvent
	colorMergedEvents

	colorLast
)

var stateColors = [256]colorIndex{
	ptrace.StateUndetermined: colorStateUndetermined,

	// per-G states
	ptrace.StateInactive:                colorStateInactive,
	ptrace.StateActive:                  colorStateActive,
	ptrace.StateBlocked:                 colorStateBlocked,
	ptrace.StateBlockedSend:             colorStateBlockedHappensBefore,
	ptrace.StateBlockedRecv:             colorStateBlockedHappensBefore,
	ptrace.StateBlockedSelect:           colorStateBlockedHappensBefore,
	ptrace.StateBlockedSync:             colorStateBlockedHappensBefore,
	ptrace.StateBlockedCond:             colorStateBlockedHappensBefore,
	ptrace.StateBlockedNet:              colorStateBlockedNet,
	ptrace.StateBlockedGC:               colorStateBlockedGC,
	ptrace.StateBlockedSyscall:          colorStateBlockedSyscall,
	ptrace.StateStuck:                   colorStateStuck,
	ptrace.StateReady:                   colorStateReady,
	ptrace.StateWaitingPreempted:        colorStateWaitingPreempted,
	ptrace.StateCreated:                 colorStateReady,
	ptrace.StateGCMarkAssist:            colorStateGC,
	ptrace.StateGCSweep:                 colorStateGC,
	ptrace.StateGCIdle:                  colorStateGC,
	ptrace.StateGCDedicated:             colorStateGC,
	ptrace.StateGCFractional:            colorStateGC,
	ptrace.StateBlockedSyncOnce:         colorStateBlockedHappensBefore,
	ptrace.StateBlockedSyncTriggeringGC: colorStateGC,
	ptrace.StateUserRegion:              colorStateUserRegion,
	ptrace.StateStack:                   colorStateStack,
	ptrace.StateCPUSample:               colorStateCPUSample,

	// per-P states
	ptrace.StateProcRunningNoG:     colorStateProcRunningNoG,
	ptrace.StateProcRunningG:       colorStateProcRunningG,
	ptrace.StateProcRunningBlocked: colorStateProcRunningBlocked,
}

// oklch specifies a color in Oklch.
// 100 >= l >= 0
// 0.37 >= c >= 0
// 360 > h >= 0
func oklch(l, c, h float32) color.Oklch {
	return color.Oklch{L: l / 100, C: c, H: h, A: 1}
}

func oklcha(l, c, h, a float32) color.Oklch {
	return color.Oklch{L: l / 100, C: c, H: h, A: a}
}

func oklchDelta(b color.Oklch, l, c, h float32) color.Oklch {
	b.L += l / 100
	b.C += c
	b.H += h
	if b.L < 0 {
		b.L = 0
	}
	if b.L > 1 {
		b.L = 1
	}
	if b.C < 0 {
		b.C = 0
	}
	return b
}

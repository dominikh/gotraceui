package main

import (
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/trace/ptrace"
)

var colors [colorLast]color.Oklch

func init() {
	// Our base lightness is 56.51, and our base chroma is 0.122
	const l = 58.51
	const c = 0.122
	const lStep1 = 15
	const lStep2 = 10

	colors[colorStateActive] = oklch(l, c, 143.74) // Manually chosen
	colors[colorStateStack] = oklchDelta(colors[colorStateActive], lStep1, -0.01, 0)
	colors[colorStateCPUSample] = oklchDelta(colors[colorStateStack], lStep2, -0.01, 0)

	colors[colorStateReady] = oklch(l, c, 206.35) // Manually chosen
	colors[colorStateInactive] = oklch(l, 0, 0)

	colors[colorStateUserRegion] = oklch(l+lStep1+lStep2, c, 331.18) // Manually chosen

	// Manually chosen. This is the rarest blocked state, so we darken it to have more range for the other states.
	colors[colorStateBlocked] = oklch(l-5, c, 23.89)
	colors[colorStateBlockedSyscall] = oklch(l, c, 23.89)
	colors[colorStateBlockedNet] = oklch(l+6, c-0.01, 23.89)
	colors[colorStateBlockedHappensBefore] = oklch(l+lStep2, c, 23.89)
	colors[colorStateBlockedGC] = oklch(l, c, 0) // a blend of colorStateGC and red

	colors[colorStateGC] = oklch(l, c, 302.36)
	colors[colorStateSTW] = oklch(l, c+0.072, 23.89) // STW is the most severe form of blocking, hence the increased chroma

	colors[colorTimelineLabel] = oklch(62.68, 0, 0)
	colors[colorTimelineBorder] = oklch(89.75, 0, 0)

	// 	// TODO(dh): find a nice color for this
	// We don't use the l constant for thse colors because they're independent from the span colors
	colors[colorSpanHighlightedPrimaryOutline] = oklch(70.71, 0.322, 328.36)
	colors[colorSpanHighlightedSecondaryOutline] = oklch(88.44, 0.27, 137.68)

	colors[colorStateMerged] = oklch(l+lStep1, c, 109.91) // Manually chosen, made brighter so it stands out in gradients

	colors[colorStateStuck] = oklch(0, 0, 0)
	colors[colorStateDone] = oklch(0, 0, 0)

	colors[colorStateUnknown] = oklch(96.8, 0.211, 109.77)
	colors[colorStatePlaceholderStackSpan] = oklch(92.59, 0.025, 106.88)
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
	colorStateSTW

	colorStateReady
	colorStateStuck
	colorStateMerged
	colorStateUserRegion
	colorStateStack
	colorStateCPUSample
	colorStateDone
	colorStatePlaceholderStackSpan

	colorStateLast

	colorTimelineLabel
	colorTimelineBorder

	colorSpanHighlightedPrimaryOutline
	colorSpanHighlightedSecondaryOutline

	colorLast
)

var stateColors = [256]colorIndex{
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
	ptrace.StateDone:                    colorStateDone,

	// per-P states
	ptrace.StateRunningG: colorStateActive,

	// per-M states
	ptrace.StateRunningP: colorStateActive,
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

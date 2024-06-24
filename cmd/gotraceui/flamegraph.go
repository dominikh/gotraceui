package main

import (
	"fmt"
	"hash/fnv"
	"math"
	"strings"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

type FlameGraphComponent struct {
	g     *ptrace.Goroutine
	fg    *theme.Future[*widget.FlameGraph]
	state theme.FlameGraphState
}

func (fc *FlameGraphComponent) Title() string {
	if fc.g == nil {
		return "Flame graph"
	} else {
		// OPT(dh): avoid the allocation
		return local.Sprintf("Flame graph for goroutine %d", fc.g.ID)
	}
}

func (tlc *FlameGraphComponent) Transition(theme.ComponentState) {
}

func (tlc *FlameGraphComponent) WantsTransition(gtx layout.Context) theme.ComponentState {
	return theme.ComponentStateNone
}

func NewFlameGraphComponent(win *theme.Window, tr *ptrace.Trace, g *ptrace.Goroutine) *FlameGraphComponent {
	return &FlameGraphComponent{
		g: g,
		fg: theme.NewFuture(win, func(cancelled <-chan struct{}) *widget.FlameGraph {
			// Compute the sample duration by dividing the active time of all Ps by the total number of samples. This should
			// closely approximate the inverse of the configured sampling rate.
			//
			// For the global flame graph, this is the most obvious choice. For goroutine flame graphs, we could arguably
			// compute per-G averages, so that a goroutine that ran for 1ms won't show a flame graph span that's 10ms long.
			// However, this wouldn't solve other, related problems, such as limiting the global flame graph to a portion of
			// time.
			//
			// In the end, samples happen on Ms, not Gs, and using an average is the simplest approximation that we can
			// explain. It also corresponds to what go tool pprof does, although it doesn't have the trouble of showing
			// graphs for individual goroutines.
			var (
				totalDuration  time.Duration
				sampleDuration time.Duration
			)
			for _, p := range tr.Processors {
				for _, s := range p.Spans {
					totalDuration += s.Duration()
				}
			}
			totalSamples := len(tr.CPUSamples)
			sampleDuration = time.Duration(math.Round(float64(totalDuration) / float64(totalSamples)))

			var fg widget.FlameGraph
			do := func(samples []ptrace.EventID) {
				for _, sample := range samples {
					pcs := tr.Stacks[tr.Event(sample).Stack()]
					var frames widget.FlamegraphSample
					for i := len(pcs) - 1; i >= 0; i-- {
						fn := tr.PCs[pcs[i]].Func
						frames = append(frames, widget.FlamegraphFrame{
							Name:     fn,
							Duration: sampleDuration,
						})
					}

					fg.AddSample(frames, "Running")
				}
			}
			if g == nil {
				for _, samples := range tr.CPUSamplesByP {
					do(samples)
				}
			} else {
				do(tr.CPUSamplesByG[g.ID])

				for _, span := range g.Spans {
					var root string

					switch span.State {
					case ptrace.StateInactive:
					case ptrace.StateActive:
					case ptrace.StateGCIdle:
					case ptrace.StateGCDedicated:
					case ptrace.StateGCFractional:
					case ptrace.StateBlocked:
						root = "blocked"
					case ptrace.StateBlockedSend:
						root = "send"
					case ptrace.StateBlockedRecv:
						root = "recv"
					case ptrace.StateBlockedSelect:
						root = "select"
					case ptrace.StateBlockedSync:
						root = "sync"
					case ptrace.StateBlockedSyncOnce:
						root = "sync.Once"
					case ptrace.StateBlockedSyncTriggeringGC:
						root = "triggering GC"
					case ptrace.StateBlockedCond:
						root = "sync.Cond"
					case ptrace.StateBlockedNet:
						root = "I/O"
					case ptrace.StateBlockedGC:
						root = "GC"
					case ptrace.StateBlockedSyscall:
						root = "blocking syscall"
					case ptrace.StateStuck:
					case ptrace.StateReady, ptrace.StateCreated, ptrace.StateWaitingPreempted:
						root = "ready"
					case ptrace.StateGCMarkAssist:
					case ptrace.StateGCSweep:
					default:
						panic(fmt.Sprintf("unhandled state %d", span.State))
					}

					if root != "" {
						var frames widget.FlamegraphSample
						if root != "ready" {
							pcs := tr.Stacks[tr.Event(span.StartEvent).Stack()]
							for i := len(pcs) - 1; i >= 0; i-- {
								fn := tr.PCs[pcs[i]].Func
								frames = append(frames, widget.FlamegraphFrame{
									Name:     fn,
									Duration: span.Duration(),
								})
							}
						}
						fg.AddSample(frames, root)
					}
				}
			}

			fg.Compute()
			return &fg
		}),
	}
}

func (fgc *FlameGraphComponent) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
	theme.Fill(win, gtx.Ops, win.Theme.Palette.Background)
	fg, ok := fgc.fg.Result()
	if !ok {
		// XXX
		return layout.Dimensions{}
	}
	fgs := theme.FlameGraph(fg, &fgc.state)
	fgs.Color = flameGraphColorFn
	return fgs.Layout(win, gtx)
}

func flameGraphColorFn(level, idx int, f *widget.FlamegraphFrame, hovered bool) color.Oklch {
	if hovered {
		return color.Oklch{
			L: 0.94,
			C: 0.222,
			H: 119,
			A: 1,
		}
	}

	// Mapping from index to color adjustments. The adjustments are sorted to maximize
	// the differences between neighboring spans.
	offsets := [...]float32{4, 9, 3, 8, 2, 7, 1, 6, 0, 5}
	adjustLight := func(mc color.Oklch) color.Oklch {
		var (
			oldMax = float32(len(offsets))
			newMin = float32(-0.05)
			newMax = float32(0.12)
		)

		v := offsets[idx%len(offsets)]
		delta := (v/oldMax)*(newMax-newMin) + newMin
		mc.L += delta
		if mc.L < 0 {
			mc.L = 0
		}
		if mc.L > 1 {
			mc.L = 1
		}

		return mc
	}

	if level == 0 {
		switch f.Name {
		case "Running":
			return adjustLight(colors[colorStateActive])
		case "blocked":
			return adjustLight(colors[colorStateBlocked])
		case "send", "recv", "select", "sync", "sync.Once", "sync.Cond":
			return adjustLight(colors[colorStateBlockedHappensBefore])
		case "GC", "triggering GC":
			return adjustLight(colors[colorStateBlockedGC])
		case "I/O":
			return adjustLight(colors[colorStateBlockedNet])
		case "blocking syscall":
			return adjustLight(colors[colorStateBlockedSyscall])
		case "ready":
			return adjustLight(colors[colorStateReady])
		}
	}

	cRuntime := color.Oklch{ // #cb77e1
		L: 0.699,
		C: 0.173,
		H: 318.89,
		A: 1.0,
	}
	cStdlib := color.Oklch{ // #ffb300
		L: 0.8179,
		C: 0.1705233575429752,
		H: 77.9481021312874,
		A: 1.0,
	}
	cMain := color.Oklch{ // #007d34
		L: 0.5167,
		C: 0.13481202013716384,
		H: 152.37558843925763,
		A: 1.0,
	}

	var c color.Oklch
	if strings.HasPrefix(f.Name, "runtime.") || strings.HasPrefix(f.Name, "runtime/") || !strings.Contains(f.Name, ".") {
		c = cRuntime
	} else {
		slashIdx := strings.Index(f.Name, "/")
		if strings.HasPrefix(f.Name, "main.") {
			c = cMain
		} else if slashIdx == -1 {
			// No slash means it has to be in the standard library
			c = cStdlib
		} else if !strings.Contains(f.Name[:slashIdx], ".") {
			// No dot in the first path element means it has to be in the standard library
			c = cStdlib
		} else {
			last := strings.LastIndex(f.Name, "/")
			dot := strings.Index(f.Name[last:], ".")
			pkg := f.Name[:last+dot]

			// Select color by hashing the import path
			h := fnv.New64()
			// Note that Go 1.22 doesn't allocate for this conversion from string to []byte.
			h.Write([]byte(pkg))
			sum := h.Sum64()

			// For this combination of lightness and chroma, all hues are representable in sRGB, with enough
			// room to adjust the lightness in both directions for varying shades.
			const (
				baseLightness = float32(0.699)
				baseChroma    = float32(0.103)
				hueStep       = float32(20)
			)

			hue := hueStep * float32(sum%uint64(360.0/hueStep))

			c = color.Oklch{
				L: baseLightness,
				C: baseChroma,
				H: hue,
				A: 1.0,
			}
		}
	}

	return adjustLight(c)
}

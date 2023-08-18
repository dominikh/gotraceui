package main

import (
	"hash/fnv"
	"math"
	"reflect"
	"strings"
	"time"
	"unsafe"

	"gioui.org/app"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/paint"
	mycolor "honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"
)

type FlameGraphWindow struct {
	fg *theme.Future[*widget.FlameGraph]
}

func (fgwin *FlameGraphWindow) Run(win *app.Window, trace *ptrace.Trace, g *ptrace.Goroutine) error {
	tWin := theme.NewWindow(win)
	fgwin.fg = theme.NewFuture(tWin, func(cancelled <-chan struct{}) *widget.FlameGraph {
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
			totalSamples   uint64
			sampleDuration time.Duration
		)
		for _, p := range trace.Processors {
			for _, s := range p.Spans {
				totalDuration += s.Duration()
			}
		}
		for _, samples := range trace.CPUSamples {
			totalSamples += uint64(len(samples))
		}
		sampleDuration = time.Duration(math.Round(float64(totalDuration) / float64(totalSamples)))

		var fg widget.FlameGraph
		do := func(samples []ptrace.EventID) {
			for _, sample := range samples {
				stack := trace.Stacks[trace.Event(sample).StkID]
				var frames widget.FlamegraphSample
				for i := len(stack) - 1; i >= 0; i-- {
					fn := trace.PCs[stack[i]].Fn
					frames = append(frames, widget.FlamegraphFrame{
						Name:     fn,
						Duration: sampleDuration,
					})
				}

				fg.AddSample(frames, "Running")
			}
		}
		if g == nil {
			for _, samples := range trace.CPUSamples {
				do(samples)
			}
		} else {
			do(trace.CPUSamples[g.ID])
		}

		fg.Compute()
		return &fg
	})

	var (
		ops     op.Ops
		fgState theme.FlameGraphState
	)
	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			tWin.Render(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				paint.Fill(gtx.Ops, tWin.Theme.Palette.Background)
				fg, ok := fgwin.fg.Result()
				if !ok {
					// XXX
					return layout.Dimensions{}
				}
				fgs := theme.FlameGraph(fg, &fgState)
				fgs.Color = flameGraphColorFn
				return fgs.Layout(win, gtx)
			})

			ev.Frame(&ops)
		}
	}

	return nil
}

func flameGraphColorFn(level, idx int, f *widget.FlamegraphFrame, hovered bool) mycolor.Oklch {
	// For this combination of lightness and chroma, all hues are representable in sRGB, with enough
	// room to adjust the lightness in both directions for varying shades.
	baseLightness := float32(0.699)
	baseChroma := float32(0.103)
	hueStep := float32(20)

	if hovered {
		return mycolor.Oklch{
			L:     0.94,
			C:     0.222,
			H:     119,
			Alpha: 1,
		}
	}

	cRuntime := mycolor.Oklch{ // #b400d7
		L:     0.5639,
		C:     0.272,
		H:     318.89,
		Alpha: 1.0,
	}
	cStdlib := mycolor.Oklch{ // #ffb300
		L:     0.8179,
		C:     0.1705233575429752,
		H:     77.9481021312874,
		Alpha: 1.0,
	}
	cMain := mycolor.Oklch{ // #007d34
		L:     0.5167,
		C:     0.13481202013716384,
		H:     152.37558843925763,
		Alpha: 1.0,
	}

	var c mycolor.Oklch
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
			var pkg string
			if strings.HasPrefix(f.Name, "main.") {
				// XXX this is impossible, right?
				pkg = "main"
			} else {
				last := strings.LastIndex(f.Name, "/")
				dot := strings.Index(f.Name[last:], ".")
				pkg = f.Name[:last+dot]
			}

			// Select color by hashing the import path
			h := fnv.New64()
			key := unsafe.Slice((*byte)(unsafe.Pointer((*reflect.StringHeader)(unsafe.Pointer(&pkg)).Data)), len(pkg))
			h.Write(key)
			sum := h.Sum64()

			hue := hueStep * float32(sum%uint64(360.0/hueStep))

			c = mycolor.Oklch{
				L:     baseLightness,
				C:     baseChroma,
				H:     hue,
				Alpha: 1.0,
			}
		}
	}

	mc := c
	mod := 10
	oldMax := float32(mod)
	newMin := float32(-0.05)
	newMax := float32(0.12)

	// Mapping from index to color adjustments. The adjustments are sorted to maximize
	// the differences between neighboring spans.
	offsets := [...]float32{4, 9, 3, 8, 2, 7, 1, 6, 0, 5}
	v := offsets[idx%mod]

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

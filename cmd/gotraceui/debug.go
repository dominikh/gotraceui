//go:build debug

package main

import (
	"context"
	"math"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/text"
)

const debug = true

func (g *debugGraph) addValue(ts time.Time, val float64) {
	g.mu.Lock()
	defer g.mu.Unlock()

	// We keep at least one value from outside the window so that [value 1, gap, gap, value 2] won't draw as
	// [value 2] after some time has passed, which matters in stickyLastValue mode, as suddenly part of the
	// graph would disappear.
	for len(g.values) > 1 && ts.Sub(g.values[1].when) > g.width {
		// OPT(dh): use a ring buffer instead of moving memory
		copy(g.values, g.values[1:])
		g.values = g.values[:len(g.values)-1]
	}
	g.values = append(g.values, struct {
		when time.Time
		val  float64
	}{ts, val})
}

func (g *debugGraph) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.debugGraph.Layout").End()

	// OPT(dh): this function allocates a fair amount. It's only debug code, but even then we want to keep memory usage
	// low, so as not to affect debugging of other memory usage issues. The expensive part is drawing all that text that
	// is constantly changing.

	defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
	theme.Fill(win, gtx.Ops, g.background)

	g.mu.Lock()
	values := g.values
	g.mu.Unlock()

	gtx.Constraints.Max = gtx.Constraints.Min
	return layout.UniformInset(5).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min.Y = 0
				return widget.Label{Alignment: text.Middle}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, g.title, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
			}),

			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				if len(values) == 0 {
					return layout.Dimensions{Size: gtx.Constraints.Min}
				}

				// Remove values that are outside the rendered time window. This can happen if addValue hasn't been
				// called in a while, and is relevant when we draw with stickyLastValue, because we want to compute the
				// min/max based on the visible points only.
				//
				// We keep at least one value from outside the window so that [value 1, gap, gap, value 2] won't draw as
				// [value 2] after some time has passed, which matters in stickyLastValue mode, as suddenly part of the
				// graph would disappear.
				for len(values) > 1 && gtx.Now.Sub(values[1].when) > g.width {
					values = values[1:]
				}

				min := values[0].val
				max := values[0].val
				cur := values[len(values)-1].val

				for _, e := range values[1:] {
					if e.val < min {
						min = e.val
					}
					if e.val > max {
						max = e.val
					}
				}

				widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, local.Sprintf("Min: %f\nMax: %f\nCurrent: %f", min, max, cur), win.ColorMaterial(gtx, win.Theme.Palette.Foreground))

				if g.fixedZero {
					min = 0
				}

				width := gtx.Constraints.Min.X
				height := gtx.Constraints.Min.Y

				xStep := float64(width) / float64(g.width) // px / ns
				yStep := float64(height) / (max - min)     // px / 1
				if min == max {
					yStep = 1
				}

				var p clip.Path
				p.Begin(gtx.Ops)
				var prevY float32
				for i, e := range values {
					x := float32(math.Max(0, float64(width)-float64(gtx.Now.Sub(e.when))*xStep))
					y := float32(height) - float32(yStep*(e.val-min))
					if i != 0 && (g.stickyLastValue || e.when.Sub(values[i-1].when) < 500*time.Millisecond) {
						// Draw a staircase graph. It doesn't look as pretty, but it shows more accurate data,
						// especially because we don't indicate individual points. A staircase graph makes it clear
						// whether we interpolated cleanly between two values, or just jumped between them.
						p.LineTo(f32.Pt(x, prevY))
						if y != prevY {
							// Gio seems to have a bug where drawing this line when y == prevY causes a thin, stray line
							// to shoot straight down.
							p.LineTo(f32.Pt(x, y))
						}
					} else {
						p.MoveTo(f32.Pt(x, y))
					}
					prevY = y
				}

				if g.stickyLastValue {
					e := values[len(values)-1]
					x := float32(width)
					y := float32(height) - float32(yStep*(e.val-min))
					p.LineTo(f32.Pt(x, y))
				}

				o := clip.Stroke{
					Path:  p.End(),
					Width: float32(gtx.Dp(1)),
				}.Op()
				theme.FillShape(win, gtx.Ops, oklch(0, 0, 0), o)

				return layout.Dimensions{Size: gtx.Constraints.Min}
			}),
		)
	})
}

func NewDebugWindow() *DebugWindow {
	const timeWindow = 20 * time.Second
	dwin := &DebugWindow{}

	bgs := []color.Oklch{
		oklch(95, 0.02, 3.81),
		oklch(95, 0.02, 196.89),
		oklch(95, 0.02, 106.77),
		oklch(95, 0.02, 145.37),
		oklch(95, 0.02, 231.58),
		oklch(95, 0, 0),
		oklch(95, 0.02, 3.81),
	}

	dwin.cvStart = debugGraph{
		title:           "Canvas start",
		width:           timeWindow,
		background:      bgs[0],
		stickyLastValue: true,
	}
	dwin.cvEnd = debugGraph{
		title:           "Canvas end",
		width:           timeWindow,
		background:      bgs[1],
		stickyLastValue: true,
	}
	dwin.cvY = debugGraph{
		title:           "Canvas Y",
		width:           timeWindow,
		background:      bgs[2],
		stickyLastValue: true,
	}
	dwin.cvPxPerNs = debugGraph{
		title:           "Canvas pxPerNs (px/ns)",
		width:           timeWindow,
		background:      bgs[3],
		stickyLastValue: true,
	}
	dwin.animationProgress = debugGraph{
		title:      "Animation Δt",
		width:      timeWindow,
		background: bgs[4],
	}
	dwin.animationRatio = debugGraph{
		title:      "Animation ease(Δt)",
		width:      timeWindow,
		background: bgs[5],
	}
	dwin.frametimes = debugGraph{
		title:      "Frametime (ms)",
		width:      timeWindow,
		background: bgs[6],
		fixedZero:  true,
	}

	return dwin
}

func (dwin *DebugWindow) Run(win *app.Window) error {
	var ops op.Ops
	twin := theme.NewWindow(win)
	for {
		e := win.NextEvent()
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			twin.Layout(&ops, ev, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
				themeWidget := func(fn func(win *theme.Window, gtx layout.Context) layout.Dimensions) layout.Widget {
					return func(gtx layout.Context) layout.Dimensions { return fn(twin, gtx) }
				}

				return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
					layout.Flexed(1, themeWidget(dwin.cvStart.Layout)),
					layout.Flexed(1, themeWidget(dwin.cvEnd.Layout)),
					layout.Flexed(1, themeWidget(dwin.cvY.Layout)),
					layout.Flexed(1, themeWidget(dwin.cvPxPerNs.Layout)),
					layout.Flexed(1, themeWidget(dwin.animationProgress.Layout)),
					layout.Flexed(1, themeWidget(dwin.animationRatio.Layout)),
					layout.Flexed(1, themeWidget(dwin.frametimes.Layout)),
				)
			})
			op.InvalidateOp{}.Add(&ops)
			ev.Frame(&ops)
		}
	}

	return nil
}

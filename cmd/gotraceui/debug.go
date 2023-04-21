//go:build debug

package main

import (
	"image/color"
	"math"
	"time"

	ourfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
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

func (g *debugGraph) Layout(gtx layout.Context, th *theme.Theme) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.debugGraph.Layout").End()

	// OPT(dh): this function allocates a fair amount. It's only debug code, but even then we want to keep memory usage
	// low, so as not to affect debugging of other memory usage issues. The expensive part is drawing all that text that
	// is constantly changing.

	defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
	paint.Fill(gtx.Ops, g.background)

	g.mu.Lock()
	values := g.values
	g.mu.Unlock()

	gtx.Constraints.Max = gtx.Constraints.Min
	return layout.UniformInset(5).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min.Y = 0
				paint.ColorOp{Color: rgba(0x000000FF)}.Add(gtx.Ops)
				return widget.Label{Alignment: text.Middle}.Layout(gtx, th.Shaper, font.Font{}, 12, g.title, widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
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

				widget.Label{}.Layout(gtx, th.Shaper, font.Font{}, 12, local.Sprintf("Min: %f\nMax: %f\nCurrent: %f", min, max, cur), widget.ColorTextMaterial(gtx, rgba(0x000000FF)))

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
				paint.FillShape(gtx.Ops, color.NRGBA{0, 0, 0, 0xFF}, o)

				return layout.Dimensions{Size: gtx.Constraints.Min}
			}),
		)
	})
}

func NewDebugWindow() *DebugWindow {
	const timeWindow = 20 * time.Second
	dwin := &DebugWindow{}

	bgs := []color.NRGBA{
		rgba(0xFFEBEFFF),
		rgba(0xEFFFFFFF),
		rgba(0xFFFFEFFF),
		rgba(0xEFFFEFFF),
		rgba(0xC6EBFFFF),
		rgba(0xEFEFEFFF),
		rgba(0xFFEBEFFF),
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
	for e := range win.Events() {
		switch ev := e.(type) {
		case system.DestroyEvent:
			return ev.Err
		case system.FrameEvent:
			gtx := layout.NewContext(&ops, ev)
			// gtx.Constraints.Min = image.Point{}

			th := theme.NewTheme(ourfont.Collection())

			themeWidget := func(fn func(gtx layout.Context, th *theme.Theme) layout.Dimensions) layout.Widget {
				return func(gtx layout.Context) layout.Dimensions { return fn(gtx, th) }
			}

			layout.Flex{Axis: layout.Vertical}.Layout(gtx,
				layout.Flexed(1, themeWidget(dwin.cvStart.Layout)),
				layout.Flexed(1, themeWidget(dwin.cvEnd.Layout)),
				layout.Flexed(1, themeWidget(dwin.cvY.Layout)),
				layout.Flexed(1, themeWidget(dwin.cvPxPerNs.Layout)),
				layout.Flexed(1, themeWidget(dwin.animationProgress.Layout)),
				layout.Flexed(1, themeWidget(dwin.animationRatio.Layout)),
				layout.Flexed(1, themeWidget(dwin.frametimes.Layout)),
			)

			op.InvalidateOp{}.Add(gtx.Ops)
			ev.Frame(gtx.Ops)
		}
	}

	return nil
}

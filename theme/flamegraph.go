package theme

import (
	"context"
	"fmt"
	"image/color"
	"math"
	rtrace "runtime/trace"
	"time"

	"honnef.co/go/gotraceui/clip"
	mycolor "honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	myslices "honnef.co/go/gotraceui/slices"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"golang.org/x/exp/slices"
)

// TODO(dh): would it make sense to support collapsing self-recursive calls?

type fgSpanLocation struct {
	frame *widget.FlamegraphFrame
	x     Unit
	level int
	width Unit
}

type fgZoom struct {
	offsetX     Unit
	offsetLevel float32
	scale       Unit
}

type Unit float32

type FlameGraphState struct {
	hover   gesture.Hover
	click   gesture.Click
	tooltip string

	zoom        fgZoom
	zoomHistory []fgZoom

	animateTo struct {
		animating bool

		initial fgZoom
		target  fgZoom

		startedAt time.Time
	}

	prevFrame struct {
		zoom        fgZoom
		hovered     fgSpanLocation
		constraints layout.Constraints

		ops  mem.ReusableOps
		call op.CallOp
		dims layout.Dimensions
	}

	// scratch space reused between frames
	pathsByColor map[mycolor.Oklch]*clip.Path
	paths        mem.BucketSlice[clip.Path]
	ops          mem.BucketSlice[op.Ops]
	indices      []int
}

type FlameGraphStyle struct {
	State      *widget.FlameGraph
	StyleState *FlameGraphState
	Color      func(level, idx int, f *widget.FlamegraphFrame, hovered bool) mycolor.Oklch
}

func FlameGraph(state *widget.FlameGraph, sstate *FlameGraphState) FlameGraphStyle {
	return FlameGraphStyle{
		State:      state,
		StyleState: sstate,
		Color: func(level, idx int, f *widget.FlamegraphFrame, hovered bool) mycolor.Oklch {
			return mycolor.Oklch{}
		},
	}
}

func easeBezier(t float64) float64 {
	return t * t * (3.0 - 2.0*t)
}

func (fg FlameGraphStyle) Layout(win *Window, gtx layout.Context) (dims layout.Dimensions) {
	// Note that this function considers Y == 0 to be the bottom, not the top. Pointer coordinates get flipped before
	// use. Constraints and clips don't, because they aren't part of any calculations, except for final display.
	//
	// The span x and width are primarily tracked in the unit coordinate system, because these depend on the window width.
	// The span y and height are tracked in pixels, because these don't depend on the window size and shouldn't be subject to rounding errors.

	defer rtrace.StartRegion(context.Background(), "theme.Flamegraph.Layout").End()

	const (
		animateLength = 500 * time.Millisecond
		// XXX figure out decent height. at least be high enough for the chosen font
		height       int     = 30
		rowSpacingDp unit.Dp = 1
		rowPaddingDp unit.Dp = 1
		radiusDp     unit.Dp = 2
		debugCaching         = false
	)

	var (
		toReal = func(from Unit) float32 {
			return float32(from) * float32(gtx.Constraints.Min.X)
		}

		offset = func(off f32.Point) op.TransformOp {
			return op.Affine(f32.Affine2D{}.Offset(off))
		}

		totalDuration = func() time.Duration {
			var total time.Duration
			for _, root := range fg.State.Samples {
				total += root.Duration
			}
			return total
		}()
		// fgPerNs is strictly speaking a Unit, but we only use it to scale sample counts, which are
		// float64.
		fgPerNs     = 1.0 / float64(totalDuration)
		clickedSpan = fgSpanLocation{nil, -1, -1, -1}
		hoveredSpan = fgSpanLocation{nil, -1, -1, -1}
		ptPx        = fg.StyleState.hover.Pointer()
		levelHeight = float32(height + gtx.Dp(rowSpacingDp))

		flipY = func(y float32) float32 {
			return float32(gtx.Constraints.Min.Y) - y
		}

		// Calling gtx.Dp has non-negligible cost, so lift it ouside of the loop. We also convert values that
		// are always used as floats.
		radius     = float32(gtx.Dp(radiusDp))
		rowPadding = float32(gtx.Dp(rowPaddingDp))
	)

	if fg.StyleState.zoom.scale <= 0 {
		fg.StyleState.zoom.scale = 1
	}

	if fg.StyleState.animateTo.animating {
		dt := gtx.Now.Sub(fg.StyleState.animateTo.startedAt)
		if dt >= animateLength {
			fg.StyleState.animateTo.animating = false
			fg.StyleState.zoom = fg.StyleState.animateTo.target

			// TODO hook up debug window
		} else {
			timeRatio := float64(dt) / float64(animateLength)

			initial := fg.StyleState.animateTo.initial
			target := fg.StyleState.animateTo.target

			r := Unit(easeBezier(timeRatio))

			fg.StyleState.zoom.offsetX = initial.offsetX + (target.offsetX-initial.offsetX)*r
			fg.StyleState.zoom.offsetLevel = initial.offsetLevel + (target.offsetLevel-initial.offsetLevel)*float32(r)
			fg.StyleState.zoom.scale = initial.scale + (target.scale-initial.scale)*r

			op.InvalidateOp{}.Add(gtx.Ops)
		}
	}

	defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
	key.InputOp{Tag: fg.StyleState, Keys: "Short-Z"}.Add(gtx.Ops)
	fg.StyleState.hover.Update(gtx.Queue)

	var trackClicked bool
	for _, ev := range fg.StyleState.click.Events(gtx.Queue) {
		if ev.Type == gesture.TypeClick &&
			ev.Button == pointer.ButtonPrimary &&
			ev.Modifiers == key.ModShortcut {
			trackClicked = true
		}
	}

	for _, ev := range gtx.Events(fg.StyleState) {
		if ev, ok := ev.(key.Event); ok && ev.State == key.Press && ev.Modifiers == key.ModShortcut && ev.Name == "Z" {
			var (
				prev fgZoom
				ok   bool
			)
			prev, fg.StyleState.zoomHistory, ok = myslices.Pop(fg.StyleState.zoomHistory)
			if ok {
				fg.StyleState.animateTo.animating = true
				fg.StyleState.animateTo.startedAt = gtx.Now
				fg.StyleState.animateTo.initial = fg.StyleState.zoom
				fg.StyleState.animateTo.target = prev
			}
		}
	}

	// OPT(dh): We could avoid hit detection if the pointer has only moved within the constraints of the currently
	// hovered span. However, hit detection is already so fast that the additional state tracking and branching doesn't
	// seem worth it.
	{
		ptPx.Y = flipY(ptPx.Y)
		targetLevel := int((ptPx.Y + fg.StyleState.zoom.offsetLevel*levelHeight) / levelHeight)

		var do func(level int, startX Unit, samples []widget.FlamegraphFrame)
		do = func(level int, startX Unit, samples []widget.FlamegraphFrame) {
			x := startX
			for i := range samples {
				scaledX := x*fg.StyleState.zoom.scale + fg.StyleState.zoom.offsetX
				if scaledX > 1 {
					break
				}

				var (
					frame  = &samples[i]
					width  = Unit(float64(frame.Duration) * fgPerNs)
					pxSize = f32.Pt(
						toReal(width*fg.StyleState.zoom.scale),
						float32(height),
					)
					pxSpan = clip.FRect{
						Min: f32.Pt(toReal(scaledX), (float32(level)-fg.StyleState.zoom.offsetLevel)*levelHeight),
					}
				)
				pxSpan.Max = pxSpan.Min.Add(pxSize)

				if level == targetLevel {
					if pxSpan.Max.X >= 0 {
						if pxSpan.Contains(ptPx) {
							hoveredSpan = fgSpanLocation{frame, x, level, width}
							if trackClicked {
								clickedSpan = hoveredSpan
							}
						}
					}

					if debugCaching {
						dspSpan := pxSpan
						dspSpan.Min.Y = flipY(pxSpan.Min.Y)
						dspSpan.Max.Y = flipY(pxSpan.Max.Y)
						paint.FillShape(gtx.Ops, color.NRGBA{0, 0, 0, 0xFF}, dspSpan.Op(gtx.Ops))
					}
				}
				if ptPx.X <= pxSpan.Max.X && level < targetLevel {
					// If the cursor is to the right of this span, then it will also be to the right of all children.
					// If we're on the target level, then we're done.
					do(level+1, x, frame.Children)
				}
				if ptPx.X < pxSpan.Max.X {
					// If the cursor is not to the right of this span, then it cannot be on any other spans on the same
					// level.
					break
				}

				x += width
			}
		}

		do(0, 0, fg.State.Samples)
	}

	if clickedSpan.x == -1 &&
		fg.StyleState.prevFrame.hovered == hoveredSpan &&
		fg.StyleState.prevFrame.constraints == gtx.Constraints &&
		fg.StyleState.prevFrame.zoom == fg.StyleState.zoom {
		fg.StyleState.prevFrame.call.Add(gtx.Ops)
		if l := fg.StyleState.tooltip; l != "" && fg.StyleState.prevFrame.hovered.frame != nil {
			win.SetTooltip(Tooltip(win.Theme, l).Layout)
		}
		return fg.StyleState.prevFrame.dims
	}

	{
		// OPT(dh): Instead of redrawing the whole graph because the hovered span changed, we could cache the base version
		// of the graph, then overdraw the hovered span. This works as long as hovering a span only changes its contents,
		// not its dimensions or location.

		origOps := gtx.Ops
		gtx.Ops = fg.StyleState.prevFrame.ops.Get()
		macro := op.Record(gtx.Ops)
		defer func() {
			call := macro.Stop()
			call.Add(origOps)
			fg.StyleState.prevFrame.constraints = gtx.Constraints
			fg.StyleState.prevFrame.zoom = fg.StyleState.zoom
			fg.StyleState.prevFrame.hovered = hoveredSpan
			fg.StyleState.prevFrame.call = call
			fg.StyleState.prevFrame.dims = dims
		}()

		// Only display labels in spans that can fit more than just the ellipsis and 1-2 characters. This assumes that there
		// are no labels that are shorter than that, which will be true for all real Go programs.
		pxMinLabelWidth := float32(win.TextDimensions(gtx, widget.Label{}, font.Font{}, 12, " … ").Size.X) + 2*rowPadding

		fg.StyleState.hover.Add(gtx.Ops)
		fg.StyleState.click.Add(gtx.Ops)

		// We have an order of magnitude more spans than colors, which is why we batch draw operations by color.
		fg.StyleState.paths.Reset()
		fg.StyleState.ops.Reset()
		for i := range fg.StyleState.indices {
			fg.StyleState.indices[i] = 0
		}
		fg.StyleState.indices = fg.StyleState.indices[:0]
		if fg.StyleState.pathsByColor == nil {
			fg.StyleState.pathsByColor = map[mycolor.Oklch]*clip.Path{}
		} else {
			for k := range fg.StyleState.pathsByColor {
				delete(fg.StyleState.pathsByColor, k)
			}
		}
		getPath := func(c mycolor.Oklch) *clip.Path {
			p := fg.StyleState.pathsByColor[c]
			if p == nil {
				p = fg.StyleState.paths.Append(clip.Path{})
				fg.StyleState.pathsByColor[c] = p

				ops := fg.StyleState.ops.Grow()
				ops.Reset()
				p.Begin(ops)
			}

			return p
		}

		// Indices tracks the intra-row span index per level. This is useful for color functions that want to discern
		// neighboring spans.
		black := widget.ColorTextMaterial(gtx, rgba(0x000000FF))

		labelsMacro := op.Record(gtx.Ops)

		var do func(level int, startX Unit, samples []widget.FlamegraphFrame, draw bool)
		do = func(level int, startX Unit, samples []widget.FlamegraphFrame, draw bool) {
			if len(fg.StyleState.indices) < level+1 {
				fg.StyleState.indices = slices.Grow(fg.StyleState.indices, level+1-len(fg.StyleState.indices))[:level+1]
			}

			x := startX
			idx := &fg.StyleState.indices[level]
			yMin := (float32(level) - fg.StyleState.zoom.offsetLevel) * levelHeight
			for i := range samples {
				scaledX := x*fg.StyleState.zoom.scale + fg.StyleState.zoom.offsetX
				*idx++

				if scaledX > 1 {
					break
				}

				var (
					frame  = &samples[i]
					width  = Unit(float64(frame.Duration) * fgPerNs)
					pxSize = f32.Pt(
						toReal(width*fg.StyleState.zoom.scale),
						float32(height),
					)
					pxSpan = clip.FRect{
						Min: f32.Pt(toReal(scaledX), yMin),
					}
				)
				pxSpan.Max = pxSpan.Min.Add(pxSize)

				if draw && pxSpan.Max.X >= 0 {
					// TODO don't render off-screen in the Y direction

					hovered := hoveredSpan.frame == frame
					mc := fg.Color(level, *idx, frame, hovered)
					p := getPath(mc)

					dspSpan := pxSpan
					dspSpan.Min.Y = flipY(pxSpan.Min.Y)
					dspSpan.Max.Y = flipY(pxSpan.Max.Y)

					if pxSize.X >= radius {
						shape := clip.UniformFRRect(dspSpan, radius)
						shape.IntoPath(p)
					} else {
						dspSpan.IntoPath(p)
					}

					if pxSize.X >= pxMinLabelWidth {
						// XXX center label in the Y direction, too. right now it only looks right by chance
						stack := offset(dspSpan.Min.Add(f32.Pt(rowPadding, -float32(height)))).Push(gtx.Ops)
						gtx := gtx
						gtx.Constraints.Max.X = int(math.Ceil(float64(pxSize.X - 2*rowPadding)))
						gtx.Constraints.Min.X = gtx.Constraints.Max.X

						var f font.Font
						if hovered {
							f.Weight = font.Bold
						}
						widget.Label{MaxLines: 1, Alignment: text.Middle}.Layout(gtx, win.Theme.Shaper, f, 12, frame.Name, black)
						stack.Pop()
					}
				}

				// Don't draw children if this span is already tiny. This allows one level of tiny spans — the
				// children of a large span, avoiding sudden gaps, but not wasting time on drawing
				// indiscernible spans. We still have to actually recurse through the children, however, to
				// keep a stable ordering of spans, which is of importance for the color function, which uses
				// the span's absolute index to choose a color.
				do(level+1, x, frame.Children, pxSize.X > 1)
				x += width
			}
		}

		do(0, 0, fg.State.Samples, true)

		c := labelsMacro.Stop()
		for c, p := range fg.StyleState.pathsByColor {
			cc := win.ConvertColor(c)
			if debugCaching {
				cc.A = 50
			}
			paint.FillShape(gtx.Ops, cc, clip.Outline{Path: p.End()}.Op())
		}
		c.Add(gtx.Ops)

		if clickedSpan.x != -1 {
			fg.StyleState.zoomHistory = append(fg.StyleState.zoomHistory, fg.StyleState.zoom)

			scale := 1.0 / clickedSpan.width
			fg.StyleState.animateTo.animating = true
			fg.StyleState.animateTo.initial = fg.StyleState.zoom
			fg.StyleState.animateTo.target.offsetX = -(clickedSpan.x * scale)
			fg.StyleState.animateTo.target.offsetLevel = float32(clickedSpan.level)
			fg.StyleState.animateTo.target.scale = Unit(scale)
			fg.StyleState.animateTo.startedAt = gtx.Now
		}
	}

	if hoveredSpan.frame != nil {
		// TODO(dh): use formatting in tooltip, like bold labels
		self := hoveredSpan.frame.Duration
		for _, child := range hoveredSpan.frame.Children {
			self -= child.Duration
		}
		l := fmt.Sprintf("Name: %s\nStack depth: %d\nDuration: %s (%s self)\nImmediate children: %d",
			hoveredSpan.frame.Name, hoveredSpan.level, roundDuration(hoveredSpan.frame.Duration), roundDuration(self), len(hoveredSpan.frame.Children))
		fg.StyleState.tooltip = l
		win.SetTooltip(Tooltip(win.Theme, l).Layout)
	}

	return layout.Dimensions{Size: gtx.Constraints.Min}
}

func roundDuration(d time.Duration) time.Duration {
	switch {
	case d < time.Millisecond:
		return d
	case d < time.Second:
		return d.Round(time.Microsecond)
	default:
		return d.Round(time.Millisecond)
	}
}

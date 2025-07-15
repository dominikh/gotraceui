package theme

import (
	"context"
	"fmt"
	"math"
	rtrace "runtime/trace"
	"slices"
	"strings"
	"time"
	"unicode/utf8"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/color"
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
	"gioui.org/text"
	"gioui.org/unit"
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
	// The root that was selected by clicking on it. Nil if we're fully zoomed out
	root *widget.FlamegraphFrame
}

func (z fgZoom) Lerp(end fgZoom, r float64) fgZoom {
	return fgZoom{
		offsetX:     Lerp(z.offsetX, end.offsetX, r),
		offsetLevel: Lerp(z.offsetLevel, end.offsetLevel, r),
		scale:       Lerp(z.scale, end.scale, r),
		root:        end.root,
	}
}

type Unit float32

type FlameGraphState struct {
	hover   gesture.Hover
	click   gesture.Click
	tooltip string

	zoom        fgZoom
	zoomHistory []fgZoom

	animate Animation[fgZoom]

	prevFrame struct {
		zoom        fgZoom
		hovered     fgSpanLocation
		constraints layout.Constraints

		ops  mem.ReusableOps
		call op.CallOp
		dims layout.Dimensions
	}

	// scratch space reused between frames
	pathsByColor map[color.Oklch]*clip.Path
	paths        mem.BucketSlice[clip.Path]
	ops          mem.BucketSlice[op.Ops]
	indices      []int
}

type FlameGraphStyle struct {
	State      *widget.FlameGraph
	StyleState *FlameGraphState
	Color      func(level, idx int, f *widget.FlamegraphFrame, hovered bool) color.Oklch
}

func FlameGraph(state *widget.FlameGraph, sstate *FlameGraphState) FlameGraphStyle {
	return FlameGraphStyle{
		State:      state,
		StyleState: sstate,
		Color: func(level, idx int, f *widget.FlamegraphFrame, hovered bool) color.Oklch {
			return color.Oklch{}
		},
	}
}

func (fg FlameGraphStyle) Layout(win *Window, gtx layout.Context) (dims layout.Dimensions) {
	// Note that this function considers Y == 0 to be the bottom, not the top. Pointer coordinates get flipped before
	// use. Constraints and clips don't, because they aren't part of any calculations, except for final display.
	//
	// The span x and width are primarily tracked in the unit coordinate system, because these depend on the window width.
	// The span y and height are tracked in pixels, because these don't depend on the window size and shouldn't be subject to rounding errors.

	defer rtrace.StartRegion(context.Background(), "theme.Flamegraph.Layout").End()

	const (
		animateLength         = 500 * time.Millisecond
		rowSpacingDp  unit.Dp = 1
		spanPaddingDp unit.Dp = 1
		radiusDp      unit.Dp = 2
		debugCaching          = false
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
		fgPerNs        = 1.0 / float64(totalDuration)
		clickedSpan    = fgSpanLocation{nil, -1, -1, -1}
		hoveredSpan    = fgSpanLocation{nil, -1, -1, -1}
		hoveredWidthPx float32
		ptPx           = f32.Pt(-1, -1)

		flipY = func(y float32) float32 {
			return float32(gtx.Constraints.Min.Y) - y
		}

		// Calling gtx.Dp has non-negligible cost, so lift it ouside of the loop. We also convert values that
		// are always used as floats.
		radius      = float32(gtx.Dp(radiusDp))
		spanPadding = float32(gtx.Dp(spanPaddingDp))

		height      = win.TextDimensions(gtx, widget.Label{}, font.Font{}, 12, "").Size.Y + 2*int(spanPadding)
		levelHeight = float32(height + gtx.Dp(rowSpacingDp))
	)

	if fg.StyleState.hover.Update(gtx.Queue) {
		ptPx = fg.StyleState.hover.Pointer()
	}

	if fg.StyleState.zoom.scale <= 0 {
		fg.StyleState.zoom.scale = 1
	}

	if !fg.StyleState.animate.Done() {
		fg.StyleState.zoom = fg.StyleState.animate.Value(gtx)
	}

	defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
	key.InputOp{Tag: fg.StyleState, Keys: "Short-Z"}.Add(gtx.Ops)
	fg.StyleState.hover.Update(gtx.Queue)

	var trackClicked bool
	for _, ev := range fg.StyleState.click.Update(gtx.Queue) {
		if ev.Kind == gesture.KindClick &&
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
				fg.StyleState.animate.Start(gtx, fg.StyleState.zoom, prev, animateLength, EaseBezier)
			}
		}
	}

	// OPT(dh): We could avoid hit detection if the pointer has only moved within the constraints of the currently
	// hovered span. However, hit detection is already so fast that the additional state tracking and branching doesn't
	// seem worth it.
	{
		ptPx.Y = flipY(ptPx.Y)
		targetLevel := int((ptPx.Y + fg.StyleState.zoom.offsetLevel*levelHeight) / levelHeight)

		var do func(level int, startX Unit, samples []*widget.FlamegraphFrame)
		do = func(level int, startX Unit, samples []*widget.FlamegraphFrame) {
			x := startX
			for i := range samples {
				scaledX := x*fg.StyleState.zoom.scale + fg.StyleState.zoom.offsetX
				if scaledX > 1 {
					break
				}

				var (
					frame  = samples[i]
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
						FillShape(win, gtx.Ops, oklch(0, 0, 0), dspSpan.Op(gtx.Ops))
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
		pxMinLabelWidth := float32(win.TextDimensions(gtx, widget.Label{}, font.Font{}, 12, " … ").Size.X) + 2*spanPadding

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
			fg.StyleState.pathsByColor = map[color.Oklch]*clip.Path{}
		} else {
			clear(fg.StyleState.pathsByColor)
		}
		getPath := func(c color.Oklch) *clip.Path {
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

		labelsMacro := op.Record(gtx.Ops)

		var do func(level int, startX Unit, samples []*widget.FlamegraphFrame, draw bool)
		do = func(level int, startX Unit, samples []*widget.FlamegraphFrame, draw bool) {
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
					frame  = samples[i]
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

				if draw && pxSpan.Max.X >= 0 && pxSpan.Max.Y >= 0 {
					hovered := hoveredSpan.frame == frame
					if hovered {
						hoveredWidthPx = pxSize.X
					}
					mc := fg.Color(level, *idx, frame, hovered)
					p := getPath(mc)

					dspSpan := pxSpan
					dspSpan.Min.Y = flipY(pxSpan.Min.Y)
					dspSpan.Max.Y = flipY(pxSpan.Max.Y)

					// Swap min and max Y so that the first point is in the top-left and the second point in the
					// bottom-right. Otherwise, the code that draws rounded rectangles gets confused.
					dspSpan.Min.Y, dspSpan.Max.Y = dspSpan.Max.Y, dspSpan.Min.Y

					if pxSize.X >= radius {
						shape := clip.UniformFRRect(dspSpan, radius)
						shape.IntoPath(p)
					} else {
						dspSpan.IntoPath(p)
					}

					if pxSize.X >= pxMinLabelWidth {
						stack := offset(dspSpan.Min.Add(f32.Pt(spanPadding, spanPadding))).Push(gtx.Ops)
						gtx := gtx
						gtx.Constraints.Max.X = int(math.Ceil(float64(pxSize.X - 2*spanPadding)))
						gtx.Constraints.Min.X = gtx.Constraints.Max.X

						var f font.Font
						if hovered {
							f.Weight = font.Bold
						}

						shortenFunctionName := func(s string) string {
							idx := strings.LastIndex(s, ".")
							if idx == -1 {
								return s
							} else {
								return s[idx:]
							}
						}
						m := op.Record(gtx.Ops)
						l := frame.Name
						if float32(win.TextLength(gtx, widget.Label{}, f, 12, l)) > pxSize.X {
							l = shortenFunctionName(frame.Name)
						}
						_, tinf := widget.Label{MaxLines: 1, Alignment: text.Middle}.LayoutDetailed(gtx, win.Theme.Shaper, f, 12, l, win.ColorMaterial(gtx, oklch(0, 0, 0)))
						c := m.Stop()
						// Don't display a label if it's just a period followed by an ellipsis
						if tinf.Truncated == 0 || utf8.RuneCountInString(l)-tinf.Truncated != 1 || l[0] != '.' {
							c.Add(gtx.Ops)
						}
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
			cc := c
			if debugCaching {
				cc.A = 50
			}
			FillShape(win, gtx.Ops, cc, clip.Outline{Path: p.End()}.Op())
		}
		c.Add(gtx.Ops)

	}

	// Clear old tooltip
	fg.StyleState.tooltip = ""

	// Don't react to interactions with spans that are less than one pixel wide. They're either entirely invisible,
	// or multiple spans occupy the same pixel.
	if hoveredWidthPx >= 1 {
		if clickedSpan.x != -1 {
			fg.StyleState.zoomHistory = append(fg.StyleState.zoomHistory, fg.StyleState.zoom)

			scale := 1.0 / clickedSpan.width
			fg.StyleState.animate.Start(
				gtx,
				fg.StyleState.zoom,
				fgZoom{
					offsetX:     -(clickedSpan.x * scale),
					offsetLevel: float32(clickedSpan.level),
					scale:       Unit(scale),
					root:        clickedSpan.frame,
				},
				animateLength,
				EaseBezier,
			)
		}

		if hoveredSpan.frame != nil {
			// TODO(dh): use formatting in tooltip, like bold labels
			self := hoveredSpan.frame.Duration
			for _, child := range hoveredSpan.frame.Children {
				self -= child.Duration
			}

			topRoot := hoveredSpan.frame
			for topRoot.Parent != nil {
				topRoot = topRoot.Parent
			}

			var labels [7]string
			labels[0] = fmt.Sprintf("Name: %s\n", hoveredSpan.frame.Name)
			labels[1] = fmt.Sprintf("Stack depth: %d\n", hoveredSpan.level)
			labels[2] = fmt.Sprintf("Duration: %s (%s / %.2f%% self)\n",
				roundDuration(hoveredSpan.frame.Duration),
				roundDuration(self),
				(float64(self)/float64(hoveredSpan.frame.Duration))*100,
			)
			if p := hoveredSpan.frame.Parent; p != nil {
				labels[3] = fmt.Sprintf("Duration: %.2f%% of parent\n",
					(float64(hoveredSpan.frame.Duration)/float64(p.Duration))*100)
			}
			if r := fg.StyleState.zoom.root; r != nil {
				labels[4] = fmt.Sprintf("Duration: %.2f%% of visible root\n",
					(float64(hoveredSpan.frame.Duration)/float64(r.Duration))*100)
			}
			labels[5] = fmt.Sprintf("Duration: %.2f%% of top-level root\n",
				(float64(hoveredSpan.frame.Duration)/float64(topRoot.Duration))*100)
			labels[6] = fmt.Sprintf("Immediate children: %d\n", len(hoveredSpan.frame.Children))
			l := strings.Join(labels[:], "")

			fg.StyleState.tooltip = l
			win.SetTooltip(Tooltip(win.Theme, l).Layout)
		}
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

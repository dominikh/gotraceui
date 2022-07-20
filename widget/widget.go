package widget

import (
	"image"
	"image/color"
	"math"

	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
)

// Bordered renders a widget and then draws a border around it, with constraints adjusted to make sure there is room for
// the border.
type Bordered struct {
	Color color.NRGBA
	Width unit.Dp
}

func (b Bordered) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	border := gtx.Dp(b.Width)

	// XXX handle Max going negative or Max going smaller than Min
	ngtx := gtx
	ngtx.Constraints.Max.X -= 2 * border
	ngtx.Constraints.Max.Y -= 2 * border

	macro := op.Record(gtx.Ops)
	dims := w(ngtx)
	call := macro.Stop()

	gtx.Constraints.Min = dims.Size.Add(image.Pt(2*border, 2*border))
	return Border{Color: b.Color, Width: b.Width}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		call.Add(gtx.Ops)
		return layout.Dimensions{Size: gtx.Constraints.Min}
	})
}

// Border draws a border and renders a widget inside it, with constraints adjusted for the border.
type Border struct {
	Color color.NRGBA
	Width unit.Dp
}

func (b Border) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	width := gtx.Dp(b.Width)

	mx := gtx.Constraints.Min.X
	my := gtx.Constraints.Min.Y

	nwne := clip.Rect{Min: image.Pt(0, 0), Max: image.Pt(mx, width)}.Op()
	nese := clip.Rect{Min: image.Pt(mx-width, width), Max: image.Pt(mx, my-width)}.Op()
	nwsw := clip.Rect{Min: image.Pt(0, width), Max: image.Pt(width, my)}.Op()
	swse := clip.Rect{Min: image.Pt(width, my-width), Max: image.Pt(mx, my)}.Op()

	paint.FillShape(gtx.Ops, b.Color, nwne)
	paint.FillShape(gtx.Ops, b.Color, nese)
	paint.FillShape(gtx.Ops, b.Color, nwsw)
	paint.FillShape(gtx.Ops, b.Color, swse)

	return layout.UniformInset(b.Width).Layout(gtx, w)
}

type TextLine struct {
	Color color.NRGBA
}

func (tl TextLine) Layout(gtx layout.Context, shaper text.Shaper, font text.Font, size unit.Sp, label string) layout.Dimensions {
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	// Effectively disable widget.Label's word wrapping and sentence truncation.
	gtx.Constraints.Max.X = math.MaxInt
	paint.ColorOp{Color: tl.Color}.Add(gtx.Ops)
	return widget.Label{}.Layout(gtx, shaper, font, size, label)
}

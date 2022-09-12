package widget

import (
	"image"
	"image/color"
	"math"

	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
)

// TODO(dh): Bordered, Border, and TextLine probably belong in the theme package instead.

// Bordered is like Border, but automatically applys an inset the side of the border.
type Bordered struct {
	Color color.NRGBA
	Width unit.Dp
}

func (b Bordered) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	return Border(b).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.UniformInset(b.Width).Layout(gtx, w)
	})
}

// Border draws a border and renders a widget inside it, with constraints adjusted for the border.
type Border struct {
	Color color.NRGBA
	Width unit.Dp
}

func (b Border) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	dims := w(gtx)
	sz := dims.Size
	bwidth := gtx.Dp(b.Width)

	nwne := clip.Rect{Min: image.Pt(0, 0), Max: image.Pt(sz.X, bwidth)}.Op()
	nese := clip.Rect{Min: image.Pt(sz.X-bwidth, bwidth), Max: image.Pt(sz.X, sz.Y-bwidth)}.Op()
	nwsw := clip.Rect{Min: image.Pt(0, bwidth), Max: image.Pt(bwidth, sz.Y)}.Op()
	swse := clip.Rect{Min: image.Pt(bwidth, sz.Y-bwidth), Max: image.Pt(sz.X, sz.Y)}.Op()

	paint.FillShape(gtx.Ops, b.Color, nwne)
	paint.FillShape(gtx.Ops, b.Color, nese)
	paint.FillShape(gtx.Ops, b.Color, nwsw)
	paint.FillShape(gtx.Ops, b.Color, swse)

	return dims
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

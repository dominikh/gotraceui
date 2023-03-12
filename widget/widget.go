package widget

import (
	"context"
	"image"
	"image/color"
	rtrace "runtime/trace"

	"gioui.org/f32"
	"gioui.org/layout"
	"gioui.org/op"
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
	defer rtrace.StartRegion(context.Background(), "widget.Bordered.Layout").End()

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
	defer rtrace.StartRegion(context.Background(), "widget.Border.Layout").End()

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

func (tl TextLine) Layout(gtx layout.Context, shaper *text.Shaper, font text.Font, size unit.Sp, label string) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.TextLine.Layout").End()

	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	// Effectively disable widget.Label's word wrapping and sentence truncation.
	gtx.Constraints.Max.X = 1e6
	paint.ColorOp{Color: tl.Color}.Add(gtx.Ops)
	return widget.Label{MaxLines: 1}.Layout(gtx, shaper, font, size, label)
}

type Background struct {
	Color color.NRGBA
}

func (b Background) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Background.Layout").End()

	macro := op.Record(gtx.Ops)
	dims := w(gtx)
	call := macro.Stop()

	paint.FillShape(gtx.Ops, b.Color, clip.Rect{Max: dims.Size}.Op())
	call.Add(gtx.Ops)
	return dims
}

type List struct {
	Main        widget.Scrollbar
	Cross       widget.Scrollbar
	CrossOffset float32
	Widest      int
	layout.List
}

// Image is like the official widget.Image but doesn't apply any scaling, not even for PxPerDp.
// In other words, it maps one image pixel to one output pixel.
type Image struct {
	Src      paint.ImageOp
	Position layout.Direction
}

func (im Image) Layout(gtx layout.Context) layout.Dimensions {
	size := im.Src.Size()

	dims, trans := scale(gtx.Constraints, im.Position, layout.Dimensions{Size: size})
	defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
	defer op.Affine(trans).Push(gtx.Ops).Pop()

	im.Src.Add(gtx.Ops)
	paint.PaintOp{}.Add(gtx.Ops)

	return dims
}

// scale computes the new dimensions and transformation required to fit dims to cs, given the position.
func scale(cs layout.Constraints, pos layout.Direction, dims layout.Dimensions) (layout.Dimensions, f32.Affine2D) {
	widgetSize := dims.Size

	dims.Size = cs.Constrain(dims.Size)

	offset := pos.Position(widgetSize, dims.Size)
	dims.Baseline += offset.Y
	return dims, f32.Affine2D{}.Offset(layout.FPt(offset))
}

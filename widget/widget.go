package widget

import (
	"context"
	"image"
	"image/color"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/layout"

	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
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

func ColorTextMaterial(gtx layout.Context, c color.NRGBA) op.CallOp {
	m := op.Record(gtx.Ops)
	paint.ColorOp{Color: c}.Add(gtx.Ops)
	return m.Stop()
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
	Main        Scrollbar
	Cross       Scrollbar
	CrossOffset float32
	Widest      int
	layout.List
}

package theme

import (
	"context"
	"image"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"

	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/unit"
)

// TODO(dh): Bordered, Border, and TextLine probably belong in the theme package instead.

// Bordered is like Border, but automatically applys an inset.
type Bordered struct {
	Color color.Oklch
	Width unit.Dp
}

func (b Bordered) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	return Border(b).Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		return layout.UniformInset(b.Width).Layout(gtx, Dumb(win, w))
	})
}

// Border draws a border and renders a widget inside it.
type Border struct {
	Color color.Oklch
	Width unit.Dp
}

func (b Border) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Border.Layout").End()

	dims := w(win, gtx)
	sz := dims.Size
	bwidth := gtx.Dp(b.Width)

	nwne := clip.Rect{Min: image.Pt(0, 0), Max: image.Pt(sz.X, bwidth)}.Op()
	nese := clip.Rect{Min: image.Pt(sz.X-bwidth, bwidth), Max: image.Pt(sz.X, sz.Y-bwidth)}.Op()
	nwsw := clip.Rect{Min: image.Pt(0, bwidth), Max: image.Pt(bwidth, sz.Y)}.Op()
	swse := clip.Rect{Min: image.Pt(bwidth, sz.Y-bwidth), Max: image.Pt(sz.X, sz.Y)}.Op()

	FillShape(win, gtx.Ops, b.Color, nwne)
	FillShape(win, gtx.Ops, b.Color, nese)
	FillShape(win, gtx.Ops, b.Color, nwsw)
	FillShape(win, gtx.Ops, b.Color, swse)

	return dims
}

type Background struct {
	Color color.Oklch
}

func (b Background) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Background.Layout").End()

	macro := op.Record(gtx.Ops)
	dims := w(win, gtx)
	call := macro.Stop()

	FillShape(win, gtx.Ops, b.Color, clip.Rect{Max: dims.Size}.Op())
	call.Add(gtx.Ops)
	return dims
}

// FillShape fills the clip shape with a color.
func FillShape(win *Window, ops *op.Ops, c color.Oklch, shape clip.Op) {
	defer shape.Push(ops).Pop()
	Fill(win, ops, c)
}

// Fill paints an infinitely large plane with the provided color. It
// is intended to be used with a clip.Op already in place to limit
// the painted area. Use FillShape unless you need to paint several
// times within the same clip.Op.
func Fill(win *Window, ops *op.Ops, c color.Oklch) {
	paint.ColorOp{Color: win.ConvertColor(c)}.Add(ops)
	paint.PaintOp{}.Add(ops)
}

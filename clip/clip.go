package clip

import (
	"gioui.org/f32"
	"gioui.org/op"
	"gioui.org/op/clip"
)

type FRect struct {
	Min f32.Point
	Max f32.Point
}

func (r FRect) Path(ops *op.Ops) clip.PathSpec {
	var p clip.Path
	p.Begin(ops)
	r.IntoPath(&p)
	return p.End()
}

func (r FRect) IntoPath(p *clip.Path) {
	p.MoveTo(r.Min)
	p.LineTo(f32.Pt(r.Max.X, r.Min.Y))
	p.LineTo(r.Max)
	p.LineTo(f32.Pt(r.Min.X, r.Max.Y))
	p.LineTo(r.Min)
}

func (r FRect) IntoPathR(p *clip.Path) {
	p.MoveTo(r.Min)
	p.LineTo(f32.Pt(r.Min.X, r.Max.Y))
	p.LineTo(r.Max)
	p.LineTo(f32.Pt(r.Max.X, r.Min.Y))
	p.LineTo(r.Min)
}

func (r FRect) Op(ops *op.Ops) clip.Op {
	return clip.Outline{Path: r.Path(ops)}.Op()
}

type RectangularOutline struct {
	Rect  FRect
	Width float32
}

func (out RectangularOutline) Op(ops *op.Ops) clip.Op {
	var p clip.Path
	p.Begin(ops)
	out.Rect.IntoPath(&p)
	inner := FRect{
		Min: f32.Pt(out.Rect.Min.X+out.Width, out.Rect.Min.Y+out.Width),
		Max: f32.Pt(out.Rect.Max.X-out.Width, out.Rect.Max.Y-out.Width),
	}
	inner.IntoPathR(&p)
	p.Close()

	return clip.Outline{Path: p.End()}.Op()
}

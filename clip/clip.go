package clip

import (
	"math"

	"gioui.org/f32"
	"gioui.org/op"
	"gioui.org/op/clip"
)

type Ellipse = clip.Ellipse
type Op = clip.Op
type Outline = clip.Outline
type Path = clip.Path
type PathSpec = clip.PathSpec
type RRect = clip.RRect
type Rect = clip.Rect
type Stack = clip.Stack
type Stroke = clip.Stroke

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

func (r FRect) Contains(pt f32.Point) bool {
	return pt.X >= r.Min.X && pt.X < r.Max.X &&
		pt.Y >= r.Min.Y && pt.Y < r.Max.Y
}

func (r FRect) Dx() float32 {
	return r.Max.X - r.Min.X
}

func (r FRect) Dy() float32 {
	return r.Max.Y - r.Min.Y
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

// FRRect represents the clip area of a rectangle with rounded
// corners.
//
// Specify a square with corner radii equal to half the square size to
// construct a circular clip area.
type FRRect struct {
	Rect FRect
	// The corner radii.
	SE, SW, NW, NE float32
}

// Op returns the op for the rounded rectangle.
func (rr FRRect) Op(ops *op.Ops) Op {
	return Outline{Path: rr.Path(ops)}.Op()
}

// Push the rectangle clip on the clip stack.
func (rr FRRect) Push(ops *op.Ops) Stack {
	return rr.Op(ops).Push(ops)
}

func (rr FRRect) IntoPath(p *clip.Path) {
	// https://pomax.github.io/bezierinfo/#circles_cubic.
	const q = 4 * (math.Sqrt2 - 1) / 3
	const iq = 1 - q

	se, sw, nw, ne := (rr.SE), (rr.SW), (rr.NW), (rr.NE)
	rrf := FRect{
		Min: f32.Pt((rr.Rect.Min.X), (rr.Rect.Min.Y)),
		Max: f32.Pt((rr.Rect.Max.X), (rr.Rect.Max.Y)),
	}
	w, n, e, s := rrf.Min.X, rrf.Min.Y, rrf.Max.X, rrf.Max.Y

	p.MoveTo(f32.Point{X: w + nw, Y: n})
	p.LineTo(f32.Point{X: e - ne, Y: n}) // N
	p.CubeTo(                            // NE
		f32.Point{X: e - ne*iq, Y: n},
		f32.Point{X: e, Y: n + ne*iq},
		f32.Point{X: e, Y: n + ne})
	p.LineTo(f32.Point{X: e, Y: s - se}) // E
	p.CubeTo(                            // SE
		f32.Point{X: e, Y: s - se*iq},
		f32.Point{X: e - se*iq, Y: s},
		f32.Point{X: e - se, Y: s})
	p.LineTo(f32.Point{X: w + sw, Y: s}) // S
	p.CubeTo(                            // SW
		f32.Point{X: w + sw*iq, Y: s},
		f32.Point{X: w, Y: s - sw*iq},
		f32.Point{X: w, Y: s - sw})
	p.LineTo(f32.Point{X: w, Y: n + nw}) // W
	p.CubeTo(                            // NW
		f32.Point{X: w, Y: n + nw*iq},
		f32.Point{X: w + nw*iq, Y: n},
		f32.Point{X: w + nw, Y: n})

}

// Path returns the PathSpec for the rounded rectangle.
func (rr FRRect) Path(ops *op.Ops) PathSpec {
	var p Path
	p.Begin(ops)
	rr.IntoPath(&p)
	return p.End()
}

func UniformFRRect(rect FRect, radius float32) FRRect {
	return FRRect{
		Rect: rect,
		SE:   radius,
		SW:   radius,
		NE:   radius,
		NW:   radius,
	}
}

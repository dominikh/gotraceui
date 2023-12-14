package widget

import (
	"fmt"
	"image"
	"image/draw"
	"image/gif"
	"time"

	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/widget"
	"honnef.co/go/gotraceui/layout"
)

type GIF struct {
	GIF  *gif.GIF
	imgs []paint.ImageOp
	keys []time.Duration
	sum  time.Duration
}

func NewGIF(g *gif.GIF) *GIF {
	keys := make([]time.Duration, len(g.Delay))
	var sum time.Duration
	for i, d := range g.Delay {
		keys[i] = sum
		sum += time.Duration(d) * 10 * time.Millisecond
	}
	for i, d := range g.Disposal {
		if d != 2 {
			panic(fmt.Sprintf("unsupported disposal %d for frame %d", d, i))
		}
	}

	width, height := g.Config.Width, g.Config.Height
	out := &GIF{
		imgs: make([]paint.ImageOp, len(g.Image)),
		keys: keys,
		sum:  sum,
	}
	for i, src := range g.Image {
		// Gio only supports image.Uniform and image.RGBA, and converts other images. If we're already copying, we might as
		// well create an image of the right dimensions so that we don't have to calculate offsets. We would have to use
		// offsets otherwise because individual frames in image/gif.GIF have bounds with non-zero origins and are sized to
		// the portions of the frames that aren't empty.
		dst := image.NewRGBA(image.Rectangle{Max: image.Pt(width, height)})
		draw.Draw(dst, dst.Bounds(), src, image.Pt(0, 0), draw.Src)
		out.imgs[i] = paint.NewImageOp(dst)
	}

	return out
}

func (g *GIF) Layout(gtx layout.Context, dir layout.Direction) layout.Dimensions {
	// TODO(dh): support a non-zero loop count
	// TODO(dh): support starting animation on frame 0
	// TODO(dh): support Disposal other than Background

	frame := -1
	mod := time.Duration(gtx.Now.UnixNano()) % g.sum
	for i := len(g.keys) - 1; i >= 0; i-- {
		if mod >= g.keys[i] {
			frame = i
			break
		}
	}
	if frame == -1 {
		panic("unreachable")
	}

	// OPT(dh): set invalidation in time for next frame
	op.InvalidateOp{}.Add(gtx.Ops)

	// OPT(dh): cache image ops
	// OPT(dh): Gio deletes a texture from the GPU if it hasn't been used in a single frame. That is, of course,
	//   unfortunate for an animation.
	// TODO(dh): allow setting Image.Fit
	return widget.Image{Src: g.imgs[frame], Position: dir, Scale: 1.0 / gtx.Metric.PxPerDp}.Layout(gtx)
}

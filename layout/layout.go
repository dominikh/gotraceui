package layout

import (
	"context"
	"image"
	rtrace "runtime/trace"

	"gioui.org/op"
	"gioui.org/x/outlay"
)

type SmallGrid struct {
	Grid          outlay.Grid
	RowPadding    int
	ColumnPadding int
}

func (sg SmallGrid) Layout(gtx Context, rows, cols int, sizeEstimator outlay.Cell, cellFunc outlay.Cell) Dimensions {
	defer rtrace.StartRegion(context.Background(), "layout.SmallGrid.Layout").End()

	colWidths := make([]int, cols)
	// Storing dims isn't strictly necessarily, since we only need to know the row height (which Grid assumes is the
	// same for each row) and the column widths, as outlay.Grid passes an exact constraint to the cell function with
	// those dimensions. However, as written, the code depends less on implementation details.
	dims := make([]Dimensions, rows*cols)

	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			dim := sizeEstimator(gtx, row, col)
			dims[row*cols+col] = dim
			if dim.Size.X > colWidths[col] {
				colWidths[col] = dim.Size.X
			}
		}
	}

	dimmer := func(axis Axis, index, constraint int) int {
		switch axis {
		case Vertical:
			// outlay.Grid doesn't support different row heights, so we can return any of them
			return dims[0].Size.Y + sg.RowPadding
		case Horizontal:
			return colWidths[index] + sg.ColumnPadding
		default:
			panic("unreachable")
		}
	}

	// outlay.Grid fills the Max constraint
	height := rows*(dims[0].Size.Y+sg.RowPadding) - sg.RowPadding
	var width int
	for _, cw := range colWidths {
		width += cw + sg.ColumnPadding
	}
	gtx.Constraints.Max = gtx.Constraints.Constrain(image.Pt(width, height))
	wrapper := func(gtx Context, row, col int) Dimensions {
		ogtx := gtx
		gtx.Constraints.Min.X -= sg.ColumnPadding
		gtx.Constraints.Max.X -= sg.ColumnPadding
		dims := cellFunc(gtx, row, col)
		dims.Size = ogtx.Constraints.Constrain(dims.Size)
		return dims
	}
	return sg.Grid.Layout(gtx, rows, cols, dimmer, wrapper)
}

// PixelInset is like Inset, but using pixel coordinates instead of dp.
type PixelInset struct {
	Top, Bottom, Left, Right int
}

func (in PixelInset) Layout(gtx Context, w Widget) Dimensions {
	defer rtrace.StartRegion(context.Background(), "layout.PixelInset.Layout").End()

	top := (in.Top)
	right := (in.Right)
	bottom := (in.Bottom)
	left := (in.Left)
	mcs := gtx.Constraints
	mcs.Max.X -= left + right
	if mcs.Max.X < 0 {
		left = 0
		right = 0
		mcs.Max.X = 0
	}
	if mcs.Min.X > mcs.Max.X {
		mcs.Min.X = mcs.Max.X
	}
	mcs.Max.Y -= top + bottom
	if mcs.Max.Y < 0 {
		bottom = 0
		top = 0
		mcs.Max.Y = 0
	}
	if mcs.Min.Y > mcs.Max.Y {
		mcs.Min.Y = mcs.Max.Y
	}
	gtx.Constraints = mcs
	trans := op.Offset(image.Pt(left, top)).Push(gtx.Ops)
	dims := w(gtx)
	trans.Pop()
	return Dimensions{
		Size:     dims.Size.Add(image.Point{X: right + left, Y: top + bottom}),
		Baseline: dims.Baseline + bottom,
	}
}

func Normalize(c Constraints) Constraints {
	if c.Min.X < 0 {
		c.Min.X = 0
	}
	if c.Min.Y < 0 {
		c.Min.Y = 0
	}
	if c.Max.X < 0 {
		c.Max.X = 0
	}
	if c.Max.Y < 0 {
		c.Max.Y = 0
	}

	if c.Min.X > c.Max.X {
		c.Min.X = c.Max.X
	}
	if c.Min.Y > c.Max.Y {
		c.Min.Y = c.Max.Y
	}

	return c
}

func Main(a Axis, pt *image.Point) *int {
	if a == Horizontal {
		return &pt.X
	}
	return &pt.Y
}

func Cross(a Axis, pt *image.Point) *int {
	if a == Horizontal {
		return &pt.Y
	}
	return &pt.X
}

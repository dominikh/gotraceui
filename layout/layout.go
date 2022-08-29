package layout

import (
	"image"

	"gioui.org/layout"
	"gioui.org/x/outlay"
)

type SmallGrid struct {
	Grid          outlay.Grid
	RowPadding    int
	ColumnPadding int
}

func (sg SmallGrid) Layout(gtx layout.Context, rows, cols int, sizeEstimator outlay.Cell, cellFunc outlay.Cell) layout.Dimensions {
	colWidths := make([]int, cols)
	// Storing dims isn't strictly necessarily, since we only need to know the row height (which Grid assumes is the
	// same for each row) and the column widths, as outlay.Grid passes an exact constraint to the cell function with
	// those dimensions. However, as written, the code depends less on implementation details.
	dims := make([]layout.Dimensions, rows*cols)

	for row := 0; row < rows; row++ {
		for col := 0; col < cols; col++ {
			dim := sizeEstimator(gtx, row, col)
			dims[row*cols+col] = dim
			if dim.Size.X > colWidths[col] {
				colWidths[col] = dim.Size.X
			}
		}
	}

	dimmer := func(axis layout.Axis, index, constraint int) int {
		switch axis {
		case layout.Vertical:
			// outlay.Grid doesn't support different row heights, so we can return any of them
			return dims[0].Size.Y + sg.RowPadding
		case layout.Horizontal:
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
	wrapper := func(gtx layout.Context, row, col int) layout.Dimensions {
		ogtx := gtx
		gtx.Constraints.Min.X -= sg.ColumnPadding
		gtx.Constraints.Max.X -= sg.ColumnPadding
		dims := cellFunc(gtx, row, col)
		dims.Size = ogtx.Constraints.Constrain(dims.Size)
		return dims
	}
	return sg.Grid.Layout(gtx, rows, cols, dimmer, wrapper)
}

package theme

import (
	"image"
	"image/color"
	"math"

	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
)

// TODO(dh): this should be in package widget
type Table struct {
	ColumnNames  []string
	ColumnWidths []float32

	prevMetric   unit.Metric
	prevMaxWidth int
	drags        []tableDrag
}

func (tbl *Table) SetColumns(win *Window, gtx layout.Context, names ...string) {
	var (
		total = gtx.Constraints.Max.X - gtx.Dp(Scrollbar(win.Theme, nil).Width()) - len(names)*gtx.Dp(DefaultDividerWidth)
		width = float32(total) / float32(len(names))
	)

	tbl.ColumnNames = make([]string, len(names))
	tbl.ColumnWidths = make([]float32, len(names))
	for i, name := range names {
		tbl.ColumnNames[i] = name
		tbl.ColumnWidths[i] = width
	}

	tbl.prevMaxWidth = gtx.Constraints.Max.X
	tbl.prevMetric = gtx.Metric
}

func (tbl *Table) Resize(win *Window, gtx layout.Context) {
	if gtx.Constraints.Max.X == tbl.prevMaxWidth {
		return
	}

	var (
		oldAvailable = tbl.prevMaxWidth - tbl.prevMetric.Dp(Scrollbar(win.Theme, nil).Width())
		available    = gtx.Constraints.Max.X - gtx.Dp(Scrollbar(win.Theme, nil).Width())
	)

	for i, width := range tbl.ColumnWidths {
		r := float32(width) / float32(oldAvailable)
		tbl.ColumnWidths[i] = r * float32(available)
	}

	tbl.prevMaxWidth = gtx.Constraints.Max.X
	tbl.prevMetric = gtx.Metric
}

type tableDrag struct {
	drag           gesture.Drag
	hover          gesture.Hover
	startPos       float32
	shrinkNeighbor bool
}

type TableRowStyle struct {
	Table  *Table
	Header bool
}

func TableRow(tbl *Table, hdr bool) TableRowStyle {
	return TableRowStyle{
		Table:  tbl,
		Header: hdr,
	}
}

const DefaultDividerWidth unit.Dp = 1

func (row TableRowStyle) Layout(win *Window, gtx layout.Context, w func(win *Window, gtx layout.Context, idx int) layout.Dimensions) layout.Dimensions {
	var (
		cols          = len(row.Table.ColumnNames)
		dividers      = cols
		tallestHeight = gtx.Constraints.Min.Y

		dividerWidth                   = gtx.Dp(DefaultDividerWidth)
		dividerMargin                  = gtx.Dp(1)
		dividerHandleMinVerticalMargin = gtx.Dp(2)
		dividerHandleMaxHeight         = gtx.Dp(12)
		dividerHandleWidth             = gtx.Dp(3)
		dividerHandleRadius            = gtx.Dp(2)

		minWidth = float32(dividerWidth + dividerMargin + dividerHandleWidth)
	)
	if cols == 0 {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	if len(row.Table.drags) < dividers {
		row.Table.drags = make([]tableDrag, dividers)
	}

	// OPT(dh): we don't need to do this for each row, only once per table
	for i := range row.Table.drags {
		drag := &row.Table.drags[i]
		drag.hover.Update(gtx.Queue)
		// OPT(dh): Events allocates
		var delta float32
		for _, ev := range drag.drag.Events(gtx.Metric, gtx.Queue, gesture.Horizontal) {
			switch ev.Type {
			case pointer.Press:
				drag.startPos = ev.Position.X
				drag.shrinkNeighbor = !ev.Modifiers.Contain(key.ModShift)
			case pointer.Drag:
				// There may be multiple drag events in a single frame. We mustn't apply all of them or we'll drag too
				// far. Only react to the final event.
				delta = ev.Position.X - drag.startPos
			}
		}
		if delta != 0 {
			colWidth := &row.Table.ColumnWidths[i]
			*colWidth += delta
			if drag.shrinkNeighbor && i != len(row.Table.ColumnNames)-1 {
				nextColWidth := &row.Table.ColumnWidths[i+1]
				*nextColWidth -= delta
				if *colWidth < minWidth {
					d := minWidth - *colWidth
					*colWidth = minWidth
					*nextColWidth -= d
				}
				if *nextColWidth < minWidth {
					d := minWidth - *nextColWidth
					*nextColWidth = minWidth
					*colWidth -= d
				}
			} else {
				if *colWidth < minWidth {
					*colWidth = minWidth
				}
			}

			total := 0
			for _, w := range row.Table.ColumnWidths {
				total += int(w)
			}
			total += len(row.Table.ColumnWidths) * gtx.Dp(DefaultDividerWidth)
			if total < gtx.Constraints.Min.X {
				row.Table.ColumnWidths[len(row.Table.ColumnWidths)-1] += float32(gtx.Constraints.Min.X - total)
			}
		}
	}

	for {
		// First draw all columns, leaving gaps for the drag handlers
		var (
			start             = 0
			origTallestHeight = tallestHeight
		)
		r := op.Record(gtx.Ops)
		for i := range row.Table.ColumnWidths {
			colWidth := int((row.Table.ColumnWidths[i]))
			gtx := gtx
			gtx.Constraints.Min.X = colWidth
			gtx.Constraints.Max.X = colWidth
			gtx.Constraints.Min.Y = tallestHeight

			stack := op.Offset(image.Pt(start, 0)).Push(gtx.Ops)

			dims := w(win, gtx, i)
			dims.Size = gtx.Constraints.Constrain(dims.Size)
			tallestHeight = dims.Size.Y
			if i == 0 && tallestHeight > origTallestHeight {
				origTallestHeight = tallestHeight
			}

			start += colWidth + dividerWidth
			stack.Pop()
		}
		call := r.Stop()

		if tallestHeight > origTallestHeight {
			continue
		}

		call.Add(gtx.Ops)

		// Then draw the drag handlers. The handlers overdraw the columns when hovered.
		var (
			dividerHandleHeight    = min(tallestHeight-2*dividerHandleMinVerticalMargin, dividerHandleMaxHeight)
			dividerHandleTopMargin = (tallestHeight - dividerHandleHeight) / 2
			dividerStart           = 0
		)
		for i := range row.Table.drags {
			var (
				drag     = &row.Table.drags[i]
				colWidth = int((row.Table.ColumnWidths[i]))
			)
			dividerStart += colWidth

			// We add the drag handler slightly outside the drawn divider, to make it easier to press.
			//
			// We use op.Offset instead of folding dividerStart into the clip.Rect because we want to set the origin of the
			// drag coordinates.
			stack := op.Offset(image.Pt(dividerStart, 0)).Push(gtx.Ops)
			stack2 := clip.Rect{Min: image.Pt(-dividerMargin-dividerHandleWidth, 0), Max: image.Pt(dividerWidth+dividerMargin+dividerHandleWidth, tallestHeight)}.Push(gtx.Ops)

			if row.Header {
				drag.hover.Update(gtx.Queue)
				drag.drag.Add(gtx.Ops)
				drag.hover.Add(gtx.Ops)
				pointer.CursorColResize.Add(gtx.Ops)

				// Draw the left and right extensions when hovered.
				if drag.hover.Hovered() || drag.drag.Dragging() {
					handleShape := clip.UniformRRect(image.Rect(0, dividerHandleTopMargin, dividerHandleWidth, dividerHandleTopMargin+dividerHandleHeight), dividerHandleRadius)
					handleLeft := handleShape
					handleLeft.Rect = handleShape.Rect.Add(image.Pt(-(dividerMargin + dividerHandleWidth), 0))
					handleRight := handleShape
					handleRight.Rect = handleRight.Rect.Add(image.Pt(dividerWidth+dividerMargin, 0))

					paint.FillShape(gtx.Ops, win.Theme.Palette.Table.DragHandle, handleLeft.Op(gtx.Ops))
					paint.FillShape(gtx.Ops, win.Theme.Palette.Table.DragHandle, handleRight.Op(gtx.Ops))
				}

				// Draw the vertical bar
				stack3 := clip.Rect{Max: image.Pt(dividerWidth, tallestHeight)}.Push(gtx.Ops)
				paint.Fill(gtx.Ops, win.Theme.Palette.Table.Divider)
				stack3.Pop()
			}

			dividerStart += dividerWidth
			stack2.Pop()
			stack.Pop()
		}

		return layout.Dimensions{
			Size: image.Pt(start, tallestHeight),
		}
	}
}

type FauxTableRowStyle struct {
	Table      *Table
	Background color.NRGBA
}

func FauxTableRow(tbl *Table, bg color.NRGBA) FauxTableRowStyle {
	return FauxTableRowStyle{
		Table:      tbl,
		Background: bg,
	}
}

func (row FauxTableRowStyle) Layout(win *Window, gtx layout.Context, w func(win *Window, gtx layout.Context) layout.Dimensions) layout.Dimensions {
	var (
		cols     = len(row.Table.ColumnNames)
		dividers = cols

		dividerWidth = gtx.Dp(DefaultDividerWidth)
	)
	if cols == 0 {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	if len(row.Table.drags) < dividers {
		row.Table.drags = make([]tableDrag, dividers)
	}

	var totalWidth int
	for _, colWidth := range row.Table.ColumnWidths {
		totalWidth += int((colWidth))
	}
	totalWidth += len(row.Table.ColumnWidths) * dividerWidth

	r := Record(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		ngtx := gtx
		ngtx.Constraints.Max.X = totalWidth
		return w(win, ngtx)
	})

	tallestHeight := r.Dimensions.Size.Y

	paint.FillShape(gtx.Ops, row.Background, clip.Rect{Max: image.Pt(totalWidth, tallestHeight)}.Op())

	// Then draw the drag handlers. The handlers overdraw the columns when hovered.
	dividerStart := 0
	for i := range row.Table.drags {
		dividerStart += int((row.Table.ColumnWidths[i]))

		// We add the drag handler slightly outside the drawn divider, to make it easier to press.
		//
		// We use op.Offset instead of folding dividerStart into the clip.Rect because we want to set the origin of the
		// drag coordinates.
		stack := op.Offset(image.Pt(dividerStart, 0)).Push(gtx.Ops)
		// Draw the vertical bar
		stack3 := clip.Rect{Max: image.Pt(dividerWidth, tallestHeight)}.Push(gtx.Ops)
		paint.Fill(gtx.Ops, win.Theme.Palette.Table.Divider)

		dividerStart += dividerWidth
		stack3.Pop()
		stack.Pop()
	}

	r.Layout(win, gtx)
	return layout.Dimensions{
		Size: image.Pt(totalWidth, tallestHeight),
	}
}

func round32(f float32) float32 {
	return float32(math.Round(float64(f)))
}

type YScrollableListState struct {
	rememberingList RememberingList
	vertList        layout.List
	horizList       layout.List
	vertScroll      widget.Scrollbar
	horizScroll     widget.Scrollbar
}

type YScrollableListStyle struct {
	state *YScrollableListState
}

func YScrollableList(state *YScrollableListState) YScrollableListStyle {
	state.vertList.Axis = layout.Vertical
	state.horizList.Axis = layout.Horizontal
	return YScrollableListStyle{state}
}

type RememberingList struct {
	list *layout.List
	len  int
	dims layout.Dimensions
}

func (rlist *RememberingList) Layout(gtx layout.Context, len int, w layout.ListElement) layout.Dimensions {
	rlist.len = len
	rlist.dims = rlist.list.Layout(gtx, len, w)
	return rlist.dims
}

func (tbl YScrollableListStyle) Layout(win *Window, gtx layout.Context, body func(win *Window, gtx layout.Context, list *RememberingList) layout.Dimensions) layout.Dimensions {
	scrollbarWidth := Scrollbar(win.Theme, nil).Width()
	dims := layout.Inset{Bottom: scrollbarWidth}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		// Body
		origGtx := gtx
		tbl.state.horizList.Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
			gtx.Constraints.Min.X = max(0, origGtx.Constraints.Min.X-gtx.Dp(Scrollbar(win.Theme, nil).Width()))
			return layout.Inset{Right: scrollbarWidth}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				tbl.state.rememberingList.list = &tbl.state.vertList
				return body(win, gtx, &tbl.state.rememberingList)
			})
		})

		// Vertical scrollbar
		defer op.Offset(image.Pt(gtx.Constraints.Max.X-gtx.Dp(scrollbarWidth), 0)).Push(gtx.Ops).Pop()
		l, h := FromListPosition(tbl.state.vertList.Position, tbl.state.rememberingList.len, tbl.state.rememberingList.dims.Size.Y)
		Scrollbar(win.Theme, &tbl.state.vertScroll).Layout(gtx, layout.Vertical, l, h)
		if delta := tbl.state.vertScroll.ScrollDistance(); delta != 0 {
			tbl.state.vertList.ScrollBy(delta * float32(tbl.state.rememberingList.len))
		}

		return layout.Dimensions{Size: gtx.Constraints.Max}
	})

	// Horizontal scrollbar
	layout.PixelInset{Top: dims.Size.Y - gtx.Dp(scrollbarWidth), Right: gtx.Dp(scrollbarWidth)}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		l, h := FromListPosition(tbl.state.horizList.Position, 1, dims.Size.X)
		dims := Scrollbar(win.Theme, &tbl.state.horizScroll).Layout(gtx, layout.Horizontal, l, h)
		if delta := tbl.state.horizScroll.ScrollDistance(); delta != 0 {
			tbl.state.horizList.ScrollBy(delta)
		}
		return dims
	})

	return layout.Dimensions{Size: gtx.Constraints.Max}
}

type TableHeaderRowStyle struct {
	Table *Table
}

func TableHeaderRow(tbl *Table) TableHeaderRowStyle {
	return TableHeaderRowStyle{Table: tbl}
}

// XXX having columns and rightAlign is awkward. have a Column abstraction
func (row TableHeaderRowStyle) Layout(win *Window, gtx layout.Context, columns []string, rightAlign []bool) layout.Dimensions {
	if len(columns) != len(row.Table.ColumnNames) {
		panic("wrong number of columns")
	}
	return TableRow(row.Table, true).Layout(win, gtx, func(win *Window, gtx layout.Context, col int) layout.Dimensions {
		var (
			f          = font.Font{Weight: font.ExtraBold}
			fg         = widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground)
			lineHeight = win.TextDimensions(gtx, widget.Label{}, f, win.Theme.TextSize, "").Size.Y
			paddingDp  = unit.Dp(5)
			borderDp   = unit.Dp(1)
			height     = max(gtx.Constraints.Min.Y, lineHeight+gtx.Dp(paddingDp)*2+gtx.Dp(borderDp))
		)

		paint.FillShape(gtx.Ops, win.Theme.Palette.Table.HeaderBackground, clip.Rect{Max: image.Pt(gtx.Constraints.Min.X, height)}.Op())

		layout.Inset{Top: paddingDp, Bottom: paddingDp + borderDp, Left: paddingDp, Right: paddingDp}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			l := widget.Label{MaxLines: 1, Alignment: text.Start}
			if rightAlign[col] {
				l.Alignment = text.End
			}
			return l.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, columns[col], fg)
		})

		paint.FillShape(gtx.Ops, win.Theme.Palette.Table.Divider, clip.Rect{Min: image.Pt(0, height-gtx.Dp(borderDp)), Max: image.Pt(gtx.Constraints.Min.X, height)}.Op())
		return layout.Dimensions{
			Size: image.Pt(gtx.Constraints.Min.X, height),
		}
	})
}

type TableSimpleRowStyle struct {
	Table *Table
}

func TableSimpleRow(tbl *Table) TableSimpleRowStyle {
	return TableSimpleRowStyle{Table: tbl}
}

func (row TableSimpleRowStyle) Layout(win *Window, gtx layout.Context, rowIdx int, cellFn func(win *Window, gtx layout.Context, row, col int) layout.Dimensions) layout.Dimensions {
	c := win.Theme.Palette.Table.EvenRowBackground
	if rowIdx%2 == 1 {
		c = win.Theme.Palette.Table.OddRowBackground
	}

	return TableRow(row.Table, false).Layout(win, gtx, func(win *Window, gtx layout.Context, col int) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

		const padding = 5
		return widget.Background{Color: c}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			dims := layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				gtx.Constraints.Min.Y = 0
				return cellFn(win, gtx, rowIdx, col)
			})

			return layout.Dimensions{
				Size: gtx.Constraints.Constrain(dims.Size),
			}
		})
	})
}

func TableExpandedRow(tbl *Table) TableExpandedRowStyle {
	return TableExpandedRowStyle{
		Table: tbl,
	}
}

type TableExpandedRowStyle struct {
	Table *Table
}

func (ex TableExpandedRowStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	// XXX palette colors instead of rgba()

	return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			size := image.Pt(gtx.Constraints.Min.X, gtx.Dp(1))
			paint.FillShape(gtx.Ops, rgba(0xBEBEBEFF), clip.Rect{Max: size}.Op())
			return layout.Dimensions{
				Size: size,
			}
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return FauxTableRow(ex.Table, win.Theme.Palette.Background).Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
				return widget.Background{Color: rgba(0xF5F5E1FF)}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return w(win, gtx)
				})
			})
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			size := image.Pt(gtx.Constraints.Min.X, gtx.Dp(1))
			paint.FillShape(gtx.Ops, rgba(0xBEBEBEFF), clip.Rect{Max: size}.Op())
			return layout.Dimensions{
				Size: size,
			}
		}),
	)
}

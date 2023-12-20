package theme

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/container"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/text"
	"gioui.org/unit"
)

type SortOrder uint8
type CellFn func(win *Window, gtx layout.Context, row, col int) layout.Dimensions
type RowFn func(win *Window, gtx layout.Context, row int) layout.Dimensions

const (
	SortNone SortOrder = iota
	SortAscending
	SortDescending
)

const (
	DefaultDividerWidth                   unit.Dp = 1
	DefaultDividerMargin                  unit.Dp = 1
	DefaultDividerHandleMinVerticalMargin unit.Dp = 2
	DefaultDividerHandleMaxHeight         unit.Dp = 12
	DefaultDividerHandleWidth             unit.Dp = 3
	DefaultDividerHandleRadius            unit.Dp = 2
	DefaultHeaderPadding                  unit.Dp = 5
	DefaultHeaderBorder                   unit.Dp = 1
	DefaultExpandedBorder                 unit.Dp = 1
)

// TODO(dh): this should be in package widget
type Table struct {
	Columns   []Column
	SortOrder SortOrder
	SortedBy  int

	prevMetric    unit.Metric
	prevMaxWidth  int
	drags         []tableDrag
	rowHovers     mem.BucketSlice[gesture.Hover]
	headerClicks  []gesture.Click
	clickedColumn container.Option[int]
}

type Column struct {
	Name      string
	Width     float32
	MinWidth  float32
	Alignment text.Alignment
	Clickable bool
}

func (tbl *Table) SetColumns(win *Window, gtx layout.Context, cols []Column) {
	var (
		total = gtx.Constraints.Max.X - gtx.Dp(Scrollbar(win.Theme, nil).Width()) - len(cols)*gtx.Dp(DefaultDividerWidth)
		width = float32(total) / float32(len(cols))
	)

	allZero := true
	for _, col := range cols {
		if col.Width != 0 {
			allZero = false
			break
		}
	}

	if allZero {
		for i := range cols {
			cols[i].Width = width
		}
	}

	tbl.Columns = cols
	tbl.headerClicks = make([]gesture.Click, len(cols))

	tbl.prevMaxWidth = gtx.Constraints.Max.X
	tbl.prevMetric = gtx.Metric
}

func (tbl *Table) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.Table.Layout").End()

	tbl.resize(win, gtx)
	tbl.rowHovers.Reset()
	dims := w(win, gtx)
	dims.Size = gtx.Constraints.Constrain(dims.Size)

	tbl.clickedColumn = container.None[int]()
	for i := range tbl.headerClicks {
		click := &tbl.headerClicks[i]
		for _, ev := range click.Update(gtx.Queue) {
			if ev.Button == pointer.ButtonPrimary && ev.Kind == gesture.KindClick {
				tbl.clickedColumn = container.Some(i)
			}
		}
	}

	return dims
}

func (tbl *Table) ClickedColumn() (int, bool) {
	return tbl.clickedColumn.Get()
}

func (tbl *Table) SortByClickedColumn() (int, bool) {
	if col, ok := tbl.ClickedColumn(); ok {
		if col == tbl.SortedBy {
			switch tbl.SortOrder {
			case SortNone:
				tbl.SortOrder = SortAscending
			case SortAscending:
				tbl.SortOrder = SortDescending
			case SortDescending:
				tbl.SortOrder = SortAscending
			default:
				panic(fmt.Sprintf("unhandled case %v", tbl.SortOrder))
			}
		} else {
			tbl.SortedBy = col
			tbl.SortOrder = SortAscending
		}

		return col, true
	}

	return 0, false
}

func (tbl *Table) resize(win *Window, gtx layout.Context) {
	if gtx.Constraints.Max.X == tbl.prevMaxWidth && gtx.Metric == tbl.prevMetric {
		return
	}

	var (
		oldAvailable = tbl.prevMaxWidth -
			tbl.prevMetric.Dp(Scrollbar(win.Theme, nil).Width()) -
			len(tbl.Columns)*tbl.prevMetric.Dp(DefaultDividerWidth)
		available = gtx.Constraints.Max.X -
			gtx.Dp(Scrollbar(win.Theme, nil).Width()) -
			len(tbl.Columns)*gtx.Dp(DefaultDividerWidth)
	)

	// Avoid values <1. Negative values are naturally bad, and zero leads to NaN when we compute ratios.
	if oldAvailable < 1 {
		oldAvailable = 1
	}
	if available < 1 {
		available = 1
	}

	defer func() {
		tbl.prevMaxWidth = gtx.Constraints.Max.X
		tbl.prevMetric = gtx.Metric
	}()

	if available > oldAvailable {
		var totalWidth float32
		for i := range tbl.Columns {
			totalWidth += tbl.Columns[i].Width
		}
		if totalWidth > float32(available) {
			// Don't grow columns if the table is already wider than the available space. The user probably resized the
			// container to see more of the table.
			return
		}
	}

	var (
		dividerWidth       = gtx.Dp(DefaultDividerWidth)
		dividerMargin      = gtx.Dp(DefaultDividerMargin)
		dividerHandleWidth = gtx.Dp(DefaultDividerHandleWidth)

		globalMinWidth = float32(dividerWidth + dividerMargin + dividerHandleWidth)
	)

	for i := range tbl.Columns {
		col := &tbl.Columns[i]
		r := float32(col.Width) / float32(oldAvailable)
		col.Width = max(col.MinWidth, globalMinWidth, r*float32(available))
	}
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

func (row TableRowStyle) Layout(win *Window, gtx layout.Context, w RowFn) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TableRowStyle.Layout").End()

	var (
		cols          = len(row.Table.Columns)
		dividers      = cols
		tallestHeight = gtx.Constraints.Min.Y

		dividerWidth                   = gtx.Dp(DefaultDividerWidth)
		dividerMargin                  = gtx.Dp(DefaultDividerMargin)
		dividerHandleMinVerticalMargin = gtx.Dp(DefaultDividerHandleMinVerticalMargin)
		dividerHandleMaxHeight         = gtx.Dp(DefaultDividerHandleMaxHeight)
		dividerHandleWidth             = gtx.Dp(DefaultDividerHandleWidth)
		dividerHandleRadius            = gtx.Dp(DefaultDividerHandleRadius)

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
		col := &row.Table.Columns[i]
		drag.hover.Update(gtx.Queue)
		// OPT(dh): Events allocates
		var delta float32
		for _, ev := range drag.drag.Update(gtx.Metric, gtx.Queue, gesture.Horizontal) {
			switch ev.Kind {
			case pointer.Press:
				drag.startPos = ev.Position.X
				drag.shrinkNeighbor = !ev.Modifiers.Contain(key.ModShift)
			case pointer.Drag:
				// There may be multiple drag events in a single frame. We mustn't apply all of them or we'll
				// drag too far. Only react to the final event.
				delta = ev.Position.X - drag.startPos
			}
		}
		if delta != 0 {
			col.Width += delta
			if drag.shrinkNeighbor && i != len(row.Table.Columns)-1 {
				nextCol := &row.Table.Columns[i+1]
				nextCol.Width -= delta
				if col.Width < minWidth {
					d := minWidth - col.Width
					col.Width = minWidth
					nextCol.Width -= d
				}
				if nextCol.Width < minWidth {
					d := minWidth - nextCol.Width
					nextCol.Width = minWidth
					col.Width -= d
				}
			} else {
				if col.Width < minWidth {
					col.Width = minWidth
				}
			}

			if col.Width < col.MinWidth {
				col.MinWidth = col.Width
			}

			var total float32
			for _, col := range row.Table.Columns {
				total += col.Width
			}
			total += float32(len(row.Table.Columns) * gtx.Dp(DefaultDividerWidth))
			if total < float32(gtx.Constraints.Min.X) {
				row.Table.Columns[len(row.Table.Columns)-1].Width += float32(gtx.Constraints.Min.X) - total
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
		totalWidth := 0
		for i := range row.Table.Columns {
			colWidth := int(row.Table.Columns[i].Width)
			totalWidth += colWidth
		}
		extra := gtx.Constraints.Min.X - len(row.Table.Columns)*gtx.Dp(DefaultDividerWidth) - totalWidth
		colExtra := extra

		for i := range row.Table.Columns {
			colWidth := int(row.Table.Columns[i].Width)
			if colExtra > 0 {
				colWidth++
				colExtra--
			}

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
			dividerExtra           = extra
		)
		for i := range row.Table.drags {
			var (
				drag     = &row.Table.drags[i]
				colWidth = int(row.Table.Columns[i].Width)
			)
			dividerStart += colWidth
			if dividerExtra > 0 {
				dividerStart++
				dividerExtra--
			}

			// We add the drag handler slightly outside the drawn divider, to make it easier to press.
			//
			// We use op.Offset instead of folding dividerStart into the clip.Rect because we want to set the
			// origin of the drag coordinates.
			stack := op.Offset(image.Pt(dividerStart, 0)).Push(gtx.Ops)
			stack2 := clip.Rect{
				Min: image.Pt(-dividerMargin-dividerHandleWidth, 0),
				Max: image.Pt(dividerWidth+dividerMargin+dividerHandleWidth, tallestHeight),
			}.Push(gtx.Ops)

			if row.Header {
				drag.hover.Update(gtx.Queue)
				drag.drag.Add(gtx.Ops)
				drag.hover.Add(gtx.Ops)
				pointer.CursorColResize.Add(gtx.Ops)

				// Draw the left and right extensions when hovered.
				if drag.hover.Update(gtx.Queue) || drag.drag.Dragging() {
					handleShape := clip.UniformRRect(
						image.Rect(
							0,
							dividerHandleTopMargin,
							dividerHandleWidth,
							dividerHandleTopMargin+dividerHandleHeight),
						dividerHandleRadius,
					)
					handleLeft := handleShape
					handleLeft.Rect = handleShape.Rect.Add(image.Pt(-(dividerMargin + dividerHandleWidth), 0))
					handleRight := handleShape
					handleRight.Rect = handleRight.Rect.Add(image.Pt(dividerWidth+dividerMargin, 0))

					FillShape(win, gtx.Ops, win.Theme.Palette.Table.DragHandle, handleLeft.Op(gtx.Ops))
					FillShape(win, gtx.Ops, win.Theme.Palette.Table.DragHandle, handleRight.Op(gtx.Ops))
				}

				// Draw the vertical bar
				stack3 := clip.Rect{Max: image.Pt(dividerWidth, tallestHeight)}.Push(gtx.Ops)
				Fill(win, gtx.Ops, win.Theme.Palette.Table.Divider)
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
	Background color.Oklch
}

func FauxTableRow(tbl *Table, bg color.Oklch) FauxTableRowStyle {
	return FauxTableRowStyle{
		Table:      tbl,
		Background: bg,
	}
}

func (row FauxTableRowStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.FauxTableRowStyle.Layout").End()

	var (
		cols     = len(row.Table.Columns)
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
	for i := range row.Table.Columns {
		colWidth := int(row.Table.Columns[i].Width)
		totalWidth += colWidth
	}
	extra := gtx.Constraints.Min.X - len(row.Table.Columns)*gtx.Dp(DefaultDividerWidth) - totalWidth
	totalWidth += len(row.Table.Columns) * dividerWidth
	totalWidth += extra

	r := Record(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		ngtx := gtx
		ngtx.Constraints.Max.X = totalWidth
		return w(win, ngtx)
	})

	tallestHeight := r.Dimensions.Size.Y

	FillShape(win, gtx.Ops, row.Background, clip.Rect{Max: image.Pt(totalWidth, tallestHeight)}.Op())

	dividerStart := 0
	for i := range row.Table.drags {
		dividerStart += int(row.Table.Columns[i].Width)
		if extra > 0 {
			dividerStart++
			extra--
		}

		stack := op.Offset(image.Pt(dividerStart, 0)).Push(gtx.Ops)
		// Draw the vertical bar
		stack3 := clip.Rect{Max: image.Pt(dividerWidth, tallestHeight)}.Push(gtx.Ops)
		Fill(win, gtx.Ops, win.Theme.Palette.Table.Divider)

		dividerStart += dividerWidth
		stack3.Pop()
		stack.Pop()
	}

	r.Layout(win, gtx)
	return layout.Dimensions{
		Size: image.Pt(totalWidth, tallestHeight),
	}
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
	defer rtrace.StartRegion(context.Background(), "theme.RememberingList.Layout").End()

	rlist.len = len
	rlist.dims = rlist.list.Layout(gtx, len, w)
	return rlist.dims
}

func (tbl YScrollableListStyle) Layout(
	win *Window,
	gtx layout.Context,
	body func(win *Window, gtx layout.Context, list *RememberingList) layout.Dimensions,
) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.YScrollableListStyle.Layout").End()

	scrollbarWidth := Scrollbar(win.Theme, nil).Width()

	var bodyDims layout.Dimensions
	return layout.Rigids(gtx, layout.Vertical,
		func(gtx layout.Context) layout.Dimensions {
			// Reduce vertical space available to body to provide space for the horizontal scrollbar.
			gtx.Constraints.Min.Y -= gtx.Dp(scrollbarWidth)
			gtx.Constraints.Max.Y -= gtx.Dp(scrollbarWidth)
			gtx.Constraints = layout.Normalize(gtx.Constraints)
			{
				// Body
				gtx := gtx
				// Reduce horizontal space available to body to provide space for vertical scrollbar
				gtx.Constraints.Min.X -= gtx.Dp(scrollbarWidth)
				gtx.Constraints.Max.X -= gtx.Dp(scrollbarWidth)
				gtx.Constraints = layout.Normalize(gtx.Constraints)
				min := gtx.Constraints.Min
				bodyDims = tbl.state.horizList.Layout(gtx, 1, func(gtx layout.Context, index int) layout.Dimensions {
					gtx.Constraints.Min = min
					tbl.state.rememberingList.list = &tbl.state.vertList
					return body(win, gtx, &tbl.state.rememberingList)
				})
			}

			{
				// Draw vertical scrollbar at the right edge.
				defer op.Offset(image.Pt(gtx.Constraints.Max.X-gtx.Dp(scrollbarWidth), 0)).Push(gtx.Ops).Pop()
				l, h := FromListPosition(tbl.state.vertList.Position, tbl.state.rememberingList.len, tbl.state.rememberingList.dims.Size.Y)
				Scrollbar(win.Theme, &tbl.state.vertScroll).Layout(win, gtx, layout.Vertical, l, h)
				if delta := tbl.state.vertScroll.ScrollDistance(); delta != 0 {
					tbl.state.vertList.ScrollBy(delta * float32(tbl.state.rememberingList.len))
				}
			}

			return layout.Dimensions{
				Size: image.Pt(gtx.Constraints.Max.X, bodyDims.Size.Y),
			}
		},

		func(gtx layout.Context) layout.Dimensions {
			// Horizontal scrollbar
			// Horizontal scrollbar should end before the start of the vertical scrollbar.
			gtx.Constraints.Min.X -= gtx.Dp(scrollbarWidth)
			gtx.Constraints.Max.X -= gtx.Dp(scrollbarWidth)
			gtx.Constraints = layout.Normalize(gtx.Constraints)
			l, h := FromListPosition(tbl.state.horizList.Position, 1, bodyDims.Size.X)
			dims := Scrollbar(win.Theme, &tbl.state.horizScroll).Layout(win, gtx, layout.Horizontal, l, h)
			if delta := tbl.state.horizScroll.ScrollDistance(); delta != 0 {
				tbl.state.horizList.ScrollBy(delta)
			}
			return dims
		},
	)
}

type TableHeaderRowStyle struct {
	Table *Table
}

func TableHeaderRow(tbl *Table) TableHeaderRowStyle {
	return TableHeaderRowStyle{Table: tbl}
}

func (row TableHeaderRowStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TableHeaderRowStyle.Layout").End()

	return TableRow(row.Table, true).Layout(win, gtx, func(win *Window, gtx layout.Context, colIdx int) layout.Dimensions {
		var (
			f          = font.Font{Weight: font.ExtraBold}
			lineHeight = win.TextDimensions(gtx, widget.Label{}, f, win.Theme.TextSize, "").Size.Y
			height     = max(gtx.Constraints.Min.Y, lineHeight+2*gtx.Dp(DefaultHeaderPadding)+gtx.Dp(DefaultHeaderBorder))
			col        = &row.Table.Columns[colIdx]
		)

		FillShape(win, gtx.Ops, win.Theme.Palette.Table.HeaderBackground, clip.Rect{Max: image.Pt(gtx.Constraints.Min.X, height)}.Op())

		layout.Overlay(gtx,
			func(gtx layout.Context) layout.Dimensions {
				return layout.Inset{
					Top:    DefaultHeaderPadding,
					Bottom: DefaultHeaderPadding + DefaultHeaderBorder,
					Left:   DefaultHeaderPadding,
					Right:  DefaultHeaderPadding,
				}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					l := widget.Label{MaxLines: 1, Alignment: text.Start}
					l.Alignment = col.Alignment
					var s string
					// OPT(dh): avoid allocations for string building by precomputing and storing the column headers.
					if row.Table.SortedBy == colIdx {
						switch row.Table.SortOrder {
						case SortNone:
							s = col.Name
						case SortAscending:
							s = "▲" + col.Name
						case SortDescending:
							s = "▼" + col.Name
						default:
							panic(fmt.Sprintf("unhandled case %v", row.Table.SortOrder))
						}
					} else {
						s = col.Name
					}

					return l.Layout(gtx, win.Theme.Shaper, f, win.Theme.TextSize, s, win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
				})
			},

			func(gtx layout.Context) layout.Dimensions {
				if col.Clickable {
					defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
					row.Table.headerClicks[colIdx].Add(gtx.Ops)
					pointer.CursorPointer.Add(gtx.Ops)
				}
				return layout.Dimensions{
					Size: gtx.Constraints.Min,
				}
			},
		)

		FillShape(
			win,
			gtx.Ops,
			win.Theme.Palette.Table.Divider,
			clip.Rect{
				Min: image.Pt(0, height-gtx.Dp(DefaultHeaderBorder)),
				Max: image.Pt(gtx.Constraints.Min.X, height),
			}.Op())
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

func (row TableSimpleRowStyle) Layout(
	win *Window,
	gtx layout.Context,
	rowIdx int,
	cellFn CellFn,
) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TableSimpleRowStyle.Layout").End()

	c := win.Theme.Palette.Table.EvenRowBackground
	if rowIdx%2 == 1 {
		c = win.Theme.Palette.Table.OddRowBackground
	}

	if rowIdx >= row.Table.rowHovers.Len() {
		row.Table.rowHovers.GrowN(rowIdx - row.Table.rowHovers.Len() + 1)
	}
	hover := row.Table.rowHovers.Ptr(rowIdx)
	hover.Update(gtx.Queue)
	if hover.Update(gtx.Queue) {
		c = win.Theme.Palette.Table.HoveredRowBackground
	}

	return layout.Overlay(gtx,
		func(gtx layout.Context) layout.Dimensions {
			return Background{Color: c}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
				return TableRow(row.Table, false).Layout(win, gtx, func(win *Window, gtx layout.Context, col int) layout.Dimensions {
					defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

					const padding = 3
					dims := layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						gtx.Constraints.Min.Y = 0
						return cellFn(win, gtx, rowIdx, col)
					})

					return layout.Dimensions{
						Size: gtx.Constraints.Constrain(dims.Size),
					}
				})
			})
		},
		func(gtx layout.Context) layout.Dimensions {
			defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
			hover.Add(gtx.Ops)
			return layout.Dimensions{
				Size: gtx.Constraints.Min,
			}
		},
	)
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
	defer rtrace.StartRegion(context.Background(), "theme.TableExpandedRowStyle.Layout").End()

	return layout.Rigids(gtx, layout.Vertical,
		func(gtx layout.Context) layout.Dimensions {
			size := image.Pt(gtx.Constraints.Max.X, gtx.Dp(DefaultExpandedBorder))
			FillShape(win, gtx.Ops, win.Theme.Palette.Table.ExpandedBorder, clip.Rect{Max: size}.Op())
			return layout.Dimensions{
				Size: size,
			}
		},

		func(gtx layout.Context) layout.Dimensions {
			return FauxTableRow(ex.Table, win.Theme.Palette.Background).Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
				return Background{
					Color: win.Theme.Palette.Table.ExpandedBackground,
				}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
					return w(win, gtx)
				})
			})
		},

		func(gtx layout.Context) layout.Dimensions {
			size := image.Pt(gtx.Constraints.Max.X, gtx.Dp(DefaultExpandedBorder))
			FillShape(win, gtx.Ops, win.Theme.Palette.Table.ExpandedBorder, clip.Rect{Max: size}.Op())
			return layout.Dimensions{
				Size: size,
			}
		},
	)
}

func SimpleTable(
	win *Window,
	gtx layout.Context,
	tbl *Table,
	scroll *YScrollableListState,
	nrows int,
	cellFn CellFn,
) layout.Dimensions {
	return FairlySimpleTable(
		win,
		gtx,
		tbl,
		scroll,
		nrows,
		func(win *Window, gtx layout.Context, row int) layout.Dimensions {
			return TableSimpleRow(tbl).Layout(win, gtx, row, cellFn)
		},
	)
}

func FairlySimpleTable(
	win *Window,
	gtx layout.Context,
	tbl *Table,
	scroll *YScrollableListState,
	nrows int,
	rowFn RowFn,
) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.FairlySimpleTable").End()

	return tbl.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		return YScrollableList(scroll).Layout(win, gtx, func(win *Window, gtx layout.Context, list *RememberingList) layout.Dimensions {
			return layout.Rigids(gtx, layout.Vertical,
				func(gtx layout.Context) layout.Dimensions {
					return TableHeaderRow(tbl).Layout(win, gtx)
				},

				func(gtx layout.Context) layout.Dimensions {
					return list.Layout(gtx, nrows, func(gtx layout.Context, row int) layout.Dimensions {
						return rowFn(win, gtx, row)
					})
				},
			)
		})
	})
}

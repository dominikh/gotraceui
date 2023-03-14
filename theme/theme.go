package theme

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"math"
	rtrace "runtime/trace"

	mylayout "honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/x/component"
	"gioui.org/x/outlay"
)

type Theme struct {
	Shaper        *text.Shaper
	Palette       Palette
	TextSize      unit.Sp
	TextSizeLarge unit.Sp

	WindowPadding unit.Dp
	WindowBorder  unit.Dp
}

type Palette struct {
	Background       color.NRGBA
	Foreground       color.NRGBA
	Link             color.NRGBA
	PrimarySelection color.NRGBA

	Border color.NRGBA

	Popup struct {
		TitleForeground color.NRGBA
		TitleBackground color.NRGBA
		Background      color.NRGBA
	}

	Menu struct {
		Background color.NRGBA
		Selected   color.NRGBA
		Border     color.NRGBA
		Disabled   color.NRGBA
	}
}

var DefaultPalette = Palette{
	Background:       rgba(0xFFFFEAFF),
	Foreground:       rgba(0x000000FF),
	Link:             rgba(0x0000FFFF),
	PrimarySelection: rgba(0xeeee9e99),
	Border:           rgba(0x000000FF),

	Popup: struct {
		TitleForeground color.NRGBA
		TitleBackground color.NRGBA
		Background      color.NRGBA
	}{
		TitleForeground: rgba(0x000000FF),
		TitleBackground: rgba(0xEFFFFFFF),
		Background:      rgba(0xEEFFEEFF),
	},

	Menu: struct {
		Background color.NRGBA
		Selected   color.NRGBA
		Border     color.NRGBA
		Disabled   color.NRGBA
	}{
		Background: rgba(0xEFFFFFFF),
		Selected:   rgba(0x9CEFEFFF),
		Border:     rgba(0x9CEFEFFF),
		Disabled:   rgba(0xAAAAAAFF),
	},
}

func NewTheme(fontCollection []text.FontFace) *Theme {
	return &Theme{
		Palette:       DefaultPalette,
		Shaper:        text.NewShaper(fontCollection),
		TextSize:      12,
		TextSizeLarge: 14,

		WindowPadding: 2,
		WindowBorder:  1,
	}
}

type ProgressBarStyle struct {
	ForegroundColor color.NRGBA
	BackgroundColor color.NRGBA
	BorderWidth     unit.Dp
	Progress        float32
}

func ProgressBar(th *Theme, progress float32) ProgressBarStyle {
	return ProgressBarStyle{
		ForegroundColor: rgba(0x478847FF),
		BackgroundColor: rgba(0),
		BorderWidth:     1,
		Progress:        progress,
	}
}

func (p ProgressBarStyle) Layout(gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ProgressBarStyle.Layout").End()

	return widget.Border{
		Color: p.ForegroundColor,
		Width: p.BorderWidth,
	}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		// Draw background
		bg := clip.Rect{Max: gtx.Constraints.Min}.Op()
		paint.FillShape(gtx.Ops, p.BackgroundColor, bg)

		// Draw foreground
		fg := frect{Max: f32.Pt(float32(gtx.Constraints.Min.X)*p.Progress, float32(gtx.Constraints.Min.Y))}.Op(gtx.Ops)
		paint.FillShape(gtx.Ops, p.ForegroundColor, fg)

		return layout.Dimensions{
			Size: gtx.Constraints.Min,
		}
	})
}

type CheckBoxStyle struct {
	Checkbox        *widget.Bool
	Label           string
	TextSize        unit.Sp
	ForegroundColor color.NRGBA
	BackgroundColor color.NRGBA
	TextColor       color.NRGBA
}

func CheckBox(th *Theme, checkbox *widget.Bool, label string) CheckBoxStyle {
	return CheckBoxStyle{
		Checkbox:        checkbox,
		Label:           label,
		TextColor:       th.Palette.Foreground,
		ForegroundColor: th.Palette.Foreground,
		BackgroundColor: rgba(0),
		TextSize:        th.TextSize,
	}
}

func (c CheckBoxStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.CheckBoxStyle.Layout").End()

	return c.Checkbox.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				sizeDp := gtx.Metric.SpToDp(c.TextSize)
				sizePx := gtx.Dp(sizeDp)

				ngtx := gtx
				ngtx.Constraints = layout.Exact(image.Pt(sizePx, sizePx))
				return widget.Border{
					Color: c.ForegroundColor,
					Width: 1,
				}.Layout(ngtx, func(gtx layout.Context) layout.Dimensions {
					paint.FillShape(gtx.Ops, c.BackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
					if c.Checkbox.Value {
						padding := gtx.Constraints.Min.X / 4
						if padding == 0 {
							padding = gtx.Dp(1)
						}
						minx := padding
						miny := minx
						maxx := gtx.Constraints.Min.X - padding
						maxy := maxx
						paint.FillShape(gtx.Ops, c.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
					}

					return layout.Dimensions{Size: gtx.Constraints.Min}
				})
			}),

			layout.Rigid(layout.Spacer{Width: 3}.Layout),

			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				return widget.TextLine{Color: c.TextColor}.Layout(gtx, win.Theme.Shaper, text.Font{}, c.TextSize, c.Label)
			}),
		)
	})
}

func rgba(c uint32) color.NRGBA {
	// XXX does endianness matter?
	return color.NRGBA{
		A: uint8(c & 0xFF),
		B: uint8(c >> 8 & 0xFF),
		G: uint8(c >> 16 & 0xFF),
		R: uint8(c >> 24 & 0xFF),
	}
}

type frect struct {
	Min f32.Point
	Max f32.Point
}

func (r frect) Path(ops *op.Ops) clip.PathSpec {
	var p clip.Path
	p.Begin(ops)
	r.IntoPath(&p)
	return p.End()
}

func (r frect) IntoPath(p *clip.Path) {
	p.MoveTo(r.Min)
	p.LineTo(f32.Pt(r.Max.X, r.Min.Y))
	p.LineTo(r.Max)
	p.LineTo(f32.Pt(r.Min.X, r.Max.Y))
	p.LineTo(r.Min)
}

func (r frect) Op(ops *op.Ops) clip.Op {
	return clip.Outline{Path: r.Path(ops)}.Op()
}

func max(a, b int) int {
	if a >= b {
		return a
	} else {
		return b
	}
}

// clamp1 limits v to range [0..1].
func clamp1(v float32) float32 {
	if v >= 1 {
		return 1
	} else if v <= 0 {
		return 0
	} else {
		return v
	}
}

type FoldableStyle struct {
	Title  string
	Closed *widget.Bool

	TextSize  unit.Sp
	TextColor color.NRGBA
}

func Foldable(th *Theme, b *widget.Bool, title string) FoldableStyle {
	return FoldableStyle{
		Closed: b,
		Title:  title,

		TextSize:  th.TextSize,
		TextColor: th.Palette.Foreground,
	}
}

func (f FoldableStyle) Layout(win *Window, gtx layout.Context, contents Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.Foldable.Layout").End()

	var size image.Point
	dims := f.Closed.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		// TODO(dh): show an icon indicating state of the foldable. We tried using ▶ and ▼ but the Go font only has ▼…
		var l string
		if f.Closed.Value {
			l = "[C] " + f.Title
		} else {
			l = "[O] " + f.Title
		}
		gtx.Constraints.Min.Y = 0
		paint.ColorOp{Color: f.TextColor}.Add(gtx.Ops)
		pointer.CursorPointer.Add(gtx.Ops)
		return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, f.TextSize, l)

	})
	size = dims.Size

	if !f.Closed.Value {
		defer op.Offset(image.Pt(0, size.Y)).Push(gtx.Ops).Pop()
		gtx.Constraints.Max.Y -= size.Y
		dims := contents(win, gtx)

		max := func(a, b int) int {
			if a >= b {
				return a
			} else {
				return b
			}
		}
		size.X = max(size.X, dims.Size.X)
		size.Y += dims.Size.Y
	}

	size = gtx.Constraints.Constrain(size)
	return layout.Dimensions{Size: size}
}

type TooltipStyle struct {
	Message string

	Padding         unit.Dp
	BorderSize      unit.Dp
	BorderColor     color.NRGBA
	TextSize        unit.Sp
	TextColor       color.NRGBA
	BackgroundColor color.NRGBA
}

func Tooltip(th *Theme, msg string) TooltipStyle {
	return TooltipStyle{
		Message:         msg,
		BorderSize:      th.WindowBorder,
		BorderColor:     th.Palette.Border,
		Padding:         th.WindowPadding,
		TextSize:        th.TextSize,
		TextColor:       th.Palette.Foreground,
		BackgroundColor: th.Palette.Popup.Background,
	}
}

func (tt TooltipStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.Tooltip.Layout").End()

	return BorderedTextStyle{
		Text:            tt.Message,
		Padding:         tt.Padding,
		BorderSize:      tt.BorderSize,
		BorderColor:     tt.BorderColor,
		TextSize:        tt.TextSize,
		TextColor:       tt.TextColor,
		BackgroundColor: tt.BackgroundColor,
	}.Layout(win, gtx)
}

type BorderedTextStyle struct {
	Text string

	Padding         unit.Dp
	BorderSize      unit.Dp
	BorderColor     color.NRGBA
	TextSize        unit.Sp
	TextColor       color.NRGBA
	BackgroundColor color.NRGBA
}

func BorderedText(th *Theme, s string) BorderedTextStyle {
	return BorderedTextStyle{
		Text:            s,
		BorderSize:      th.WindowBorder,
		BorderColor:     th.Palette.Border,
		Padding:         th.WindowPadding,
		TextSize:        th.TextSize,
		TextColor:       th.Palette.Foreground,
		BackgroundColor: th.Palette.Popup.Background,
	}
}

func (bt BorderedTextStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	return widget.Bordered{Color: bt.BorderColor, Width: bt.BorderSize}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		// Don't inherit the minimum constraint from the parent widget. In this specific case, this widget is being
		// rendered as part of a flex child.
		gtx.Constraints.Min = image.Pt(0, 0)
		var padding = gtx.Dp(bt.Padding)

		macro := op.Record(gtx.Ops)
		paint.ColorOp{Color: bt.TextColor}.Add(gtx.Ops)
		dims := widget.Label{}.Layout(gtx, win.Theme.Shaper, text.Font{}, bt.TextSize, bt.Text)
		call := macro.Stop()

		total := clip.Rect{
			Min: image.Pt(0, 0),
			Max: image.Pt(dims.Size.X+2*padding, dims.Size.Y+2*padding),
		}

		paint.FillShape(gtx.Ops, bt.BackgroundColor, total.Op())

		stack := op.Offset(image.Pt(padding, padding)).Push(gtx.Ops)
		call.Add(gtx.Ops)
		stack.Pop()

		return layout.Dimensions{
			Baseline: dims.Baseline,
			Size:     total.Max,
		}
	})
}

type ButtonStyle struct {
	Text   string
	Button *widget.Clickable

	ActiveBackgroundColor color.NRGBA
	BackgroundColor       color.NRGBA
	BorderColor           color.NRGBA
	TextColor             color.NRGBA
}

func Button(th *Theme, button *widget.Clickable, txt string) ButtonStyle {
	return ButtonStyle{
		Text:                  txt,
		Button:                button,
		ActiveBackgroundColor: rgba(0xFFFF00FF),
		BackgroundColor:       rgba(0xFFFFFFFF),
		BorderColor:           th.Palette.Border,
		TextColor:             th.Palette.Foreground,
	}
}

func (b ButtonStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ButtonStyle.Layout").End()

	return b.Button.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return widget.Bordered{Color: b.BorderColor, Width: 1}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return layout.Stack{Alignment: layout.Center}.Layout(gtx,
				layout.Expanded(func(gtx layout.Context) layout.Dimensions {
					if b.Button.Pressed(pointer.ButtonPrimary) {
						paint.FillShape(gtx.Ops, b.ActiveBackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
					} else {
						paint.FillShape(gtx.Ops, b.BackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
					}
					return layout.Dimensions{Size: gtx.Constraints.Min}
				}),
				layout.Stacked(func(gtx layout.Context) layout.Dimensions {
					paint.ColorOp{Color: b.TextColor}.Add(gtx.Ops)
					return widget.Label{Alignment: text.Middle}.Layout(gtx, win.Theme.Shaper, text.Font{}, 12, b.Text)
				}),
			)
		})
	})
}

type GridStyle struct {
	State           *component.GridState
	VScrollbarStyle ScrollbarStyle
	HScrollbarStyle ScrollbarStyle
	// material.AnchorStrategy
}

// Grid makes a grid with its persistent state.
func Grid(th *Theme, state *component.GridState) GridStyle {
	return GridStyle{
		State:           state,
		VScrollbarStyle: Scrollbar(th, &state.VScrollbar),
		HScrollbarStyle: Scrollbar(th, &state.HScrollbar),
	}
}

// Layout will draw a grid, using fixed column widths and row height.
func (g GridStyle) Layout(gtx layout.Context, rows, cols int, dimensioner outlay.Dimensioner, cellFunc outlay.Cell) layout.Dimensions {
	// Determine how much space the scrollbars occupy when present.
	hBarWidth := gtx.Dp(g.HScrollbarStyle.Width())
	vBarWidth := gtx.Dp(g.VScrollbarStyle.Width())

	// Reserve space for the scrollbars using the gtx constraints.
	gtx.Constraints.Max.X -= vBarWidth
	gtx.Constraints.Max.Y -= hBarWidth
	gtx.Constraints = mylayout.Normalize(gtx.Constraints)

	defer pointer.PassOp{}.Push(gtx.Ops).Pop()
	// Draw grid.
	dim := g.State.Grid.Layout(gtx, rows, cols, dimensioner, cellFunc)

	// Calculate column widths in pixels. Width is sum of widths.
	totalWidth := g.State.Horizontal.Length
	totalHeight := g.State.Vertical.Length

	// Make the scroll bar stick to the grid.
	if gtx.Constraints.Max.X > dim.Size.X {
		gtx.Constraints.Max.X = dim.Size.X
		gtx.Constraints.Max.X += vBarWidth
	}

	// Get horizontal scroll info.
	delta := g.HScrollbarStyle.Scrollbar.ScrollDistance()
	if delta != 0 {
		g.State.Horizontal.Offset += int(float32(totalWidth-vBarWidth) * delta)
	}

	// Get vertical scroll info.
	delta = g.VScrollbarStyle.Scrollbar.ScrollDistance()
	if delta != 0 {
		g.State.Vertical.Offset += int(math.Round(float64(float32(totalHeight-hBarWidth) * delta)))
	}

	var start float32
	var end float32

	// Draw vertical scroll-bar.
	if vBarWidth > 0 {
		c := gtx
		start = float32(g.State.Vertical.OffsetAbs) / float32(totalHeight)
		end = start + float32(c.Constraints.Max.Y)/float32(totalHeight)
		c.Constraints.Min = c.Constraints.Max
		c.Constraints.Min.X += vBarWidth
		layout.E.Layout(c, func(gtx layout.Context) layout.Dimensions {
			return g.VScrollbarStyle.Layout(gtx, layout.Vertical, start, end)
		})
	}

	// Draw horizontal scroll-bar if it is visible.
	if hBarWidth > 0 {
		c := gtx
		start = float32(g.State.Horizontal.OffsetAbs) / float32(totalWidth)
		end = start + float32(c.Constraints.Max.X)/float32(totalWidth)
		c.Constraints.Min = c.Constraints.Max
		c.Constraints.Min.Y += hBarWidth
		layout.S.Layout(c, func(gtx layout.Context) layout.Dimensions {
			return g.HScrollbarStyle.Layout(gtx, layout.Horizontal, start, end)
		})
	}
	dim.Size.Y += hBarWidth

	return dim
}

type ResizeStyle struct {
	res         *component.Resize
	BorderColor color.NRGBA
}

func Resize(th *Theme, state *component.Resize) ResizeStyle {
	return ResizeStyle{
		res:         state,
		BorderColor: th.Palette.Border,
	}
}

func (rs ResizeStyle) Layout(win *Window, gtx layout.Context, w1, w2 Widget) layout.Dimensions {
	var hnd layout.Widget
	switch rs.res.Axis {
	case layout.Horizontal:
		hnd = func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Max.X = 5
			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
			paint.Fill(gtx.Ops, rs.BorderColor)
			return layout.Dimensions{Size: gtx.Constraints.Max}
		}
	case layout.Vertical:
		hnd = func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Max.Y = 5
			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
			paint.Fill(gtx.Ops, rs.BorderColor)
			return layout.Dimensions{Size: gtx.Constraints.Max}
		}
	default:
		panic(fmt.Sprintf("unhandled case %v", rs.res.Axis))
	}
	return rs.res.Layout(gtx, Dumb(win, w1), Dumb(win, w2), hnd)
}

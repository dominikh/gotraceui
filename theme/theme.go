package theme

import (
	"context"
	"fmt"
	"image"
	"image/color"
	"math"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/x/component"
	"gioui.org/x/outlay"
	"golang.org/x/exp/slices"
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
	Background         color.NRGBA
	Foreground         color.NRGBA
	ForegroundDisabled color.NRGBA
	Link               color.NRGBA
	PrimarySelection   color.NRGBA

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
	Background:         rgba(0xFFFFEAFF),
	Foreground:         rgba(0x000000FF),
	ForegroundDisabled: rgba(0x727272FF),
	Link:               rgba(0x0000FFFF),
	PrimarySelection:   rgba(0xeeee9e99),
	Border:             rgba(0x000000FF),

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

func NewTheme(fontCollection []font.FontFace) *Theme {
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
	Checkbox        widget.Boolean
	Label           string
	TextSize        unit.Sp
	ForegroundColor color.NRGBA
	BackgroundColor color.NRGBA
	TextColor       color.NRGBA
}

func CheckBox(th *Theme, checkbox widget.Boolean, label string) CheckBoxStyle {
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
					if c.Checkbox.Get() {
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
				return widget.TextLine{Color: c.TextColor}.Layout(gtx, win.Theme.Shaper, font.Font{}, c.TextSize, c.Label)
			}),
		)
	})
}

type CheckBoxGroupStyle struct {
	Clickable       *widget.Clickable
	Label           string
	TextSize        unit.Sp
	ForegroundColor color.NRGBA
	BackgroundColor color.NRGBA
	TextColor       color.NRGBA
}

func CheckBoxGroup(th *Theme, clickable *widget.Clickable, label string) CheckBoxGroupStyle {
	return CheckBoxGroupStyle{
		Clickable:       clickable,
		Label:           label,
		TextColor:       th.Palette.Foreground,
		ForegroundColor: th.Palette.Foreground,
		BackgroundColor: rgba(0),
		TextSize:        th.TextSize,
	}
}

func (chkgrp CheckBoxGroupStyle) Layout(win *Window, gtx layout.Context, checkboxes ...CheckBoxStyle) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.CheckBoxGroupStyle.Layout").End()

	const (
		none = iota
		noneThenSome
		some
		all
	)

	sm := [...][2]int{
		none:         {0: noneThenSome, 1: all},
		all:          {0: some, 1: all},
		some:         {0: some, 1: some},
		noneThenSome: {0: noneThenSome, 1: some},
	}

	var state int

	for i := range checkboxes {
		chk := &checkboxes[i]

		var b int
		if chk.Checkbox.Get() {
			b = 1
		}
		state = sm[state][b]
		if state == some {
			break
		}
	}

	if state == noneThenSome {
		state = none
	}

	sizeDp := gtx.Metric.SpToDp(chkgrp.TextSize)
	sizePx := gtx.Dp(sizeDp)

	dims := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return chkgrp.Clickable.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						ngtx := gtx
						ngtx.Constraints = layout.Exact(image.Pt(sizePx, sizePx))
						return widget.Border{
							Color: chkgrp.ForegroundColor,
							Width: 1,
						}.Layout(ngtx, func(gtx layout.Context) layout.Dimensions {
							paint.FillShape(gtx.Ops, chkgrp.BackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
							switch state {
							case none:
							case some:
								padding := gtx.Constraints.Min.X / 4
								if padding == 0 {
									padding = gtx.Dp(1)
								}
								minx := padding
								maxx := gtx.Constraints.Min.X - padding

								miny := minx + padding/2
								maxy := maxx - padding/2

								paint.FillShape(gtx.Ops, chkgrp.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
							case all:
								padding := gtx.Constraints.Min.X / 4
								if padding == 0 {
									padding = gtx.Dp(1)
								}
								minx := padding
								miny := minx
								maxx := gtx.Constraints.Min.X - padding
								maxy := maxx
								paint.FillShape(gtx.Ops, chkgrp.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
							}

							return layout.Dimensions{Size: gtx.Constraints.Min}
						})
					}),

					layout.Rigid(layout.Spacer{Width: 3}.Layout),

					layout.Rigid(func(gtx layout.Context) layout.Dimensions {
						return widget.TextLine{Color: chkgrp.TextColor}.Layout(gtx, win.Theme.Shaper, font.Font{}, chkgrp.TextSize, chkgrp.Label)
					}),
				)
			})
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			var children []layout.FlexChild
			if len(checkboxes) <= 16 {
				// Specifying a constant capacity allows the slice to be stack-allocated.
				children = make([]layout.FlexChild, len(checkboxes), 16)
			} else {
				children = make([]layout.FlexChild, len(checkboxes))
			}
			for i := range checkboxes {
				children[i] = layout.Rigid(Dumb(win, checkboxes[i].Layout))
			}
			return layout.Inset{Left: sizeDp + 3}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return layout.Flex{Axis: layout.Vertical}.Layout(gtx, children...)
			})
		}),
	)

	for {
		click, ok := chkgrp.Clickable.Clicked()
		if !ok {
			break
		}
		if click.Button != pointer.ButtonPrimary {
			continue
		}
		for i := range checkboxes {
			checkboxes[i].Checkbox.Set(state == none || state == some)
		}
	}

	return dims
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
		pointer.CursorPointer.Add(gtx.Ops)
		return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, f.TextSize, l, widget.ColorTextMaterial(gtx, f.TextColor))

	})
	size = dims.Size

	if !f.Closed.Value {
		gtx := gtx
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
	defer rtrace.StartRegion(context.Background(), "theme.BorderedTextStyle.Layout").End()

	return widget.Bordered{Color: bt.BorderColor, Width: bt.BorderSize}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		// Don't inherit the minimum constraint from the parent widget. In this specific case, this widget is being
		// rendered as part of a flex child.
		gtx.Constraints.Min = image.Pt(0, 0)
		var padding = gtx.Dp(bt.Padding)

		macro := op.Record(gtx.Ops)
		dims := widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, bt.TextSize, bt.Text, widget.ColorTextMaterial(gtx, bt.TextColor))
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
	TextColorDisabled     color.NRGBA
}

func Button(th *Theme, button *widget.Clickable, txt string) ButtonStyle {
	return ButtonStyle{
		Text:                  txt,
		Button:                button,
		ActiveBackgroundColor: rgba(0xDDDDFFFF),
		BackgroundColor:       rgba(0xFFFFFFFF),
		BorderColor:           th.Palette.Border,
		TextColor:             th.Palette.Foreground,
		TextColorDisabled:     th.Palette.ForegroundDisabled,
	}
}

func (b ButtonStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ButtonStyle.Layout").End()

	var bg color.NRGBA
	if b.Button.Pressed(pointer.ButtonPrimary) {
		bg = b.ActiveBackgroundColor
	} else {
		bg = b.BackgroundColor
	}

	var fg color.NRGBA
	if gtx.Queue != nil {
		fg = b.TextColor
	} else {
		fg = b.TextColorDisabled
	}

	return widget.Background{Color: bg}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return b.Button.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return widget.Bordered{Color: b.BorderColor, Width: 1}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return layout.UniformInset(1).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return widget.Label{Alignment: text.Middle}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, b.Text, widget.ColorTextMaterial(gtx, fg))
				})
			})
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
	defer rtrace.StartRegion(context.Background(), "theme.GridStyle.Layout").End()

	// Determine how much space the scrollbars occupy when present.
	hBarWidth := gtx.Dp(g.HScrollbarStyle.Width())
	vBarWidth := gtx.Dp(g.VScrollbarStyle.Width())

	// Reserve space for the scrollbars using the gtx constraints.
	gtx.Constraints.Max.X -= vBarWidth
	gtx.Constraints.Max.Y -= hBarWidth
	gtx.Constraints = layout.Normalize(gtx.Constraints)

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
	defer rtrace.StartRegion(context.Background(), "theme.ResizeStyle.Layout").End()

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

type SwitchStyle struct {
	State       widget.Boolean
	Left, Right string
}

func Switch(b widget.Boolean, left, right string) SwitchStyle {
	return SwitchStyle{State: b, Left: left, Right: right}
}

func (ss SwitchStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.SwitchStyle.Layout").End()

	noMin := func(gtx layout.Context) layout.Context {
		gtx.Constraints.Min = image.Point{}
		return gtx
	}

	const padding = 5
	const borderWidth = 1

	activeForeground := widget.ColorTextMaterial(gtx, rgba(0x000000FF))
	inactiveForeground := widget.ColorTextMaterial(gtx, rgba(0x3E3E3EFF))

	activeBackground := rgba(0xDDDDFFFF)
	inactiveBackground := rgba(0xFFFFFFFF)

	activeFont := font.Font{Weight: font.Bold}
	inactiveFont := font.Font{}

	activeBorder := rgba(0xAAAAFFFF)
	inactiveBorder := rgba(0xBBBBBBFF)

	var (
		leftForeground, rightForeground = activeForeground, inactiveForeground
		leftBackground, rightBackground = activeBackground, inactiveBackground
		leftFont, rightFont             = activeFont, inactiveFont
		leftBorder, rightBorder         = activeBorder, inactiveBorder
	)

	if ss.State.Get() {
		leftForeground, rightForeground = rightForeground, leftForeground
		leftBackground, rightBackground = rightBackground, leftBackground
		leftFont, rightFont = rightFont, leftFont
		leftBorder, rightBorder = rightBorder, leftBorder
	}

	// Compute the sizes of the labels. We use the active font for both under the assumption that the active font
	// results in dimensions >= the inactive font. If we used the actual fonts (one active and one inactive) then the
	// overall size might change when toggling the switch.
	m := op.Record(gtx.Ops)
	dimsLeft := widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, activeFont, 12, ss.Left, leftForeground)
	m.Stop()

	m = op.Record(gtx.Ops)
	dimsRight := widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, activeFont, 12, ss.Right, rightForeground)
	m.Stop()

	var labelWidth int
	if dimsLeft.Size.X >= dimsRight.Size.X {
		labelWidth = dimsLeft.Size.X
	} else {
		labelWidth = dimsRight.Size.X
	}

	minWidth := 2*labelWidth + 4*gtx.Dp(padding) + gtx.Dp(borderWidth)
	gtx.Constraints.Max.X = gtx.Constraints.Constrain(image.Pt(minWidth, 0)).X

	// FIXME(dh): the way we "remove" parts of the border by drawing over it assumes that our background colors have no
	// transparency.

	return ss.State.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				dims := widget.Border{Color: leftBorder, Width: borderWidth}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return widget.Background{Color: leftBackground}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
							return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
								return widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, leftFont, 12, ss.Left, leftForeground)
							})
						})
					})
				})

				// Remove right part of the border
				paint.FillShape(gtx.Ops, leftBackground, clip.Rect{Min: image.Pt(dims.Size.X-gtx.Dp(borderWidth), gtx.Dp(borderWidth)), Max: image.Pt(dims.Size.X, dims.Size.Y-gtx.Dp(borderWidth))}.Op())

				return dims
			}),

			// The left and right buttons share a single border in the middle.
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				width := gtx.Dp(borderWidth)
				height := dimsLeft.Size.Y + gtx.Dp(padding)*2
				size := image.Pt(width, height)
				paint.FillShape(gtx.Ops, activeBorder, clip.Rect{Max: size}.Op())
				return layout.Dimensions{Size: size}
			}),

			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				dims := widget.Border{Color: rightBorder, Width: borderWidth}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return widget.Background{Color: rightBackground}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
							return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
								return widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, rightFont, 12, ss.Right, rightForeground)
							})
						})
					})
				})

				// Remove left part of the border
				paint.FillShape(gtx.Ops, rightBackground, clip.Rect{Min: image.Pt(0, gtx.Dp(borderWidth)), Max: image.Pt(gtx.Dp(borderWidth), dims.Size.Y-gtx.Dp(borderWidth))}.Op())

				return dims
			}),
		)
	})
}

type TabbedState struct {
	Current    int
	clickables []widget.PrimaryClickable
}

type TabbedStyle struct {
	State *TabbedState
	Tabs  []string
}

func Tabbed(state *TabbedState, tabs []string) TabbedStyle {
	return TabbedStyle{
		State: state,
		Tabs:  tabs,
	}
}

func (ts TabbedStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TabbedStyle.Layout").End()

	if ts.State.Current >= len(ts.Tabs) {
		ts.State.Current = len(ts.Tabs) - 1
	}
	if len(ts.State.clickables) < len(ts.Tabs) {
		ts.State.clickables = slices.Grow(ts.State.clickables, len(ts.Tabs))[:len(ts.Tabs)]
	}

	const padding = 5
	const lineThickness = 1
	const activeLineThickness = 3

	dims := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		// Tabs
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Min = image.Point{}
			var lineHeight int
			for i, tab := range ts.Tabs {
				dims := ts.State.clickables[i].Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					stack := op.Offset(image.Pt(gtx.Dp(padding), 0)).Push(gtx.Ops)

					dims := widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, tab, widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
					stack.Pop()

					dims.Size.X += 2 * gtx.Dp(padding)
					dims.Size.Y += gtx.Dp(padding)

					if i == ts.State.Current {
						x0 := 0
						x1 := dims.Size.X
						y0 := dims.Size.Y - gtx.Dp(activeLineThickness)
						y1 := y0 + gtx.Dp(activeLineThickness)
						paint.FillShape(gtx.Ops, rgba(0x9696FFFF), clip.Rect{Min: image.Pt(x0, y0), Max: image.Pt(x1, y1)}.Op())
					}

					return dims
				})

				defer op.Offset(image.Pt(dims.Size.X, 0)).Push(gtx.Ops).Pop()
				lineHeight = dims.Size.Y
			}

			// The X size is bogus, but nobody cares.
			return layout.Dimensions{Size: image.Pt(0, lineHeight)}
		}),

		// Line
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			r := clip.Rect{Max: image.Pt(gtx.Constraints.Min.X, gtx.Dp(lineThickness))}
			paint.FillShape(gtx.Ops, rgba(0x000000FF), r.Op())
			return layout.Dimensions{Size: r.Max}
		}),

		// Padding
		layout.Rigid(func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: padding}.Layout(gtx) }),

		// Content
		layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
			return w(win, gtx)
		}),
	)

	for i := range ts.State.clickables {
		for ts.State.clickables[i].Clicked() {
			ts.State.Current = i
		}
	}

	return dims
}

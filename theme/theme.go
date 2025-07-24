package theme

import (
	"context"
	"fmt"
	"image"
	"math"
	rtrace "runtime/trace"
	"slices"
	"strings"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"
	"honnef.co/go/stuff/math/mathutil"

	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
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
	Background         color.Oklch
	Foreground         color.Oklch
	ForegroundDisabled color.Oklch
	OpenLink           color.Oklch
	NavigationLink     color.Oklch
	Link               color.Oklch
	PrimarySelection   color.Oklch

	Border color.Oklch

	Popup struct {
		TitleForeground color.Oklch
		TitleBackground color.Oklch
		Background      color.Oklch
	}

	Menu struct {
		Background color.Oklch
		Selected   color.Oklch
		Border     color.Oklch
		Disabled   color.Oklch
	}

	Table struct {
		EvenRowBackground    color.Oklch
		OddRowBackground     color.Oklch
		HoveredRowBackground color.Oklch
		HeaderBackground     color.Oklch
		Divider              color.Oklch
		DragHandle           color.Oklch
		ExpandedBorder       color.Oklch
		ExpandedBackground   color.Oklch
	}
}

var DefaultPalette = Palette{
	Background:         oklch(99.44, 0.027, 106.89),
	Foreground:         oklch(0, 0, 0),
	ForegroundDisabled: oklch(55.21, 0, 0),
	NavigationLink:     oklch(57.32, 0.235, 29.23),
	OpenLink:           oklch(45.2, 0.31, 264.05),
	Link:               oklch(45.2, 0.31, 264.05),
	PrimarySelection:   oklcha(93.11, 0.101, 108.21, 0.6),
	Border:             oklch(0, 0, 0),

	Popup: struct {
		TitleForeground color.Oklch
		TitleBackground color.Oklch
		Background      color.Oklch
	}{
		TitleForeground: oklch(0, 0, 0),
		TitleBackground: oklch(98.82, 0.017, 196.89),
		Background:      oklch(98.29, 0.029, 145.35),
	},

	Menu: struct {
		Background color.Oklch
		Selected   color.Oklch
		Border     color.Oklch
		Disabled   color.Oklch
	}{
		Background: oklch(98.82, 0.017, 196.89),
		Selected:   oklch(89.92, 0.081, 195.81),
		Border:     oklch(89.92, 0.081, 195.81),
		Disabled:   oklch(73.8, 0, 0),
	},

	Table: struct {
		EvenRowBackground    color.Oklch
		OddRowBackground     color.Oklch
		HoveredRowBackground color.Oklch
		HeaderBackground     color.Oklch
		Divider              color.Oklch
		DragHandle           color.Oklch
		ExpandedBorder       color.Oklch
		ExpandedBackground   color.Oklch
	}{
		EvenRowBackground:    oklch(99.44, 0.027, 106.89),
		OddRowBackground:     oklch(99.44, 0.027, 106.89),
		HoveredRowBackground: oklch(95.63, 0.024, 106.84),
		HeaderBackground:     oklch(96.48, 0.026, 106.88),
		Divider:              oklch(80.15, 0, 0),
		DragHandle:           oklch(0, 0, 0),
		ExpandedBorder:       oklch(80.15, 0, 0),
		ExpandedBackground:   oklch(88.63, 0.053, 346),
	},
}

func NewTheme(fontCollection []font.FontFace) *Theme {
	return &Theme{
		Palette:       DefaultPalette,
		Shaper:        text.NewShaper(text.WithCollection(fontCollection), text.NoSystemFonts()),
		TextSize:      12,
		TextSizeLarge: 14,

		WindowPadding: 2,
		WindowBorder:  1,
	}
}

type ProgressBarStyle struct {
	ForegroundColor color.Oklch
	BackgroundColor color.Oklch
	BorderWidth     unit.Dp
	Progress        float32
}

func ProgressBar(th *Theme, progress float32) ProgressBarStyle {
	return ProgressBarStyle{
		ForegroundColor: oklch(56.7, 0.118, 143.83),
		BackgroundColor: oklcha(0, 0, 0, 0),
		BorderWidth:     1,
		Progress:        progress,
	}
}

func (p ProgressBarStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ProgressBarStyle.Layout").End()

	return Border{
		Color: p.ForegroundColor,
		Width: p.BorderWidth,
	}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		// Draw background
		bg := clip.Rect{Max: gtx.Constraints.Min}.Op()
		FillShape(win, gtx.Ops, p.BackgroundColor, bg)

		// Draw foreground
		fg := frect{Max: f32.Pt(float32(gtx.Constraints.Min.X)*p.Progress, float32(gtx.Constraints.Min.Y))}.Op(gtx.Ops)
		FillShape(win, gtx.Ops, p.ForegroundColor, fg)

		return layout.Dimensions{
			Size: gtx.Constraints.Min,
		}
	})
}

type CheckBoxStyle struct {
	Checkbox        widget.Boolean
	Label           string
	TextSize        unit.Sp
	ForegroundColor color.Oklch
	BackgroundColor color.Oklch
	TextColor       color.Oklch
}

func CheckBox(th *Theme, checkbox widget.Boolean, label string) CheckBoxStyle {
	return CheckBoxStyle{
		Checkbox:        checkbox,
		Label:           label,
		TextColor:       th.Palette.Foreground,
		ForegroundColor: th.Palette.Foreground,
		BackgroundColor: oklcha(0, 0, 0, 0),
		TextSize:        th.TextSize,
	}
}

func (c CheckBoxStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.CheckBoxStyle.Layout").End()

	return c.Checkbox.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Rigids(gtx, layout.Horizontal,
			func(gtx layout.Context) layout.Dimensions {
				sizeDp := gtx.Metric.SpToDp(c.TextSize)
				sizePx := gtx.Dp(sizeDp)

				ngtx := gtx
				ngtx.Constraints = layout.Exact(image.Pt(sizePx, sizePx))
				return Border{
					Color: c.ForegroundColor,
					Width: 1,
				}.Layout(win, ngtx, func(win *Window, gtx layout.Context) layout.Dimensions {
					FillShape(win, gtx.Ops, c.BackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
					if c.Checkbox.Get() {
						padding := gtx.Constraints.Min.X / 4
						if padding == 0 {
							padding = gtx.Dp(1)
						}
						minx := padding
						miny := minx
						maxx := gtx.Constraints.Min.X - padding
						maxy := maxx
						FillShape(win, gtx.Ops, c.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
					}

					return layout.Dimensions{Size: gtx.Constraints.Min}
				})
			},

			layout.Spacer{Width: 3}.Layout,

			func(gtx layout.Context) layout.Dimensions {
				return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{}, c.TextSize, c.Label, win.ColorMaterial(gtx, c.TextColor))
			},
		)
	})
}

type CheckBoxGroupStyle struct {
	Clickable       *widget.Clickable
	Label           string
	TextSize        unit.Sp
	ForegroundColor color.Oklch
	BackgroundColor color.Oklch
	TextColor       color.Oklch
}

func CheckBoxGroup(th *Theme, clickable *widget.Clickable, label string) CheckBoxGroupStyle {
	return CheckBoxGroupStyle{
		Clickable:       clickable,
		Label:           label,
		TextColor:       th.Palette.Foreground,
		ForegroundColor: th.Palette.Foreground,
		BackgroundColor: oklcha(0, 0, 0, 0),
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
		if chk.Checkbox.Update(gtx) {
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

	for {
		click, ok := chkgrp.Clickable.Clicked(gtx)
		if !ok {
			break
		}
		if click.Button != pointer.ButtonPrimary {
			continue
		}
		for i := range checkboxes {
			checkboxes[i].Checkbox.Set(state == none || state == some)
		}
		if state == none || state == some {
			state = all
		} else {
			state = none
		}
	}

	sizeDp := gtx.Metric.SpToDp(chkgrp.TextSize)
	sizePx := gtx.Dp(sizeDp)

	dims := layout.Rigids(gtx, layout.Vertical,
		func(gtx layout.Context) layout.Dimensions {
			return chkgrp.Clickable.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return layout.Rigids(gtx, layout.Horizontal,
					func(gtx layout.Context) layout.Dimensions {
						ngtx := gtx
						ngtx.Constraints = layout.Exact(image.Pt(sizePx, sizePx))
						return Border{
							Color: chkgrp.ForegroundColor,
							Width: 1,
						}.Layout(win, ngtx, func(win *Window, gtx layout.Context) layout.Dimensions {
							FillShape(win, gtx.Ops, chkgrp.BackgroundColor, clip.Rect{Max: gtx.Constraints.Min}.Op())
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

								FillShape(win, gtx.Ops, chkgrp.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
							case all:
								padding := gtx.Constraints.Min.X / 4
								if padding == 0 {
									padding = gtx.Dp(1)
								}
								minx := padding
								miny := minx
								maxx := gtx.Constraints.Min.X - padding
								maxy := maxx
								FillShape(win, gtx.Ops, chkgrp.ForegroundColor, clip.Rect{Min: image.Pt(minx, miny), Max: image.Pt(maxx, maxy)}.Op())
							}

							return layout.Dimensions{Size: gtx.Constraints.Min}
						})
					},

					layout.Spacer{Width: 3}.Layout,

					func(gtx layout.Context) layout.Dimensions {
						return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{}, chkgrp.TextSize, chkgrp.Label, win.ColorMaterial(gtx, chkgrp.TextColor))
					},
				)
			})
		},

		func(gtx layout.Context) layout.Dimensions {
			var children []layout.Widget
			if len(checkboxes) <= 16 {
				// Specifying a constant capacity allows the slice to be stack-allocated.
				children = make([]layout.Widget, len(checkboxes), 16)
			} else {
				children = make([]layout.Widget, len(checkboxes))
			}
			for i := range checkboxes {
				children[i] = Dumb(win, checkboxes[i].Layout)
			}
			return layout.Inset{Left: sizeDp + 3}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return layout.Rigids(gtx, layout.Vertical, children...)
			})
		},
	)

	return dims
}

// oklch specifies a color in Oklch.
// 100 >= l >= 0
// 0.37 >= c >= 0
// 360 > h >= 0
func oklch(l, c, h float32) color.Oklch {
	return color.Oklch{L: l / 100, C: c, H: h, A: 1}
}

func oklcha(l, c, h, a float32) color.Oklch {
	return color.Oklch{L: l / 100, C: c, H: h, A: a}
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

// clamp1 limits v to range [0..1].
func clamp1(v float32) float32 {
	return mathutil.Clamp(v, 0, 1)
}

type FoldableStyle struct {
	Title  string
	Closed *widget.Bool

	TextSize  unit.Sp
	TextColor color.Oklch
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
		return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, f.TextSize, l, win.ColorMaterial(gtx, f.TextColor))

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
	BorderColor     color.Oklch
	TextSize        unit.Sp
	TextColor       color.Oklch
	BackgroundColor color.Oklch
}

func Tooltip(th *Theme, msg string) TooltipStyle {
	msg = strings.TrimRight(msg, "\n")
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
	BorderColor     color.Oklch
	TextSize        unit.Sp
	TextColor       color.Oklch
	BackgroundColor color.Oklch
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

	return Bordered{Color: bt.BorderColor, Width: bt.BorderSize}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		// Don't inherit the minimum constraint from the parent widget. In this specific case, this widget is being
		// rendered as part of a flex child.
		gtx.Constraints.Min = image.Pt(0, 0)
		var padding = gtx.Dp(bt.Padding)

		macro := op.Record(gtx.Ops)
		dims := widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, bt.TextSize, bt.Text, win.ColorMaterial(gtx, bt.TextColor))
		call := macro.Stop()

		total := clip.Rect{
			Min: image.Pt(0, 0),
			Max: image.Pt(dims.Size.X+2*padding, dims.Size.Y+2*padding),
		}

		FillShape(win, gtx.Ops, bt.BackgroundColor, total.Op())

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

	ActiveBackgroundColor color.Oklch
	BackgroundColor       color.Oklch
	BorderColor           color.Oklch
	TextColor             color.Oklch
	TextColorDisabled     color.Oklch
}

func Button(th *Theme, button *widget.Clickable, txt string) ButtonStyle {
	return ButtonStyle{
		Text:                  txt,
		Button:                button,
		ActiveBackgroundColor: oklch(90.81, 0.046, 285.44),
		BackgroundColor:       oklch(100, 0, 0),
		BorderColor:           th.Palette.Border,
		TextColor:             th.Palette.Foreground,
		TextColorDisabled:     th.Palette.ForegroundDisabled,
	}
}

func (b ButtonStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ButtonStyle.Layout").End()

	var bg color.Oklch
	if b.Button.Pressed(pointer.ButtonPrimary) {
		bg = b.ActiveBackgroundColor
	} else {
		bg = b.BackgroundColor
	}

	var fg color.Oklch
	if gtx.Queue != nil {
		fg = b.TextColor
	} else {
		fg = b.TextColorDisabled
	}

	return Background{Color: bg}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		return b.Button.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return Bordered{Color: b.BorderColor, Width: 1}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
				return layout.UniformInset(1).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return widget.Label{Alignment: text.Middle}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, b.Text, win.ColorMaterial(gtx, fg))
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
func (g GridStyle) Layout(win *Window, gtx layout.Context, rows, cols int, dimensioner outlay.Dimensioner, cellFunc outlay.Cell) layout.Dimensions {
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
			return g.VScrollbarStyle.Layout(win, gtx, layout.Vertical, start, end)
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
			return g.HScrollbarStyle.Layout(win, gtx, layout.Horizontal, start, end)
		})
	}
	dim.Size.Y += hBarWidth

	return dim
}

type ResizeStyle struct {
	res         *component.Resize
	BorderColor color.Oklch
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
			Fill(win, gtx.Ops, rs.BorderColor)
			return layout.Dimensions{Size: gtx.Constraints.Max}
		}
	case layout.Vertical:
		hnd = func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints.Max.Y = 5
			defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
			Fill(win, gtx.Ops, rs.BorderColor)
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

	activeForeground := win.ColorMaterial(gtx, oklch(0, 0, 0))
	inactiveForeground := win.ColorMaterial(gtx, oklch(36.39, 0, 0))

	activeBackground := oklch(90.81, 0.046, 285.44)
	inactiveBackground := oklch(100, 0, 0)

	activeFont := font.Font{Weight: font.Bold}
	inactiveFont := font.Font{}

	activeBorder := oklch(77.08, 0.121, 283.20)
	inactiveBorder := oklch(79.21, 0, 0)

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

	labelWidth := max(dimsLeft.Size.X, dimsRight.Size.X)
	minWidth := 2*labelWidth + 4*gtx.Dp(padding) + gtx.Dp(borderWidth)
	gtx.Constraints.Max.X = gtx.Constraints.Constrain(image.Pt(minWidth, 0)).X

	// FIXME(dh): the way we "remove" parts of the border by drawing over it assumes that our background colors have no
	// transparency.

	return ss.State.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				dims := Border{Color: leftBorder, Width: borderWidth}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
					return Background{Color: leftBackground}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
							return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
								return widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, leftFont, 12, ss.Left, leftForeground)
							})
						})
					})
				})

				// Remove right part of the border
				FillShape(win, gtx.Ops, leftBackground, clip.Rect{Min: image.Pt(dims.Size.X-gtx.Dp(borderWidth), gtx.Dp(borderWidth)), Max: image.Pt(dims.Size.X, dims.Size.Y-gtx.Dp(borderWidth))}.Op())

				return dims
			}),

			// The left and right buttons share a single border in the middle.
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				width := gtx.Dp(borderWidth)
				height := dimsLeft.Size.Y + gtx.Dp(padding)*2
				size := image.Pt(width, height)
				FillShape(win, gtx.Ops, activeBorder, clip.Rect{Max: size}.Op())
				return layout.Dimensions{Size: size}
			}),

			layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
				dims := Border{Color: rightBorder, Width: borderWidth}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
					return Background{Color: rightBackground}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
							return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
								return widget.Label{MaxLines: 1}.Layout(noMin(gtx), win.Theme.Shaper, rightFont, 12, ss.Right, rightForeground)
							})
						})
					})
				})

				// Remove left part of the border
				FillShape(win, gtx.Ops, rightBackground, clip.Rect{Min: image.Pt(0, gtx.Dp(borderWidth)), Max: image.Pt(gtx.Dp(borderWidth), dims.Size.Y-gtx.Dp(borderWidth))}.Op())

				return dims
			}),
		)
	})
}

type TabbedState struct {
	Current    int
	clickables []widget.Clickable
	list       layout.List
}

type TabClick struct {
	Index int
	Click widget.Click
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

func (ts *TabbedState) Update(gtx layout.Context) []TabClick {
	var clicked []TabClick
	for i := range ts.clickables {
		for {
			click, ok := ts.clickables[i].Clicked(gtx)
			if !ok {
				break
			}
			if click.Modifiers == 0 && click.Button == pointer.ButtonPrimary {
				ts.Current = i
			} else {
				clicked = append(clicked, TabClick{i, click})
			}
		}
	}

	return clicked
}

func (ts TabbedStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TabbedStyle.Layout").End()

	ts.State.Update(gtx)
	if ts.State.Current >= len(ts.Tabs) {
		ts.State.Current = len(ts.Tabs) - 1
	}
	if len(ts.State.clickables) < len(ts.Tabs) {
		ts.State.clickables = slices.Grow(ts.State.clickables, len(ts.Tabs))[:len(ts.Tabs)]
	}
	const padding = 5
	const lineThickness = 1
	const activeLineThickness = 3

	dims := Background{Color: win.Theme.Palette.Background}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		return layout.Rigids(gtx, layout.Vertical,
			// Tabs
			func(gtx layout.Context) layout.Dimensions {
				return ts.State.list.Layout(gtx, len(ts.Tabs), func(gtx layout.Context, i int) layout.Dimensions {
					return ts.State.clickables[i].Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						stack := op.Offset(image.Pt(gtx.Dp(padding), gtx.Dp(padding))).Push(gtx.Ops)

						dims := widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, ts.Tabs[i], win.ColorMaterial(gtx, win.Theme.Palette.Foreground))
						stack.Pop()

						dims.Size.X += 2 * gtx.Dp(padding)
						dims.Size.Y += 2 * gtx.Dp(padding)

						if i == ts.State.Current {
							x0 := 0
							x1 := dims.Size.X
							y0 := dims.Size.Y - gtx.Dp(activeLineThickness)
							y1 := y0 + gtx.Dp(activeLineThickness)
							FillShape(win, gtx.Ops, oklch(71.79, 0.151, 281.92), clip.Rect{Min: image.Pt(x0, y0), Max: image.Pt(x1, y1)}.Op())
						}

						return dims
					})
				})
			},

			// Line
			func(gtx layout.Context) layout.Dimensions {
				r := clip.Rect{Max: image.Pt(gtx.Constraints.Min.X, gtx.Dp(lineThickness))}
				FillShape(win, gtx.Ops, oklch(0, 0, 0), r.Op())
				return layout.Dimensions{Size: r.Max}
			},

			// Padding
			func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Height: padding}.Layout(gtx) },

			// Content
			func(gtx layout.Context) layout.Dimensions {
				return w(win, gtx)
			},
		)
	})

	return dims
}

type Recording struct {
	Call       op.CallOp
	Dimensions layout.Dimensions
}

func (r Recording) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Recording.Layout").End()

	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	r.Call.Add(gtx.Ops)
	return r.Dimensions
}

func Record(win *Window, gtx layout.Context, w Widget) Recording {
	m := op.Record(gtx.Ops)
	dims := w(win, gtx)
	c := m.Stop()

	return Recording{c, dims}
}

type LabelStyle struct {
	Font      font.Font
	Color     color.Oklch
	Alignment text.Alignment
	MaxLines  int
	TextSize  unit.Sp
	Text      string
}

func Label(th *Theme, txt string) LabelStyle {
	return LabelStyle{
		Font:      font.Font{},
		Color:     th.Palette.Foreground,
		Alignment: text.Start,
		TextSize:  th.TextSize,
		Text:      txt,
	}
}

func LineLabel(th *Theme, txt string) LabelStyle {
	l := Label(th, txt)
	l.MaxLines = 1
	return l
}

func (ls LabelStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	return widget.Label{
		MaxLines:  ls.MaxLines,
		Alignment: ls.Alignment,
	}.Layout(gtx, win.Theme.Shaper, ls.Font, ls.TextSize, ls.Text, win.ColorMaterial(gtx, ls.Color))
}

func (ls LabelStyle) Dimensions(win *Window, gtx layout.Context) layout.Dimensions {
	l := widget.Label{
		MaxLines:  ls.MaxLines,
		Alignment: ls.Alignment,
	}
	return win.TextDimensions(gtx, l, ls.Font, ls.TextSize, ls.Text)
}

func (ls LabelStyle) Length(win *Window, gtx layout.Context) int {
	return ls.Dimensions(win, gtx).Size.X
}

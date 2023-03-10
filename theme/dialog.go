package theme

import (
	"image"
	"image/color"

	mylayout "honnef.co/go/gotraceui/layout"

	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
	"gioui.org/widget"
)

type DialogStyle struct {
	BorderWidth     unit.Dp
	BorderColor     color.NRGBA
	Title           string
	TitleSize       unit.Sp
	TitleColor      color.NRGBA
	TitleBackground color.NRGBA
	TitlePadding    unit.Dp
	Background      color.NRGBA
	Padding         unit.Dp
}

func Dialog(th *Theme, title string) DialogStyle {
	return DialogStyle{
		BorderWidth:     th.WindowBorder,
		BorderColor:     th.Palette.Border,
		Title:           title,
		TitleSize:       th.TextSize,
		TitleColor:      th.Palette.Foreground,
		TitleBackground: menuColor,
		TitlePadding:    th.WindowPadding,
		Background:      color.NRGBA{0xFF, 0xFF, 0xFF, 0xFF},
		Padding:         th.WindowPadding * 2,
	}
}

func (ds DialogStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	innerGtx := gtx
	innerGtx.Constraints.Max.X -= 2 * gtx.Dp(ds.BorderWidth+ds.TitlePadding)
	innerGtx.Constraints.Max.Y -= 2 * gtx.Dp(ds.BorderWidth+ds.TitlePadding)
	innerGtx.Constraints = mylayout.Normalize(innerGtx.Constraints)

	m := op.Record(innerGtx.Ops)
	paint.ColorOp{Color: ds.TitleColor}.Add(innerGtx.Ops)
	labelDims := widget.Label{}.Layout(innerGtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, ds.TitleSize, ds.Title)
	labelCall := m.Stop()

	wGtx := innerGtx
	wGtx.Constraints.Max.Y -= labelDims.Size.Y + 2*gtx.Dp(ds.TitlePadding) + gtx.Dp(ds.BorderWidth) + 2*gtx.Dp(ds.Padding)
	wGtx.Constraints = mylayout.Normalize(wGtx.Constraints)

	var wDims layout.Dimensions
	var wCall op.CallOp
	m = op.Record(wGtx.Ops)
	wDims = w(win, wGtx)
	wCall = m.Stop()

	return widget.Border{Width: ds.BorderWidth, Color: ds.BorderColor}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return layout.UniformInset(ds.BorderWidth).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: image.Pt(wDims.Size.X+gtx.Dp(ds.Padding)*2, labelDims.Size.Y+gtx.Dp(ds.TitlePadding)*2)}.Push(gtx.Ops).Pop()
					paint.Fill(gtx.Ops, ds.TitleBackground)
					return layout.UniformInset(ds.TitlePadding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						labelCall.Add(gtx.Ops)
						return labelDims
					})
				}),

				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					defer clip.Rect{Max: image.Pt(wDims.Size.X+gtx.Dp(ds.Padding)*2, wDims.Size.Y+gtx.Dp(ds.Padding)*2)}.Push(gtx.Ops).Pop()
					paint.Fill(gtx.Ops, ds.Background)
					return layout.UniformInset(ds.Padding).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						wCall.Add(gtx.Ops)
						return wDims
					})
				}),
			)
		})
	})
}

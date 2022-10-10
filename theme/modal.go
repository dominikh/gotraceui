package theme

import (
	"image/color"

	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/widget"
)

type Modal struct {
	Background color.NRGBA
	click      widget.Clickable
}

func (m *Modal) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	// TODO(dh): prevent keyboard input from bubbling up
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	paint.Fill(gtx.Ops, m.Background)
	m.click.Layout(gtx, w)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}

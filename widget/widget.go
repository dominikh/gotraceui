package widget

import (
	"image/color"

	"honnef.co/go/gotraceui/layout"

	"gioui.org/op"
	"gioui.org/op/paint"
)

func ColorTextMaterial(gtx layout.Context, c color.NRGBA) op.CallOp {
	m := op.Record(gtx.Ops)
	paint.ColorOp{Color: c}.Add(gtx.Ops)
	return m.Stop()
}

type List struct {
	Main        Scrollbar
	Cross       Scrollbar
	CrossOffset float32
	Widest      int
	layout.List
}

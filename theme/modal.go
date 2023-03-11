package theme

import (
	"context"
	"image/color"
	rtrace "runtime/trace"

	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
)

type ModalStyle struct {
	Background color.NRGBA
	Cancelled  *bool
}

func Modal(cancelled *bool) ModalStyle {
	return ModalStyle{
		Cancelled: cancelled,
	}
}

func (m ModalStyle) Layout(win *Window, gtx layout.Context, w Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.Modal.Layout").End()

	// FIXME(dh): the modal doesn't cover the whole window if an offset or transform is active
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	paint.Fill(gtx.Ops, m.Background)

	for _, ev := range gtx.Events(m) {
		switch ev := ev.(type) {
		case pointer.Event:
			if (ev.Priority == pointer.Foremost || ev.Priority == pointer.Grabbed) && ev.Type == pointer.Press {
				*m.Cancelled = true
			}

		case key.Event:
			// TODO(dh): set cancelled when pressing Esc
		}
	}

	pointer.InputOp{Tag: m, Types: 0xFF}.Add(gtx.Ops)
	// TODO(dh): prevent keyboard input from bubbling up
	w(win, gtx)
	return layout.Dimensions{Size: gtx.Constraints.Max}
}

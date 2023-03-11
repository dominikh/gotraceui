package theme

import (
	"context"
	"image"
	rtrace "runtime/trace"
	"time"

	"gioui.org/f32"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	mylayout "honnef.co/go/gotraceui/layout"
)

type Window struct {
	Theme       *Theme
	Menu        *Menu
	contextMenu []*MenuItem

	pointerAt f32.Point

	modal struct {
		modal Modal
		at    f32.Point
		w     Widget
	}
	notification notification
	windowFrameState
}

type windowFrameState struct {
	tooltip Widget
}

type Widget func(win *Window, gtx layout.Context) layout.Dimensions

func Dumb(win *Window, w Widget) layout.Widget {
	return func(gtx layout.Context) layout.Dimensions {
		return w(win, gtx)
	}
}

func (win *Window) Render(ops *op.Ops, ev system.FrameEvent, w func(win *Window, gtx layout.Context) layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "theme.Window.Render").End()
	gtx := layout.NewContext(ops, ev)

	win.windowFrameState = windowFrameState{}

	for _, ev := range gtx.Events(win) {
		switch ev := ev.(type) {
		case pointer.Event:
			win.pointerAt = ev.Position
		}
	}

	stack := clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops)
	pointer.InputOp{Tag: win, Types: 0xFF}.Add(gtx.Ops)

	for _, item := range win.contextMenu {
		if item.Clicked() {
			item.Do(gtx)
			win.CloseModal()
			win.contextMenu = nil
		}
	}

	if win.Menu != nil {
		dims := NewMenuStyle(win.Theme, win.Menu).Layout(win, gtx)
		mylayout.PixelInset{
			Top: dims.Size.Y,
		}.Layout(gtx, Dumb(win, w))
	} else {
		w(win, gtx)
	}

	win.notification.Layout(win, gtx)
	stack.Pop()

	if win.tooltip != nil {
		macro := op.Record(gtx.Ops)
		dims := win.tooltip(win, gtx)
		call := macro.Stop()

		var x, y int
		ptr := win.pointerAt.Round()
		// Prevent cursor from obscuring part of the tooltip.
		//
		// TODO(dh): we don't know the actual size of the cursor. The chosen values worked for us, but may not work for
		// everyone. Can we somehow query for the size of the cursor?
		ptr.X += gtx.Dp(8)
		ptr.Y += gtx.Dp(8)
		if ptr.X+dims.Size.X < gtx.Constraints.Max.X {
			x = ptr.X
		} else {
			x = gtx.Constraints.Max.X - dims.Size.X
		}
		if ptr.Y+dims.Size.Y < gtx.Constraints.Max.Y {
			y = ptr.Y
		} else {
			y = gtx.Constraints.Max.Y - dims.Size.Y
		}

		stack := op.Offset(image.Pt(x, y)).Push(gtx.Ops)
		call.Add(gtx.Ops)
		stack.Pop()
	}

	if win.modal.modal.Cancelled() {
		win.modal.w = nil
	}

	if win.modal.w != nil {
		gtx := gtx
		gtx.Constraints.Min = image.Point{}
		// FIXME(dh): don't render context menu out of bounds
		win.modal.modal.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
			defer op.Offset(win.modal.at.Round()).Push(gtx.Ops).Pop()

			m := op.Record(gtx.Ops)
			dims := win.modal.w(win, gtx)
			call := m.Stop()

			// win.modal.w might not register pointer input ops for all of the space it occupies. We don't want clicking
			// on those areas to close the model, so register our own input op covering the whole area. win.modal.w
			// still has precedence for the areas that it does register input ops for.
			defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
			pointer.InputOp{Tag: &gtx, Types: 0xFF}.Add(gtx.Ops)

			call.Add(gtx.Ops)
			return dims
		})
	}
}

func (win *Window) SetContextMenu(items []*MenuItem) {
	win.contextMenu = items
	widgets := make([]Widget, len(items))
	for i, item := range items {
		widgets[i] = NewMenuItemStyle(win.Theme, item).Layout
	}
	win.SetPopup(NewMenuGroupStyle(win.Theme, &MenuGroup{Items: widgets}).Layout)
}

func (win *Window) SetTooltip(w Widget) {
	win.tooltip = w
}

// TODO(dh): support specifying the minimum/maximum size of the modal, but make it optional
func (win *Window) SetPopup(w Widget) {
	win.modal.at = win.pointerAt
	win.modal.w = w
}

func (win *Window) CloseModal() {
	win.modal.w = nil
}

func (win *Window) ShowNotification(gtx layout.Context, msg string) {
	win.notification.message = msg
	win.notification.shownAt = gtx.Now
}

type notification struct {
	message string
	shownAt time.Time
}

func (notif *notification) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.notification.Layout").End()

	if gtx.Now.After(notif.shownAt.Add(1000 * time.Millisecond)) {
		return layout.Dimensions{}
	}

	// XXX compute width based on window size
	// TODO(dh): limit height to something sensible, just in case
	ngtx := gtx
	ngtx.Constraints.Max.X = 500
	macro := op.Record(gtx.Ops)
	dims := BorderedText(win.Theme, notif.message).Layout(win, ngtx)
	call := macro.Stop()

	defer op.Offset(image.Pt(gtx.Constraints.Max.X/2-dims.Size.X/2, gtx.Constraints.Max.Y-dims.Size.Y-gtx.Dp(30))).Push(gtx.Ops).Pop()
	call.Add(gtx.Ops)

	op.InvalidateOp{At: notif.shownAt.Add(1000 * time.Millisecond)}.Add(gtx.Ops)

	return dims
}

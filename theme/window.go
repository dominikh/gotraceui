package theme

import (
	"context"
	"image"
	"image/color"
	rtrace "runtime/trace"
	"strings"
	"time"
	"unsafe"

	myfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/tinylfu"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/unit"
)

type Window struct {
	AppWindow *app.Window
	Futures   *Futures
	Theme     *Theme
	Menu      *Menu
	// The current frame number
	Frame       uint64
	contextMenu []*MenuItem

	pointerAt f32.Point

	modal struct {
		cancelled bool
		at        f32.Point
		w         Widget
	}
	notification notification
	windowFrameState

	textLengths *tinylfu.T[string, layout.Dimensions]
}

func (win *Window) TextLength(gtx layout.Context, l widget.Label, font font.Font, size unit.Sp, s string) int {
	return win.TextDimensions(gtx, l, font, size, s).Size.X
}

func (win *Window) TextDimensions(gtx layout.Context, l widget.Label, font font.Font, size unit.Sp, s string) layout.Dimensions {
	b := new(strings.Builder)

	// This assumes that people use string literals whose addresses don't change when specifying text.Font
	b.Write((*[unsafe.Sizeof(font)]byte)(unsafe.Pointer(&font))[:])
	b.Write((*[unsafe.Sizeof(win.Theme.Shaper)]byte)(unsafe.Pointer(&win.Theme.Shaper))[:])
	b.Write((*[unsafe.Sizeof(gtx.Metric.PxPerSp)]byte)(unsafe.Pointer(&gtx.Metric.PxPerSp))[:])
	b.Write((*[unsafe.Sizeof(size)]byte)(unsafe.Pointer(&size))[:])
	b.WriteString(s)
	key := b.String()

	if dims, ok := win.textLengths.Get(key); ok {
		return dims
	}

	gtx.Constraints.Min = image.Point{}
	dims := labelDimensions(gtx, l, win.Theme.Shaper, font, size, s)
	win.textLengths.Add(key, dims)
	return dims
}

func NewWindow(win *app.Window) *Window {
	return &Window{
		AppWindow: win,
		Theme:     NewTheme(myfont.Collection()),
		Futures: &Futures{
			win: win,
		},
		textLengths: tinylfu.New[string, layout.Dimensions](1024, 1024*10),
	}
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
	win.Frame++
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
			win.CloseModal()
			item.Do(gtx)
			win.contextMenu = nil
		}
	}

	if win.Menu != nil {
		dims := NewMenuStyle(win.Theme, win.Menu).Layout(win, gtx)
		layout.PixelInset{
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

	if win.modal.cancelled {
		win.modal.w = nil
		win.modal.cancelled = false
	}

	if win.modal.w != nil {
		gtx := gtx

		isPopup := win.modal.at != f32.Pt(-1, -1)

		modal := Modal(&win.modal.cancelled)
		if isPopup {
			gtx.Constraints.Min = image.Point{}
			modal.Background = color.NRGBA{}
		} else {
			gtx.Constraints.Min = gtx.Constraints.Max
			modal.Background = rgba(0x000000DD)
		}
		modal.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
			return layout.Center.Layout(gtx, func(gtx layout.Context) layout.Dimensions {

				m := op.Record(gtx.Ops)
				dims := win.modal.w(win, gtx)
				call := m.Stop()

				if isPopup {
					off := win.modal.at.Round()
					if off.X+dims.Size.X > gtx.Constraints.Max.X {
						off.X = gtx.Constraints.Max.X - dims.Size.X
						if off.X < 0 {
							off.X = 0
						}
					}
					if off.Y+dims.Size.Y > gtx.Constraints.Max.Y {
						off.Y = gtx.Constraints.Max.Y - dims.Size.Y
						if off.Y < 0 {
							off.Y = 0
						}
					}
					defer op.Offset(off).Push(gtx.Ops).Pop()
				}

				// win.modal.w might not register pointer input ops for all of the space it occupies. We don't want clicking
				// on those areas to close the model, so register our own input op covering the whole area. win.modal.w
				// still has precedence for the areas that it does register input ops for.
				defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
				pointer.InputOp{Tag: &gtx, Types: 0xFF}.Add(gtx.Ops)

				call.Add(gtx.Ops)
				return dims
			})
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

func (win *Window) SetModal(w Widget) {
	win.modal.at = f32.Pt(-1, -1)
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

package theme

import (
	"context"
	"fmt"
	"image"
	stdcolor "image/color"
	"math"
	rtrace "runtime/trace"
	"strings"
	"sync"
	"time"
	"unsafe"

	"honnef.co/go/gotraceui/color"
	myfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/tinylfu"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/app"
	"gioui.org/f32"
	"gioui.org/font"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/system"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/unit"
)

type Action interface{ IsAction() }

type ExecuteAction func(gtx layout.Context)

func (ExecuteAction) IsAction() {}

type Window struct {
	AppWindow *app.Window
	Futures   *Futures
	Theme     *Theme
	Menu      *Menu
	// The current frame number
	Frame                uint64
	contextMenu          []*MenuItem
	shortcuts            map[Shortcut]struct{}
	pressedShortcuts     []Shortcut
	commandProviders     []CommandProvider
	prevCommandProviders []CommandProvider

	actionsMu          sync.Mutex
	emittedActions     []Action
	prevEmittedActions []Action

	pointerAt f32.Point

	scale float32

	modal struct {
		cancelled bool
		at        f32.Point
		w         Widget
	}
	notification notification
	windowFrameState

	textLengths    *tinylfu.T[string, layout.Dimensions]
	oklchToSRGB    *tinylfu.T[color.Oklch, stdcolor.NRGBA]
	colorMaterials map[struct {
		ops *op.Ops
		c   color.Oklch
	}]op.CallOp
}

func (win *Window) ColorMaterial(gtx layout.Context, c color.Oklch) op.CallOp {
	key := struct {
		ops *op.Ops
		c   color.Oklch
	}{
		gtx.Ops,
		c,
	}
	if op, ok := win.colorMaterials[key]; ok {
		return op
	} else {
		op := widget.ColorTextMaterial(gtx, win.ConvertColor(c))
		win.colorMaterials[key] = op
		return op
	}
}

func (win *Window) ConvertColor(c color.Oklch) stdcolor.NRGBA {
	if math.IsNaN(float64(c.L)) || math.IsNaN(float64(c.C)) || math.IsNaN(float64(c.H)) || math.IsNaN(float64(c.A)) {
		panic(fmt.Sprintf("got NaN in color: %v", c))
	}

	if c.L > 1 || c.C > 1 || c.A > 1 || c.L < 0 || c.C < 0 || c.H < 0 || c.A < 0 {
		panic(fmt.Sprintf("color value out of range: %v", c))
	}

	if out, ok := win.oklchToSRGB.Get(c); ok {
		return out
	}
	out := stdcolor.NRGBAModel.Convert(c.MapToSRGBGamut().SRGB()).(stdcolor.NRGBA)
	win.oklchToSRGB.Add(c, out)
	return out
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
	gtx.Constraints.Max = image.Point{1e6, 1e6}
	dims := labelDimensions(gtx, l, win.Theme.Shaper, font, size, s)
	win.textLengths.Add(key, dims)
	return dims
}

func (win *Window) AddCommandProvider(cp CommandProvider) {
	win.commandProviders = append(win.commandProviders, cp)
}

func NewWindow(win *app.Window) *Window {
	return &Window{
		AppWindow: win,
		Theme:     NewTheme(myfont.Collection()),
		Futures: &Futures{
			win: win,
		},
		textLengths: tinylfu.New[string, layout.Dimensions](1024, 1024*10),
		oklchToSRGB: tinylfu.New[color.Oklch, stdcolor.NRGBA](1024, 1024*10),
		shortcuts:   map[Shortcut]struct{}{},
		scale:       1,
		colorMaterials: map[struct {
			ops *op.Ops
			c   color.Oklch
		}]op.CallOp{},
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

type Shortcut struct {
	Modifiers key.Modifiers
	Name      string
}

// AddShortcut adds a new shortcut. The shortcut will only remain registered until the next call to Render.
func (win *Window) AddShortcut(s Shortcut) {
	win.shortcuts[s] = struct{}{}
}

// PressedShortcuts returns the shortcuts that have been pressed since the last call to Render. The slice is only valid
// until the next call to Render.
func (win *Window) PressedShortcuts() []Shortcut {
	return win.pressedShortcuts
}

func (win *Window) EmitAction(l Action) {
	win.actionsMu.Lock()
	defer win.actionsMu.Unlock()
	win.emittedActions = append(win.emittedActions, l)
	win.AppWindow.Invalidate()
}

func (win *Window) Actions() []Action {
	return win.prevEmittedActions
}

func doubleBuffer[E any, S ~[]E](dst, src *S) {
	if cap(*dst) < len(*src) {
		*dst = make(S, len(*src))
	}
	*dst = (*dst)[:len(*src)]
	copy(*dst, *src)
	*src = (*src)[:0]
}

func (win *Window) CommandProviders() []CommandProvider {
	return win.prevCommandProviders
}

func (win *Window) Update(ops *op.Ops, ev system.FrameEvent, w func(win *Window, gtx layout.Context)) {
	defer rtrace.StartRegion(context.Background(), "theme.Window.Update").End()

	win.Frame++
	gtx := layout.NewContext(ops, ev)
	gtx.Metric.PxPerDp *= win.scale
	gtx.Metric.PxPerSp *= win.scale
	win.windowFrameState = windowFrameState{}
	win.pressedShortcuts = win.pressedShortcuts[:0]
	clear(win.colorMaterials)

	for _, ev := range gtx.Events(win) {
		switch ev := ev.(type) {
		case key.Event:
			if ev.State != key.Press {
				continue
			}

			if ev.Modifiers == key.ModCtrl {
				if ev.Name == "=" || ev.Name == "+" {
					win.scale = min(win.scale+0.1, 5)
				} else if ev.Name == "-" {
					win.scale = max(win.scale-0.1, 0.3)
				} else if ev.Name == "0" {
					win.scale = 1
				}
			}

			key := Shortcut{ev.Modifiers, ev.Name}
			if _, ok := win.shortcuts[key]; ok {
				win.pressedShortcuts = append(win.pressedShortcuts, key)
			}
		case pointer.Event:
			win.pointerAt = ev.Position
		}
	}

	clear(win.shortcuts)

	doubleBuffer(&win.prevCommandProviders, &win.commandProviders)
	win.actionsMu.Lock()
	doubleBuffer(&win.prevEmittedActions, &win.emittedActions)
	win.actionsMu.Unlock()
	{
		// Use command providers in LIFO order, under the assumption that more specific providers get registered after
		// general ones.
		s := win.prevCommandProviders
		for i := range s[:len(s)/2] {
			s[i], s[len(s)-i-1] = s[len(s)-i-1], s[i]
		}
	}

	for _, item := range win.contextMenu {
		if item.Clicked(gtx) {
			win.CloseModal()
			win.EmitAction(item.Action())
			win.contextMenu = nil
		}
	}

	w(win, gtx)
}

func (win *Window) Layout(ops *op.Ops, ev system.FrameEvent, w func(win *Window, gtx layout.Context) layout.Dimensions) {
	defer rtrace.StartRegion(context.Background(), "theme.Window.Layout").End()

	gtx := layout.NewContext(ops, ev)
	gtx.Metric.PxPerDp *= win.scale
	gtx.Metric.PxPerSp *= win.scale

	stack := clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops)
	// Handle all keyboard input that wasn't handled by the window contents
	key.InputOp{Tag: win}.Add(gtx.Ops)
	pointer.InputOp{Tag: win, Kinds: 0xFF}.Add(gtx.Ops)

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
			modal.Background = color.Oklch{}
		} else {
			gtx.Constraints.Min = gtx.Constraints.Max
			modal.Background = oklcha(0, 0, 0, 0.85)
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
				pointer.InputOp{Tag: &gtx, Kinds: 0xFF}.Add(gtx.Ops)

				call.Add(gtx.Ops)
				return dims
			})
		})
	}

	win.Futures.Sweep()
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

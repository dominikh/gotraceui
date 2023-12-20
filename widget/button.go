package widget

import (
	"context"
	"image"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"

	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/io/semantic"
	"gioui.org/op"
	"gioui.org/op/clip"
)

// Clickable represents a clickable area.
type Activatable struct {
	Clickable

	keyTag       struct{}
	requestFocus bool
	focused      bool
	pressedKey   string
}

// Focus requests the input focus for the element.
func (b *Activatable) Focus() {
	b.requestFocus = true
}

// Focused reports whether b has focus.
func (b *Activatable) Focused() bool {
	return b.focused
}

// Layout and update the button state.
func (b *Activatable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Activatable.Layout").End()

	b.Update(gtx)
	m := op.Record(gtx.Ops)
	dims := w(gtx)
	c := m.Stop()
	defer clip.Rect(image.Rectangle{Max: dims.Size}).Push(gtx.Ops).Pop()
	enabled := gtx.Queue != nil
	semantic.EnabledOp(enabled).Add(gtx.Ops)
	b.click.Add(gtx.Ops)
	if enabled {
		keys := key.Set("⏎|Space")
		if !b.focused {
			keys = ""
		}
		key.InputOp{Tag: &b.keyTag, Keys: keys}.Add(gtx.Ops)
		if b.requestFocus {
			key.FocusOp{Tag: &b.keyTag}.Add(gtx.Ops)
			b.requestFocus = false
		}
	} else {
		b.focused = false
	}
	c.Add(gtx.Ops)
	return dims
}

// Update the button state by processing events, and return the resulting
// clicks, if any.
func (b *Activatable) Update(gtx layout.Context) []Click {
	b.clicks = nil
	if gtx.Queue == nil {
		b.focused = false
	}
	if b.requestFocus {
		key.FocusOp{Tag: &b.keyTag}.Add(gtx.Ops)
		b.requestFocus = false
	}

	var clicks []Click
	clicks = append(clicks, b.requestedClicks...)
	b.requestedClicks = b.requestedClicks[:0]

	for _, e := range b.click.Update(gtx) {
		switch e.Kind {
		case gesture.KindClick:
			clicks = append(clicks, Click{
				Button:    e.Button,
				Modifiers: e.Modifiers,
				NumClicks: e.NumClicks,
			})
		case gesture.KindCancel:
		case gesture.KindPress:
			if e.Source == pointer.Mouse {
				key.FocusOp{Tag: &b.keyTag}.Add(gtx.Ops)
			}
		}
	}
	for _, e := range gtx.Events(&b.keyTag) {
		switch e := e.(type) {
		case key.FocusEvent:
			b.focused = e.Focus
			if !e.Focus {
				b.pressedKey = ""
			}
		case key.Event:
			if !b.focused {
				break
			}
			if e.Name != key.NameReturn && e.Name != key.NameSpace {
				break
			}
			switch e.State {
			case key.Press:
				b.pressedKey = e.Name
			case key.Release:
				if b.pressedKey != e.Name {
					break
				}
				// only register a key as a click if the key was pressed and released while this button was focused
				b.pressedKey = ""
				clicks = append(clicks, Click{
					Modifiers: e.Modifiers,
					NumClicks: 1,
				})
			}
		}
	}

	return clicks
}

// Clickable represents a clickable area.
type Clickable struct {
	click gesture.Click
	// clicks is for saved clicks to support Clicked.
	clicks          []Click
	requestedClicks []Click
}

// Click represents a click.
type Click struct {
	Button    pointer.Buttons
	Modifiers key.Modifiers
	NumClicks int
}

// Click executes a simple programmatic click.
func (b *Clickable) Click(btn pointer.Buttons) {
	b.requestedClicks = append(b.requestedClicks, Click{
		Button:    btn,
		Modifiers: 0,
		NumClicks: 1,
	})
}

// Clicked reports whether there are pending clicks as would be
// reported by Clicks. If so, Clicked removes the earliest click.
func (b *Clickable) Clicked(gtx layout.Context) (Click, bool) {
	if len(b.clicks) > 0 {
		c := b.clicks[0]
		b.clicks = b.clicks[1:]
		return c, true
	}

	b.clicks = b.Update(gtx)

	if len(b.clicks) > 0 {
		c := b.clicks[0]
		b.clicks = b.clicks[1:]
		return c, true
	}
	return Click{}, false
}

// Hovered reports whether a pointer is over the element.
func (b *Clickable) Hovered() bool {
	return b.click.Hovered()
}

// Pressed reports whether a pointer is pressing the element.
func (b *Clickable) Pressed(btn pointer.Buttons) bool {
	return b.click.Pressed(btn)
}

// Layout and update the button state.
func (b *Clickable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Clickable.Layout").End()

	b.Update(gtx)
	m := op.Record(gtx.Ops)
	dims := w(gtx)
	c := m.Stop()
	defer clip.Rect(image.Rectangle{Max: dims.Size}).Push(gtx.Ops).Pop()
	semantic.EnabledOp(gtx.Queue != nil).Add(gtx.Ops)
	b.click.Add(gtx.Ops)
	c.Add(gtx.Ops)
	return dims
}

// Update the button state by processing events, and return the resulting
// clicks, if any.
func (b *Clickable) Update(gtx layout.Context) []Click {
	var clicks []Click
	clicks = append(clicks, b.requestedClicks...)
	b.requestedClicks = b.requestedClicks[:0]

	for _, e := range b.click.Update(gtx) {
		switch e.Kind {
		case gesture.KindClick:
			clicks = append(clicks, Click{
				Button:    e.Button,
				Modifiers: e.Modifiers,
				NumClicks: e.NumClicks,
			})
		case gesture.KindCancel:
		case gesture.KindPress:
		}
	}

	return clicks
}

// PrimaryActivatable is like Activatable but ignores all press and click events for buttons other than the primary one.
type PrimaryActivatable struct {
	Activatable
}

func (b *PrimaryActivatable) Clicked(gtx layout.Context) bool {
	for {
		clk, ok := b.Clickable.Clicked(gtx)
		if !ok {
			return false
		}
		if clk.Button == pointer.ButtonPrimary {
			return true
		}
	}
}

func (b *PrimaryActivatable) Click()        { b.Activatable.Click(pointer.ButtonPrimary) }
func (b *PrimaryActivatable) Hovered() bool { return b.Activatable.Hovered() }
func (b *PrimaryActivatable) Pressed() bool { return b.Activatable.Pressed(pointer.ButtonPrimary) }
func (b *PrimaryActivatable) Focus()        { b.Activatable.Focus() }
func (b *PrimaryActivatable) Focused() bool { return b.Activatable.Focused() }
func (b *PrimaryActivatable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.PrimaryActivatable.Layout").End()
	return b.Clickable.Layout(gtx, w)
}

// PrimaryClickable is like Clickable but ignores all press and click events for buttons other than the primary one.
type PrimaryClickable struct {
	Clickable
}

func (b *PrimaryClickable) Clicked(gtx layout.Context) bool {
	for {
		clk, ok := b.Clickable.Clicked(gtx)
		if !ok {
			return false
		}
		if clk.Button == pointer.ButtonPrimary {
			return true
		}
	}
}

func (b *PrimaryClickable) Click()        { b.Clickable.Click(pointer.ButtonPrimary) }
func (b *PrimaryClickable) Hovered() bool { return b.Clickable.Hovered() }
func (b *PrimaryClickable) Pressed() bool { return b.Clickable.Pressed(pointer.ButtonPrimary) }
func (b *PrimaryClickable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.PrimaryClickable.Layout").End()
	return b.Clickable.Layout(gtx, w)
}

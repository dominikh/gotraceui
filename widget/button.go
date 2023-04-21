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
type Clickable struct {
	click  gesture.Click
	clicks []Click
	// prevClicks is the index into clicks that marks the clicks
	// from the most recent Layout call. prevClicks is used to keep
	// clicks bounded.
	prevClicks int

	keyTag       struct{}
	requestFocus bool
	focused      bool
}

// Click represents a click.
type Click struct {
	Button    pointer.Buttons
	Modifiers key.Modifiers
	NumClicks int
}

// Click executes a simple programmatic click
func (b *Clickable) Click(btn pointer.Buttons) {
	b.clicks = append(b.clicks, Click{
		Button:    btn,
		Modifiers: 0,
		NumClicks: 1,
	})
}

// Clicked reports whether there are pending clicks as would be
// reported by Clicks. If so, Clicked removes the earliest click.
func (b *Clickable) Clicked() (Click, bool) {
	if len(b.clicks) == 0 {
		return Click{}, false
	}
	click := b.clicks[0]
	n := copy(b.clicks, b.clicks[1:])
	b.clicks = b.clicks[:n]
	if b.prevClicks > 0 {
		b.prevClicks--
	}
	return click, true
}

// Hovered reports whether a pointer is over the element.
func (b *Clickable) Hovered() bool {
	return b.click.Hovered()
}

// Pressed reports whether a pointer is pressing the element.
func (b *Clickable) Pressed(btn pointer.Buttons) bool {
	return b.click.Pressed(btn)
}

// Focus requests the input focus for the element.
func (b *Clickable) Focus() {
	b.requestFocus = true
}

// Focused reports whether b has focus.
func (b *Clickable) Focused() bool {
	return b.focused
}

// Clicks returns and clear the clicks since the last call to Clicks.
func (b *Clickable) Clicks() []Click {
	clicks := b.clicks
	b.clicks = nil
	b.prevClicks = 0
	return clicks
}

// Layout and update the button state
func (b *Clickable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Clickable.Layout").End()

	b.update(gtx)
	m := op.Record(gtx.Ops)
	dims := w(gtx)
	c := m.Stop()
	defer clip.Rect(image.Rectangle{Max: dims.Size}).Push(gtx.Ops).Pop()
	disabled := gtx.Queue == nil
	semantic.DisabledOp(disabled).Add(gtx.Ops)
	b.click.Add(gtx.Ops)
	if !disabled {
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

// update the button state by processing events.
func (b *Clickable) update(gtx layout.Context) {
	// Flush clicks from before the last update.
	n := copy(b.clicks, b.clicks[b.prevClicks:])
	b.clicks = b.clicks[:n]
	b.prevClicks = n

	if gtx.Queue == nil {
		return
	}

	for _, e := range b.click.Events(gtx.Queue) {
		switch e.Type {
		case gesture.TypeClick:
			b.clicks = append(b.clicks, Click{
				Button:    e.Button,
				Modifiers: e.Modifiers,
				NumClicks: e.NumClicks,
			})
		case gesture.TypeCancel:
		case gesture.TypePress:
			if e.Source == pointer.Mouse {
				key.FocusOp{Tag: &b.keyTag}.Add(gtx.Ops)
			}
		}
	}
	for _, e := range gtx.Events(&b.keyTag) {
		switch e := e.(type) {
		case key.FocusEvent:
			b.focused = e.Focus
		case key.Event:
			if !b.focused || e.State != key.Release {
				break
			}
			if e.Name != key.NameReturn && e.Name != key.NameSpace {
				break
			}
			b.clicks = append(b.clicks, Click{
				Button:    pointer.ButtonPrimary,
				Modifiers: e.Modifiers,
				NumClicks: 1,
			})
		}
	}
}

// PrimaryClickable is like Clickable but ignores all press and click events for buttons other than the primary one.
type PrimaryClickable struct {
	Clickable
}

func (b *PrimaryClickable) Clicked() bool {
	for {
		clk, ok := b.Clickable.Clicked()
		if !ok {
			return false
		}
		if clk.Button == pointer.ButtonPrimary {
			return true
		}
	}
}

func (b *PrimaryClickable) Clicks() []Click {
	clicks := b.Clickable.Clicks()
	out := clicks[:0]
	for _, click := range clicks {
		if click.Button != pointer.ButtonPrimary {
			continue
		}
		out = append(out, click)
	}
	return out
}

func (b *PrimaryClickable) Click()        { b.Clickable.Click(pointer.ButtonPrimary) }
func (b *PrimaryClickable) Hovered() bool { return b.Clickable.Hovered() }
func (b *PrimaryClickable) Pressed() bool { return b.Clickable.Pressed(pointer.ButtonPrimary) }
func (b *PrimaryClickable) Focus()        { b.Clickable.Focus() }
func (b *PrimaryClickable) Focused() bool { return b.Clickable.Focused() }
func (b *PrimaryClickable) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.PrimaryClickable.Layout").End()
	return b.Clickable.Layout(gtx, w)
}

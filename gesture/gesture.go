package gesture

import (
	"image"
	"time"

	"gioui.org/f32"
	"gioui.org/io/event"
	"gioui.org/io/key"
	"gioui.org/io/pointer"
	"gioui.org/op"
)

// The duration is somewhat arbitrary.
const doubleClickDuration = 200 * time.Millisecond

const (
	// TypePress is reported for the first pointer
	// press.
	TypePress ClickType = iota
	// TypeClick is reported when a click action
	// is complete.
	TypeClick
	// TypeCancel is reported when the gesture is
	// cancelled.
	TypeCancel
)

type clickButton struct {
	// clickedAt is the timestamp at which
	// the last click occurred.
	clickedAt time.Duration
	// clicks is incremented if successive clicks
	// are performed within a fixed duration.
	clicks int
	// pressed tracks whether the pointer is pressed.
	pressed bool
	// pid is the pointer.ID.
	pid pointer.ID
}

// Click detects click gestures in the form
// of ClickEvents.
type Click struct {
	// hovered tracks whether the pointer is inside the gesture.
	hovered bool
	// entered tracks whether an Enter event has been received.
	entered bool
	buttons [3]clickButton
	// The e.Buttons of the previous pointer event.
	prevButtons pointer.Buttons
}

// ClickEvent represent a click action, either a
// TypePress for the beginning of a click or a
// TypeClick for a completed click.
type ClickEvent struct {
	Type      ClickType
	Position  image.Point
	Source    pointer.Source
	Modifiers key.Modifiers
	Button    pointer.Buttons
	// NumClicks records successive clicks occurring
	// within a short duration of each other.
	NumClicks int
}

type ClickType uint8

// Add the handler to the operation list to receive click events.
func (c *Click) Add(ops *op.Ops) {
	pointer.InputOp{
		Tag:   c,
		Types: pointer.Press | pointer.Release | pointer.Enter | pointer.Leave,
	}.Add(ops)
}

// Hovered returns whether a pointer is inside the area.
func (c *Click) Hovered() bool {
	return c.hovered
}

// Pressed returns whether a pointer is pressing.
func (c *Click) Pressed(btn pointer.Buttons) bool {
	for i := 0; i < 3; i++ {
		if btn&1<<i == 0 {
			continue
		}
		if c.buttons[i].pressed {
			return true
		}
	}
	return false
}

// Events returns the next click events, if any.
func (c *Click) Events(q event.Queue) []ClickEvent {
	var events []ClickEvent
	for _, evt := range q.Events(c) {
		e, ok := evt.(pointer.Event)
		if !ok {
			continue
		}

		prevButtons := c.prevButtons
		c.prevButtons = e.Buttons

		switch e.Type {
		case pointer.Release:
			// e.Buttons contains the buttons which are still pressed, so we need to process the bits that aren't set

			for i := 0; i < 3; i++ {
				if e.Buttons&(1<<i) != 0 {
					continue
				}
				btn := &c.buttons[i]
				if !btn.pressed || btn.pid != e.PointerID {
					continue
				}
				btn.pressed = false
				if !c.entered || c.hovered {
					events = append(events, ClickEvent{Type: TypeClick, Position: e.Position.Round(), Source: e.Source, Button: 1 << i, Modifiers: e.Modifiers, NumClicks: btn.clicks})
				} else {
					events = append(events, ClickEvent{Type: TypeCancel, Button: 1 << i})
				}
			}

		case pointer.Cancel:
			// Cancel affects all buttons
			c.prevButtons = 0
			for i := 0; i < 3; i++ {
				btn := &c.buttons[i]
				wasPressed := btn.pressed
				btn.pressed = false
				c.hovered = false
				c.entered = false
				if wasPressed {
					events = append(events, ClickEvent{Type: TypeCancel, Button: 1 << i})
				}
			}
		case pointer.Press:
			// e.Buttons contains all buttons currently held. e.Buttons &^ prevButtons are all newly pressed buttons.
			buttons := e.Buttons &^ prevButtons
			for i := 0; i < 3; i++ {
				btn := &c.buttons[i]
				if btn.pressed {
					continue
				}
				if e.Source == pointer.Mouse && buttons&(1<<i) == 0 {
					continue
				}
				if !c.hovered {
					btn.pid = e.PointerID
				}
				if btn.pid != e.PointerID {
					continue
				}
				btn.pressed = true
				if e.Time-btn.clickedAt < doubleClickDuration {
					btn.clicks++
				} else {
					btn.clicks = 1
				}
				btn.clickedAt = e.Time
				events = append(events, ClickEvent{Type: TypePress, Position: e.Position.Round(), Source: e.Source, Button: 1 << i, Modifiers: e.Modifiers, NumClicks: btn.clicks})
			}
		case pointer.Leave:
			// Leave affects all buttons
			c.prevButtons = 0
			for i := 0; i < 3; i++ {
				btn := &c.buttons[i]
				if !btn.pressed {
					btn.pid = e.PointerID
				}
				if btn.pid == e.PointerID {
					c.hovered = false
				}
			}
		case pointer.Enter:
			// Enter affects all buttons
			for i := 0; i < 3; i++ {
				btn := &c.buttons[i]
				if !btn.pressed {
					btn.pid = e.PointerID
				}
				if btn.pid == e.PointerID {
					c.hovered = true
					c.entered = true
				}
			}
		}
	}
	return events
}

func (ClickEvent) ImplementsEvent() {}

// Hover detects hover events and tracks the pointer's position.
type Hover struct {
	hovered   bool
	pointerAt f32.Point
}

// Hovered returns whether a pointer is inside the area.
func (h *Hover) Hovered() bool {
	return h.hovered
}

// Add the handler to the operation list to detect hovering.
func (h *Hover) Add(ops *op.Ops) {
	defer pointer.PassOp{}.Push(ops).Pop()
	pointer.InputOp{
		Tag:   h,
		Types: pointer.Move | pointer.Drag | pointer.Enter | pointer.Leave | pointer.Cancel,
	}.Add(ops)
}

func (h *Hover) Update(q event.Queue) {
	for _, evt := range q.Events(h) {
		e, ok := evt.(pointer.Event)
		if !ok {
			continue
		}

		h.pointerAt = e.Position

		switch e.Type {
		case pointer.Cancel:
			h.hovered = false
		case pointer.Leave:
			// FIXME(dh): track hovered per pointer ID
			h.hovered = false
		case pointer.Enter:
			// FIXME(dh): track hovered per pointer ID
			h.hovered = true
		}
	}
}

func (h *Hover) Pointer() f32.Point {
	return h.pointerAt
}

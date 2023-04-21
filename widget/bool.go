package widget

import (
	"context"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/layout"

	"gioui.org/io/semantic"
)

type Bool struct {
	Value bool

	clk PrimaryClickable

	changed bool
}

func (b *Bool) Get() bool  { return b.Value }
func (b *Bool) Set(v bool) { b.Value = v }

// Changed reports whether Value has changed since the last
// call to Changed.
func (b *Bool) Changed() bool {
	changed := b.changed
	b.changed = false
	return changed
}

// Hovered reports whether pointer is over the element.
func (b *Bool) Hovered() bool {
	return b.clk.Hovered()
}

// Pressed reports whether pointer is pressing the element.
func (b *Bool) Pressed() bool {
	return b.clk.Pressed()
}

// Focused reports whether b has focus.
func (b *Bool) Focused() bool {
	return b.clk.Focused()
}

func (b *Bool) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.Bool.Layout").End()

	dims := b.clk.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		for b.clk.Clicked() {
			b.Value = !b.Value
			b.changed = true
		}
		semantic.SelectedOp(b.Value).Add(gtx.Ops)
		semantic.DisabledOp(gtx.Queue == nil).Add(gtx.Ops)
		return w(gtx)
	})
	return dims
}

type BackedBit[T uint8 | uint16 | uint32 | uint64] struct {
	Bits *T
	Bit  int

	clk PrimaryClickable

	changed bool
}

func (bit *BackedBit[T]) Get() bool {
	return *bit.Bits&(1<<bit.Bit) != 0
}

func (bit *BackedBit[T]) Set(b bool) {
	if b {
		*bit.Bits |= 1 << bit.Bit
	} else {
		*bit.Bits &^= 1 << bit.Bit
	}
}

func (bit *BackedBit[T]) Changed() bool {
	changed := bit.changed
	bit.changed = false
	return changed
}

// Hovered reports whether pointer is over the element.
func (bit *BackedBit[T]) Hovered() bool {
	return bit.clk.Hovered()
}

// Pressed reports whether pointer is pressing the element.
func (bit *BackedBit[T]) Pressed() bool {
	return bit.clk.Pressed()
}

// Focused reports whether the element is focused.
func (bit *BackedBit[T]) Focused() bool {
	return bit.clk.Focused()
}

func (bit *BackedBit[T]) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "widget.BackedBit.Layout").End()

	dims := bit.clk.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		for bit.clk.Clicked() {
			bit.Set(!bit.Get())
			bit.changed = true
		}
		semantic.SelectedOp(bit.Get()).Add(gtx.Ops)
		semantic.DisabledOp(gtx.Queue == nil).Add(gtx.Ops)
		return w(gtx)
	})
	return dims
}

type Boolean interface {
	Set(bool)
	Get() bool
	Layout(layout.Context, layout.Widget) layout.Dimensions
}

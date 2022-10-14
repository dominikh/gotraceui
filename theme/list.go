package theme

// This is copied almost verbatim from material, but uses our scrollbar.

import (
	"context"
	"math"
	rtrace "runtime/trace"

	"gioui.org/layout"
	"gioui.org/widget"
)

// fromListPosition converts a layout.Position into two floats representing
// the location of the viewport on the underlying content. It needs to know
// the number of elements in the list and the major-axis size of the list
// in order to do this. The returned values will be in the range [0,1], and
// start will be less than or equal to end.
func fromListPosition(lp layout.Position, elements int, majorAxisSize int) (start, end float32) {
	// Approximate the size of the scrollable content.
	lengthPx := float32(lp.Length)
	meanElementHeight := lengthPx / float32(elements)

	// Determine how much of the content is visible.
	listOffsetF := float32(lp.Offset)
	visiblePx := float32(majorAxisSize)
	visibleFraction := visiblePx / lengthPx

	// Compute the location of the beginning of the viewport.
	viewportStart := (float32(lp.First)*meanElementHeight + listOffsetF) / lengthPx

	return viewportStart, clamp1(viewportStart + visibleFraction)
}

// AnchorStrategy defines a means of attaching a scrollbar to content.
type AnchorStrategy uint8

const (
	// Occupy reserves space for the scrollbar, making the underlying
	// content region smaller on one axis.
	Occupy AnchorStrategy = iota
	// Overlay causes the scrollbar to float atop the content without
	// occupying any space. Content in the underlying area can be occluded
	// by the scrollbar.
	Overlay
)

// ListStyle configures the presentation of a layout.List with a scrollbar.
type ListStyle struct {
	state *widget.List
	ScrollbarStyle
	AnchorStrategy
}

// List constructs a ListStyle using the provided theme and state.
func List(th *Theme, state *widget.List) ListStyle {
	return ListStyle{
		state:          state,
		ScrollbarStyle: Scrollbar(th, &state.Scrollbar),
	}
}

// Layout the list and its scrollbar.
func (l ListStyle) Layout(gtx layout.Context, length int, w layout.ListElement) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ListStyle.Layout").End()

	originalConstraints := gtx.Constraints

	// Determine how much space the scrollbar occupies.
	barWidth := gtx.Dp(l.Width())

	if l.AnchorStrategy == Occupy {

		// Reserve space for the scrollbar using the gtx constraints.
		max := l.state.Axis.Convert(gtx.Constraints.Max)
		min := l.state.Axis.Convert(gtx.Constraints.Min)
		max.Y -= barWidth
		if max.Y < 0 {
			max.Y = 0
		}
		min.Y -= barWidth
		if min.Y < 0 {
			min.Y = 0
		}
		gtx.Constraints.Max = l.state.Axis.Convert(max)
		gtx.Constraints.Min = l.state.Axis.Convert(min)
	}

	listDims := l.state.List.Layout(gtx, length, w)
	gtx.Constraints = originalConstraints

	// Draw the scrollbar.
	anchoring := layout.E
	if l.state.Axis == layout.Horizontal {
		anchoring = layout.S
	}
	majorAxisSize := l.state.Axis.Convert(listDims.Size).X
	start, end := fromListPosition(l.state.Position, length, majorAxisSize)
	// layout.Direction respects the minimum, so ensure that the
	// scrollbar will be drawn on the correct edge even if the provided
	// layout.Context had a zero minimum constraint.
	gtx.Constraints.Min = listDims.Size
	if l.AnchorStrategy == Occupy {
		min := l.state.Axis.Convert(gtx.Constraints.Min)
		min.Y += barWidth
		gtx.Constraints.Min = l.state.Axis.Convert(min)
	}
	anchoring.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return l.ScrollbarStyle.Layout(gtx, l.state.Axis, start, end)
	})

	if delta := l.state.ScrollDistance(); delta != 0 {
		// Handle any changes to the list position as a result of user interaction
		// with the scrollbar.
		l.state.List.Position.Offset += int(math.Round(float64(float32(l.state.Position.Length) * delta)))

		// Ensure that the list pays attention to the Offset field when the scrollbar drag
		// is started while the bar is at the end of the list. Without this, the scrollbar
		// cannot be dragged away from the end.
		l.state.List.Position.BeforeEnd = true
	}

	if l.AnchorStrategy == Occupy {
		// Increase the width to account for the space occupied by the scrollbar.
		cross := l.state.Axis.Convert(listDims.Size)
		cross.Y += barWidth
		listDims.Size = l.state.Axis.Convert(cross)
	}

	return listDims
}

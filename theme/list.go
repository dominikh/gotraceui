package theme

// This is copied almost verbatim from material, but uses our scrollbar.

import (
	"context"
	"image"
	"math"
	rtrace "runtime/trace"

	mylayout "honnef.co/go/gotraceui/layout"
	mywidget "honnef.co/go/gotraceui/widget"

	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
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
	state *mywidget.List
	Main  ScrollbarStyle
	Cross ScrollbarStyle
	AnchorStrategy
	EnableCrossScrolling bool
}

// List constructs a ListStyle using the provided theme and state.
func List(th *Theme, state *mywidget.List) ListStyle {
	return ListStyle{
		state: state,
		Cross: Scrollbar(th, &state.Cross),
		Main:  Scrollbar(th, &state.Main),
	}
}

// Layout the list and its scrollbar.
func (l ListStyle) Layout(gtx layout.Context, length int, w layout.ListElement) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ListStyle.Layout").End()

	// originalConstraints are the constraints that the user passed to ListStyle.Layout. These are the constraints in
	// which the list and its scrollbars have to fit.
	originalConstraints := gtx.Constraints

	// Determine how much space the scrollbars occupy.
	mainBarWidth := gtx.Dp(l.Main.Width())
	crossBarWidth := gtx.Dp(l.Cross.Width())

	if l.AnchorStrategy == Occupy {
		// Reserve space for the scrollbar using the gtx constraints.
		max := l.state.Axis.Convert(gtx.Constraints.Max)
		min := l.state.Axis.Convert(gtx.Constraints.Min)
		max.Y -= mainBarWidth
		if max.Y < 0 {
			max.Y = 0
		}
		min.Y -= mainBarWidth
		if min.Y < 0 {
			min.Y = 0
		}
		if l.EnableCrossScrolling {
			max.X -= crossBarWidth
			if max.X < 0 {
				max.X = 0
			}
			min.X -= crossBarWidth
			if min.X < 0 {
				min.X = 0
			}
		}
		gtx.Constraints.Max = l.state.Axis.Convert(max)
		gtx.Constraints.Min = l.state.Axis.Convert(min)
	}

	// tightListConstraints are the original constraints reduced by the space reserved for scrollbars. These will limit
	// the visible portion of the layout.List.
	tightListConstraints := gtx.Constraints

	if l.EnableCrossScrolling {
		// When cross scrolling is enabled we allow the list to draw itself however wide it wants. We'll later pan and
		// crop the visible portion.
		*mylayout.Cross(l.state.Axis, &gtx.Constraints.Max) = 1e6
	}

	m := op.Record(gtx.Ops)
	if l.EnableCrossScrolling {
		// Extend the minimum width to the largest element we've seen so far. layout.List.Layout adds a scroll handler
		// to the area covered by the minimum constraint or the widest element it saw during that call to Layout. By
		// setting the minimum to the widest known element we ensure that the scroll region doesn't shrink when large
		// items go out of view.
		if l.state.Widest > gtx.Constraints.Min.X {
			gtx.Constraints.Min.X = l.state.Widest
		}
	}
	listDims := l.state.List.Layout(gtx, length, w)
	// crossWidth is the total, unconstrained width of the visible list elements.
	crossWidth := l.state.Axis.Convert(listDims.Size).Y
	// Use the widest width we've seen so far. That way, the scrollbar's handle size only changes when new, larger items
	// appear, not when large items disappear.
	if crossWidth > l.state.Widest {
		l.state.Widest = crossWidth
	} else {
		crossWidth = l.state.Widest
	}
	// Limit listDims.Size to the size of the visible portion.
	listDims.Size = tightListConstraints.Constrain(listDims.Size)
	call := m.Stop()

	// Constrain to the original constraints and display cropped & panned version of the list.
	gtx.Constraints = originalConstraints
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
	delta := l.state.Cross.ScrollDistance()
	l.state.CrossOffset += delta * float32(crossWidth)
	if max := float32(crossWidth - listDims.Size.X); l.state.CrossOffset > max {
		l.state.CrossOffset = max
	}
	if l.state.CrossOffset < 0 {
		l.state.CrossOffset = 0
	}
	off := -int(math.Round(float64(l.state.CrossOffset)))
	stack := op.Offset(image.Pt(off, 0)).Push(gtx.Ops)
	call.Add(gtx.Ops)
	stack.Pop()

	// Draw the scrollbars.
	mainAnchoring := layout.NE
	crossAnchoring := layout.SW
	if l.state.Axis == layout.Horizontal {
		mainAnchoring = layout.SW
		crossAnchoring = layout.NE
	}
	majorAxisSize := *mylayout.Main(l.state.Axis, &listDims.Size)

	// layout.Direction respects the minimum, so ensure that the
	// scrollbar will be drawn on the correct edge even if the provided
	// layout.Context had a zero minimum constraint.
	gtx.Constraints.Min = listDims.Size
	if l.AnchorStrategy == Occupy {
		min := l.state.Axis.Convert(gtx.Constraints.Min)
		min.Y += mainBarWidth
		if l.EnableCrossScrolling {
			min.X += crossBarWidth
		}
		gtx.Constraints.Min = l.state.Axis.Convert(min)
	}

	mainAnchoring.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		if l.EnableCrossScrolling {
			// Make sure the two scrollbars don't overlap, by leaving a gap in the corner where they meet.
			*mylayout.Main(l.state.Axis, &gtx.Constraints.Min) -= crossBarWidth
			*mylayout.Main(l.state.Axis, &gtx.Constraints.Max) -= crossBarWidth
			gtx.Constraints = mylayout.Normalize(gtx.Constraints)
		}

		start, end := fromListPosition(l.state.Position, length, majorAxisSize)
		return l.Main.Layout(gtx, l.state.Axis, start, end)
	})

	if l.EnableCrossScrolling {
		crossAnchoring.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			// Make sure the two scrollbars don't overlap, by leaving a gap in the corner where they meet.
			*mylayout.Cross(l.state.Axis, &gtx.Constraints.Min) -= mainBarWidth
			*mylayout.Cross(l.state.Axis, &gtx.Constraints.Max) -= mainBarWidth
			gtx.Constraints = mylayout.Normalize(gtx.Constraints)

			start, end := float32(0), float32(1)
			renderedWidth := *mylayout.Cross(l.state.Axis, &listDims.Size)
			if crossWidth > renderedWidth {
				width := float32(renderedWidth) / float32(crossWidth)
				start = float32(l.state.CrossOffset) / float32(crossWidth)
				end = start + width
			}
			return l.Cross.Layout(gtx, (l.state.Axis+1)%2, start, end)
		})
	}

	if delta := l.state.Main.ScrollDistance(); delta != 0 {
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
		cross.Y += mainBarWidth
		if l.EnableCrossScrolling {
			cross.X += crossBarWidth
		}
		listDims.Size = l.state.Axis.Convert(cross)
	}

	return listDims
}

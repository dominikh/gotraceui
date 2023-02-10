package main

import "gioui.org/layout"

func grow[E any, S ~[]E](x S, n int) S {
	if len(x) > n {
		return x
	}
	if cap(x) >= n {
		return x[:n]
	}
	y := make(S, n)
	copy(y, x)
	return y
}

func scale[T float32 | float64](oldStart, oldEnd, newStart, newEnd, v T) T {
	slope := (newEnd - newStart) / (oldEnd - oldStart)
	output := newStart + slope*(v-oldStart)
	return output

	ratio := v / (oldEnd - oldStart)
	return (1-ratio)*newEnd + ratio*newStart
}

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

// clamp1 limits v to range [0..1].
func clamp1(v float32) float32 {
	if v >= 1 {
		return 1
	} else if v <= 0 {
		return 0
	} else {
		return v
	}
}

func normalize(c layout.Constraints) layout.Constraints {
	if c.Min.X < 0 {
		c.Min.X = 0
	}
	if c.Min.Y < 0 {
		c.Min.Y = 0
	}
	if c.Max.X < 0 {
		c.Max.X = 0
	}
	if c.Max.Y < 0 {
		c.Max.Y = 0
	}

	if c.Min.X > c.Max.X {
		c.Min.X = c.Max.X
	}
	if c.Min.Y > c.Max.Y {
		c.Min.Y = c.Max.Y
	}

	return c
}

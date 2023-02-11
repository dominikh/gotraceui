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

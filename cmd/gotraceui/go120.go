//go:build !go1.21

package main

import "golang.org/x/exp/constraints"

func min[T constraints.Ordered](a, b T) T {
	// This is not a true replacement for Go 1.21's min function, but it avoids allocating for a slice.
	if a <= b {
		return a
	} else {
		return b
	}
}

func max[T constraints.Ordered](a, b T) T {
	// This is not a true replacement for Go 1.21's max function, but it avoids allocating for a slice.
	if a >= b {
		return a
	} else {
		return b
	}
}

func clear[E any, T ~[]E](x T) {
	// This is not a true replacement for Go 1.21's clear function, but we only care about clearing slices for now.
	var zero E
	for i := range x {
		x[i] = zero
	}
}

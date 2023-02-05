package main

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

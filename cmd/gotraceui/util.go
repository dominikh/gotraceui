package main

func scale[T float32 | float64](oldStart, oldEnd, newStart, newEnd, v T) T {
	slope := (newEnd - newStart) / (oldEnd - oldStart)
	output := newStart + slope*(v-oldStart)
	return output
}

func last[E any, S ~[]E](s S) E {
	return s[len(s)-1]
}

func lastPtr[E any, S ~[]E](s S) *E {
	return &s[len(s)-1]
}

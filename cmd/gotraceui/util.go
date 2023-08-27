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

func ifelse[T any](b bool, x, y T) T {
	if b {
		return x
	} else {
		return y
	}
}

func firstNonNil[E any, T ~*E](els ...T) T {
	for _, el := range els {
		if el != nil {
			return el
		}
	}
	return nil
}

type Option[T any] struct {
	v   T
	set bool
}

func None[T any]() Option[T] {
	return Option[T]{
		set: false,
	}
}

func Some[T any](v T) Option[T] {
	return Option[T]{
		v:   v,
		set: true,
	}
}

func (m Option[T]) Get() (T, bool) {
	return m.v, m.set
}

package container

import "fmt"

type Option[T any] struct {
	v   T
	set bool
}

func (opt Option[T]) String() string {
	if !opt.set {
		return "None"
	}
	return fmt.Sprintf("%v", opt.v)
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

func (m Option[T]) GetOr(alt T) T {
	if m.set {
		return m.v
	} else {
		return alt
	}
}

func (m Option[T]) Set() bool {
	return m.set
}

func (m Option[T]) MustGet() T {
	if !m.set {
		panic("called MustGet on unset Option")
	}
	return m.v
}

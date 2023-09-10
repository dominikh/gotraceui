package container

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

func (m Option[T]) GetOr(alt T) T {
	if m.set {
		return m.v
	} else {
		return alt
	}
}

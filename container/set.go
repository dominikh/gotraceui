package container

type Set[T comparable] map[T]struct{}

func (set Set[T]) Add(v T) {
	set[v] = struct{}{}
}

func (set Set[T]) Delete(v T) {
	delete(set, v)
}

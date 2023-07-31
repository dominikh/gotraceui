package mem

import "gioui.org/op"

const allocatorBucketSize = 64

// BucketSlice is like a slice, but grows one bucket at a time, instead of growing exponentially. This allows
// for overall lower memory usage when the total capacity isn't known ahead of time, at the cost of more
// overall allocations.
type BucketSlice[T any] struct {
	n       int
	buckets [][]T
}

// Append appends v to the slice and returns a pointer to the new element.
func (l *BucketSlice[T]) Append(v T) *T {
	a, _ := l.index(l.n)
	if a >= len(l.buckets) {
		l.buckets = append(l.buckets, make([]T, 0, allocatorBucketSize))
	}
	l.buckets[a] = append(l.buckets[a], v)
	ptr := &l.buckets[a][len(l.buckets[a])-1]
	l.n++
	return ptr
}

func (l *BucketSlice[T]) index(i int) (int, int) {
	return i / allocatorBucketSize, i % allocatorBucketSize
}

func (l *BucketSlice[T]) Ptr(i int) *T {
	a, b := l.index(i)
	return &l.buckets[a][b]
}

func (l *BucketSlice[T]) Get(i int) T {
	a, b := l.index(i)
	return l.buckets[a][b]
}

func (l *BucketSlice[T]) Set(i int, v T) {
	a, b := l.index(i)
	l.buckets[a][b] = v
}

func (l *BucketSlice[T]) Len() int {
	return l.n
}

func (l *BucketSlice[T]) Reset() {
	for i := range l.buckets {
		l.buckets[i] = l.buckets[i][:0]
	}
	l.n = 0
}

func (l *BucketSlice[T]) Truncate(n int) {
	if n >= l.n {
		return
	}
	a, b := l.index(n)
	l.buckets[a] = l.buckets[a][:b]
	for i := a + 1; i < len(l.buckets); i++ {
		l.buckets[i] = l.buckets[i][:0]
	}
	l.n = n
}

type ReusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *ReusableOps) Get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
}

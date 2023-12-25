package mem

import (
	"gioui.org/op"
)

const allocatorBucketSize = 64

// BucketSlice is like a slice, but grows one bucket at a time, instead of growing exponentially. This allows
// for overall lower memory usage when the total capacity isn't known ahead of time, at the cost of more
// overall allocations.
type BucketSlice[T any] struct {
	n       int
	buckets [][]T
}

// Grow grows the slice by one and returns a pointer to the new element, without overwriting it.
func (l *BucketSlice[T]) Grow() *T {
	a, _ := l.index(l.n)
	if a >= len(l.buckets) {
		l.buckets = append(l.buckets, make([]T, 0, allocatorBucketSize))
	}
	l.buckets[a] = l.buckets[a][:len(l.buckets[a])+1]
	ptr := &l.buckets[a][len(l.buckets[a])-1]
	l.n++
	return ptr
}

// GrowN grows the slice by n elements.
func (l *BucketSlice[T]) GrowN(n int) {
	for i := 0; i < n; i++ {
		l.Grow()
	}
}

// Append appends v to the slice and returns a pointer to the new element.
func (l *BucketSlice[T]) Append(v T) *T {
	ptr := l.Grow()
	*ptr = v
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

const largeAllocatorBucketSize = 524288

type LargeBucketSlice[T any] struct {
	n       int
	buckets [][]T
}

func (l *LargeBucketSlice[T]) Grow() *T {
	a, b := l.index(l.n)
	if a >= len(l.buckets) {
		l.buckets = append(l.buckets, make([]T, allocatorBucketSize))
	}
	ptr := &l.buckets[a][b]
	l.n++
	return ptr
}

func (l *LargeBucketSlice[T]) GrowN(n int) {
	for i := 0; i < n; i++ {
		l.Grow()
	}
}

// Append appends v to the slice and returns a pointer to the new element.
func (l *LargeBucketSlice[T]) Append(v T) *T {
	ptr := l.Grow()
	*ptr = v
	return ptr
}

func (l *LargeBucketSlice[T]) index(i int) (int, int) {
	// Doing the division on uint instead of int compiles this function to a shift and an AND (for power of 2
	// bucket sizes), versus a whole bunch of instructions for int.
	return int(uint(i) / allocatorBucketSize), int(uint(i) % allocatorBucketSize)
}

func (l *LargeBucketSlice[T]) Ptr(i int) *T {
	a, b := l.index(i)
	return &l.buckets[a][b]
}

func (l *LargeBucketSlice[T]) Get(i int) T {
	a, b := l.index(i)
	return l.buckets[a][b]
}

func (l *LargeBucketSlice[T]) Set(i int, v T) {
	a, b := l.index(i)
	l.buckets[a][b] = v
}

func (l *LargeBucketSlice[T]) Len() int { return l.n }
func (l *LargeBucketSlice[T]) Reset()   { l.n = 0 }

func (l *LargeBucketSlice[T]) Truncate(n int) {
	if n >= l.n {
		return
	}
	a, b := l.index(n)
	clear(l.buckets[a][b:])
	for i := a + 1; i < len(l.buckets); i++ {
		clear(l.buckets[i])
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

// AllocationCache is a trivial cache of allocations. Put appends a value to a slice and Get pops a value from the
// slice, or allocates a new value.
type AllocationCache[T any] struct {
	items []*T
}

func (c *AllocationCache[T]) Put(x *T) {
	c.items = append(c.items, x)
}

func (c *AllocationCache[T]) Get() *T {
	if len(c.items) == 0 {
		return new(T)
	} else {
		item := c.items[len(c.items)-1]
		c.items = c.items[:len(c.items)-1]
		return item
	}
}

// GrowLen increases the slice's length by n elements.
func GrowLen[S ~[]E, E any](s S, n int) S {
	return append(s, make([]E, n)...)
}

func EnsureLen[S ~[]E, E any](s S, n int) S {
	if len(s) >= n {
		return s
	}
	return GrowLen(s, n-len(s))
}

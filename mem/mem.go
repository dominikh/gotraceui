package mem

import (
	"math/bits"

	"honnef.co/go/gotraceui/mysync"

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

type ReusableOps struct {
	ops op.Ops
}

// get resets and returns an op.Ops
func (rops *ReusableOps) Get() *op.Ops {
	rops.ops.Reset()
	return &rops.ops
}

// Mapping from log2 to size that is to be used
//
// TODO(dh): remove some of the steps
var poolSizes = [...]int{
	0:  1,
	1:  2,
	2:  4,
	3:  8,
	4:  16,
	5:  32,
	6:  64,
	7:  128,
	8:  256,
	9:  512,
	10: 1024,
	11: 2048,
	12: 4096,
	13: 8192,
	14: 16384,
	15: 32768,
	16: 65536,
	17: 131072,
	18: 262144,
	19: 524288,
	20: 1048576,
	21: 2097152,
	22: 4194304,
	23: 8388608,
	24: 16777216,
	25: 33554432,
	26: 67108864,
}

// ConcurrentSliceCache caches slices, grouped by capacities. When requesting a slice, the requested capacity
// is rounded up to the next group and then fetched from a sync.Pool dedicated to that group.
type ConcurrentSliceCache[E any, T ~[]E] struct {
	pools [len(poolSizes)]*mysync.Pool[T]
}

func NewConcurrentSliceCache[E any, T ~[]E]() *ConcurrentSliceCache[E, T] {
	var pools [len(poolSizes)]*mysync.Pool[T]
	for i, v := range poolSizes {
		i := i
		v := v
		if i == 0 {
			pools[i] = mysync.NewPool(func() T { return make(T, 0, 1) })
			continue
		}

		if v == poolSizes[i-1] {
			pools[i] = pools[i-1]
		} else {
			pools[i] = mysync.NewPool(func() T { return make(T, 0, v) })
		}
	}

	return &ConcurrentSliceCache[E, T]{
		pools: pools,
	}
}

func (c *ConcurrentSliceCache[E, T]) Put(s T) {
	log2 := 64 - bits.LeadingZeros64(uint64(cap(s))-1)
	if log2 >= len(poolSizes) {
		return
	}
	c.pools[log2].Put(s[:0])
}

func (c *ConcurrentSliceCache[E, T]) Get(minCap int) T {
	if minCap < 0 {
		panic("invalid capacity")
	}
	if minCap == 0 {
		return T{}
	}

	// log₂ of next power of two
	log2 := 64 - bits.LeadingZeros64(uint64(minCap)-1)
	if log2 >= len(poolSizes) {
		// This is larger than any slices we cache
		return make(T, 0, minCap)
	}

	return c.pools[log2].Get()[:0]
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

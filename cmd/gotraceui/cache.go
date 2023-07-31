package main

import (
	"math/bits"
	"sync"

	"honnef.co/go/gotraceui/trace/ptrace"
)

var (
	uint64SliceCache        = NewConcurrentSliceCache[uint64, []uint64]()
	boolSliceCache          = NewConcurrentSliceCache[bool, []bool]()
	spanSliceCache          = NewConcurrentSliceCache[ptrace.Span, []ptrace.Span]()
	stackSpanMetaSliceCache = NewConcurrentSliceCache[stackSpanMeta, []stackSpanMeta]()
)

type Cache[T any] struct {
	items []*T
}

func (c *Cache[T]) Put(x *T) {
	c.items = append(c.items, x)
}

func (c *Cache[T]) Get() *T {
	if len(c.items) == 0 {
		return new(T)
	} else {
		item := c.items[len(c.items)-1]
		c.items = c.items[:len(c.items)-1]
		return item
	}
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

type ConcurrentSliceCache[E any, T ~[]E] struct {
	pools [len(poolSizes)]*sync.Pool
}

func NewConcurrentSliceCache[E any, T ~[]E]() *ConcurrentSliceCache[E, T] {
	var pools [len(poolSizes)]*sync.Pool
	for i, v := range poolSizes {
		i := i
		v := v
		if i == 0 {
			pools[i] = &sync.Pool{
				New: func() any {
					return make(T, 0, 1)
				},
			}
			continue
		}

		if v == poolSizes[i-1] {
			pools[i] = pools[i-1]
		} else {
			pools[i] = &sync.Pool{
				New: func() any {
					return make(T, 0, v)
				},
			}
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

	return c.pools[log2].Get().(T)[:0]
}

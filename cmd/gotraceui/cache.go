package main

import (
	"sync"

	"honnef.co/go/gotraceui/trace/ptrace"
)

var (
	uint64SliceCache        = &ConcurrentSliceCache[uint64, []uint64]{}
	boolSliceCache          = &ConcurrentSliceCache[bool, []bool]{}
	spanSliceCache          = &ConcurrentSliceCache[ptrace.Span, []ptrace.Span]{}
	stackSpanMetaSliceCache = &ConcurrentSliceCache[stackSpanMeta, []stackSpanMeta]{}
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

type SliceCache[E any, T ~[]E] struct {
	// OPT(dh): use a sorted data structure that we can query and modify in sub-linear time.
	items []T
}

func (c *SliceCache[E, T]) Put(s T) {
	if cap(s) == 0 {
		return
	}
	c.items = append(c.items, s)
}

func (c *SliceCache[E, T]) Get(minCap int) T {
	for i, s := range c.items {
		if cap(s) < minCap {
			continue
		}
		c.items[i] = nil
		if n := len(c.items) - 1; i < n {
			c.items[i], c.items[n] = c.items[n], c.items[i]
			c.items = c.items[:n]
		} else {
			c.items = c.items[:n]
		}
		return s[:0]
	}
	return make(T, 0, minCap)
}

func (c *SliceCache[E, T]) Clear() {
	// TODO(dh): if T is storing pointers then we should clear each slice, too
	clear(c.items)
	c.items = c.items[:0]
}

type ConcurrentSliceCache[E any, T ~[]E] struct {
	mu    sync.Mutex
	cache SliceCache[E, T]
}

func (c *ConcurrentSliceCache[E, T]) Put(s T) {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.cache.Put(s)
}

func (c *ConcurrentSliceCache[E, T]) Get(minCap int) T {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.cache.Get(minCap)
}

func (c *ConcurrentSliceCache[E, T]) Clear() {
	c.mu.Lock()
	defer c.mu.Unlock()
	c.cache.Clear()
}

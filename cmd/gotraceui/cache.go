package main

import (
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/trace/ptrace"
)

var (
	uint64SliceCache        = mem.NewConcurrentSliceCache[uint64, []uint64]()
	boolSliceCache          = mem.NewConcurrentSliceCache[bool, []bool]()
	spanSliceCache          = mem.NewConcurrentSliceCache[ptrace.Span, []ptrace.Span]()
	stackSpanMetaSliceCache = mem.NewConcurrentSliceCache[stackSpanMeta, []stackSpanMeta]()
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

package main

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

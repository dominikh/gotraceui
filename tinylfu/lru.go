package tinylfu

import "honnef.co/go/gotraceui/tinylfu/internal/list"

// Cache is an LRU cache.  It is not safe for concurrent access.
type lruCache[V any] struct {
	data map[string]*list.Element[*slruItem[V]]
	cap  int
	ll   *list.List[*slruItem[V]]
}

func newLRU[V any](cap int, data map[string]*list.Element[*slruItem[V]]) *lruCache[V] {
	return &lruCache[V]{
		data: data,
		cap:  cap,
		ll:   list.New[*slruItem[V]](),
	}
}

// Get returns a value from the cache
func (lru *lruCache[V]) get(v *list.Element[*slruItem[V]]) {
	lru.ll.MoveToFront(v)
}

// Set sets a value in the cache
func (lru *lruCache[V]) add(newitem slruItem[V]) (oitem slruItem[V], evicted bool) {
	if lru.ll.Len() < lru.cap {
		lru.data[newitem.key] = lru.ll.PushFront(&newitem)
		return slruItem[V]{}, false
	}

	// reuse the tail item
	e := lru.ll.Back()
	item := e.Value

	delete(lru.data, item.key)

	oitem = *item
	*item = newitem

	lru.data[item.key] = e
	lru.ll.MoveToFront(e)

	return oitem, true
}

// Len returns the total number of items in the cache
func (lru *lruCache[V]) Len() int {
	return len(lru.data)
}

// Remove removes an item from the cache, returning the item and a boolean indicating if it was found
func (lru *lruCache[V]) Remove(key string) (V, bool) {
	v, ok := lru.data[key]
	if !ok {
		return *new(V), false
	}
	item := v.Value
	lru.ll.Remove(v)
	delete(lru.data, key)
	return item.value, true
}

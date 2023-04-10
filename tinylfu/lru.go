package tinylfu

import "honnef.co/go/gotraceui/tinylfu/internal/list"

// Cache is an LRU cache.  It is not safe for concurrent access.
type lruCache[K comparable, V any] struct {
	data map[K]*list.Element[*slruItem[K, V]]
	cap  int
	ll   *list.List[*slruItem[K, V]]
}

func newLRU[K comparable, V any](cap int, data map[K]*list.Element[*slruItem[K, V]]) *lruCache[K, V] {
	return &lruCache[K, V]{
		data: data,
		cap:  cap,
		ll:   list.New[*slruItem[K, V]](),
	}
}

// Get returns a value from the cache
func (lru *lruCache[K, V]) get(v *list.Element[*slruItem[K, V]]) {
	lru.ll.MoveToFront(v)
}

// Set sets a value in the cache
func (lru *lruCache[K, V]) add(newitem slruItem[K, V]) (oitem slruItem[K, V], evicted bool) {
	if lru.ll.Len() < lru.cap {
		lru.data[newitem.key] = lru.ll.PushFront(&newitem)
		return slruItem[K, V]{}, false
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
func (lru *lruCache[K, V]) Len() int {
	return len(lru.data)
}

// Remove removes an item from the cache, returning the item and a boolean indicating if it was found
func (lru *lruCache[K, V]) Remove(key K) (V, bool) {
	v, ok := lru.data[key]
	if !ok {
		return *new(V), false
	}
	item := v.Value
	lru.ll.Remove(v)
	delete(lru.data, key)
	return item.value, true
}

package tinylfu

import "honnef.co/go/gotraceui/tinylfu/internal/list"

type slruItem[K comparable, V any] struct {
	listid int
	key    K
	value  V
	keyh   uint64
}

// Cache is an LRU cache.  It is not safe for concurrent access.
type slruCache[K comparable, V any] struct {
	data           map[K]*list.Element[*slruItem[K, V]]
	onecap, twocap int
	one, two       *list.List[*slruItem[K, V]]
}

func newSLRU[K comparable, V any](onecap, twocap int, data map[K]*list.Element[*slruItem[K, V]]) *slruCache[K, V] {
	return &slruCache[K, V]{
		data:   data,
		onecap: onecap,
		one:    list.New[*slruItem[K, V]](),
		twocap: twocap,
		two:    list.New[*slruItem[K, V]](),
	}
}

// get updates the cache data structures for a get
func (slru *slruCache[K, V]) get(v *list.Element[*slruItem[K, V]]) {
	item := v.Value

	// already on list two?
	if item.listid == 2 {
		slru.two.MoveToFront(v)
		return
	}

	// must be list one

	// is there space on the next list?
	if slru.two.Len() < slru.twocap {
		// just do the remove/add
		slru.one.Remove(v)
		item.listid = 2
		slru.data[item.key] = slru.two.PushFront(item)
		return
	}

	back := slru.two.Back()
	bitem := back.Value

	// swap the key/values
	*bitem, *item = *item, *bitem

	bitem.listid = 2
	item.listid = 1

	// update pointers in the map
	slru.data[item.key] = v
	slru.data[bitem.key] = back

	// move the elements to the front of their lists
	slru.one.MoveToFront(v)
	slru.two.MoveToFront(back)
}

// Set sets a value in the cache
func (slru *slruCache[K, V]) add(newitem slruItem[K, V]) {

	newitem.listid = 1

	if slru.one.Len() < slru.onecap || (slru.Len() < slru.onecap+slru.twocap) {
		slru.data[newitem.key] = slru.one.PushFront(&newitem)
		return
	}

	// reuse the tail item
	e := slru.one.Back()
	item := e.Value

	delete(slru.data, item.key)

	*item = newitem

	slru.data[item.key] = e
	slru.one.MoveToFront(e)
}

func (slru *slruCache[K, V]) victim() *slruItem[K, V] {

	if slru.Len() < slru.onecap+slru.twocap {
		return nil
	}

	v := slru.one.Back()

	return v.Value
}

// Len returns the total number of items in the cache
func (slru *slruCache[K, V]) Len() int {
	return slru.one.Len() + slru.two.Len()
}

// Remove removes an item from the cache, returning the item and a boolean indicating if it was found
func (slru *slruCache[K, V]) Remove(key K) (V, bool) {
	v, ok := slru.data[key]
	if !ok {
		return *new(V), false
	}

	item := v.Value

	if item.listid == 2 {
		slru.two.Remove(v)
	} else {
		slru.one.Remove(v)
	}

	delete(slru.data, key)

	return item.value, true
}

// Package tinylfu is an implementation of the TinyLFU caching algorithm
/*
   http://arxiv.org/abs/1512.00727
*/
package tinylfu

import (
	"hash/maphash"
	"unsafe"

	"honnef.co/go/gotraceui/tinylfu/internal/list"
)

type T[K comparable, V any] struct {
	c       *cm4
	bouncer *doorkeeper
	w       int
	samples int
	lru     *lruCache[K, V]
	slru    *slruCache[K, V]
	data    map[K]*list.Element[*slruItem[K, V]]
	seed    maphash.Seed
}

func New[K comparable, V any](size int, samples int) *T[K, V] {

	const lruPct = 1

	lruSize := (lruPct * size) / 100
	if lruSize < 1 {
		lruSize = 1
	}
	slruSize := size - lruSize
	if slruSize < 1 {
		slruSize = 1

	}
	slru20 := slruSize / 5
	if slru20 < 1 {
		slru20 = 1
	}

	data := make(map[K]*list.Element[*slruItem[K, V]], size)

	return &T[K, V]{
		c:       newCM4(size),
		w:       0,
		samples: samples,
		bouncer: newDoorkeeper(samples, 0.01),

		data: data,

		lru:  newLRU(lruSize, data),
		slru: newSLRU(slru20, slruSize-slru20, data),

		seed: maphash.MakeSeed(),
	}
}

func (t *T[K, V]) Get(key K) (V, bool) {

	t.w++
	if t.w == t.samples {
		t.c.reset()
		t.bouncer.reset()
		t.w = 0
	}

	val, ok := t.data[key]
	if !ok {
		var akey string
		switch ikey := any(key).(type) {
		case string:
			akey = ikey
		default:
			akey = unsafe.String((*byte)(unsafe.Pointer(&key)), unsafe.Sizeof(key))
		}
		keyh := maphash.String(t.seed, akey)
		t.c.add(keyh)
		return *new(V), false
	}

	item := val.Value

	t.c.add(item.keyh)

	v := item.value
	if item.listid == 0 {
		t.lru.get(val)
	} else {
		t.slru.get(val)
	}

	return v, true
}

func (t *T[K, V]) Add(key K, val V) {

	if e, ok := t.data[key]; ok {
		// Key is already in our cache.
		// `Add` will act as a `Get` for list movements
		item := e.Value
		item.value = val
		t.c.add(item.keyh)

		if item.listid == 0 {
			t.lru.get(e)
		} else {
			t.slru.get(e)
		}
		return
	}

	var akey string
	switch ikey := any(key).(type) {
	case string:
		akey = ikey
	default:
		akey = unsafe.String((*byte)(unsafe.Pointer(&key)), unsafe.Sizeof(key))
	}
	newitem := slruItem[K, V]{0, key, val, maphash.String(t.seed, akey)}

	oitem, evicted := t.lru.add(newitem)
	if !evicted {
		return
	}

	// estimate count of what will be evicted from slru
	victim := t.slru.victim()
	if victim == nil {
		t.slru.add(oitem)
		return
	}

	if !t.bouncer.allow(oitem.keyh) {
		return
	}

	vcount := t.c.estimate(victim.keyh)
	ocount := t.c.estimate(oitem.keyh)

	if ocount < vcount {
		return
	}

	t.slru.add(oitem)
}

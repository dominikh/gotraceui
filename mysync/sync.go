package mysync

import (
	"runtime"
	"sync"
)

func Distribute[T any](items []T, limit int, fn func(group int, step int, subitems []T) error) error {
	if len(items) == 0 {
		return nil
	}

	if limit <= 0 {
		limit = runtime.GOMAXPROCS(0)
	}

	if limit > len(items) {
		limit = len(items)
	}

	step := len(items) / limit
	var muGerr sync.Mutex
	var gerr error
	var wg sync.WaitGroup
	wg.Add(limit)
	for g := 0; g < limit; g++ {
		g := g
		go func() {
			defer wg.Done()
			var subset []T
			if g < limit-1 {
				subset = items[g*step : (g+1)*step]
			} else {
				subset = items[g*step:]
			}
			if err := fn(g, step, subset); err != nil {
				muGerr.Lock()
				if gerr == nil {
					gerr = err
				}
				muGerr.Unlock()
			}
		}()
	}
	wg.Wait()
	return gerr
}

type Mutex[T any] struct {
	mu sync.RWMutex
	v  T
}

type MutexUnlock struct {
	mu *sync.RWMutex
}

type MutexRUnlock struct {
	mu *sync.RWMutex
}

func NewMutex[T any](v T) *Mutex[T] {
	return &Mutex[T]{v: v}
}

func (mu *Mutex[T]) Lock() (T, MutexUnlock) {
	mu.mu.Lock()
	return mu.v, MutexUnlock{&mu.mu}
}

func (mu *Mutex[T]) RLock() (T, MutexRUnlock) {
	mu.mu.RLock()
	return mu.v, MutexRUnlock{&mu.mu}
}

func (u MutexUnlock) Unlock()   { u.mu.Unlock() }
func (u MutexRUnlock) RUnlock() { u.mu.RUnlock() }

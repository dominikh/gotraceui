package mysync

import (
	"sync"
)

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

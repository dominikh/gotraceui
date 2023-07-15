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

package theme

import (
	"time"

	"gioui.org/app"
)

type Future[T any] struct {
	// Concurrently accessed by computation function, controller, and user code
	result chan T

	// Accessed in the window goroutine
	//
	// The channel itself is accessed concurrently, but the field isn't
	cancelled chan struct{}
	res       T
	resSet    bool
	read      bool
	fn        func(cancelled chan struct{})
	ff        *Futures
}

type future interface {
	wasRead() bool
	isDone() bool
	cancel()
}

func Immediate[T any](value T) *Future[T] {
	return &Future[T]{
		res:    value,
		resSet: true,
	}
}

func NewFuture[T any](win *Window, fn func(cancelled <-chan struct{}) T) *Future[T] {
	ft := &Future[T]{
		cancelled: make(chan struct{}),
		result:    make(chan T, 1),
		ff:        win.Futures,
		// Don't immediately cancel future if it was created but not read from in this frame
		// read: true,
	}
	ft.fn = func(cancelled chan struct{}) {
		res := fn(cancelled)
		select {
		case <-cancelled:
			// We got cancelled, the return value is meaningless
		default:
			select {
			case ft.result <- res:
				// We've got the result of the computation. Invalidate the window so a new frame gets drawn with the
				// result.
				win.AppWindow.Invalidate()
			default:
				// We've already gotten a valid result before and this goroutine raced with it. Discard the new result.
				//
				// Invalidate the frame, anyway, in case canceling raced with reading the value earlier.
				win.AppWindow.Invalidate()
			}
		}
	}

	ft.ff.add(ft)
	go ft.fn(ft.cancelled)

	return ft
}

func (ft *Future[T]) wasRead() bool {
	b := ft.read
	ft.read = false
	return b
}

func (ft *Future[T]) cancel() {
	close(ft.cancelled)
}

func (ft *Future[T]) Result() (T, bool) {
	if ft.resSet {
		// We already have the value
		return ft.res, true
	}

	// Prevent sweep from cancelling this future
	ft.read = true

	t := time.NewTimer(500 * time.Microsecond)
	defer t.Stop()
	select {
	case res := <-ft.result:
		// First time reading the computed value
		ft.res = res
		ft.resSet = true
		return res, true
	case <-ft.cancelled:
		// Don't discard result if cancellation raced with the computation finishing.
		select {
		case res := <-ft.result:
			ft.res = res
			ft.resSet = true
			return res, true
		default:
		}

		// Future got cancelled and reused later, restart the computation
		ft.cancelled = make(chan struct{})
		// Re-add the future to the controller so it gets swept again
		ft.ff.add(ft)
		go ft.fn(ft.cancelled)
		return *new(T), false
	case <-t.C:
		// Not ready yet, we'll try again next frame
		return *new(T), false
	}
}

func (ft *Future[T]) isDone() bool {
	return ft.resSet
}

type Futures struct {
	win     *app.Window
	futures []future
}

func (ff *Futures) Sweep() {
	n := ff.futures[:0]
	for _, ft := range ff.futures {
		if ft.isDone() {
			continue
		}

		if !ft.wasRead() {
			// The future wasn't read this frame. Cancel the work it is doing, under the assumption that it won't be
			// needed anymore. If it ends up being needed later (e.g. because the user went back to an old panel), the
			// future will restart itself.
			ft.cancel()
			continue
		}

		n = append(n, ft)
	}
	ff.futures = n
}

func (ff *Futures) add(ft future) {
	ff.futures = append(ff.futures, ft)
}

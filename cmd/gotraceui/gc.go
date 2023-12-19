package main

import (
	"context"
	rdebug "runtime/debug"
	"runtime/metrics"
	rtrace "runtime/trace"
	"time"
)

// GCScheduler monitors memory usage and manipulates GOGC to enforce an absolute limit on the amount of
// possible garbage.
type GCScheduler struct {
	// How much additional memory can be allocated before GC has to run. Defaults to 1 GiB.
	Overhead int
	// Don't manage GOGC until live memory exceeds this threshold. Defaults to Overhead, set to positive
	// non-zero value to override.
	Threshold     int
	pause, resume chan struct{}
}

func NewGCScheduler(overhead, threshold int) *GCScheduler {
	return &GCScheduler{
		Overhead:  overhead,
		Threshold: threshold,
		pause:     make(chan struct{}),
		resume:    make(chan struct{}),
	}
}

func (gc *GCScheduler) Pause() {
	gc.pause <- struct{}{}
}

func (gc *GCScheduler) Resume() {
	gc.resume <- struct{}{}
}

func (gc *GCScheduler) Run() {
	// Collecting /gc/heap metrics is cheaper than calling runtime.ReadMemStats, but it isn't free. It still
	// involves locking, disabling preemption, and waiting on writers. Don't do it too often.
	const interval = 1 * time.Second
	t := time.NewTicker(interval)
	defer t.Stop()

	// Get original value of GOGC, which we restore when pausing. There is no way to get the current GOGC
	// value without setting a new one. We temporarily set it to 100 because that's most likely the GOGC we're
	// already running with. This is nicer than setting it to -1, as GOGC=off sets a heap target of MaxUint64,
	// which makes looking at traces of Gotraceui slightly more annoying.
	origGOGC := rdebug.SetGCPercent(-1)
	prevGOGC := origGOGC
	rdebug.SetGCPercent(origGOGC)

	// The sum of these 3 metrics matches what the Go runtime uses to set the GC goal (as per
	// $GOROOT/src/runtime/mgcpacer.go.)
	samples := []metrics.Sample{
		{
			// The size of the heap, as per the last GC cycle
			Name: "/gc/heap/live:bytes",
		},
		{
			// The size of scannable stacks
			Name: "/gc/scan/stack:bytes",
		},
		{
			// The size of scannable globals
			Name: "/gc/scan/globals:bytes",
		},
	}
	var overhead, threshold uint64
	if gc.Overhead <= 0 {
		overhead = 1024 * 1024 * 1024 // 1 GiB
	} else {
		overhead = uint64(gc.Overhead)
	}
	if gc.Threshold <= 0 {
		threshold = overhead
	} else {
		threshold = uint64(gc.Threshold)
	}
	for {
		select {
		case <-t.C:
			metrics.Read(samples)
			if samples[0].Value.Kind() == metrics.KindBad {
				// This version of Go doesn't support the requested metric. Stop the timer, but don't return,
				// so that Pause and Resume continue to work, albeit as (mostly) no-ops.
				rtrace.Logf(context.Background(), "GCScheduler", "missing metric, turning on Go's pacer")
				t.Stop()
				rdebug.SetGCPercent(origGOGC)
				continue
			}
			var live uint64
			for _, s := range samples {
				if s.Value.Kind() == metrics.KindBad {
					// We don't care if we couldn't get the size of stacks or globals, and we've already
					// validated the heap metric.
					continue
				}
				live += s.Value.Uint64()
			}

			var newGOGC int
			if live < threshold {
				// Guard against computing invalid GOGC (when live == 0). Also default to normal GC pacing if
				// the live memory is smaller than the threshold.
				rtrace.Logf(context.Background(), "GCScheduler", "live %d < threshold %d, setting GOGC=100", live, threshold)
				newGOGC = 100
			} else {
				newGOGC = int(100 * (float64(live+overhead)/float64(live) - 1))
				rtrace.Logf(context.Background(), "GCScheduler", "live %d, overhead %d, setting GOGC=%d", live, overhead, newGOGC)
			}
			// Don't call SetGCPercent unnecessarily. It has to run on the system stack and hold a lock.
			if newGOGC != prevGOGC {
				rtrace.Logf(context.Background(), "GCScheduler", "prevGOGC=%d != newGOGC=%d, updating GOGC", prevGOGC, newGOGC)
				rdebug.SetGCPercent(newGOGC)
			}

			prevGOGC = newGOGC
		case <-gc.pause:
			rtrace.Logf(context.Background(), "GCScheduler", "pausing scheduler, restoring original GOGC %d", origGOGC)
			t.Stop()
			rdebug.SetGCPercent(origGOGC)
		case <-gc.resume:
			rtrace.Logf(context.Background(), "GCScheduler", "resuming scheduler")
			t.Reset(interval)
		}
	}
}

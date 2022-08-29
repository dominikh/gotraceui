package main

import (
	"errors"
	"fmt"
	"image/color"
	"os"
	"runtime"
	"runtime/pprof"
	"sync"
	"time"

	"honnef.co/go/gotraceui/trace"
)

var (
	errExitAfterParsing = errors.New("we were instructed to exit after parsing")
	errExitAfterLoading = errors.New("we were instructed to exit after loading")
)

type debugGraph struct {
	title           string
	width           time.Duration
	background      color.NRGBA
	fixedZero       bool
	stickyLastValue bool

	mu     sync.Mutex
	values []struct {
		when time.Time
		val  float64
	}
}

type DebugWindow struct {
	tlStart           debugGraph
	tlEnd             debugGraph
	tlY               debugGraph
	tlPxPerNs         debugGraph
	animationProgress debugGraph
	animationRatio    debugGraph
	frametimes        debugGraph
}

// For debugging.
//
//lint:ignore U1000 debug aid we sometimes use.
func dumpFrames(frames []*trace.Frame) {
	if len(frames) == 0 {
		fmt.Println("no frames")
	}
	for _, f := range frames {
		fmt.Println(f)
	}
}

func writeMemprofile(s string) {
	f, err := os.Create(s)
	if err != nil {
		fmt.Fprintln(os.Stderr, "couldn't write memory profile:", err)
		return
	}
	defer f.Close()
	runtime.GC()
	if err := pprof.WriteHeapProfile(f); err != nil {
		fmt.Fprintln(os.Stderr, "couldn't write memory profile:", err)
	}
}

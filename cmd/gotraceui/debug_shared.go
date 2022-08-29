package main

import (
	"fmt"
	"image/color"
	"sync"
	"time"

	"honnef.co/go/gotraceui/trace"
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

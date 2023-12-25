package main

import (
	"honnef.co/go/gotraceui/trace/ptrace"

	exptrace "golang.org/x/exp/trace"
)

type Trace struct {
	*ptrace.Trace

	// The offset to apply to all timestamps from the trace.
	TimeOffset exptrace.Time

	GOROOT string
	GOPATH string

	allGoroutineSpanLabels [][]string
	allProcessorSpanLabels [][]string
}

// AdjustedTime represents a timestamp with the time offset already applied.
type AdjustedTime exptrace.Time

func (t *Trace) AdjustedTime(ts exptrace.Time) AdjustedTime {
	return AdjustedTime(ts + t.TimeOffset)
}

func (t *Trace) UnadjustedTime(ts AdjustedTime) exptrace.Time {
	return exptrace.Time(ts) - t.TimeOffset
}

func (t *Trace) goroutineSpanLabels(g *ptrace.Goroutine) []string {
	return t.allGoroutineSpanLabels[g.SeqID]
}

func (t *Trace) processorSpanLabels(p *ptrace.Processor) []string {
	return t.allProcessorSpanLabels[p.SeqID]
}

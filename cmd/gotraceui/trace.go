package main

import (
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
)

type reason uint8

const (
	reasonNone reason = iota
	reasonNewlyCreated
	reasonGosched
	reasonTimeSleep
	reasonPreempted
)

var reasonByEventType = [256]reason{
	trace.EvGoCreate:  reasonNewlyCreated,
	trace.EvGoSched:   reasonGosched,
	trace.EvGoSleep:   reasonTimeSleep,
	trace.EvGoPreempt: reasonPreempted,
}

type Trace struct {
	*ptrace.Trace

	allGoroutineSpanLabels   [][]string
	allProcessorSpanLabels   [][]string
	allGoroutineFilterLabels [][]string
	allProcessorFilterLabels [][]string
}

func (t *Trace) goroutineSpanLabels(g *ptrace.Goroutine) []string {
	return t.allGoroutineSpanLabels[g.SeqID]
}

func (t *Trace) processorSpanLabels(p *ptrace.Processor) []string {
	return t.allProcessorSpanLabels[p.SeqID]
}

func (t *Trace) goroutineFilterLabels(g *ptrace.Goroutine) []string {
	return t.allGoroutineFilterLabels[g.SeqID]
}

func (t *Trace) processorFilterLabels(p *ptrace.Processor) []string {
	return t.allProcessorFilterLabels[p.SeqID]
}

//gcassert:inline
func (t *Trace) Reason(s ptrace.Span) reason {
	return reasonByEventType[t.Events[s.Event].Type]
}

// MergedSpans and Spans have the same functionality. The two different types are used to make APIs easier to read, to
// be able to tell apart functions that operate on multiple spans as if they were individual items and functions that
// treat them as one unit, because they get merged during rendering.

type setProgresser interface {
	SetProgress(float64)
	SetProgressLossy(float64)
}

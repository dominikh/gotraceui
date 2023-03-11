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

package main

import (
	"fmt"
	"strconv"
	"strings"
	"time"

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
	// XXX why are we allocating these lazily? won't we need all of them the moment we use the filter window?

	labels := t.allGoroutineFilterLabels[g.SeqID]
	if labels == nil {
		labels = []string{
			strconv.FormatUint(g.ID, 10),
			local.Sprintf("%d", g.ID),
			fmt.Sprintf("g%d", g.ID),
			local.Sprintf("g%d", g.ID),
			g.Function,
			strings.ToLower(g.Function),
			"goroutine", // allow queries like "goroutine 1234" to work
		}
		t.allGoroutineFilterLabels[g.SeqID] = labels
	}
	return labels
}

func (t *Trace) processorFilterLabels(p *ptrace.Processor) []string {
	// XXX why are we allocating these lazily? won't we need all of them the moment we use the filter window?

	labels := t.allProcessorFilterLabels[p.SeqID]
	if labels == nil {
		labels = []string{
			strconv.FormatInt(int64(p.ID), 10),
			local.Sprintf("%d", p.ID),
			fmt.Sprintf("p%d", p.ID),
			local.Sprintf("p%d", p.ID),
			"processor", // allow queries like "processor 1234" to work
		}
		t.allProcessorFilterLabels[p.SeqID] = labels
	}
	return labels
}

//gcassert:inline
func (t *Trace) Reason(s *ptrace.Span) reason {
	return reasonByEventType[t.Events[s.Event].Type]
}

// MergedSpans and Spans have the same functionality. The two different types are used to make APIs easier to read, to
// be able to tell apart functions that operate on multiple spans as if they were individual items and functions that
// treat them as one unit, because they get merged during rendering.

// Spans represents a list of consecutive spans from a shared timeline.

// MergedSpans represents a list of consecutive spans from a shared timeline, which were merged during display.
//
// OPT(dh): we could theoretically save 8 bytes by storing the start and end indices instead of a slice, as merged
// spans have to be consecutive. It would also prevent potential misuse of MergedSpans, e.g. by creating an entirely
// new slice, instead of slicing an existing one. However, a slice is easier to access and iterate over.
type MergedSpans ptrace.Spans

func (ms MergedSpans) Start() trace.Timestamp  { return ptrace.Spans(ms).Start() }
func (ms MergedSpans) End() trace.Timestamp    { return ptrace.Spans(ms).End() }
func (ms MergedSpans) Duration() time.Duration { return ptrace.Spans(ms).Duration() }
func (ms MergedSpans) Events(all []ptrace.EventID, tr *Trace) []ptrace.EventID {
	return ptrace.Spans(ms).Events(all, tr.Trace)
}

type setProgresser interface {
	SetProgress(float64)
	SetProgressLossy(float64)
}

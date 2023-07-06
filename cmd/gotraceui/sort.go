package main

import (
	"sort"

	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
)

type indexable[T any] interface {
	At(idx int) T
	AtPtr(idx int) *T
	Len() int
}

func indirectlySortSpans(spans indexable[ptrace.Span]) ptrace.Spans {
	indices := make([]int, spans.Len())
	for i := range indices {
		indices[i] = i
	}
	sort.Slice(indices, func(i, j int) bool {
		i = indices[i]
		j = indices[j]
		return spans.At(i).Start < spans.At(j).Start
	})
	return indirectlySortedSpans{
		spans:   spans,
		indices: indices,
		start:   0,
		length:  spans.Len(),
	}
}

type indirectlySortedSpans struct {
	spans   indexable[ptrace.Span]
	indices []int

	start  int
	length int
}

func (ind indirectlySortedSpans) index(idx int) int {
	return ind.indices[ind.start+idx]
}

// At implements ptrace.Spans.
func (ind indirectlySortedSpans) At(idx int) ptrace.Span {
	return ind.spans.At(ind.index(idx))
}

// AtPtr implements ptrace.Spans.
func (ind indirectlySortedSpans) AtPtr(idx int) *ptrace.Span {
	return ind.spans.AtPtr(ind.index(idx))
}

// End implements ptrace.Spans.
func (ind indirectlySortedSpans) End() trace.Timestamp {
	return ind.spans.At(ind.index(ind.length - 1)).End
}

// Events implements ptrace.Spans.
func (ind indirectlySortedSpans) Events(all []ptrace.EventID, tr *ptrace.Trace) []ptrace.EventID {
	return ptrace.Events(ind, all, tr)
}

// Len implements ptrace.Spans.
func (ind indirectlySortedSpans) Len() int {
	return ind.length
}

// Slice implements ptrace.Spans.
func (ind indirectlySortedSpans) Slice(start int, end int) ptrace.Spans {
	ind.start += start
	ind.length = end - start
	return ind
}

// Start implements ptrace.Spans.
func (ind indirectlySortedSpans) Start() trace.Timestamp {
	return ind.spans.At(ind.index(0)).Start
}

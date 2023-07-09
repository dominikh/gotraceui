package ptrace

import (
	"fmt"
	"math"
	"sort"
	"time"
)

func ComputeProcessorBusy(tr *Trace, p *Processor, bucketSize time.Duration) []int {
	total := tr.Events[len(tr.Events)-1].Ts
	buckets := make([]time.Duration, int(math.Ceil(float64(total)/float64(bucketSize))))
	for i := 0; i < len(p.Spans); i++ {
		span := p.Spans[i]
		d := time.Duration(span.End - span.Start)
		bucket := time.Duration(span.Start) / bucketSize
		bucketRemainder := bucketSize - (time.Duration(span.Start) % bucketSize)

		for d > bucketRemainder {
			buckets[bucket] += bucketRemainder
			d -= bucketRemainder
			bucket++
			bucketRemainder = bucketSize
		}
		if d > 0 {
			buckets[bucket] += d
		}
	}

	out := make([]int, len(buckets))
	for i, n := range buckets {
		if n > bucketSize {
			panic(fmt.Sprintf("bucket %d has value %d, which exceeds bucket size of %d", i, n, bucketSize))
		}
		out[i] = int(math.Round((float64(n) / float64(bucketSize)) * 100))
	}

	return out
}

// Spans is an interface that allows ComputeStatistics to operate on abstract collections of spans, not just slices.
type Spans interface {
	At(idx int) Span
	AtPtr(idx int) *Span
	Len() int
}

func ToSpans(spans []Span) Spans {
	return spansSlice(spans)
}

type spansSlice []Span

func (spans spansSlice) At(idx int) Span     { return spans[idx] }
func (spans spansSlice) AtPtr(idx int) *Span { return &spans[idx] }
func (spans spansSlice) Len() int            { return len(spans) }

func ComputeStatistics(spans Spans) Statistics {
	var values [StateLast][]time.Duration

	var stats Statistics

	for i := range values {
		values[i] = values[i][:0]
	}

	for i := 0; i < spans.Len(); i++ {
		s := spans.AtPtr(i)
		stat := &stats[s.State]
		stat.Count++
		d := s.Duration()
		if d > stat.Max {
			stat.Max = d
		}
		if d < stat.Min || stat.Min == 0 {
			stat.Min = d
		}
		stat.Total += d
		values[s.State] = append(values[s.State], d)
	}

	for state := range stats {
		stat := &stats[state]

		if len(values[state]) == 0 {
			continue
		}

		stat.Average = float64(stat.Total) / float64(len(values[state]))

		sort.Slice(values[state], func(i, j int) bool {
			return values[state][i] < values[state][j]
		})

		if len(values[state])%2 == 0 {
			mid := len(values[state]) / 2
			stat.Median = float64(values[state][mid]+values[state][mid-1]) / 2
		} else {
			stat.Median = float64(values[state][len(values[state])/2])
		}
	}

	return stats
}

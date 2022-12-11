package ptrace

import (
	"fmt"
	"math"
	"time"
)

func ComputeProcessorBusy(tr *Trace, p *Processor, bucketSize time.Duration) []int {
	total := tr.Events[len(tr.Events)-1].Ts
	buckets := make([]time.Duration, int(math.Ceil(float64(total)/float64(bucketSize))))
	for _, span := range p.Spans {
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

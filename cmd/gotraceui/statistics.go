package main

import (
	"fmt"
	"math"
	"time"
)

func computeProcessorBusy(tr *Trace, p *Processor, bucketSize time.Duration) []int {
	total := tr.Events[len(tr.Events)-1].Ts
	buckets := make([]time.Duration, int(math.Ceil(float64(total)/float64(bucketSize))))
	for _, span := range p.spans {
		start := tr.Events[span.event()].Ts
		d := time.Duration(span.end - start)
		bucket := time.Duration(start) / bucketSize
		bucketRemainder := bucketSize - (time.Duration(start) % bucketSize)

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

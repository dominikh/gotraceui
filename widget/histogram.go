package widget

import (
	"math"
	"sort"
	"time"
)

type FloatDuration float64

func (d FloatDuration) Floor() time.Duration {
	return time.Duration(math.Floor(float64(d)))
}

func (d FloatDuration) Ceil() time.Duration {
	return time.Duration(math.Ceil(float64(d)))
}

type Histogram struct {
	// The config that was passed to NewHistogram
	Config      *HistogramConfig
	Start       FloatDuration
	Bins        []int
	BinWidth    FloatDuration
	Overflow    FloatDuration
	MaxValue    time.Duration
	MaxBinValue int
}

func quartiles(data []time.Duration) (first, second, third float64) {
	if len(data) == 0 {
		return 0, 0, 0
	}
	if len(data) == 1 {
		v := float64(data[0])
		return v, v, v
	}

	median := func(points []time.Duration) float64 {
		if len(points)%2 == 1 {
			return float64(points[len(points)/2])
		} else {
			return (float64(points[len(points)/2]) + float64(points[len(points)/2-1])) / 2
		}
	}

	var left, right []time.Duration
	if len(data)%2 == 1 {
		left = data[0 : len(data)/2]
		right = data[len(data)/2+1:]
	} else {
		left = data[0 : len(data)/2]
		right = data[len(data)/2:]
	}

	if len(left) == 0 {
		first = second
	} else {
		first = median(left)
	}
	second = median(data)
	if len(right) == 0 {
		third = second
	} else {
		third = median(right)
	}

	return
}

type HistogramConfig struct {
	Start, End     FloatDuration
	RejectOutliers bool
	Bins           int
}

func NewHistogram(cfg *HistogramConfig, values []time.Duration) *Histogram {
	const defaultBins = 100

	var (
		start, end     FloatDuration
		rejectOutliers bool
		bins           int
	)

	if cfg != nil {
		if cfg.Bins == 0 {
			cfg.Bins = defaultBins
		}
		start, end = cfg.Start, cfg.End
		rejectOutliers = cfg.RejectOutliers
		bins = cfg.Bins
	}

	if bins == 0 {
		bins = defaultBins
	}

	if end != 0 {
		rejectOutliers = false
	}

	if rejectOutliers {
		sort.Slice(values, func(i, j int) bool { return values[i] < values[j] })

		first, _, third := quartiles(values)
		iqr := third - first
		cutoff := third + 2.5*iqr

		firstCutoffIdx := sort.Search(len(values), func(i int) bool {
			return float64(values[i]) >= cutoff
		})

		if firstCutoffIdx == 0 {
			// If all values are outliers then none are. This should only happen when there is exactly 1 value.
			firstCutoffIdx = len(values)
		}

		var binWidth FloatDuration
		if firstCutoffIdx > 0 {
			lastFittingValue := FloatDuration(values[firstCutoffIdx-1])
			if end != 0 {
				lastFittingValue = end
			}
			binWidth = FloatDuration(lastFittingValue-start) / FloatDuration(bins)
			if binWidth == 0 {
				binWidth = 1
			}
		} else {
			bins = 0
		}

		var overflow FloatDuration
		if firstCutoffIdx < len(values) {
			bins++
			overflow = start + binWidth*FloatDuration(bins)
		}

		hist := &Histogram{
			Config:   cfg,
			Start:    start,
			Overflow: overflow,
			BinWidth: binWidth,
			Bins:     make([]int, bins),
		}

		// We've sorted the values to find the median. This means we don't have to compute the bin index for each value,
		// only when the value falls out of the previous bin. The extra branch is much cheaper than the division.
		var curBin int
		var curEnd FloatDuration = 1 * binWidth
		for _, v := range values[:firstCutoffIdx] {
			v := FloatDuration(v)
			if v >= curEnd {
				// Truncate, don't round, to find the bucket
				curBin = int((v - start) / binWidth)
				curEnd = FloatDuration(curBin+1) * binWidth
				if curBin == len(hist.Bins) {
					// If the maximum value is 10 and we have 10 bins, then the final bin has to be [9, 10] instead of [9, 10).
					curBin--
				}
			}

			hist.Bins[curBin]++
		}
		if hist.Overflow > 0 || binWidth == 0 {
			hist.Bins[bins-1] += len(values[firstCutoffIdx:])
		}

		var maxBinValue int
		for _, v := range hist.Bins {
			if v > maxBinValue {
				maxBinValue = v
			}
		}

		hist.MaxValue = values[len(values)-1]
		hist.MaxBinValue = maxBinValue

		return hist
	} else {
		var maxValue time.Duration
		for _, v := range values {
			if v > maxValue {
				maxValue = v
			}
		}

		var binWidth FloatDuration
		lastFittingValue := FloatDuration(maxValue)
		if end != 0 {
			lastFittingValue = end
		}
		binWidth = FloatDuration(lastFittingValue-start) / FloatDuration(bins)
		if binWidth == 0 {
			binWidth = 1
		}

		hist := &Histogram{
			Config:   cfg,
			Start:    start,
			BinWidth: binWidth,
			Bins:     make([]int, bins),
		}

		for _, v := range values {
			v := FloatDuration(v)
			// Truncate, don't round, to find the bucket
			curBin := int((v - start) / binWidth)
			if curBin == len(hist.Bins) {
				// If the maximum value is 10 and we have 10 bins, then the final bin has to be [9, 10] instead of [9, 10).
				curBin--
			}

			hist.Bins[curBin]++
		}

		var maxBinValue int
		for _, v := range hist.Bins {
			if v > maxBinValue {
				maxBinValue = v
			}
		}

		hist.MaxValue = maxValue
		hist.MaxBinValue = maxBinValue

		return hist
	}
}

func (hist *Histogram) HasOverflow() bool {
	return hist.Overflow > 0 || hist.BinWidth == 0
}

func (hist *Histogram) BucketRange(i int) (start, end FloatDuration) {
	start = hist.Start + (hist.BinWidth * FloatDuration(i))
	if !hist.HasOverflow() || i < len(hist.Bins)-1 {
		end = hist.Start + hist.BinWidth*FloatDuration(i+1)
	} else {
		end = FloatDuration(hist.MaxValue) + 1
	}

	return start, end
}

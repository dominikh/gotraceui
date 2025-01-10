package main

func argminmax[T ~uint64 | ~uint32 | ~uint16 | ~uint8](values []T) (minIdx, maxIdx int) {
	// OPT dispatch to SIMD where beneficial

	if len(values) == 0 {
		return -1, -1
	}
	min, max := values[0], values[0]
	for i, v := range values {
		if v < min {
			min = v
			minIdx = i
		}
		if v > max {
			max = v
			maxIdx = i
		}
	}

	return minIdx, maxIdx
}

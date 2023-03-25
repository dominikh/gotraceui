package main

import (
	"math"
)

func min(a, b float32) float32 {
	if a <= b {
		return a
	} else {
		return b
	}
}

func max(a, b float32) float32 {
	if a >= b {
		return a
	} else {
		return b
	}
}

func round32(f float32) float32 {
	return float32(math.Round(float64(f)))
}

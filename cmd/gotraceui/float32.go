package main

import (
	"math"
)

func round32(f float32) float32 {
	return float32(math.Round(float64(f)))
}

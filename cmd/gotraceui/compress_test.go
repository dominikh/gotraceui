package main

import (
	"math/rand"
	"testing"
	"time"

	"golang.org/x/exp/slices"
)

var benchNums []uint64

func init() {
	// Produce a mix of numbers with runs of short and long bit widths. This benchmarks isn't entirely representative of
	// performance on real data, because our real data compresses much better, particularly due to the use of delta
	// encoding.
	for i := 0; i < 10_000; i++ {
		benchNums = append(benchNums, rand.Uint64()&(1<<60-1))
	}
	for i := 0; i < 50_000; i++ {
		benchNums = append(benchNums, rand.Uint64()&(1<<10-1))
	}
	for i := 0; i < 50_000; i++ {
		benchNums = append(benchNums, rand.Uint64()&1)
	}
	rand.Shuffle(len(benchNums), func(i, j int) {
		benchNums[i], benchNums[j] = benchNums[j], benchNums[i]
	})
}

func BenchmarkEncode(b *testing.B) {
	// The output slice is larger than necessary, but we want to measure pure encoding performance, not allocations due
	// to slice growing.
	out := make([]uint64, len(benchNums))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		Encode(benchNums, out[:0])
	}
	b.ReportMetric(float64(len(benchNums)*b.N)/float64(b.Elapsed().Nanoseconds()), "ints/ns")
}

func BenchmarkDecode(b *testing.B) {
	encoded := Encode(benchNums, nil)
	out := make([]uint64, len(benchNums))
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		DecodeUnsafe(encoded, &out[0])
	}
	b.ReportMetric(float64(len(benchNums)*b.N)/float64(b.Elapsed().Nanoseconds()), "ints/ns")
}

func BenchmarkDeltaZigZagEncode(b *testing.B) {
	in := make([]uint64, 1e6)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		for i := range in {
			in[i] = rand.Uint64() >> 4
		}
		b.StartTimer()
		deltaZigZagEncode(in)
	}
	b.ReportMetric(float64(len(in)*b.N)/float64(b.Elapsed().Nanoseconds()), "ints/ns")
}

func BenchmarkDeltaZigZagDecode(b *testing.B) {
	in := make([]uint64, 1e6)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		b.StopTimer()
		for i := range in {
			in[i] = rand.Uint64() >> 4
		}
		deltaZigZagEncode(in)
		b.StartTimer()
		deltaZigZagDecode(in)
	}
	b.ReportMetric(float64(len(in)*b.N)/float64(b.Elapsed().Nanoseconds()), "ints/ns")
}

func TestZigZag(t *testing.T) {
	for i := 0; i < 1e6; i++ {
		n := rand.Uint64()
		if ret := unzigzag(zigzag(n)); ret != n {
			t.Fatalf("%d: %d incorrectly roundtripped to %d", i, n, ret)
		}
	}
}

func TestDeltaZigZag(t *testing.T) {
	var raw []uint64
	for i := 0; i < 1e6; i++ {
		raw = append(raw, rand.Uint64()>>4)
	}
	encoded := make([]uint64, len(raw))
	copy(encoded, raw)
	deltaZigZagEncode(encoded)
	deltaZigZagDecode(encoded)
	for i := range raw {
		if raw[i] != encoded[i] {
			t.Fatalf("%d: %d incorrectly roundtripped to %d", i, raw[i], encoded[i])
		}
	}
}

func TestEncode(t *testing.T) {
	{
		out := Encode(nil, nil)
		if len(out) != 0 {
			t.Errorf("got %d values, expected none", len(out))
		}
	}

	{
		nums := Encode([]uint64{0}, nil)
		if len(nums) != 1 {
			t.Errorf("got %d values, expected 1", len(nums))
		}
		if nums[0] != 15 {
			t.Errorf("got value %b, expected %b", nums[0], 15)
		}
	}

	{
		in := make([]uint64, 125)
		out := Encode(in, nil)
		want := []uint64{1, 11}
		if !slices.Equal(out, want) {
			t.Errorf("got %v, expected %v", out, want)
		}
	}

	{
		in := make([]uint64, 241)
		out := Encode(in, nil)
		want := []uint64{0, 15}
		if !slices.Equal(out, want) {
			t.Errorf("got %v, expected %v", out, want)
		}
	}

	{
		in := make([]uint64, 100e6)
		seed := time.Now().UnixNano()
		rng := rand.New(rand.NewSource(seed))
		for i := range in {
			in[i] = rng.Uint64() & (1<<60 - 1)
		}

		encoded := Encode(in, nil)
		var decoded []uint64
		j := 0
		decoded = Decode(encoded, decoded)
		for _, d := range decoded {
			if d != in[j] {
				t.Fatalf("decoded value %d is %d, expected %d. seed: %d", j, d, in[j], seed)
			}
			j++
		}
	}

	{
		in := make([]uint64, 100e6)
		seed := time.Now().UnixNano()
		rng := rand.New(rand.NewSource(seed))
		for i := range in {
			in[i] = rng.Uint64() & (1<<60 - 1)
		}

		encoded := Encode(in, nil)
		decoded := make([]uint64, len(in))
		j := 0
		DecodeUnsafe(encoded, &decoded[0])
		for _, d := range decoded {
			if d != in[j] {
				t.Fatalf("decoded value %d is %d, expected %d. seed: %d", j, d, in[j], seed)
			}
			j++
		}
	}
}

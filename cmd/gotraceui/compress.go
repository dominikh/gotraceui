package main

import (
	"math/bits"
	"unsafe"

	myunsafe "honnef.co/go/gotraceui/unsafe"

	"golang.org/x/exp/slices"
)

type compressedStackSpans struct {
	// number of uncompressed spans
	count int
	// simple-8b packed zigzag encoded uint64 deltas
	startsEnds []uint64
	eventIDs   []uint64
	pcs        []uint64
	nums       []uint64
	// bitpacked booleans
	isCPUSample []uint64
}

var sizes = [16]int{
	0:  240,
	1:  120,
	2:  60,
	3:  30,
	4:  20,
	5:  15,
	6:  12,
	7:  10,
	8:  8,
	9:  7,
	10: 6,
	11: 5,
	12: 4,
	13: 3,
	14: 2,
	15: 1,
}

// DecodeUnsafe is like Decode, but expects out to be big enough to hold all decoded values.
func DecodeUnsafe(v []uint64, out *uint64) {
	if len(v) == 0 {
		return
	}
	outPtr := unsafe.Pointer(out)
	var prevN uintptr
	for _, e := range v {
		// We have to increment the pointer at the beginning of the iteration, not the end. Otherwise, on the final
		// iteration, we'll compute a pointer that points at memory after the slice data. This gets flagged by checkptr.
		outPtr = unsafe.Pointer(uintptr(outPtr) + prevN*8)
		switch e & 0b1111 {
		case 0:
			const n = 240
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			clear(ptr[:])
		case 1:
			const n = 120
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			clear(ptr[:])
		case 2:
			const n = 60
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack60(e, ptr)
		case 3:
			const n = 30
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack30(e, ptr)
		case 4:
			const n = 20
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack20(e, ptr)
		case 5:
			const n = 15
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack15(e, ptr)
		case 6:
			const n = 12
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack12(e, ptr)
		case 7:
			const n = 10
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack10(e, ptr)
		case 8:
			const n = 8
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack8(e, ptr)
		case 9:
			const n = 7
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack7(e, ptr)
		case 10:
			const n = 6
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack6(e, ptr)
		case 11:
			const n = 5
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack5(e, ptr)
		case 12:
			const n = 4
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack4(e, ptr)
		case 13:
			const n = 3
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack3(e, ptr)
		case 14:
			const n = 2
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack2(e, ptr)
		case 15:
			const n = 1
			prevN = n
			ptr := (*[n]uint64)(outPtr)
			unpack1(e, ptr)
		}
	}
}

func Decode(v []uint64, out []uint64) []uint64 {
	if len(v) == 0 {
		return out
	}
	var n int
	for _, e := range v {
		n += sizes[e&0b1111]
	}
	out = slices.Grow(out[:0], n)[:n]
	DecodeUnsafe(v, &out[0])
	return out
}

func unpack60(v uint64, b *[60]uint64) {
	b[0] = v >> 4 & 0b1
	b[1] = v >> 5 & 0b1
	b[2] = v >> 6 & 0b1
	b[3] = v >> 7 & 0b1
	b[4] = v >> 8 & 0b1
	b[5] = v >> 9 & 0b1
	b[6] = v >> 10 & 0b1
	b[7] = v >> 11 & 0b1
	b[8] = v >> 12 & 0b1
	b[9] = v >> 13 & 0b1
	b[10] = v >> 14 & 0b1
	b[11] = v >> 15 & 0b1
	b[12] = v >> 16 & 0b1
	b[13] = v >> 17 & 0b1
	b[14] = v >> 18 & 0b1
	b[15] = v >> 19 & 0b1
	b[16] = v >> 20 & 0b1
	b[17] = v >> 21 & 0b1
	b[18] = v >> 22 & 0b1
	b[19] = v >> 23 & 0b1
	b[20] = v >> 24 & 0b1
	b[21] = v >> 25 & 0b1
	b[22] = v >> 26 & 0b1
	b[23] = v >> 27 & 0b1
	b[24] = v >> 28 & 0b1
	b[25] = v >> 29 & 0b1
	b[26] = v >> 30 & 0b1
	b[27] = v >> 31 & 0b1
	b[28] = v >> 32 & 0b1
	b[29] = v >> 33 & 0b1
	b[30] = v >> 34 & 0b1
	b[31] = v >> 35 & 0b1
	b[32] = v >> 36 & 0b1
	b[33] = v >> 37 & 0b1
	b[34] = v >> 38 & 0b1
	b[35] = v >> 39 & 0b1
	b[36] = v >> 40 & 0b1
	b[37] = v >> 41 & 0b1
	b[38] = v >> 42 & 0b1
	b[39] = v >> 43 & 0b1
	b[40] = v >> 44 & 0b1
	b[41] = v >> 45 & 0b1
	b[42] = v >> 46 & 0b1
	b[43] = v >> 47 & 0b1
	b[44] = v >> 48 & 0b1
	b[45] = v >> 49 & 0b1
	b[46] = v >> 50 & 0b1
	b[47] = v >> 51 & 0b1
	b[48] = v >> 52 & 0b1
	b[49] = v >> 53 & 0b1
	b[50] = v >> 54 & 0b1
	b[51] = v >> 55 & 0b1
	b[52] = v >> 56 & 0b1
	b[53] = v >> 57 & 0b1
	b[54] = v >> 58 & 0b1
	b[55] = v >> 59 & 0b1
	b[56] = v >> 60 & 0b1
	b[57] = v >> 61 & 0b1
	b[58] = v >> 62 & 0b1
	b[59] = v >> 63 & 0b1
}

func unpack30(e uint64, ptr *[30]uint64) {
	ptr[0] = e >> 4 & 0b11
	ptr[1] = e >> 6 & 0b11
	ptr[2] = e >> 8 & 0b11
	ptr[3] = e >> 10 & 0b11
	ptr[4] = e >> 12 & 0b11
	ptr[5] = e >> 14 & 0b11
	ptr[6] = e >> 16 & 0b11
	ptr[7] = e >> 18 & 0b11
	ptr[8] = e >> 20 & 0b11
	ptr[9] = e >> 22 & 0b11
	ptr[10] = e >> 24 & 0b11
	ptr[11] = e >> 26 & 0b11
	ptr[12] = e >> 28 & 0b11
	ptr[13] = e >> 30 & 0b11
	ptr[14] = e >> 32 & 0b11
	ptr[15] = e >> 34 & 0b11
	ptr[16] = e >> 36 & 0b11
	ptr[17] = e >> 38 & 0b11
	ptr[18] = e >> 40 & 0b11
	ptr[19] = e >> 42 & 0b11
	ptr[20] = e >> 44 & 0b11
	ptr[21] = e >> 46 & 0b11
	ptr[22] = e >> 48 & 0b11
	ptr[23] = e >> 50 & 0b11
	ptr[24] = e >> 52 & 0b11
	ptr[25] = e >> 54 & 0b11
	ptr[26] = e >> 56 & 0b11
	ptr[27] = e >> 58 & 0b11
	ptr[28] = e >> 60 & 0b11
	ptr[29] = e >> 62 & 0b11
}

func unpack20(e uint64, ptr *[20]uint64) {
	ptr[0] = e >> 4 & 0b111
	ptr[1] = e >> 7 & 0b111
	ptr[2] = e >> 10 & 0b111
	ptr[3] = e >> 13 & 0b111
	ptr[4] = e >> 16 & 0b111
	ptr[5] = e >> 19 & 0b111
	ptr[6] = e >> 22 & 0b111
	ptr[7] = e >> 25 & 0b111
	ptr[8] = e >> 28 & 0b111
	ptr[9] = e >> 31 & 0b111
	ptr[10] = e >> 34 & 0b111
	ptr[11] = e >> 37 & 0b111
	ptr[12] = e >> 40 & 0b111
	ptr[13] = e >> 43 & 0b111
	ptr[14] = e >> 46 & 0b111
	ptr[15] = e >> 49 & 0b111
	ptr[16] = e >> 52 & 0b111
	ptr[17] = e >> 55 & 0b111
	ptr[18] = e >> 58 & 0b111
	ptr[19] = e >> 61 & 0b111
}

func unpack15(v uint64, b *[15]uint64) {
	b[0] = v >> 4 & 0b1111
	b[1] = v >> 8 & 0b1111
	b[2] = v >> 12 & 0b1111
	b[3] = v >> 16 & 0b1111
	b[4] = v >> 20 & 0b1111
	b[5] = v >> 24 & 0b1111
	b[6] = v >> 28 & 0b1111
	b[7] = v >> 32 & 0b1111
	b[8] = v >> 36 & 0b1111
	b[9] = v >> 40 & 0b1111
	b[10] = v >> 44 & 0b1111
	b[11] = v >> 48 & 0b1111
	b[12] = v >> 52 & 0b1111
	b[13] = v >> 56 & 0b1111
	b[14] = v >> 60 & 0b1111
}

func unpack12(v uint64, b *[12]uint64) {
	b[0] = v >> 4 & 0b11111
	b[1] = v >> 9 & 0b11111
	b[2] = v >> 14 & 0b11111
	b[3] = v >> 19 & 0b11111
	b[4] = v >> 24 & 0b11111
	b[5] = v >> 29 & 0b11111
	b[6] = v >> 34 & 0b11111
	b[7] = v >> 39 & 0b11111
	b[8] = v >> 44 & 0b11111
	b[9] = v >> 49 & 0b11111
	b[10] = v >> 54 & 0b11111
	b[11] = v >> 59 & 0b11111
}

func unpack10(v uint64, b *[10]uint64) {
	b[0] = v >> 4 & 0b111111
	b[1] = v >> 10 & 0b111111
	b[2] = v >> 16 & 0b111111
	b[3] = v >> 22 & 0b111111
	b[4] = v >> 28 & 0b111111
	b[5] = v >> 34 & 0b111111
	b[6] = v >> 40 & 0b111111
	b[7] = v >> 46 & 0b111111
	b[8] = v >> 52 & 0b111111
	b[9] = v >> 58 & 0b111111
}

func unpack8(v uint64, b *[8]uint64) {
	b[0] = v >> 4 & 0b1111111
	b[1] = v >> 11 & 0b1111111
	b[2] = v >> 18 & 0b1111111
	b[3] = v >> 25 & 0b1111111
	b[4] = v >> 32 & 0b1111111
	b[5] = v >> 39 & 0b1111111
	b[6] = v >> 46 & 0b1111111
	b[7] = v >> 53 & 0b1111111
}

func unpack7(v uint64, b *[7]uint64) {
	b[0] = v >> 4 & 0b11111111
	b[1] = v >> 12 & 0b11111111
	b[2] = v >> 20 & 0b11111111
	b[3] = v >> 28 & 0b11111111
	b[4] = v >> 36 & 0b11111111
	b[5] = v >> 44 & 0b11111111
	b[6] = v >> 52 & 0b11111111
}

func unpack6(v uint64, b *[6]uint64) {
	b[0] = v >> 4 & 0b1111111111
	b[1] = v >> 14 & 0b1111111111
	b[2] = v >> 24 & 0b1111111111
	b[3] = v >> 34 & 0b1111111111
	b[4] = v >> 44 & 0b1111111111
	b[5] = v >> 54 & 0b1111111111
}

func unpack5(v uint64, b *[5]uint64) {
	b[0] = v >> 4 & 0b111111111111
	b[1] = v >> 16 & 0b111111111111
	b[2] = v >> 28 & 0b111111111111
	b[3] = v >> 40 & 0b111111111111
	b[4] = v >> 52 & 0b111111111111
}

func unpack4(v uint64, b *[4]uint64) {
	b[0] = v >> 4 & 0b111111111111111
	b[1] = v >> 19 & 0b111111111111111
	b[2] = v >> 34 & 0b111111111111111
	b[3] = v >> 49 & 0b111111111111111
}

func unpack3(v uint64, b *[3]uint64) {
	b[0] = v >> 4 & 0b11111111111111111111
	b[1] = v >> 24 & 0b11111111111111111111
	b[2] = v >> 44 & 0b11111111111111111111
}

func unpack2(v uint64, b *[2]uint64) {
	b[0] = v >> 4 & 0b111111111111111111111111111111
	b[1] = v >> 34 & 0b111111111111111111111111111111
}

func unpack1(v uint64, b *[1]uint64) {
	b[0] = v >> 4
}

func Encode(in []uint64, out []uint64) []uint64 {
	if len(in) == 0 {
		return nil
	}

	reallyUnsafeSlice := func(ptr unsafe.Pointer, n int) []uint64 {
		type slice struct {
			data     unsafe.Pointer
			len, cap int
		}
		return myunsafe.Cast[[]uint64](slice{ptr, n, n})
	}

	ptrIn := unsafe.Pointer(&in[0])
	var prevN uintptr
	for left := len(in); left > 0; left -= int(prevN) {
		// We have to increment the pointer at the beginning of the iteration, not the end. Otherwise, on the final
		// iteration, we'll compute a pointer that points at memory after the slice data. This gets flagged by checkptr.
		ptrIn = unsafe.Add(ptrIn, prevN*8)
		num := pack(reallyUnsafeSlice(ptrIn, left))
		switch num {
		case 0:
			panic("unreachable")
		case 1:
			const n = 1
			out = append(out, pack1((*[n]uint64)(ptrIn)))
			prevN = n
		case 2:
			const n = 2
			out = append(out, pack2((*[n]uint64)(ptrIn)))
			prevN = n
		case 3:
			const n = 3
			out = append(out, pack3((*[n]uint64)(ptrIn)))
			prevN = n
		case 4:
			const n = 4
			out = append(out, pack4((*[n]uint64)(ptrIn)))
			prevN = n
		case 5:
			const n = 5
			out = append(out, pack5((*[n]uint64)(ptrIn)))
			prevN = n
		case 6:
			const n = 6
			out = append(out, pack6((*[n]uint64)(ptrIn)))
			prevN = n
		case 7:
			const n = 7
			out = append(out, pack7((*[n]uint64)(ptrIn)))
			prevN = n
		case 8:
			const n = 8
			out = append(out, pack8((*[n]uint64)(ptrIn)))
			prevN = n
		case 10:
			const n = 10
			out = append(out, pack10((*[n]uint64)(ptrIn)))
			prevN = n
		case 12:
			const n = 12
			out = append(out, pack12((*[n]uint64)(ptrIn)))
			prevN = n
		case 15:
			const n = 15
			out = append(out, pack15((*[n]uint64)(ptrIn)))
			prevN = n
		case 20:
			const n = 20
			out = append(out, pack20((*[n]uint64)(ptrIn)))
			prevN = n
		case 30:
			const n = 30
			out = append(out, pack30((*[n]uint64)(ptrIn)))
			prevN = n
		case 60:
			const n = 60
			out = append(out, pack60((*[n]uint64)(ptrIn)))
			prevN = n
		case 120:
			const n = 120
			out = append(out, pack120((*[n]uint64)(ptrIn)))
			prevN = n
		case 240:
			const n = 240
			out = append(out, pack240((*[n]uint64)(ptrIn)))
			prevN = n
		}
	}

	return out
}

// bitsToCount maps from width to how many numbers we need to fill a word.
var bitsToCount = [256]uint8{
	240, 60, 30, 20, 15, 12, 10, 8, 7, 6, 6, 5, 5, 4, 4, 4, 3, 3, 3, 3, 3,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
}

// The first phase of the packing algorithm figures out how many bits we need to encode the largest
// integer in a group, and how many numbers we have for that group. It can happen that we have fewer
// numbers than is needed to fill a word at the given width. For example, the input {1, 1} needs one
// bit per number, but encoding a word at that width would require 60 numbers. Since we have only
// two numbers, we need to choose a larger width, namely 30.
//
// This table maps from the actual count to the count we have to use instead.
var countToCount = [256]uint8{
	0, 1, 2, 3, 4, 5, 6, 7,
	8, 8, 10, 10, 12, 12, 12, 15,
	15, 15, 15, 15, 20, 20, 20, 20,
	20, 20, 20, 20, 20, 20, 30, 30,
	30, 30, 30, 30, 30, 30, 30, 30,
	30, 30, 30, 30, 30, 30, 30, 30,
	30, 30, 30, 30, 30, 30, 30, 30,
	30, 30, 30, 30, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	60, 60, 60, 60, 60, 60, 60, 60,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	120, 120, 120, 120, 120, 120, 120, 120,
	240,
}

// pack takes a slice of integers and returns how many of them can be packed into a single word.
func pack(in []uint64) uint8 {
	if len(in) > 240 {
		in = in[:240]
	}

	countBits := func(n uint64) uint8 {
		return uint8(64 - bits.LeadingZeros64(n))
	}

	var numBits uint8
	var count uint8
	var num uint64
	for count = 0; count < uint8(len(in)); count++ {
		// Check which width we'd have to select to include the next number
		num = in[count]
		req := max(countBits(num), numBits)
		// Check if given the selected width we'd still have room for the number
		if count+1 > bitsToCount[req] {
			break
		}
		numBits = req
	}

	if count < 120 {
		// If we have too few numbers to fill the word at the minimum width, choose the next larger
		// width that we can fill.
		return countToCount[count]
	} else if count < 240 {
		return 120
	} else {
		return 240
	}
}

func pack240(in *[240]uint64) uint64 { return 0 }
func pack120(in *[120]uint64) uint64 { return 1 }
func pack60(in *[60]uint64) uint64 {
	var out uint64
	out |= 2
	out |= in[0] << 4
	out |= in[1] << 5
	out |= in[2] << 6
	out |= in[3] << 7
	out |= in[4] << 8
	out |= in[5] << 9
	out |= in[6] << 10
	out |= in[7] << 11
	out |= in[8] << 12
	out |= in[9] << 13
	out |= in[10] << 14
	out |= in[11] << 15
	out |= in[12] << 16
	out |= in[13] << 17
	out |= in[14] << 18
	out |= in[15] << 19
	out |= in[16] << 20
	out |= in[17] << 21
	out |= in[18] << 22
	out |= in[19] << 23
	out |= in[20] << 24
	out |= in[21] << 25
	out |= in[22] << 26
	out |= in[23] << 27
	out |= in[24] << 28
	out |= in[25] << 29
	out |= in[26] << 30
	out |= in[27] << 31
	out |= in[28] << 32
	out |= in[29] << 33
	out |= in[30] << 34
	out |= in[31] << 35
	out |= in[32] << 36
	out |= in[33] << 37
	out |= in[34] << 38
	out |= in[35] << 39
	out |= in[36] << 40
	out |= in[37] << 41
	out |= in[38] << 42
	out |= in[39] << 43
	out |= in[40] << 44
	out |= in[41] << 45
	out |= in[42] << 46
	out |= in[43] << 47
	out |= in[44] << 48
	out |= in[45] << 49
	out |= in[46] << 50
	out |= in[47] << 51
	out |= in[48] << 52
	out |= in[49] << 53
	out |= in[50] << 54
	out |= in[51] << 55
	out |= in[52] << 56
	out |= in[53] << 57
	out |= in[54] << 58
	out |= in[55] << 59
	out |= in[56] << 60
	out |= in[57] << 61
	out |= in[58] << 62
	out |= in[59] << 63
	return out
}

func pack30(in *[30]uint64) uint64 {
	var out uint64
	out |= 3
	out |= in[0] << 4
	out |= in[1] << 6
	out |= in[2] << 8
	out |= in[3] << 10
	out |= in[4] << 12
	out |= in[5] << 14
	out |= in[6] << 16
	out |= in[7] << 18
	out |= in[8] << 20
	out |= in[9] << 22
	out |= in[10] << 24
	out |= in[11] << 26
	out |= in[12] << 28
	out |= in[13] << 30
	out |= in[14] << 32
	out |= in[15] << 34
	out |= in[16] << 36
	out |= in[17] << 38
	out |= in[18] << 40
	out |= in[19] << 42
	out |= in[20] << 44
	out |= in[21] << 46
	out |= in[22] << 48
	out |= in[23] << 50
	out |= in[24] << 52
	out |= in[25] << 54
	out |= in[26] << 56
	out |= in[27] << 58
	out |= in[28] << 60
	out |= in[29] << 62
	return out
}

func pack20(in *[20]uint64) uint64 {
	var out uint64
	out |= 4
	out |= in[0] << 4
	out |= in[1] << 7
	out |= in[2] << 10
	out |= in[3] << 13
	out |= in[4] << 16
	out |= in[5] << 19
	out |= in[6] << 22
	out |= in[7] << 25
	out |= in[8] << 28
	out |= in[9] << 31
	out |= in[10] << 34
	out |= in[11] << 37
	out |= in[12] << 40
	out |= in[13] << 43
	out |= in[14] << 46
	out |= in[15] << 49
	out |= in[16] << 52
	out |= in[17] << 55
	out |= in[18] << 58
	out |= in[19] << 61
	return out
}

func pack15(in *[15]uint64) uint64 {
	var out uint64
	out |= 5
	out |= in[0] << 4
	out |= in[1] << 8
	out |= in[2] << 12
	out |= in[3] << 16
	out |= in[4] << 20
	out |= in[5] << 24
	out |= in[6] << 28
	out |= in[7] << 32
	out |= in[8] << 36
	out |= in[9] << 40
	out |= in[10] << 44
	out |= in[11] << 48
	out |= in[12] << 52
	out |= in[13] << 56
	out |= in[14] << 60
	return out
}

func pack12(in *[12]uint64) uint64 {
	var out uint64
	out |= 6
	out |= in[0] << 4
	out |= in[1] << 9
	out |= in[2] << 14
	out |= in[3] << 19
	out |= in[4] << 24
	out |= in[5] << 29
	out |= in[6] << 34
	out |= in[7] << 39
	out |= in[8] << 44
	out |= in[9] << 49
	out |= in[10] << 54
	out |= in[11] << 59
	return out
}

func pack10(in *[10]uint64) uint64 {
	var out uint64
	out |= 7
	out |= in[0] << 4
	out |= in[1] << 10
	out |= in[2] << 16
	out |= in[3] << 22
	out |= in[4] << 28
	out |= in[5] << 34
	out |= in[6] << 40
	out |= in[7] << 46
	out |= in[8] << 52
	out |= in[9] << 58
	return out
}

func pack8(in *[8]uint64) uint64 {
	var out uint64
	out |= 8
	out |= in[0] << 4
	out |= in[1] << 11
	out |= in[2] << 18
	out |= in[3] << 25
	out |= in[4] << 32
	out |= in[5] << 39
	out |= in[6] << 46
	out |= in[7] << 53
	return out
}

func pack7(in *[7]uint64) uint64 {
	var out uint64
	out |= 9
	out |= in[0] << 4
	out |= in[1] << 12
	out |= in[2] << 20
	out |= in[3] << 28
	out |= in[4] << 36
	out |= in[5] << 44
	out |= in[6] << 52
	return out
}

func pack6(in *[6]uint64) uint64 {
	var out uint64
	out |= 10
	out |= in[0] << 4
	out |= in[1] << 14
	out |= in[2] << 24
	out |= in[3] << 34
	out |= in[4] << 44
	out |= in[5] << 54
	return out
}

func pack5(in *[5]uint64) uint64 {
	var out uint64
	out |= 11
	out |= in[0] << 4
	out |= in[1] << 16
	out |= in[2] << 28
	out |= in[3] << 40
	out |= in[4] << 52
	return out
}

func pack4(in *[4]uint64) uint64 {
	var out uint64
	out |= 12
	out |= in[0] << 4
	out |= in[1] << 19
	out |= in[2] << 34
	out |= in[3] << 49
	return out
}

func pack3(in *[3]uint64) uint64 {
	var out uint64
	out |= 13
	out |= in[0] << 4
	out |= in[1] << 24
	out |= in[2] << 44
	return out
}

func pack2(in *[2]uint64) uint64 {
	var out uint64
	out |= 14
	out |= in[0] << 4
	out |= in[1] << 34
	return out
}

func pack1(in *[1]uint64) uint64 {
	var out uint64
	out |= 15
	out |= in[0] << 4
	return out
}

func zigzag(v uint64) uint64 {
	d := int64(v)
	return uint64(d>>63 ^ d<<1)
}

func unzigzag(v uint64) uint64 {
	return v>>1 ^ -(v & 1)
}

func deltaZigZagDecode(vs []uint64) {
	// OPT(dh): optimize prefix sum computation
	var n uint64
	for i, v := range vs {
		sv := int64(unzigzag(v))
		n = uint64(int64(n) + sv)
		vs[i] = n
	}
}

func deltaZigZagEncode(vs []uint64) {
	if len(vs) == 0 {
		return
	}
	if len(vs) == 1 {
		vs[0] = zigzag(vs[0])
		return
	}
	for i := len(vs) - 1; i > 0; i-- {
		d := int64(vs[i] - vs[i-1])
		vs[i] = zigzag(uint64(d))
	}
	vs[0] = zigzag(vs[0])
}

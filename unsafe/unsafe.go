package unsafe

import (
	"unsafe"

	"golang.org/x/exp/constraints"
)

func Cast[Dst, Src any](x Src) Dst {
	return *(*Dst)(unsafe.Pointer(&x))
}

func Map[Dst any](x []byte) *Dst {
	return (*Dst)(unsafe.Pointer(&x[:unsafe.Sizeof(*new(Dst))][0]))
}

func SliceCast[Dst ~[]DstE, Src ~[]SrcE, DstE, SrcE any](x Src) Dst {
	// We don't use our Cast helper in this function because it increases the function complexity, making inlining
	// more difficult.

	type sliceHeader struct {
		data unsafe.Pointer
		len  int
		cap  int
	}

	if cap(x) == 0 {
		return nil
	}

	// This way of getting the pointer has lower inlining complexity than &x[:1][0]
	ptrDst := (*sliceHeader)(unsafe.Pointer(&x)).data

	sizeSrc := unsafe.Sizeof(*new(SrcE))
	sizeDst := unsafe.Sizeof(*new(DstE))

	if sizeSrc >= sizeDst {
		return *(*Dst)(unsafe.Pointer(&sliceHeader{
			data: ptrDst,
			len:  len(x) * int(sizeSrc/sizeDst),
			cap:  cap(x) * int(sizeSrc/sizeDst),
		}))
	} else {
		return *(*Dst)(unsafe.Pointer(&sliceHeader{
			data: ptrDst,
			len:  len(x) / int(sizeDst/sizeSrc),
			cap:  cap(x) / int(sizeDst/sizeSrc),
		}))
	}
}

func Index[E any, S ~[]E, Int constraints.Integer](ptr S, idx Int) *E {
	offset := unsafe.Sizeof(*new(E)) * uintptr(idx)
	return (*E)(unsafe.Add(unsafe.Pointer(&ptr[0]), offset))
}

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

func MapStr[Dst any](x string) *Dst {
	_ = x[:unsafe.Sizeof(*new(Dst))]
	return (*Dst)(*(*unsafe.Pointer)(unsafe.Pointer(&x)))
}

// UnsafeMap is like Map, but doesn't check x's length.
func UnsafeMap[Dst any](x []byte) *Dst {
	ptrDst := *(*unsafe.Pointer)(unsafe.Pointer(&x))
	return (*Dst)(ptrDst)
}

func UnsafeMapStr[Dst any](x string) *Dst {
	return (*Dst)(unsafe.Pointer(unsafe.StringData(x)))
}

type sliceHeader struct {
	data unsafe.Pointer
	len  int
	cap  int
}

func ToSliceStr[Dst ~[]DstE, DstE any](x string) Dst {
	sizeDst := unsafe.Sizeof(*new(DstE))
	return *(*Dst)(unsafe.Pointer(&sliceHeader{
		data: unsafe.Pointer(unsafe.StringData(x)),
		len:  len(x) / int(sizeDst),
		cap:  len(x) / int(sizeDst),
	}))
}

func SliceCast[Dst ~[]DstE, Src ~[]SrcE, DstE, SrcE any](x Src) Dst {
	// We don't use our Cast helper in this function because it increases the function complexity, making inlining
	// more difficult.

	if cap(x) == 0 {
		return nil
	}

	ptrDst := *(*unsafe.Pointer)(unsafe.Pointer(&x))

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

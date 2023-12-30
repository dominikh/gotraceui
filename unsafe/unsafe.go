package unsafe

import (
	"unsafe"

	"golang.org/x/exp/constraints"
)

func Cast[Dst, Src any](x Src) Dst {
	return *(*Dst)(unsafe.Pointer(&x))
}

func Index[E any, S ~[]E, Int constraints.Integer](ptr S, idx Int) *E {
	offset := unsafe.Sizeof(*new(E)) * uintptr(idx)
	return (*E)(unsafe.Add(unsafe.Pointer(&ptr[0]), offset))
}

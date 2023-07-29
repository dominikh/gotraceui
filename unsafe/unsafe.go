package unsafe

import "unsafe"

func Cast[Dst, Src any](x Src) Dst {
	return *(*Dst)(unsafe.Pointer(&x))
}

package assets

import (
	"embed"
	"fmt"
	"image/png"
	"io"
	"math"
	"sync"

	"gioui.org/layout"
	"gioui.org/op/paint"
)

//go:embed data
var data embed.FS

var images sync.Map

type imageKey struct {
	name string
	size int
}

func Image(gtx layout.Context, name string, size int) paint.ImageOp {
	pxPerDp := int(math.Round(float64(gtx.Metric.PxPerDp)))
	key := imageKey{name: name, size: size * pxPerDp}
	if op, ok := images.Load(key); ok {
		return op.(paint.ImageOp)
	}

	var rc io.ReadCloser
	var err error
	if size == 0 {
		rc, err = data.Open(fmt.Sprintf("data/%s-%dx.png", name, pxPerDp))
	} else {
		rc, err = data.Open(fmt.Sprintf("data/%s-%d.png", name, size*pxPerDp))
	}

	if err != nil {
		panic(fmt.Sprintf("couldn't load asset: %s", err))
	}

	defer rc.Close()

	img, err := png.Decode(rc)
	if err != nil {
		panic(fmt.Sprintf("couldn't load asset: %s", err))
	}

	op := paint.NewImageOp(img)
	// It's fine for this to be racy, worst case we do unnecessary work loading the same asset multiple times.
	images.Store(key, op)

	return op
}

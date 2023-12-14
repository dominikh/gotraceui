package assets

import (
	"embed"
	"errors"
	"fmt"
	"image"
	"image/gif"
	"image/png"
	"io"
	"io/fs"
	"log"
	"math"
	"sync"

	"honnef.co/go/gotraceui/widget"

	"gioui.org/layout"
	"gioui.org/op/paint"
	"golang.org/x/image/draw"
)

//go:embed data
var data embed.FS

var images sync.Map
var animations sync.Map

type imageKey struct {
	name string
	size int
}

func loadGIF(gtx layout.Context, name string, size int) (*gif.GIF, error) {
	pxPerDp := int(math.Round(float64(gtx.Metric.PxPerDp)))

	var rc io.ReadCloser
	var err error
	if size == 0 {
		rc, err = data.Open(fmt.Sprintf("data/%s-%dx.gif", name, pxPerDp))
	} else {
		rc, err = data.Open(fmt.Sprintf("data/%s-%d.gif", name, size*pxPerDp))
	}

	if err != nil {
		return nil, err
	}

	defer rc.Close()

	img, err := gif.DecodeAll(rc)
	if err != nil {
		panic(fmt.Sprintf("couldn't load asset: %s", err))
	}

	return img, nil
}

func loadImage(gtx layout.Context, name string, size int) (image.Image, error) {
	pxPerDp := int(math.Round(float64(gtx.Metric.PxPerDp)))

	var rc io.ReadCloser
	var err error
	if size == 0 {
		rc, err = data.Open(fmt.Sprintf("data/%s-%dx.png", name, pxPerDp))
	} else {
		rc, err = data.Open(fmt.Sprintf("data/%s-%d.png", name, size*pxPerDp))
	}

	if err != nil {
		return nil, err
	}

	defer rc.Close()

	img, err := png.Decode(rc)
	if err != nil {
		panic(fmt.Sprintf("couldn't load asset: %s", err))
	}

	return img, nil
}

func Image(gtx layout.Context, name string, size int) paint.ImageOp {
	pxPerDp := int(math.Round(float64(gtx.Metric.PxPerDp)))
	key := imageKey{name: name, size: size * pxPerDp}
	if op, ok := images.Load(key); ok {
		return op.(paint.ImageOp)
	}

	img, err := loadImage(gtx, name, size)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) && pxPerDp != 1 {
			// The image doesn't exist at the scale we need. Load the unscaled size and scale it at runtime.
			gtx := gtx
			gtx.Metric.PxPerDp = 1
			img, err = loadImage(gtx, name, size)
			if err != nil {
				panic(fmt.Sprintf("couldn't load asset: %s", err))
			}

			log.Printf("asset %q missing at size %d and scale %d, scaling base version", name, size, pxPerDp)
			newBounds := img.Bounds()
			newBounds.Max.X *= pxPerDp
			newBounds.Max.Y *= pxPerDp
			dst := image.NewRGBA(newBounds)
			draw.NearestNeighbor.Scale(dst, dst.Bounds(), img, img.Bounds(), draw.Src, nil)

			img = dst
		} else {
			panic(fmt.Sprintf("couldn't load asset: %s", err))
		}
	}

	op := paint.NewImageOp(img)
	// It's fine for this to be racy, worst case we do unnecessary work loading the same asset multiple times.
	images.Store(key, op)

	return op
}

func Animation(gtx layout.Context, name string, size int) *widget.GIF {
	pxPerDp := int(math.Round(float64(gtx.Metric.PxPerDp)))
	key := imageKey{name: name, size: size * pxPerDp}
	if op, ok := animations.Load(key); ok {
		return op.(*widget.GIF)
	}

	g, err := loadGIF(gtx, name, size)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) && pxPerDp != 1 {
			// The image doesn't exist at the scale we need. Load the unscaled size and scale it at runtime.
			gtx := gtx
			gtx.Metric.PxPerDp = 1
			g, err = loadGIF(gtx, name, size)
			if err != nil {
				panic(fmt.Sprintf("couldn't load asset: %s", err))
			}

			log.Printf("asset %q missing at size %d and scale %d, scaling base version", name, size, pxPerDp)
			for i, img := range g.Image {
				newBounds := img.Bounds()
				newBounds.Max.X *= pxPerDp
				newBounds.Max.Y *= pxPerDp
				dst := image.NewPaletted(newBounds, img.Palette)
				draw.NearestNeighbor.Scale(dst, dst.Bounds(), img, img.Bounds(), draw.Src, nil)

				g.Image[i] = dst
			}
			g.Config.Width *= pxPerDp
			g.Config.Height *= pxPerDp
		} else {
			panic(fmt.Sprintf("couldn't load asset: %s", err))
		}
	}

	op := widget.NewGIF(g)
	// It's fine for this to be racy, worst case we do unnecessary work loading the same asset multiple times.
	animations.Store(key, op)

	return op
}

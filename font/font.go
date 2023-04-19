package font

import (
	_ "embed"
	"fmt"
	"sync"

	"gioui.org/font"
	"gioui.org/font/gofont"
	"gioui.org/font/opentype"
)

//go:embed fallback.ttf
var fallback []byte

var (
	once       sync.Once
	collection []font.FontFace
)

func Collection() []font.FontFace {
	once.Do(func() {
		c := gofont.Collection()

		face, err := opentype.Parse(fallback)
		if err != nil {
			panic(fmt.Errorf("failed to parse fallback font: %s", err))
		}

		fc := font.FontFace{
			Font: font.Font{
				Typeface: "Fallback",
			},
			Face: face,
		}

		c = append(c, fc)
		n := len(c)
		collection = c[:n:n]
	})
	return collection
}

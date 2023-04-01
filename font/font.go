package font

import (
	_ "embed"
	"fmt"
	"sync"

	"gioui.org/font/gofont"
	"gioui.org/font/opentype"
	"gioui.org/text"
)

//go:embed fallback.ttf
var fallback []byte

var (
	once       sync.Once
	collection []text.FontFace
)

func Collection() []text.FontFace {
	once.Do(func() {
		c := gofont.Collection()

		face, err := opentype.Parse(fallback)
		if err != nil {
			panic(fmt.Errorf("failed to parse fallback font: %s", err))
		}

		fc := text.FontFace{
			Font: text.Font{
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

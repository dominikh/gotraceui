package color

import (
	"fmt"
	"image/color"
	"math"
)

type Lab struct {
	L     float32
	A     float32
	B     float32
	Alpha float32
}

type LCh struct {
	L float32
	C float32
	H float32
	A float32
}

type RGB struct {
	R float32
	G float32
	B float32
	A float32
}

type Oklab Lab
type Oklch LCh
type SRGB RGB
type LinearSRGB RGB

func (c Lab) LCh() LCh {
	hue := float32(math.Atan2(float64(c.B), float64(c.A))) * (180 / math.Pi)
	if hue < 0 {
		hue += 360
	}
	return LCh{
		c.L,
		float32(math.Hypot(float64(c.A), float64(c.B))),
		hue,
		c.Alpha,
	}
}

func (c Oklab) Oklch() Oklch {
	return Oklch(Lab(c).LCh())
}

// LinearSRGB converts from Oklab to linear sRGB, without applying gamut mapping. That is, if the color falls outside
// the sRGB gamut, the resulting R, G, and B channels may have values larger than 1 or less than 0. You can use
// Oklch.MapToSRGBGamut to prevent this from happening.
func (c Oklab) LinearSRGB() LinearSRGB {
	l_ := c.L + 0.3963377774*c.A + 0.2158037573*c.B
	m_ := c.L - 0.1055613458*c.A - 0.0638541728*c.B
	s_ := c.L - 0.0894841775*c.A - 1.2914855480*c.B

	l := l_ * l_ * l_
	m := m_ * m_ * m_
	s := s_ * s_ * s_

	return LinearSRGB{
		+4.0767416621*l - 3.3077115913*m + 0.2309699292*s,
		-1.2684380046*l + 2.6097574011*m - 0.3413193965*s,
		-0.0041960863*l - 0.7034186147*m + 1.7076147010*s,
		c.Alpha,
	}
}

func (c LCh) Lab() Lab {
	h := float64(c.H * (math.Pi / 180))
	return Lab{
		L:     c.L,
		A:     c.C * float32(math.Cos(h)),
		B:     c.C * float32(math.Sin(h)),
		Alpha: c.A,
	}
}

func (c Oklch) Oklab() Oklab {
	h := float64(c.H * (math.Pi / 180))
	return Oklab{
		L:     c.L,
		A:     c.C * float32(math.Cos(h)),
		B:     c.C * float32(math.Sin(h)),
		Alpha: c.A,
	}
}

func Difference(reference, sample Oklab) (deltaEOK float32) {
	L1, a1, b1 := reference.L, reference.A, reference.B
	L2, a2, b2 := sample.L, sample.A, sample.B
	deltaL := float64(L1 - L2)
	deltaa := float64(a1 - a2)
	deltab := float64(b1 - b2)
	return float32(math.Hypot(math.Hypot(deltaL, deltaa), deltab))
}

// MapToSRGBGamut maps colors that fall outside the sRGB gamut to the sRGB gamut. It uses the same algorithm as [CSS
// Color Module Level 4]. Note that the mapping implements a relative colorimetric intent. That is, colors that are
// already inside the gamut are unchanged. This is intended for mapping individual colors, not for mapping images.
//
// [CSS Color Module Level 4]: https://www.w3.org/TR/css-color-4/#css-gamut-mapping
func (c Oklch) MapToSRGBGamut() LinearSRGB {
	// The just noticeable difference between two colors in Oklch
	const jnd = 0.02
	const epsilon = 0.0001

	if c.L >= 1 {
		return LinearSRGB{1, 1, 1, c.A}
	}
	if c.L <= 0 {
		return LinearSRGB{0, 0, 0, c.A}
	}

	inGamut := func(color Oklch) (LinearSRGB, bool) {
		// OPT(dh): is there an easier way to check if the color is in gamut than to try and convert it?
		s := color.Oklab().LinearSRGB()
		if s.R >= 0 && s.R <= 1 &&
			s.G >= 0 && s.G <= 1 &&
			s.B >= 0 && s.B <= 1 {
			return s, true
		} else {
			return LinearSRGB{}, false
		}
	}
	inGamut1 := func(color Oklch) bool {
		_, ok := inGamut(color)
		return ok
	}

	if m, ok := inGamut(c); ok {
		return m
	}

	clip := func(color Oklch) LinearSRGB {
		m := color.Oklab().LinearSRGB()
		fmin := func(a, b float32) float32 {
			if a <= b {
				return a
			} else {
				return b
			}
		}
		fmax := func(a, b float32) float32 {
			if a >= b {
				return a
			} else {
				return b
			}
		}

		m.R = fmin(fmax(m.R, 0), 1)
		m.G = fmin(fmax(m.G, 0), 1)
		m.B = fmin(fmax(m.B, 0), 1)
		return m
	}

	min := float32(0)
	max := c.C
	min_inGamut := true
	current := c
	clipped := clip(c)

	E := Difference(clipped.Oklab(), current.Oklab())
	if E < jnd {
		return clipped
	}

	for max-min > epsilon {
		chroma := (min + max) / 2
		current.C = chroma

		if min_inGamut && inGamut1(current) {
			min = chroma
		} else {
			clipped = clip(current)
			E = Difference(clipped.Oklab(), current.Oklab())
			if E < jnd {
				if jnd-E < epsilon {
					return clipped
				} else {
					min_inGamut = false
					min = chroma
				}
			} else {
				max = chroma
			}
		}
	}
	return current.Oklab().LinearSRGB()
}

func (c LinearSRGB) Oklab() Oklab {
	r := float64(c.R)
	g := float64(c.G)
	b := float64(c.B)

	l := 0.4122214708*r + 0.5363325363*g + 0.0514459929*b
	m := 0.2119034982*r + 0.6806995451*g + 0.1073969566*b
	s := 0.0883024619*r + 0.2817188376*g + 0.6299787005*b

	l_ := math.Cbrt(l)
	m_ := math.Cbrt(m)
	s_ := math.Cbrt(s)

	return Oklab{
		L:     float32(0.2104542553*l_ + 0.7936177850*m_ - 0.0040720468*s_),
		A:     float32(1.9779984951*l_ - 2.4285922050*m_ + 0.4505937099*s_),
		B:     float32(0.0259040371*l_ + 0.7827717662*m_ - 0.8086757660*s_),
		Alpha: float32(c.A),
	}
}

func (c SRGB) LinearSRGB() LinearSRGB {
	t := func(c float32) float32 {
		cp := float64(c)
		if cp >= 0.04045 {
			return float32(math.Pow((cp+0.055)/(1+0.055), 2.4))
		} else {
			return float32(cp / 12.92)
		}
	}

	return LinearSRGB{t(c.R), t(c.G), t(c.B), c.A}
}

func (c SRGB) RGBA() (r, g, b, a uint32) {
	r = uint32(math.Round(float64(c.R * c.A * 0xFFFF)))
	g = uint32(math.Round(float64(c.G * c.A * 0xFFFF)))
	b = uint32(math.Round(float64(c.B * c.A * 0xFFFF)))
	a = uint32(math.Round(float64(c.A * 0xFFFF)))
	return
}

func (c SRGB) HTML() string {
	round := func(f float32) uint8 {
		return uint8(math.Round(float64(f)))
	}
	return fmt.Sprintf("#%02x%02x%02x%02x", round(c.R*255), round(c.G*255), round(c.B*255), round(c.A*255))
}

func (c LinearSRGB) SRGB() SRGB {
	t := func(c float32) float32 {
		cp := float64(c)
		if cp >= 0.0031308 {
			return float32(1.055*math.Pow(cp, 1.0/2.4) - 0.055)
		} else {
			return float32(12.92 * cp)
		}
	}

	return SRGB{t(c.R), t(c.G), t(c.B), c.A}
}

func (c Oklch) NRGBA() color.NRGBA {
	r, g, b, a := c.MapToSRGBGamut().SRGB().RGBA()
	if a == 0xffff {
		return color.NRGBA{uint8(r >> 8), uint8(g >> 8), uint8(b >> 8), 0xff}
	}
	if a == 0 {
		return color.NRGBA{0, 0, 0, 0}
	}
	// Since Color.RGBA returns an alpha-premultiplied color, we should have r <= a && g <= a && b <= a.
	r = (r * 0xffff) / a
	g = (g * 0xffff) / a
	b = (b * 0xffff) / a
	return color.NRGBA{uint8(r >> 8), uint8(g >> 8), uint8(b >> 8), uint8(a >> 8)}
}

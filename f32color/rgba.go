// SPDX-License-Identifier: Unlicense OR MIT

package f32color

import "honnef.co/go/gotraceui/color"

// MulAlpha applies the alpha to the color.
func MulAlpha(c color.Oklch, alpha float32) color.Oklch {
	c.A *= alpha
	return c
}

// Disabled desaturates the color and multiplies alpha.
// Multiplying alpha blends the color together more with the background.
func Disabled(c color.Oklch) (d color.Oklch) {
	const r = 80 // blend ratio
	d = mix(c, color.Oklch{A: c.A, L: c.L, C: 0, H: c.H}, r)
	d = MulAlpha(d, 128+32)
	return
}

// mix mixes c1 and c2 weighted by (1 - a) and a respectively.
func mix(c1, c2 color.Oklch, a float32) color.Oklch {
	return color.Oklch{
		L: c1.L*a + c2.L*(1-a),
		C: c1.C*a + c2.C*(1-a),
		H: c1.H*a + c2.H*(1-a),
		A: c1.A*a + c2.A*(1-a),
	}
}

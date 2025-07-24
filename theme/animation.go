package theme

import (
	"fmt"
	"math"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/stuff/math/mathutil"

	"gioui.org/op"
	"golang.org/x/exp/constraints"
)

type EasingFunction func(float64) float64
type LerpFunction[T any] func(start, end T, r float64) T

type Lerper[T any] interface {
	Lerp(end T, ratio float64) T
}

type Animation[T any] struct {
	StartValue T
	EndValue   T
	StartTime  time.Time
	Duration   time.Duration
	Ease       EasingFunction
	Lerp       LerpFunction[T]

	active bool
}

func (anim *Animation[T]) Start(gtx layout.Context, v1, v2 T, d time.Duration, ease EasingFunction) {
	anim.StartValue = v1
	anim.EndValue = v2
	anim.StartTime = gtx.Now
	anim.Duration = d
	anim.Ease = ease
	anim.active = true
	defer op.InvalidateOp{}.Add(gtx.Ops)
}

func StartSimpleAnimation[T constraints.Integer | constraints.Float](gtx layout.Context, anim *Animation[T], v1, v2 T, d time.Duration, ease EasingFunction) {
	anim.Start(gtx, v1, v2, d, ease)
	anim.Lerp = mathutil.Lerp
}

func (anim *Animation[T]) Value(gtx layout.Context) T {
	if !anim.active {
		return anim.EndValue
	}

	d := gtx.Now.Sub(anim.StartTime)
	if d > anim.Duration {
		anim.active = false
		return anim.EndValue
	}

	ratio := anim.Ease(float64(d) / float64(anim.Duration))
	op.InvalidateOp{}.Add(gtx.Ops)

	if anim.Lerp == nil {
		if lerper, ok := any(anim.StartValue).(Lerper[T]); ok {
			return lerper.Lerp(anim.EndValue, ratio)
		} else {
			panic(fmt.Sprintf("anim.Lerp is nil and %T doesn't implement Lerper", anim.StartValue))
		}
	}

	return anim.Lerp(anim.StartValue, anim.EndValue, ratio)
}

func (anim *Animation[T]) Cancel() {
	anim.active = false
}

func (anim *Animation[T]) Done() bool {
	return !anim.active
}

func EaseIn(power int) EasingFunction {
	switch power {
	case 1:
		return func(r float64) float64 { return r }
	case 2:
		return func(r float64) float64 { return r * r }
	case 3:
		return func(r float64) float64 { return r * r * r }
	case 4:
		return func(r float64) float64 { return r * r * r * r }
	default:
		return func(r float64) float64 { return math.Pow(r, float64(power)) }
	}
}

func EaseOut(power int) EasingFunction {
	switch power {
	case 1:
		return func(r float64) float64 { return r }
	case 2:
		return func(r float64) float64 { r = 1 - r; return 1 - r*r }
	case 3:
		return func(r float64) float64 { r = 1 - r; return 1 - r*r*r }
	case 4:
		return func(r float64) float64 { r = 1 - r; return 1 - r*r*r*r }
	default:
		return func(r float64) float64 { return 1 - math.Pow(1-r, float64(power)) }
	}
}

func EaseBezier(t float64) float64 {
	return t * t * (3.0 - 2.0*t)
}

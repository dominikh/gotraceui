package layout

import (
	_ "unsafe"

	"gioui.org/layout"
)

type Context = layout.Context
type Dimensions = layout.Dimensions
type Constraints = layout.Constraints
type Flex = layout.Flex
type Alignment = layout.Alignment
type Axis = layout.Axis
type Direction = layout.Direction
type FlexChild = layout.FlexChild
type Spacer = layout.Spacer
type Stack = layout.Stack
type Widget = layout.Widget
type Inset = layout.Inset
type ListElement = layout.ListElement
type Spacing = layout.Spacing

//go:linkname axisCrossConstraint gioui.org/layout.Axis.crossConstraint
func axisCrossConstraint(Axis, Constraints) (int, int)

//go:linkname axisMainConstraint gioui.org/layout.Axis.mainConstraint
func axisMainConstraint(Axis, Constraints) (int, int)

//go:linkname axisConstraints gioui.org/layout.Axis.constraints
func axisConstraints(Axis, int, int, int, int) Constraints

const (
	SpaceEnd     = layout.SpaceEnd
	SpaceStart   = layout.SpaceStart
	SpaceSides   = layout.SpaceSides
	SpaceAround  = layout.SpaceAround
	SpaceBetween = layout.SpaceBetween
	SpaceEvenly  = layout.SpaceEvenly
)

var UniformInset = layout.UniformInset
var Rigid = layout.Rigid
var Flexed = layout.Flexed
var Exact = layout.Exact
var Expanded = layout.Expanded
var Stacked = layout.Stacked
var NewContext = layout.NewContext

const (
	Start    = layout.Start
	End      = layout.End
	Middle   = layout.Middle
	Baseline = layout.Baseline
)

const (
	NW     = layout.NW
	N      = layout.N
	NE     = layout.NE
	E      = layout.E
	SE     = layout.SE
	S      = layout.S
	SW     = layout.SW
	W      = layout.W
	Center = layout.Center
)

const (
	Horizontal = layout.Horizontal
	Vertical   = layout.Vertical
)

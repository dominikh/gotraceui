package layout

import "gioui.org/layout"

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
type Position = layout.Position
type List = layout.List
type ListElement = layout.ListElement
type Spacing = layout.Spacing

const (
	SpaceEnd     Spacing = layout.SpaceEnd
	SpaceStart           = layout.SpaceStart
	SpaceSides           = layout.SpaceSides
	SpaceAround          = layout.SpaceAround
	SpaceBetween         = layout.SpaceBetween
	SpaceEvenly          = layout.SpaceEvenly
)

var UniformInset = layout.UniformInset
var Rigid = layout.Rigid
var Flexed = layout.Flexed
var Exact = layout.Exact
var Expanded = layout.Expanded
var Stacked = layout.Stacked
var NewContext = layout.NewContext

const (
	Start    Alignment = layout.Start
	End      Alignment = layout.End
	Middle   Alignment = layout.Middle
	Baseline Alignment = layout.Baseline
)

const (
	NW     Direction = layout.NW
	N      Direction = layout.N
	NE     Direction = layout.NE
	E      Direction = layout.E
	SE     Direction = layout.SE
	S      Direction = layout.S
	SW     Direction = layout.SW
	W      Direction = layout.W
	Center Direction = layout.Center
)

const (
	Horizontal Axis = layout.Horizontal
	Vertical   Axis = layout.Vertical
)

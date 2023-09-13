package theme

import (
	"context"
	"image"
	"math"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/unit"
)

// rangeIsScrollable returns whether the viewport described by start and end
// is smaller than the underlying content (such that it can be scrolled).
// start and end are expected to each be in the range [0,1], and start
// must be less than or equal to end.
func rangeIsScrollable(start, end float32) bool {
	return end-start < 1
}

// ScrollTrackStyle configures the presentation of a track for a scroll area.
type ScrollTrackStyle struct {
	// MajorPadding and MinorPadding along the major and minor axis of the
	// scrollbar's track. This is used to keep the scrollbar from touching
	// the edges of the content area.
	MinorPadding unit.Dp
	// Color of the track background.
	Color color.Oklch
}

// ScrollIndicatorStyle configures the presentation of a scroll indicator.
type ScrollIndicatorStyle struct {
	// MajorMinLen is the smallest that the scroll indicator is allowed to
	// be along the major axis.
	MajorMinLen unit.Dp
	// MinorWidth is the width of the scroll indicator across the minor axis.
	MinorWidth unit.Dp
	// Color and HoverColor are the normal and hovered colors of the scroll
	// indicator.
	Color, HoverColor color.Oklch
}

// ScrollbarStyle configures the presentation of a scrollbar.
type ScrollbarStyle struct {
	Scrollbar *widget.Scrollbar
	Track     ScrollTrackStyle
	Indicator ScrollIndicatorStyle
}

// Scrollbar configures the presentation of a scrollbar using the provided
// theme and state.
func Scrollbar(th *Theme, state *widget.Scrollbar) ScrollbarStyle {
	return ScrollbarStyle{
		Scrollbar: state,
		Track: ScrollTrackStyle{
			MinorPadding: 2,
			// TODO(dh): compute color from background color
			Color: oklch(66.61, 0.1, 108.83),
		},
		Indicator: ScrollIndicatorStyle{
			MajorMinLen: 8,
			MinorWidth:  10,
			Color:       th.Palette.Background,
			HoverColor:  th.Palette.Background,
		},
	}
}

// Width returns the minor axis width of the scrollbar in its current
// configuration (taking padding for the scroll track into account).
func (s ScrollbarStyle) Width() unit.Dp {
	return s.Indicator.MinorWidth + s.Track.MinorPadding + s.Track.MinorPadding
}

// Layout the scrollbar.
func (s ScrollbarStyle) Layout(win *Window, gtx layout.Context, axis layout.Axis, viewportStart, viewportEnd float32) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ScrollbarStyle.Layout").End()
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	// Set minimum constraints in an axis-independent way, then convert to
	// the correct representation for the current axis.
	convert := axis.Convert
	maxMajorAxis := convert(gtx.Constraints.Max).X
	gtx.Constraints.Min.X = maxMajorAxis
	gtx.Constraints.Min.Y = gtx.Dp(s.Width())
	gtx.Constraints.Min = convert(gtx.Constraints.Min)
	gtx.Constraints.Max = gtx.Constraints.Min

	if !rangeIsScrollable(viewportStart, viewportEnd) {
		return layout.Dimensions{Size: gtx.Constraints.Min}
	}

	s.Scrollbar.Layout(gtx, axis, viewportStart, viewportEnd)

	// Darken indicator if hovered.
	if s.Scrollbar.IndicatorHovered() {
		s.Indicator.Color = s.Indicator.HoverColor
	}

	return s.layout(win, gtx, axis, viewportStart, viewportEnd)
}

// layout the scroll track and indicator.
func (s ScrollbarStyle) layout(win *Window, gtx layout.Context, axis layout.Axis, viewportStart, viewportEnd float32) layout.Dimensions {
	// XXX there's an off-by-some somewhere, and padding on the right side is larger than on the left

	inset := layout.Inset{
		Top:    0,
		Bottom: 0,
		Left:   s.Track.MinorPadding,
		Right:  s.Track.MinorPadding,
	}
	if axis == layout.Horizontal {
		inset.Top, inset.Bottom, inset.Left, inset.Right = inset.Left, inset.Right, inset.Top, inset.Bottom
	}
	// Capture the outer constraints because layout.Stack will reset
	// the minimum to zero.
	outerConstraints := gtx.Constraints

	return layout.Stack{}.Layout(gtx,
		layout.Expanded(func(gtx layout.Context) layout.Dimensions {
			// Lay out the draggable track underneath the scroll indicator.
			area := image.Rectangle{
				Max: gtx.Constraints.Min,
			}
			pointerArea := clip.Rect(area)
			defer pointerArea.Push(gtx.Ops).Pop()
			s.Scrollbar.AddDrag(gtx.Ops)

			// Stack a normal clickable area on top of the draggable area
			// to capture non-dragging clicks.
			defer pointer.PassOp{}.Push(gtx.Ops).Pop()
			defer pointerArea.Push(gtx.Ops).Pop()
			s.Scrollbar.AddTrack(gtx.Ops)

			FillShape(win, gtx.Ops, s.Track.Color, clip.Rect(area).Op())
			return layout.Dimensions{}
		}),
		layout.Stacked(func(gtx layout.Context) layout.Dimensions {
			gtx.Constraints = outerConstraints
			return inset.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				// Use axis-independent constraints.
				gtx.Constraints.Min = axis.Convert(gtx.Constraints.Min)
				gtx.Constraints.Max = axis.Convert(gtx.Constraints.Max)

				// Compute the pixel size and position of the scroll indicator within
				// the track.
				trackLen := gtx.Constraints.Min.X
				viewStart := int(math.Round(float64(viewportStart) * float64(trackLen)))
				viewEnd := int(math.Round(float64(viewportEnd) * float64(trackLen)))
				indicatorLen := max(viewEnd-viewStart, gtx.Dp(s.Indicator.MajorMinLen))
				if viewStart+indicatorLen > trackLen {
					viewStart = trackLen - indicatorLen
				}
				indicatorDims := axis.Convert(image.Point{
					X: indicatorLen,
					Y: gtx.Dp(s.Indicator.MinorWidth),
				})

				// Lay out the indicator.
				offset := axis.Convert(image.Pt(viewStart, 0))
				defer op.Offset(offset).Push(gtx.Ops).Pop()
				FillShape(win, gtx.Ops, s.Indicator.Color, clip.Rect{Max: indicatorDims}.Op())

				// Add the indicator pointer hit area.
				area := clip.Rect(image.Rectangle{Max: indicatorDims})
				defer pointer.PassOp{}.Push(gtx.Ops).Pop()
				defer area.Push(gtx.Ops).Pop()
				s.Scrollbar.AddIndicator(gtx.Ops)

				return layout.Dimensions{Size: axis.Convert(gtx.Constraints.Min)}
			})
		}),
	)
}

package main

import (
	"fmt"
	"image"
	"strconv"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/text"
)

type HistogramSettingsState struct {
	saved, cancelled bool

	numBinsEditor  widget.Editor
	filterOutliers widget.Bool
	save           widget.PrimaryClickable
	cancel         widget.PrimaryClickable
}

func (hss *HistogramSettingsState) Saved() bool {
	b := hss.saved
	hss.saved = false
	return b
}

func (hss *HistogramSettingsState) Cancelled() bool {
	b := hss.cancelled
	hss.cancelled = false
	return b
}

func (hss *HistogramSettingsState) NumBins() int {
	n, err := strconv.ParseInt(hss.numBinsEditor.Text(), 10, 32)
	if err != nil || n < 1 || n > 9999 {
		return 100
	}
	return int(n)
}

func (hss *HistogramSettingsState) RejectOutliers() bool {
	return hss.filterOutliers.Value
}

type HistogramSettingsStyle struct {
	State *HistogramSettingsState
}

func (s *HistogramSettingsState) Reset(cfg *widget.HistogramConfig) {
	s.filterOutliers.Set(cfg.RejectOutliers)
	numBinsStr := fmt.Sprintf("%d", cfg.Bins)
	s.numBinsEditor.SetText(numBinsStr)
	s.numBinsEditor.SingleLine = true
	s.numBinsEditor.Filter = "0123456789"
	s.numBinsEditor.SetCaret(len(numBinsStr), len(numBinsStr))
}

func HistogramSettings(state *HistogramSettingsState) HistogramSettingsStyle {
	return HistogramSettingsStyle{
		State: state,
	}
}

func (hs HistogramSettingsStyle) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	hs.State.saved = false
	hs.State.cancelled = false

	validateNumBins := func(s string) bool {
		return len(s) <= 4
	}

	settingLabel := func(s string) layout.Dimensions {
		gtx := gtx
		gtx.Constraints.Min.Y = 0
		return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, text.Font{Weight: text.Bold}, 12, s, widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
	}

	dims := layout.Flex{Axis: layout.Vertical}.Layout(gtx,
		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return settingLabel("Number of bins")
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			tb := theme.TextBox(win.Theme, &hs.State.numBinsEditor, "Number of bins")
			tb.Validate = validateNumBins
			return tb.Layout(gtx)
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Spacer{Height: 5}.Layout(gtx)
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return settingLabel("Filter outliers")
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			ngtx := gtx
			ngtx.Constraints.Min = image.Point{}
			dims := theme.Switch(&hs.State.filterOutliers, "No", "Yes").Layout(win, ngtx)
			return layout.Dimensions{
				Size:     gtx.Constraints.Constrain(dims.Size),
				Baseline: dims.Baseline,
			}
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Spacer{Height: 10}.Layout(gtx)
		}),

		layout.Rigid(func(gtx layout.Context) layout.Dimensions {
			return layout.Flex{Axis: layout.Horizontal}.Layout(gtx,
				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
					btn := theme.Button(win.Theme, &hs.State.save.Clickable, "Save settings")
					// We can't put len(t) > 0 in validateNumBins because widget.Editor doesn't support separate
					// colors for text and caret, and we don't want an empty input box to have a red caret.
					if t := hs.State.numBinsEditor.Text(); len(t) > 0 && validateNumBins(t) {
						return btn.Layout(win, gtx)
					} else {
						gtx.Queue = nil
						return btn.Layout(win, gtx)
					}
				}),

				layout.Rigid(func(gtx layout.Context) layout.Dimensions { return layout.Spacer{Width: 5}.Layout(gtx) }),

				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
					return theme.Button(win.Theme, &hs.State.cancel.Clickable, "Cancel").Layout(win, gtx)
				}),
			)
		}),
	)

	for hs.State.save.Clicked() {
		hs.State.saved = true
	}
	for hs.State.cancel.Clicked() {
		hs.State.cancelled = true
	}

	return dims
}

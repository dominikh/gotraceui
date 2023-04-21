package main

import (
	"context"
	"fmt"
	"image"
	rtrace "runtime/trace"
	"strconv"
	"time"

	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/io/pointer"
)

type InteractiveHistogram struct {
	XLabel        string
	YLabel        string
	Config        widget.HistogramConfig
	widget        *theme.Future[*widget.Histogram]
	state         theme.HistogramState
	settingsState HistogramSettingsState
	click         widget.Clickable
	changed       bool
}

func (hist *InteractiveHistogram) Set(win *theme.Window, data []time.Duration) {
	hist.widget = theme.NewFuture(win, func(cancelled <-chan struct{}) *widget.Histogram {
		whist := widget.NewHistogram(&hist.Config, data)
		// Call Reset after creating the histogram so that NewHistogram can update the config with default values.
		hist.settingsState.Reset(hist.Config)
		return whist
	})
}

func (hist *InteractiveHistogram) Changed() bool {
	b := hist.changed
	hist.changed = false
	return b
}

func (hist *InteractiveHistogram) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.InteractiveHistogram.Layout").End()

	whist, ok := hist.widget.Result()
	if ok {
		thist := theme.Histogram(win.Theme, &hist.state)
		thist.XLabel = "Duration"
		thist.YLabel = "Count"
		dims := hist.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return thist.Layout(win, gtx, whist)
		})

		if hist.state.Activated != (struct {
			Start widget.FloatDuration
			End   widget.FloatDuration
		}{}) {
			hist.Config.Start = hist.state.Activated.Start
			hist.Config.End = hist.state.Activated.End
			hist.changed = true
		}

		for {
			click, ok := hist.click.Clicked()
			if !ok {
				break
			}
			if click.Button != pointer.ButtonSecondary {
				continue
			}

			menu := []*theme.MenuItem{
				{
					Label: PlainLabel("Change settings"),
					Do: func(gtx layout.Context) {
						win.SetModal(func(win *theme.Window, gtx layout.Context) layout.Dimensions {
							gtx.Constraints.Min = gtx.Constraints.Constrain(image.Pt(1000, 500))
							gtx.Constraints.Max = gtx.Constraints.Min
							return theme.Dialog(win.Theme, "Histogram settings").Layout(win, gtx, HistogramSettings(&hist.settingsState).Layout)
						})
					},
				},

				{
					// TODO disable when there is nothing to zoom out to
					Label: PlainLabel("Zoom out"),
					Do: func(gtx layout.Context) {
						hist.Config.Start = 0
						hist.Config.End = 0
						hist.changed = true
					},
				},
			}
			win.SetContextMenu(menu)
		}

		if hist.settingsState.Saved() {
			hist.Config.Bins = hist.settingsState.NumBins()
			hist.Config.RejectOutliers = hist.settingsState.RejectOutliers()
			hist.changed = true
			win.CloseModal()
		}
		if hist.settingsState.Cancelled() {
			hist.settingsState.Reset(hist.Config)
			win.CloseModal()
		}

		return dims
	} else {
		return widget.Label{}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, "Computing histogram…", widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
	}
}

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

func (s *HistogramSettingsState) Reset(cfg widget.HistogramConfig) {
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
	defer rtrace.StartRegion(context.Background(), "main.HistogramSettingsStyle.Layout").End()

	hs.State.saved = false
	hs.State.cancelled = false

	validateNumBins := func(s string) bool {
		return len(s) <= 4
	}

	settingLabel := func(s string) layout.Dimensions {
		gtx := gtx
		gtx.Constraints.Min.Y = 0
		return widget.Label{MaxLines: 1}.Layout(gtx, win.Theme.Shaper, font.Font{Weight: font.Bold}, 12, s, widget.ColorTextMaterial(gtx, rgba(0x000000FF)))
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

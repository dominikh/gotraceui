package theme

import (
	"context"
	"fmt"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"
)

type Panel interface {
	Component
}

type PanelButtons struct {
	close  widget.PrimaryClickable
	back   widget.PrimaryClickable
	detach widget.PrimaryClickable
	attach widget.PrimaryClickable

	windowed bool
}

func (pb *PanelButtons) Transition(state ComponentState) {
	switch state {
	case ComponentStatePanel:
		pb.windowed = false
	case ComponentStateWindow:
		pb.windowed = true
	case ComponentStateClosed:
		// Nothing to do
	default:
		panic(fmt.Sprintf("unsupported transition to state %q", state))
	}
}

func (pb *PanelButtons) WantsTransition() ComponentState {
	if pb.detach.Clicked() {
		return ComponentStateWindow
	} else if pb.attach.Clicked() {
		return ComponentStatePanel
	} else if pb.close.Clicked() {
		return ComponentStateClosed
	} else {
		return ComponentStateNone
	}
}

func (pb *PanelButtons) Backed() bool {
	return pb.back.Clicked()
}

func (pb *PanelButtons) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.PanelButtons.Layout").End()

	type button struct {
		w     *widget.PrimaryClickable
		label string
		cmd   NormalCommand
	}

	var buttons []button
	if pb.windowed {
		buttons = []button{
			{
				&pb.attach,
				"Attach",
				NormalCommand{
					PrimaryLabel: "Attach panel",
				},
			},

			{
				&pb.close,
				"Close",
				NormalCommand{
					PrimaryLabel: "Close panel",
				},
			},
		}
	} else {
		buttons = []button{
			{
				&pb.back,
				"Back",
				NormalCommand{
					PrimaryLabel: "Go to previous panel",
					Aliases:      []string{"back"},
				},
			},

			{
				&pb.detach,
				"Detach",
				NormalCommand{
					PrimaryLabel: "Detach panel",
				},
			},
		}
	}

	var cmds CommandSlice
	children := make([]layout.Widget, 0, 3)
	for _, btn := range buttons {
		btn := btn
		children = append(children,
			func(gtx layout.Context) layout.Dimensions {
				return Button(win.Theme, &btn.w.Clickable, btn.label).Layout(win, gtx)
			},
			layout.Spacer{Width: 5}.Layout,
		)

		cmd := btn.cmd
		cmd.Category = "Panel"
		cmd.Color = color.Oklch{L: 0.7862, C: 0.104, H: 140, A: 1}
		cmd.Fn = func() Action {
			return ExecuteAction(func(gtx layout.Context) {
				btn.w.Click()
			})
		}
		cmds = append(cmds, cmd)
	}

	win.AddCommandProvider(CommandSlice(cmds))
	return layout.Rigids(gtx, layout.Horizontal, children...)
}

// WidgetPanel turns any widget into a Panel. You are responsible for handling panel button events.
type WidgetPanel struct {
	w Widget
	PanelButtons
}

func (wp *WidgetPanel) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.WidgetPanel.Layout").End()

	return layout.Rigids(gtx, layout.Horizontal, Dumb(win, wp.PanelButtons.Layout), Dumb(win, wp.w))
}

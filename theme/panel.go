package theme

import (
	"context"
	rtrace "runtime/trace"

	mycolor "honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"
)

type Panel interface {
	Layout(win *Window, gtx layout.Context) layout.Dimensions
	Title() string
	Closed() bool
	Detached() bool
	Attached() bool
	SetWindowed(bool)
}

type PanelButtons struct {
	close  widget.PrimaryClickable
	back   widget.PrimaryClickable
	detach widget.PrimaryClickable
	attach widget.PrimaryClickable

	windowed bool
}

func (pb *PanelButtons) SetWindowed(b bool) { pb.windowed = b }

func (pb *PanelButtons) Windowed() bool { return pb.windowed }

func (pb *PanelButtons) Closed() bool {
	return pb.close.Clicked()
}

func (pb *PanelButtons) Detached() bool {
	return pb.detach.Clicked()
}

func (pb *PanelButtons) Attached() bool {
	return pb.attach.Clicked()
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

			{
				&pb.close,
				"Close",
				NormalCommand{
					PrimaryLabel: "Close panel",
				},
			},
		}
	}

	var cmds CommandSlice
	children := make([]layout.FlexChild, 0, 3)
	for _, btn := range buttons {
		btn := btn
		children = append(children,
			layout.Rigid(func(gtx layout.Context) layout.Dimensions {
				return Button(win.Theme, &btn.w.Clickable, btn.label).Layout(win, gtx)
			}),
			layout.Rigid(layout.Spacer{Width: 5}.Layout),
		)

		cmd := btn.cmd
		cmd.Category = "Panel"
		cmd.Color = mycolor.Oklch{L: 0.7862, C: 0.104, H: 140, Alpha: 1}
		cmd.Fn = func() Action {
			return ExecuteAction(func(gtx layout.Context) {
				btn.w.Click()
			})
		}
		cmds = append(cmds, cmd)
	}

	win.AddCommandProvider(CommandSlice(cmds))
	return layout.Flex{}.Layout(gtx, children...)
}

// WidgetPanel turns any widget into a Panel. You are responsible for handling panel button events.
type WidgetPanel struct {
	w Widget
	PanelButtons
}

func (wp *WidgetPanel) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.WidgetPanel.Layout").End()

	return layout.Flex{}.Layout(gtx, layout.Rigid(Dumb(win, wp.PanelButtons.Layout)), layout.Rigid(Dumb(win, wp.w)))
}

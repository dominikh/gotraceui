package theme

// This is copied almost verbatim from material, but uses our theme.

import (
	"context"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/color"
	"honnef.co/go/gotraceui/f32color"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/op"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/unit"
)

type EditorStyle struct {
	Font     font.Font
	TextSize unit.Sp
	// Color is the text color.
	Color color.Oklch
	// Hint contains the text displayed when the editor is empty.
	Hint string
	// HintColor is the color of hint text.
	HintColor color.Oklch
	// SelectionColor is the color of the background for selected text.
	SelectionColor color.Oklch
	Editor         *widget.Editor

	shaper *text.Shaper
}

func Editor(th *Theme, editor *widget.Editor, hint string) EditorStyle {
	return EditorStyle{
		Editor:         editor,
		TextSize:       th.TextSize,
		Color:          th.Palette.Foreground,
		shaper:         th.Shaper,
		Hint:           hint,
		HintColor:      f32color.MulAlpha(th.Palette.Foreground, 0.73),
		SelectionColor: th.Palette.PrimarySelection,
	}
}

func (e EditorStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.EditorStyle.Layout").End()

	// Choose colors.
	textColorMacro := op.Record(gtx.Ops)
	paint.ColorOp{Color: win.ConvertColor(e.Color)}.Add(gtx.Ops)
	textColor := textColorMacro.Stop()
	hintColorMacro := op.Record(gtx.Ops)
	paint.ColorOp{Color: win.ConvertColor(e.HintColor)}.Add(gtx.Ops)
	hintColor := hintColorMacro.Stop()
	selectionColorMacro := op.Record(gtx.Ops)
	paint.ColorOp{Color: win.ConvertColor(blendDisabledColor(gtx.Queue == nil, e.SelectionColor))}.Add(gtx.Ops)
	selectionColor := selectionColorMacro.Stop()

	var maxlines int
	if e.Editor.SingleLine {
		maxlines = 1
	}

	macro := op.Record(gtx.Ops)
	tl := widget.Label{Alignment: e.Editor.Alignment, MaxLines: maxlines}
	dims := tl.Layout(gtx, e.shaper, e.Font, e.TextSize, e.Hint, hintColor)
	call := macro.Stop()

	if w := dims.Size.X; gtx.Constraints.Min.X < w {
		gtx.Constraints.Min.X = w
	}
	if h := dims.Size.Y; gtx.Constraints.Min.Y < h {
		gtx.Constraints.Min.Y = h
	}
	dims = e.Editor.Layout(gtx, e.shaper, e.Font, e.TextSize, textColor, selectionColor)
	if e.Editor.Len() == 0 {
		call.Add(gtx.Ops)
	}
	return dims
}

func blendDisabledColor(disabled bool, c color.Oklch) color.Oklch {
	if disabled {
		return f32color.Disabled(c)
	}
	return c
}

type TextBoxStyle struct {
	EditorStyle
	Validate func(s string) bool
}

func TextBox(th *Theme, editor *widget.Editor, hint string) TextBoxStyle {
	return TextBoxStyle{EditorStyle: Editor(th, editor, hint)}
}

func (tb TextBoxStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.TextBoxStyle.Layout").End()

	if tb.Validate != nil && !tb.Validate(tb.Editor.Text()) {
		tb.Color = oklch(62.8, 0.258, 29.234)
	}
	return Background{Color: oklch(100, 0, 0)}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
		return Bordered{Color: oklch(0, 0, 0), Width: 1}.Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
			return layout.UniformInset(2).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				return tb.EditorStyle.Layout(win, gtx)
			})
		})
	})
}

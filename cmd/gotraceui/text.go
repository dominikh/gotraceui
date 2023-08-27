package main

import (
	"context"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/clip"
	ourfont "honnef.co/go/gotraceui/font"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/theme"

	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/text"
	"gioui.org/x/styledtext"
)

type Text struct {
	// The theme must only be used for building the Text, with methods like Span. The Layout function has to use the
	// theme provided to it, to avoid race conditions when texts transition from widgets to independent windows.
	//
	// The theme must be reset in Reset.
	styles    []styledtext.SpanStyle
	Alignment text.Alignment

	events  []TextEvent
	hovered *TextSpan

	// Clickables we use for spans and reuse between frames. We allocate them one by one because it really doesn't
	// matter; we have hundreds of these at most. This won't make the GC sweat, and it avoids us having to do a bunch of
	// semi-manual memory management.
	clickables []*gesture.Click
}

type TextBuilder struct {
	Theme *theme.Theme
	Spans []TextSpan
}

type TextEvent struct {
	Span  *TextSpan
	Event gesture.ClickEvent
}

type TextSpan struct {
	styledtext.SpanStyle
	Object     any
	ObjectLink ObjectLink

	Click *gesture.Click
}

func (txt *TextBuilder) Add(s TextSpan) {
	txt.Spans = append(txt.Spans, s)
}

func (txt *TextBuilder) Span(label string) *TextSpan {
	style := styledtext.SpanStyle{
		Content: label,
		Size:    txt.Theme.TextSize,
		Color:   txt.Theme.Palette.Foreground,
		Font:    ourfont.Collection()[0].Font,
	}
	s := TextSpan{
		SpanStyle: style,
	}
	txt.Spans = append(txt.Spans, s)
	return &txt.Spans[len(txt.Spans)-1]
}

func (txt *TextBuilder) SpanWith(label string, fn func(s *TextSpan)) *TextSpan {
	s := txt.Span(label)
	fn(s)
	return s
}

func (txt *TextBuilder) Bold(label string) *TextSpan {
	s := txt.Span(label)
	s.Font.Weight = font.Bold
	return s
}

func (txt *TextBuilder) DefaultLink(label, provenance string, obj any) *TextSpan {
	return txt.Link(label, obj, defaultObjectLink(obj, provenance))
}

func (txt *TextBuilder) Link(label string, obj any, link ObjectLink) *TextSpan {
	s := txt.Span(label)
	s.Object = obj
	s.ObjectLink = link
	a := link.Action(0)
	switch a.(type) {
	case NavigationAction:
		s.Color = txt.Theme.Palette.NavigationLink
	case OpenAction:
		s.Color = txt.Theme.Palette.OpenLink
	default:
		s.Color = txt.Theme.Palette.Link
	}
	return s
}

func (txt *Text) Reset(th *theme.Theme) {
	txt.events = txt.events[:0]
	txt.styles = txt.styles[:0]
	txt.Alignment = 0
}

func (txt *Text) Events() []TextEvent {
	return txt.events
}

// Hovered returns the interactive span that was hovered in the last call to Layout.
func (txt *Text) Hovered() *TextSpan {
	return txt.hovered
}

func (txt *Text) Layout(win *theme.Window, gtx layout.Context, spans []TextSpan) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "main.Text.Layout").End()

	var clickableIdx int
	for i := range spans {
		s := &spans[i]
		if s.Object != nil {
			var clk *gesture.Click
			if clickableIdx < len(txt.clickables) {
				clk = txt.clickables[clickableIdx]
				clickableIdx++
			} else {
				clk = &gesture.Click{}
				txt.clickables = append(txt.clickables, clk)
				clickableIdx++
			}
			s.Click = clk
		}
	}

	txt.events = txt.events[:0]
	txt.hovered = nil
	for i := range spans {
		s := &spans[i]
		if s.Click != nil {
			for _, ev := range s.Click.Events(gtx.Queue) {
				txt.events = append(txt.events, TextEvent{s, ev})
			}
			if s.Click.Hovered() {
				txt.hovered = s
			}
		}
	}

	txt.styles = txt.styles[:0]
	for _, s := range spans {
		txt.styles = append(txt.styles, s.SpanStyle)
	}
	ptxt := styledtext.Text(win.Theme.Shaper, txt.styles...)
	ptxt.Alignment = txt.Alignment
	if txt.Alignment == text.Start {
		gtx.Constraints.Max.X = 1e6
	}
	return ptxt.Layout(gtx, func(gtx layout.Context, i int, dims layout.Dimensions) {
		defer clip.Rect{Max: dims.Size}.Push(gtx.Ops).Pop()
		s := &spans[i]
		if s.Object != nil {
			s.Click.Add(gtx.Ops)
			pointer.CursorPointer.Add(gtx.Ops)
		}
	})
}

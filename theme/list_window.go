package theme

import (
	"context"
	"image/color"
	rtrace "runtime/trace"

	"gioui.org/io/key"
	"gioui.org/layout"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/x/eventx"
	"honnef.co/go/gotraceui/widget"
)

type ListWindowItem struct {
	Item         any
	Label        string
	FilterLabels []string
	index        int
	click        widget.PrimaryClickable
}

type Filter interface {
	Filter(item ListWindowItem) bool
}

type ListWindow struct {
	BuildFilter func(string) Filter

	items []ListWindowItem

	filtered []int
	// index of the selected item in the filtered list
	index     int
	done      bool
	cancelled bool

	theme *Theme
	input widget.Editor
	list  widget.List
}

func NewListWindow(th *Theme) *ListWindow {
	return &ListWindow{
		theme: th,
		input: widget.Editor{
			SingleLine: true,
			Submit:     true,
		},
		list: widget.List{
			List: layout.List{
				Axis: layout.Vertical,
			},
		},
	}
}

func (w *ListWindow) SetItems(items []ListWindowItem) {
	w.items = items
	w.filtered = make([]int, len(items))
	for i := range w.items {
		w.items[i].index = i
		w.filtered[i] = i
	}
}

func (w *ListWindow) Cancelled() bool { return w.cancelled }
func (w *ListWindow) Confirmed() (any, bool) {
	if !w.done {
		return nil, false
	}
	w.done = false
	return w.items[w.filtered[w.index]].Item, true
}

// Don't bubble up normal key presses. This is a workaround for Gio's input handling. The editor widget
// primarily uses edit events to handle key presses, but edit events don't prevent key presses from bubbling up
// the input tree. That means that typing in an editor can trigger our single-key shortcuts.
var editorKeyset = key.Set("↓|↑|⎋|A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z")

func (w *ListWindow) Layout(gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.ListWindow.Layout").End()
	defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()

	key.InputOp{Tag: w, Keys: editorKeyset}.Add(gtx.Ops)

	var spy *eventx.Spy

	dims := widget.Bordered{Color: w.theme.Palette.Border, Width: w.theme.WindowBorder}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Max}.Push(gtx.Ops).Pop()
		spy, gtx = eventx.Enspy(gtx)
		gtx.Constraints.Min.X = gtx.Constraints.Max.X

		paint.Fill(gtx.Ops, w.theme.Palette.Background)

		fn2 := func(gtx layout.Context) layout.Dimensions {
			return List(w.theme, &w.list).Layout(gtx, len(w.filtered), func(gtx layout.Context, index int) layout.Dimensions {
				// XXX use constants for colors
				item := &w.items[w.filtered[index]]
				return item.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					var c color.NRGBA
					if index == w.index {
						c = rgba(0xFF0000FF)
					} else if item.click.Hovered() {
						c = rgba(0xFF00FFFF)
					} else {
						c = rgba(0x000000FF)
					}
					return widget.TextLine{Color: c}.Layout(gtx, w.theme.Shaper, text.Font{}, w.theme.TextSize, item.Label)
				})
			})
		}

		flex := layout.Flex{
			Axis: layout.Vertical,
		}
		editor := Editor(w.theme, &w.input, "")
		editor.Editor.Focus()
		return flex.Layout(gtx, layout.Rigid(editor.Layout), layout.Flexed(1, fn2))
	})

	// The editor widget selectively handles the up and down arrow keys, depending on the contents of the text field and
	// the position of the cursor. This means that our own InputOp won't always be getting all events. But due to the
	// selectiveness of the editor's InputOp, we can't fully rely on it, either. We need to combine the events of the
	// two.
	//
	// To be consistent, we handle all events after layout of the nested widgets, to have the same frame latency for all
	// events.
	handleKey := func(ev key.Event) {
		if ev.State == key.Press {
			firstVisible := w.list.Position.First
			lastVisible := w.list.Position.First + w.list.Position.Count - 1
			if w.list.Position.Offset > 0 {
				// The last element might be barely visible, even just one pixel. and we still want to scroll in that
				// case
				firstVisible++
			}
			if w.list.Position.OffsetLast < 0 {
				// The last element might be barely visible, even just one pixel. and we still want to scroll in that
				// case
				lastVisible--
			}
			visibleCount := lastVisible - firstVisible + 1

			switch ev.Name {
			case "↑":
				w.index--
				if w.index < firstVisible {
					// XXX compute the correct position. the user might have scrolled the list via its scrollbar.
					w.list.Position.First--
				}
				if w.index < 0 {
					w.index = len(w.filtered) - 1
					w.list.Position.First = w.index - visibleCount + 1
				}
			case "↓":
				w.index++
				if w.index > lastVisible {
					// XXX compute the correct position. the user might have scrolled the list via its scrollbar.
					w.list.Position.First++
				}
				if w.index >= len(w.filtered) {
					w.index = 0
					w.list.Position.First = 0
					w.list.Position.Offset = 0
				}
			case "⎋": // Escape
				w.cancelled = true
			}
		}
	}
	for _, evs := range spy.AllEvents() {
		for _, ev := range evs.Items {
			if ev, ok := ev.(key.Event); ok {
				handleKey(ev)
			}
		}
	}
	for _, ev := range w.input.Events() {
		switch ev.(type) {
		case widget.ChangeEvent:
			w.filtered = w.filtered[:0]
			f := w.BuildFilter(w.input.Text())
			for _, item := range w.items {
				if f.Filter(item) {
					w.filtered = append(w.filtered, item.index)
				}
			}
			// TODO(dh): if the previously selected entry hasn't been filtered away, then it should stay selected.
			if w.index >= len(w.filtered) {
				w.index = len(w.filtered) - 1
			}
			if w.index < 0 && len(w.filtered) > 0 {
				w.index = 0
			}
		case widget.SubmitEvent:
			if len(w.filtered) != 0 {
				w.done = true
			}
		}
	}
	for i, idx := range w.filtered {
		if w.items[idx].click.Clicked() {
			w.index = i
			w.done = true
		}
	}

	for _, ev := range gtx.Events(w) {
		switch ev := ev.(type) {
		case key.Event:
			handleKey(ev)
		}
	}

	return dims
}

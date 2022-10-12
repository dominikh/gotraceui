package theme

import (
	"image"

	"gioui.org/layout"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"gioui.org/text"
	"gioui.org/widget"
	mylayout "honnef.co/go/gotraceui/layout"
	mywidget "honnef.co/go/gotraceui/widget"
)

type ContextMenu struct {
	Items []layout.Widget

	modal Modal
	group MenuGroup
}

func (m *ContextMenu) Layout(gtx layout.Context) layout.Dimensions {
	m.group.Items = m.Items

	macro := op.Record(gtx.Ops)
	m.modal.Layout(gtx, m.group.Layout)
	op.Defer(gtx.Ops, macro.Stop())
	return layout.Dimensions{}
}

func (m *ContextMenu) Cancelled() bool {
	return m.modal.cancelled
}

// FIXME(dh): click on menu, click on item, menu closed. click on same menu, previously clicked item is still
// highlighted. This is caused by Gio merging event handling and doing layout. When the user clicks on the menu, we draw
// a frame not yet knowing about the click. Then we draw another frame, displaying the group. At that point we don't
// know that hovering has stopped yet. A third frame, which we do not draw, would draw the item correctly.

// TODO(dh): enable keyboard navigation of menus

type Menu struct {
	Theme  *Theme
	Groups []MenuGroup

	open     bool
	lastOpen struct {
		off int
		g   *MenuGroup
	}
	modal Modal
}

var (
	menuColor             = rgba(0xEFFFFFFF)
	menuSelectedColor     = rgba(0x9CEFEFFF)
	menuBorderColor       = menuSelectedColor
	menuTextColor         = rgba(0x000000FF)
	menuDisabledTextColor = rgba(0xAAAAAAFF)
	menuDividerColor      = menuTextColor
)

func (m *Menu) Close() {
	m.open = false
	m.lastOpen.g = nil
}

func (m *Menu) Layout(gtx layout.Context) layout.Dimensions {
	// TODO(dh): open a group on press, not on click. allow the user to keep the button pressed, move onto an item, and
	// release the button, to select a menu item with a single click.

	if m.modal.Cancelled() {
		m.Close()
	}

	return mywidget.Background{Color: menuColor}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		var h, b, off int

		drawGroup := func(gtx layout.Context, g *MenuGroup, off int) {
			mylayout.PixelInset{Bottom: h}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				macro := op.Record(gtx.Ops)
				stack := op.Offset(image.Pt(off, h)).Push(gtx.Ops)
				m.modal.Layout(gtx, g.Layout)
				stack.Pop()
				op.Defer(gtx.Ops, macro.Stop())
				return layout.Dimensions{}
			})
		}

		for i := range m.Groups {
			func() {
				g := &m.Groups[i]
				defer op.Offset(image.Pt(off, 0)).Push(gtx.Ops).Pop()

				for g.click.Clicked() {
					m.open = !m.open
					m.lastOpen.g = nil
				}

				if m.open && g.click.Hovered() {
					m.lastOpen.off = off
					m.lastOpen.g = g
				}

				bg := menuColor
				if g == m.lastOpen.g {
					bg = menuSelectedColor
				}

				dims := mywidget.Background{Color: bg}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return g.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(1).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							return mywidget.TextLine{Color: menuTextColor}.Layout(gtx, m.Theme.Shaper, text.Font{}, 12, g.Label)
						})
					})
				})
				// We expect all group labels to have the same height and baseline
				h = dims.Size.Y
				b = dims.Baseline

				// +5 for padding between group labels
				off += dims.Size.X + 5
			}()
		}

		if m.open && m.lastOpen.g != nil {
			drawGroup(gtx, m.lastOpen.g, m.lastOpen.off)
		}

		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, h), Baseline: b}
	})
}

type MenuGroup struct {
	Label string
	Items []layout.Widget

	list  layout.List
	click widget.Clickable
}

func (g *MenuGroup) Layout(gtx layout.Context) layout.Dimensions {
	// Render the menu in two passes. First we find the widest element, then we render for real with that width
	// set as the minimum constraint.
	origOps := gtx.Ops
	gtx.Ops = new(op.Ops)
	var maxWidth int
	for i := range g.Items {
		dims := g.Items[i](gtx)
		if dims.Size.X > maxWidth {
			maxWidth = dims.Size.X
		}
	}

	gtx.Ops = origOps
	g.list.Axis = layout.Vertical

	return mywidget.Bordered{Color: menuBorderColor, Width: 1}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return mywidget.Background{Color: menuColor}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return (g.list).Layout(gtx, len(g.Items), func(gtx layout.Context, index int) layout.Dimensions {
				gtx.Constraints.Min.X = maxWidth
				gtx.Constraints.Max.X = maxWidth
				return g.Items[index](gtx)
			})
		})
	})
}

type MenuItem struct {
	Theme    *Theme
	Label    func() string
	Shortcut string
	Disabled bool

	click widget.Clickable
}

func (item *MenuItem) Layout(gtx layout.Context) layout.Dimensions {
	fg := menuTextColor
	if item.Disabled {
		fg = menuDisabledTextColor
	}
	bg := menuColor
	if !item.Disabled && item.click.Hovered() {
		bg = menuSelectedColor
	}
	dims := mywidget.Background{Color: bg}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return item.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return layout.UniformInset(2).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				l := func(gtx layout.Context) layout.Dimensions {
					dims := mywidget.TextLine{Color: fg}.Layout(gtx, item.Theme.Shaper, text.Font{}, 12, item.Label())
					if item.Shortcut != "" {
						// add padding between label and shortcut
						dims.Size.X += gtx.Dp(10)
					}
					return dims
				}
				r := func(gtx layout.Context) layout.Dimensions {
					if item.Shortcut == "" {
						return layout.Dimensions{}
					} else {
						return mywidget.TextLine{Color: fg}.Layout(gtx, item.Theme.Shaper, text.Font{}, 12, item.Shortcut)
					}
				}
				return layout.Flex{Axis: layout.Horizontal, Spacing: layout.SpaceBetween}.Layout(gtx, layout.Rigid(l), layout.Rigid(r))
			})
		})
	})

	return dims
}

func (item *MenuItem) Clicked() bool {
	return item.click.Clicked()
}

type MenuDivider struct{}

func (MenuDivider) Layout(gtx layout.Context) layout.Dimensions {
	// XXX use font's line height
	height := 15

	defer op.Offset(image.Pt(0, height/2)).Push(gtx.Ops).Pop()

	shape := clip.Rect{
		Max: image.Pt(gtx.Constraints.Min.X, 1),
	}
	shape.Max = gtx.Constraints.Constrain(shape.Max)
	paint.FillShape(gtx.Ops, menuDividerColor, shape.Op())
	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Min.X, height)}
}

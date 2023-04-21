package theme

import (
	"context"
	"image"
	"image/color"
	rtrace "runtime/trace"

	"gioui.org/font"
	"gioui.org/op"
	"gioui.org/op/clip"
	"gioui.org/op/paint"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/widget"
)

// FIXME(dh): click on menu, click on item, menu closed. click on same menu, previously clicked item is still
// highlighted. This is caused by Gio merging event handling and doing layout. When the user clicks on the menu, we draw
// a frame not yet knowing about the click. Then we draw another frame, displaying the group. At that point we don't
// know that hovering has stopped yet. A third frame, which we do not draw, would draw the item correctly.

// TODO(dh): enable keyboard navigation of menus

type Menu struct {
	Groups []MenuGroup

	open     bool
	lastOpen struct {
		off int
		g   *MenuGroup
	}
	cancelled bool
}

func (m *Menu) Close() {
	m.open = false
	m.lastOpen.g = nil
}

func NewMenuStyle(th *Theme, menu *Menu) MenuStyle {
	return MenuStyle{
		Menu:       menu,
		Foreground: th.Palette.Foreground,
		Background: th.Palette.Menu.Background,
		Selected:   th.Palette.Menu.Selected,
		Border:     th.Palette.Menu.Border,
		Disabled:   th.Palette.Menu.Disabled,
	}
}

type MenuStyle struct {
	Menu       *Menu
	Foreground color.NRGBA
	Background color.NRGBA
	Selected   color.NRGBA
	Border     color.NRGBA
	Disabled   color.NRGBA
}

func (m MenuStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.MenuStyle.Layout").End()

	// TODO(dh): open a group on press, not on click. allow the user to keep the button pressed, move onto an item, and
	// release the button, to select a menu item with a single click.

	gtx.Constraints.Min = image.Point{}

	if m.Menu.cancelled {
		m.Menu.Close()
		m.Menu.cancelled = false
	}

	return widget.Background{Color: m.Background}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		var h, b, off int

		drawGroup := func(gtx layout.Context, g *MenuGroup, off int) {
			layout.PixelInset{Bottom: h}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				macro := op.Record(gtx.Ops)

				// We use two separate offsets to position the menu group. One purely vertical and one purely
				// horizontal. The vertical offset places the modal below the menu, so that hovering over other groups
				// opens them. The horizontal offset is inside the modal to position the group, without moving the modal
				// away from x=0.
				stack := op.Offset(image.Pt(0, h)).Push(gtx.Ops)
				Modal(&m.Menu.cancelled).Layout(win, gtx, func(win *Window, gtx layout.Context) layout.Dimensions {
					defer op.Offset(image.Pt(off, 0)).Push(gtx.Ops).Pop()
					return MenuGroupStyle{
						Group:      g,
						Background: m.Background,
						Border:     m.Border,
					}.Layout(win, gtx)
				})
				stack.Pop()
				op.Defer(gtx.Ops, macro.Stop())
				return layout.Dimensions{}
			})
		}

		for i := range m.Menu.Groups {
			func() {
				g := &m.Menu.Groups[i]
				defer op.Offset(image.Pt(off, 0)).Push(gtx.Ops).Pop()

				for g.click.Clicked() {
					m.Menu.open = !m.Menu.open
					m.Menu.lastOpen.g = nil
				}

				if m.Menu.open && g.click.Hovered() {
					m.Menu.lastOpen.off = off
					m.Menu.lastOpen.g = g
				}

				bg := m.Background
				if g == m.Menu.lastOpen.g {
					bg = m.Selected
				}

				dims := widget.Background{Color: bg}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
					return g.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
						return layout.UniformInset(1).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
							return widget.TextLine{Color: m.Foreground}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, g.Label)
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

		if m.Menu.open && m.Menu.lastOpen.g != nil {
			drawGroup(gtx, m.Menu.lastOpen.g, m.Menu.lastOpen.off)
		}

		return layout.Dimensions{Size: image.Pt(gtx.Constraints.Max.X, h), Baseline: b}
	})
}

type MenuGroup struct {
	Label string
	Items []Widget

	list  layout.List
	click widget.PrimaryClickable
}

type MenuGroupStyle struct {
	Group      *MenuGroup
	Background color.NRGBA
	Border     color.NRGBA
}

func NewMenuGroupStyle(th *Theme, group *MenuGroup) MenuGroupStyle {
	return MenuGroupStyle{
		Group:      group,
		Background: th.Palette.Menu.Background,
		Border:     th.Palette.Menu.Border,
	}
}

func (g MenuGroupStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.MenuGroupStyle.Layout").End()

	// Render the menu in two passes. First we find the widest element, then we render for real with that width
	// set as the minimum constraint.
	origOps := gtx.Ops
	gtx.Ops = new(op.Ops)
	gtx.Constraints.Min = image.Point{}
	var maxWidth int
	for i := range g.Group.Items {
		dims := g.Group.Items[i](win, gtx)
		if dims.Size.X > maxWidth {
			maxWidth = dims.Size.X
		}
	}

	gtx.Ops = origOps
	g.Group.list.Axis = layout.Vertical

	return widget.Bordered{Color: g.Border, Width: 1}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return widget.Background{Color: g.Background}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return (g.Group.list).Layout(gtx, len(g.Group.Items), func(gtx layout.Context, index int) layout.Dimensions {
				gtx.Constraints.Min.X = maxWidth
				gtx.Constraints.Max.X = maxWidth
				return g.Group.Items[index](win, gtx)
			})
		})
	})
}

type MenuItem struct {
	Label    func() string
	Shortcut string
	Disabled func() bool
	Do       func(layout.Context)

	click widget.PrimaryClickable
}

type MenuItemStyle struct {
	Item       *MenuItem
	Foreground color.NRGBA
	Background color.NRGBA
	Selected   color.NRGBA
	Disabled   color.NRGBA
}

func NewMenuItemStyle(th *Theme, item *MenuItem) MenuItemStyle {
	return MenuItemStyle{
		Item:       item,
		Foreground: th.Palette.Foreground,
		Background: th.Palette.Menu.Background,
		Selected:   th.Palette.Menu.Selected,
		Disabled:   th.Palette.Menu.Disabled,
	}
}

func (item MenuItemStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.MenuItemStyle.Layout").End()

	fg := item.Foreground
	disabled := item.Item.Disabled != nil && item.Item.Disabled()
	if disabled {
		fg = item.Disabled
	}
	bg := item.Background
	if !disabled && item.Item.click.Hovered() {
		bg = item.Selected
	}
	dims := widget.Background{Color: bg}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
		return item.Item.click.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
			return layout.UniformInset(2).Layout(gtx, func(gtx layout.Context) layout.Dimensions {
				l := func(gtx layout.Context) layout.Dimensions {
					dims := widget.TextLine{Color: fg}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, item.Item.Label())
					if item.Item.Shortcut != "" {
						// add padding between label and shortcut
						dims.Size.X += gtx.Dp(10)
					}
					return dims
				}
				r := func(gtx layout.Context) layout.Dimensions {
					if item.Item.Shortcut == "" {
						return layout.Dimensions{}
					} else {
						return widget.TextLine{Color: fg}.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, item.Item.Shortcut)
					}
				}
				return layout.Flex{Axis: layout.Horizontal, Spacing: layout.SpaceBetween}.Layout(gtx, layout.Rigid(l), layout.Rigid(r))
			})
		})
	})

	return dims
}

func (item *MenuItem) Clicked() bool {
	if item.Disabled != nil && item.Disabled() {
		return false
	}
	return item.click.Clicked()
}

type MenuDividerStyle struct {
	Color color.NRGBA
}

func MenuDivider(th *Theme) MenuDividerStyle {
	return MenuDividerStyle{
		Color: th.Palette.Border,
	}
}

func (mds MenuDividerStyle) Layout(win *Window, gtx layout.Context) layout.Dimensions {
	defer rtrace.StartRegion(context.Background(), "theme.MenuDividerStyle.Layout").End()

	// XXX use font's line height
	height := 15

	defer op.Offset(image.Pt(0, height/2)).Push(gtx.Ops).Pop()

	shape := clip.Rect{
		Max: image.Pt(gtx.Constraints.Min.X, 1),
	}
	shape.Max = gtx.Constraints.Constrain(shape.Max)
	paint.FillShape(gtx.Ops, mds.Color, shape.Op())
	return layout.Dimensions{Size: image.Pt(gtx.Constraints.Min.X, height)}
}

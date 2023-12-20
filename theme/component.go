package theme

import (
	"strconv"

	"honnef.co/go/gotraceui/layout"
)

// ComponentState describes a component's current state or desired state.
type ComponentState int

const (
	ComponentStateNone ComponentState = iota
	ComponentStatePanel
	ComponentStateTab
	ComponentStateWindow
	ComponentStateClosed
)

func (state ComponentState) String() string {
	switch state {
	case ComponentStateNone:
		return "none"
	case ComponentStatePanel:
		return "panel"
	case ComponentStateTab:
		return "tab"
	case ComponentStateClosed:
		return "closed"
	default:
		return strconv.Itoa(int(state))
	}
}

// A Component is a widget that can be displayed as a panel, tab, or in its own window.
type Component interface {
	Layout(win *Window, gtx layout.Context) layout.Dimensions
	// Title is the title to display as the window or tab title.
	Title() string
	// WantsTransition indicates that the component wants to transition to a different state. It is not
	// guaranteed that the request will be honored. Calling WantsTransition clears the desired state
	// transition.
	//
	// This method is not thread-safe.
	WantsTransition(gtx layout.Context) ComponentState
	// Transition notifies the component of its new state. This is called at least once before drawing the
	// component for the first time. There may or may not be a transition to the closed state.
	Transition(state ComponentState)
}

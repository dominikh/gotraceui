//go:build !debug

package main

import (
	"errors"
	"time"

	"gioui.org/app"
)

const debug = false

func (g *debugGraph) addValue(ts time.Time, val float64) {}

func NewDebugWindow() *DebugWindow                  { return &DebugWindow{} }
func (dwin *DebugWindow) Run(win *app.Window) error { return errors.New("debugging disabled") }

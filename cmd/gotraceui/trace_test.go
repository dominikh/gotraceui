package main

import (
	"bytes"
	"testing"
)

type nullProgresser struct{}

func (nullProgresser) SetProgress(float64)      {}
func (nullProgresser) SetProgressLossy(float64) {}

func FuzzLoadTrace(f *testing.F) {
	f.Fuzz(func(t *testing.T, in []byte) {
		loadTrace(bytes.NewReader(in), nullProgresser{})
	})
}

# gotraceui - an efficient frontend for Go execution traces

Gotraceui is a tool for visualizing and analyzing Go execution traces. It is meant to be a faster, more accessible, and
more powerful alternative to `go tool trace`. Unlike `go tool trace`, Gotraceui doesn’t use deprecated browser APIs (or a
browser at all), and its UI is tuned specifically to the unique characteristics of Go traces.

## Status

Gotraceui still has some sharp corners, hasn't had its first release yet and is lacking a manual. It is, however, fairly
usable, and people have already used it productively.

## Installation

Users of Nix can use the flake. There are no packages for other distributions or OSs yet and you will have to build
`honnef.co/go/gotraceui/cmd/gotraceui` yourself. [Gio's instructions](https://gioui.org/doc/install) explain the
dependencies for the various platforms.

## Known issues

- [runtime/trace: time stamps out of order](https://github.com/golang/go/issues/16755)
- Timelines with millions of events can be a bit slow to render

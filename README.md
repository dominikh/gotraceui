# gotraceui - an efficient frontend for Go execution traces

Gotraceui is a tool for visualizing and analyzing Go execution traces. It is meant to be a faster, more accessible, and
more powerful alternative to `go tool trace`. Unlike `go tool trace`, Gotraceui doesn’t use deprecated browser APIs (or a
browser at all), and its UI is tuned specifically to the unique characteristics of Go traces.

![Screenshot](https://user-images.githubusercontent.com/39825/228830850-d0f324dd-10bc-47e9-99d4-0936cbb79492.png)

## Installation

Users of Nix can use the flake. There are no packages for other distributions or OSs yet and you will have to build
`honnef.co/go/gotraceui/cmd/gotraceui` yourself. The manual contains information on how to.

## Manual

Gotraceui [includes a manual](https://github.com/dominikh/gotraceui/releases/latest/download/gotraceui.pdf) in PDF form.
Each [release](https://github.com/dominikh/gotraceui/releases) includes the manual as an asset.

## Notes for package maintainers

When packaging Gotraceui please take care to

- pass `-X gioui.org/app.ID=co.honnef.Gotraceui` to the linker
- install the `share` directory
- call the package `gotraceui`, _please_
- include the `LICENSE-THIRD-PARTY` file; it contains all the licenses and copyright notices of all dependencies and all
  code our code is derived from. Including this file satisfies the requirement of reproducing copyright notices and
  permission notices.

## License

The source code of the program and all assets necessary to run the program are licensed under the MIT license.
The manual (all files in `doc/manual` as well as the compiled output) is licensed under the [CC BY-SA 4.0](https://creativecommons.org/licenses/by-sa/4.0/).

Copies of the licenses of all dependencies can be found in LICENSE-THIRD-PARTY.

## Copyright

All original work is copyrighted by its respective authors (consult the git log.)  
Parts of the code are derived from Go, © The Go Authors.  
Parts of the code are derived from Gio, © The Gio authors.  
Parts of the code are derived from go-tinylfi, © Damian Gryski
`font/fallback.ttf` is derived from the DejaVu fonts, © Bitstream, © Tavmjong Bah  
`doc/manual/images/olive.jpg` is © Charlotte Brandhorst-Satzkorn, photographer and owner of the subject.

The compiled binary includes code from dependencies. These dependencies and their copyright holders can be found in `LICENSE-THIRD-PARTY`.

## Known issues

- [runtime/trace: time stamps out of order](https://github.com/golang/go/issues/16755)
- Timelines with millions of events can be a bit slow to render

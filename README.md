# gotraceui - an efficient frontend for Go execution traces

Gotraceui is a tool for visualizing and analyzing Go execution traces. It is meant to be a faster, more accessible, and
more powerful alternative to `go tool trace`. Unlike `go tool trace`, Gotraceui doesn’t use deprecated browser APIs (or a
browser at all), and its UI is tuned specifically to the unique characteristics of Go traces.

## Use

Gotraceui is still a work in progress with many sharp corners, but you can already try it out. Simply build
`honnef.co/go/gotraceui/cmd/gotraceui` and run it, passing the path to a trace file as the only argument. See the
documentation of [runtime/trace](https://pkg.go.dev/runtime/trace) on how to obtain traces, and see [Gio's
instructions](https://gioui.org/doc/install) to figure out the dependencies required for building gotraceui.

## Controls

None of these controls are final. Users without a middle mouse button will have a bad experience right now.

| Key                         | Function                                                                    |
|-----------------------------|-----------------------------------------------------------------------------|
| Middle mouse button (hold)  | Pan the view                                                                |
| Spacebar (hold)             | Pan the view                                                                |
| Shift + middle mouse button | Draw a zoom selection                                                       |
| Ctrl + middle mouse button  | Zoom to clicked span or activity                                            |
| Scroll wheel                | Zoom in and out                                                             |
| Home                        | Scroll to top of activity  list                                             |
| Ctrl + Home                 | Zooms to fit current activities                                             |
| Shift + Home                | Jump to timestamp 0                                                         |
| X                           | Toggle display of all activity  labels                                      |
| C                           | Toggle compact display                                                      |
| G                           | Open an activity selector                                                   |
| T                           | Toggle displaying tooltips; only spans -> none -> both spans and activities |
| O                           | Toggle displaying STW and GC overlays                                       |
| S                           | Toggle displaying sampling-based timelines                                  |
| H                           | Open a heatmap showing P utilization                                        |
| Ctrl + Z                    | Undo navigations (works most of the time)                                   |

## Screenshots

Enjoy some of these screenshots.

[![](https://user-images.githubusercontent.com/39825/191167780-08e98f3a-fc2f-48a8-b5cd-c5e1e81eaef9.png)](https://user-images.githubusercontent.com/39825/191164505-0b348f1b-b4ad-4732-b2d5-f83bf1964012.png)

[![](https://user-images.githubusercontent.com/39825/191167794-1c3eb92a-f691-4dbe-8316-9614dfaeb723.png)](https://user-images.githubusercontent.com/39825/191164507-8725d7af-1aea-4463-9851-7e92d726d81a.png)

[![](https://user-images.githubusercontent.com/39825/191167814-3802e06d-14e9-4188-8a4c-8c6107744181.png)](https://user-images.githubusercontent.com/39825/191164684-aad03a07-ab61-4399-9b7e-670de05caad1.png)

[![](https://user-images.githubusercontent.com/39825/194723796-011d8fdf-72c5-4d36-a3e3-ba05c52631d3.png)](https://user-images.githubusercontent.com/39825/194723659-f14b620c-99f0-4a6c-a625-0dac9ba23f79.png)

[![](https://user-images.githubusercontent.com/39825/191167809-b0798d2f-ba98-4094-86ff-7cbf20c62667.png)](https://user-images.githubusercontent.com/39825/191164519-6a357e11-f67b-468e-a39e-05e900020ff4.png)

## Notes

No aspect of gotraceui is final yet, but do note that bright pink and bright yellow are debug colors and I never thought
they were a good idea. The rest of the color scheme is actually meant to be pleasant.

## Known issues

- [runtime/trace: time stamps out of order](https://github.com/golang/go/issues/16755)
- Timelines with millions of events can be a bit slow to render

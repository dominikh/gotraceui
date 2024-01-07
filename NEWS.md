# v0.4.0 (unreleased)

- Require Go 1.21 for building Gotraceui
- All tables now have resizable and sortable columns
- Added flame graphs
- Links to timelines and spans now default to opening information
- Links that open things and links that navigate use different colors (red and blue, respectively)
- Clicking on spans in GC and STW timelines no longer crashes
- Hovering table rows highlights them
- When hovering timestamp links, their targets will be indicated in the timelines view
- Spans have a new context menu item, to open their span information
- Switch to a tabbed user interface instead of using multiple windows
- Added a Goroutines tab that lists all goroutines in the trace
- Panels can be converted into tabs
- Added context menu to goroutine labels in timelines view
- No longer create empty GC or STW timelines
- Merged spans now display a detailed representation of the contained spans
- Fixed a rare crash at startup
- Fixed a rare crash for some traces
- Handle DPI changes while Gotraceui is running
- Display user-friendly strings instead of `<unknown>` for unknown timestamps
- Hovered and highlighted spans are now indicated with a gradient instead of the border color
- Span borders more accurately show if the span's beginning or end are outside the visible area
- Merged spans no longer stop at the screen boundaries
- Lowered peak memory usage
- "active" spans in goroutine timelines now have a label
- Improved rendering of graphs with many points


# v0.3.0 (2023-08-07)

- Added support for traces produced by Go 1.21
- Improved the formatting of durations in statistics
- Made minor improvements to visual appearance
- Replaced foldables with a tabbed interface
- Allow selecting and copying stack traces
- Added list of spans to goroutine panel
- Added parent goroutine to goroutine panel
- Added statistics to spans panel
- Added individual spans' stack traces in spans panel
- Added function panel
- Navigating to timestamps now places them at the configured axis origin
- Fixed ctrl+scroll zooming on Windows
- Fixed a crash when loading traces that start in the middle of a task
- Truncate long span labels instead of hiding them
- Added a button to select all instances of a user region
- Added histograms
- Stop-the-world spans now display the reason for the STW
- Made various performance improvements
- Drastically lowered memory usage for traces with a lot of deep call stacks
- Holding shift while scrolling vertically will cause horizontal scrolling, and vice versa
- The info panel of user regions now displays events that occurred on the goroutine during said regions
- Improved accuracy of computation of durations of merged spans
- Added more features to the context menu of span links
- Expensive computations, such as creating goroutine statistics, now run in the background and no longer prevent the UI from updating.


# v0.2.0 (2023-04-11)

- Added a button for copying statistics as CSV
- Added context menu to links to processors
- Added a list of spans to the span panel for merged spans
- Added display of span tags in span panel
- Added a dialog for highlighting spans based on their states
- Automatically turn off the memory graph's "auto-extents" feature when manually resetting the extents
- Syscall spans now include the syscall's name in the span label
- Toggling compact mode now keeps the timeline under the cursor at its position
- Disabling stack tracks or compact mode ensures the cursor stays on the current timeline
- Improved the formatting of durations in statistics
- Minor performance improvements
- Fixed the display of the ⌘ sign in shortcuts
- Fixed displaying context menus in additional windows
- Fixed crash when clicking on link to processor
- The checkboxes for filtering event lists now wrap to multiple lines if necessary
- Minor improvements to the manual


# v0.1.0 (2023-03-30)

- Initial release.

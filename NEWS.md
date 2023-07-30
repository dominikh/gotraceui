# v0.3.0 (unreleased)

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

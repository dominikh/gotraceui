package main

import (
	"fmt"
	"image"
	"math"
	"math/big"
	"time"

	"honnef.co/go/gotraceui/clip"
	"honnef.co/go/gotraceui/gesture"
	"honnef.co/go/gotraceui/layout"
	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/theme"
	"honnef.co/go/gotraceui/trace"
	"honnef.co/go/gotraceui/trace/ptrace"
	"honnef.co/go/gotraceui/widget"

	"gioui.org/font"
	"gioui.org/io/pointer"
	"gioui.org/op"
	"gioui.org/text"
)

type Task struct {
	// OPT(dh): avoid the pointer by using the task's SeqID instead
	*ptrace.Task
	// Goroutines that have at least one user region or log message that is part of this task
	//
	// OPT(dh): avoid the pointer by using the goroutine's SeqID instead
	Goroutines []*ptrace.Goroutine
	NumRegions int
	// The event IDs of user log messages associated with this task
	Logs []ptrace.EventID
}

// TODO(dh): separate tasks table from TasksComponent
type TasksComponent struct {
	Trace *Trace
	Tasks SortedItems[Task]

	mwin *theme.Window
	// Tasks that have extended information shown, indexed by the task's sequential ID.
	openTasks big.Int
	table     theme.Table
	clicks    mem.BucketSlice[ClickWithData]

	scrollState  theme.YScrollableListState
	tabbedStates map[int]*struct {
		state     *theme.TabbedState
		minHeight int
	}
	goroutineLists     map[int]*GoroutineList
	expandedAnimations map[int]time.Time

	nfTs     *NumberFormatter[trace.Timestamp]
	nfInt    *NumberFormatter[int]
	nfUint64 *NumberFormatter[uint64]
}

func NewTasksComponent(mwin *theme.Window, tr *Trace, tasks []Task) *TasksComponent {
	return &TasksComponent{
		Trace: tr,
		Tasks: NewSortedItems(NewSimpleItems(tasks)),
		mwin:  mwin,
		tabbedStates: map[int]*struct {
			state     *theme.TabbedState
			minHeight int
		}{},
		goroutineLists:     map[int]*GoroutineList{},
		expandedAnimations: map[int]time.Time{},

		nfTs:     NewNumberFormatter[trace.Timestamp](local),
		nfUint64: NewNumberFormatter[uint64](local),
		nfInt:    NewNumberFormatter[int](local),
	}
}

type ClickWithData struct {
	gesture.Click
	Link ObjectLink
}

// Title implements theme.Component.
func (*TasksComponent) Title() string {
	return "Tasks"
}

// Transition implements theme.Component.
func (*TasksComponent) Transition(state theme.ComponentState) {}

// WantsTransition implements theme.Component.
func (*TasksComponent) WantsTransition() theme.ComponentState { return theme.ComponentStateNone }

func (l *ClickWithData) Layout(gtx layout.Context, w layout.Widget) layout.Dimensions {
	return layout.Overlay(gtx, w, func(gtx layout.Context) layout.Dimensions {
		defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
		pointer.CursorPointer.Add(gtx.Ops)
		l.Click.Add(gtx.Ops)
		return layout.Dimensions{Size: gtx.Constraints.Min}
	})
}

func (cp *TasksComponent) sort() {
	switch cp.table.SortedBy {
	case 0: // ID
		cp.Tasks.Sort(func(a, b Task) int {
			return cmp(a.ID, b.ID, cp.table.SortOrder == theme.SortDescending)
		})
	case 1: // Name
		cp.Tasks.Sort(func(a, b Task) int {
			return cmp(a.Name(cp.Trace.Trace), b.Name(cp.Trace.Trace), cp.table.SortOrder == theme.SortDescending)
		})
	case 2: // Start
		cp.Tasks.Sort(func(a, b Task) int {
			var starta, startb trace.Timestamp = -1, -1
			if a.StartEvent != 0 {
				starta = cp.Trace.Event(a.StartEvent).Ts
			}
			if b.StartEvent != 0 {
				startb = cp.Trace.Event(b.StartEvent).Ts
			}
			return cmp(starta, startb, cp.table.SortOrder == theme.SortDescending)
		})
	case 3: // End
		cp.Tasks.Sort(func(a, b Task) int {
			var enda, endb trace.Timestamp = math.MaxInt64, math.MaxInt64
			if a.EndEvent != 0 {
				enda = cp.Trace.Event(a.EndEvent).Ts
			}
			if b.EndEvent != 0 {
				endb = cp.Trace.Event(b.EndEvent).Ts
			}
			return cmp(enda, endb, cp.table.SortOrder == theme.SortDescending)
		})
	case 4: // Duration
		cp.Tasks.Sort(func(a, b Task) int {
			var starta, startb trace.Timestamp = -1, -1
			traceEnd := cp.Trace.Events[len(cp.Trace.Events)-1].Ts
			// We use traceEnd + 1 instead of MaxInt64 so that durations still sort usefully even if one of
			// start or end is missing. For example, even if the end is unknown, one event happening before
			// the other will have a longer duration.
			var enda, endb trace.Timestamp = traceEnd + 1, traceEnd + 1
			if a.StartEvent != 0 {
				starta = cp.Trace.Event(a.StartEvent).Ts
			}
			if b.StartEvent != 0 {
				startb = cp.Trace.Event(b.StartEvent).Ts
			}
			if a.EndEvent != 0 {
				enda = cp.Trace.Event(a.EndEvent).Ts
			}
			if b.EndEvent != 0 {
				endb = cp.Trace.Event(b.EndEvent).Ts
			}
			var da, db time.Duration = -1, -1
			da = time.Duration(enda - starta)
			db = time.Duration(endb - startb)
			return cmp(da, db, cp.table.SortOrder == theme.SortDescending)
		})
	case 5: // NumG
		cp.Tasks.Sort(func(a, b Task) int {
			return cmp(len(a.Goroutines), len(b.Goroutines), cp.table.SortOrder == theme.SortDescending)
		})
	case 6: // NumRegions
		cp.Tasks.Sort(func(a, b Task) int {
			return cmp(a.NumRegions, b.NumRegions, cp.table.SortOrder == theme.SortDescending)
		})
	case 7: // NumLogs
		cp.Tasks.Sort(func(a, b Task) int {
			return cmp(len(a.Logs), len(b.Logs), cp.table.SortOrder == theme.SortDescending)
		})
	}
}

// Layout implements theme.Component.
func (cp *TasksComponent) Layout(win *theme.Window, gtx layout.Context) layout.Dimensions {
	const (
		cID         = 0
		cName       = 1
		cStart      = 2
		cEnd        = 3
		cDuration   = 4
		cNumG       = 5
		cNumRegions = 6
		cNumLogs    = 7
	)

	if cp.table.Columns == nil {
		cp.table.SetColumns(win, gtx, []theme.Column{
			{Name: "ID", Alignment: text.End, Clickable: true},
			{Name: "Name", Alignment: text.Start, Clickable: true},
			{Name: "Start", Alignment: text.End, Clickable: true},
			{Name: "End", Alignment: text.End, Clickable: true},
			{Name: "Duration", Alignment: text.End, Clickable: true},
			{Name: "# goroutines", Alignment: text.End, Clickable: true},
			{Name: "# regions", Alignment: text.End, Clickable: true},
			{Name: "# logs", Alignment: text.End, Clickable: true},
		})

		cp.table.SortedBy = 0
		cp.table.SortOrder = theme.SortAscending
	}

	for i, n := 0, cp.clicks.Len(); i < n; i++ {
		link := cp.clicks.Ptr(i)
		for _, ev := range link.Events(gtx.Queue) {
			switch d := link.Link.(type) {
			case *TaskObjectLink:
				if ev.Button == pointer.ButtonPrimary && ev.Type == gesture.TypeClick {
					var bit uint
					if cp.openTasks.Bit(d.Task.SeqID) == 0 {
						bit = 1
					}
					cp.openTasks.SetBit(&cp.openTasks, d.Task.SeqID, bit)
					cp.expandedAnimations[d.Task.SeqID] = gtx.Now
				}
			default:
				handleLinkClick(win, ev, d)
			}
		}
	}

	for _, glist := range cp.goroutineLists {
		for _, ev := range glist.Clicked() {
			handleLinkClick(win, ev.Event, ev.Span.ObjectLink)
		}
	}

	if _, ok := cp.table.SortByClickedColumn(); ok {
		cp.sort()
	}

	cp.clicks.Reset()

	return cp.table.Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
		return theme.YScrollableList(&cp.scrollState).Layout(win, gtx, func(win *theme.Window, gtx layout.Context, list *theme.RememberingList) layout.Dimensions {
			return layout.Flex{Axis: layout.Vertical}.Layout(gtx,
				layout.Rigid(func(gtx layout.Context) layout.Dimensions {
					return theme.TableHeaderRow(&cp.table).Layout(win, gtx)
				}),

				layout.Flexed(1, func(gtx layout.Context) layout.Dimensions {
					fg := widget.ColorTextMaterial(gtx, win.Theme.Palette.Foreground)
					linkFg := widget.ColorTextMaterial(gtx, win.Theme.Palette.Link)
					objectLinkFg := widget.ColorTextMaterial(gtx, win.Theme.Palette.NavigationLink)
					return list.Layout(gtx, cp.Tasks.Len(), func(gtx layout.Context, index int) layout.Dimensions {
						var (
							task    = cp.Tasks.AtPtr(index)
							rowDims layout.Dimensions
						)
						return layout.Rigids(gtx, layout.Vertical,
							func(gtx layout.Context) layout.Dimensions {
								rowDims = theme.TableSimpleRow(&cp.table).Layout(win, gtx, index, func(win *theme.Window, gtx layout.Context, row, col int) layout.Dimensions {
									var (
										left  = widget.Label{MaxLines: 1, Alignment: text.Start}
										right = widget.Label{MaxLines: 1, Alignment: text.End}
									)
									switch col {
									case cID:
										link := cp.clicks.Grow()
										link.Link = &TaskObjectLink{Task: task}
										return layout.Overlay(gtx,
											func(gtx layout.Context) layout.Dimensions {
												// We intentionally right-align the ID using the widget.Label alignment, because we
												// want the entire cell to be clickable.
												return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, cp.nfUint64.Format("%d", task.ID), linkFg)
											},

											func(gtx layout.Context) layout.Dimensions {
												defer clip.Rect{Max: gtx.Constraints.Min}.Push(gtx.Ops).Pop()
												pointer.CursorPointer.Add(gtx.Ops)
												link.Add(gtx.Ops)
												return layout.Dimensions{Size: gtx.Constraints.Min}
											},
										)
									case cName:
										if task.Stub() {
											return left.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, "<unknown>", fg)
										} else {
											name := task.Name(cp.Trace.Trace)
											return left.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, name, fg)
										}
									case cStart:
										if task.StartEvent == 0 {
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, "<unknown>", fg)
										} else {
											ts := cp.Trace.Event(task.StartEvent).Ts
											link := cp.clicks.Grow()
											link.Link = &TimestampObjectLink{Timestamp: ts}
											return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
												return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
													return left.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, formatTimestamp(cp.nfTs, ts), objectLinkFg)
												})
											})
										}
									case cEnd:
										if task.EndEvent == 0 {
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, "<unknown>", fg)
										} else {
											ts := cp.Trace.Event(task.EndEvent).Ts
											link := cp.clicks.Grow()
											link.Link = &TimestampObjectLink{Timestamp: ts}
											return layout.RightAligned(gtx, func(gtx layout.Context) layout.Dimensions {
												return link.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
													return left.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, formatTimestamp(cp.nfTs, ts), objectLinkFg)
												})
											})
										}
									case cDuration:
										traceEnd := cp.Trace.Events[len(cp.Trace.Events)-1].Ts
										if task.StartEvent == 0 && task.EndEvent == 0 {
											d := roundDuration(time.Duration(traceEnd))
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, fmt.Sprintf("≥ %s", d), fg)
										} else if task.StartEvent == 0 {
											d := roundDuration(time.Duration(cp.Trace.Event(task.EndEvent).Ts))
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, fmt.Sprintf("≥ %s", d), fg)
										} else if task.EndEvent == 0 {
											d := roundDuration(time.Duration(traceEnd - cp.Trace.Event(task.StartEvent).Ts))
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, fmt.Sprintf("≥ %s", d), fg)
										} else {
											d := roundDuration(time.Duration(cp.Trace.Event(task.EndEvent).Ts - cp.Trace.Event(task.StartEvent).Ts))
											return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, d.String(), fg)
										}
									case cNumG:
										return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, cp.nfInt.Format("%d", len(task.Goroutines)), fg)
									case cNumRegions:
										return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, cp.nfInt.Format("%d", task.NumRegions), fg)
									case cNumLogs:
										return right.Layout(gtx, win.Theme.Shaper, font.Font{}, 12, cp.nfInt.Format("%d", len(task.Logs)), fg)
									default:
										panic(col)
									}
								})
								return rowDims
							},

							func(gtx layout.Context) layout.Dimensions {
								taskOpen := cp.openTasks.Bit(task.SeqID) != 0
								anim, ok := cp.expandedAnimations[task.SeqID]
								const animateDuration = 500 * time.Millisecond

								if !taskOpen {
									if !ok {
										return layout.Dimensions{}
									} else if gtx.Now.Sub(anim) >= animateDuration {
										delete(cp.expandedAnimations, task.SeqID)
										return layout.Dimensions{}
									}
								}

								ratio := float64(1)
								if ok {
									if gtx.Now.Sub(anim) >= animateDuration {
										delete(cp.expandedAnimations, task.SeqID)
									} else {
										if taskOpen {
											ratio = easeOutQuart(float64(gtx.Now.Sub(anim)) / float64(animateDuration))
										} else {
											ratio = easeInQuart(float64(animateDuration-gtx.Now.Sub(anim)) / float64(animateDuration))
										}
										op.InvalidateOp{}.Add(gtx.Ops)
									}
								}
								gtx.Constraints.Max.X = rowDims.Size.X
								return theme.TableExpandedRow(&cp.table).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
									r := theme.Record(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
										return layout.PixelInset{Top: 15, Left: 15, Right: 15, Bottom: 15}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
											tabbedState, ok := cp.tabbedStates[task.SeqID]
											if !ok {
												tabbedState = &struct {
													state     *theme.TabbedState
													minHeight int
												}{
													state: &theme.TabbedState{},
												}
												cp.tabbedStates[task.SeqID] = tabbedState
											}
											glist, ok := cp.goroutineLists[task.SeqID]
											if !ok {
												glist = &GoroutineList{}
												cp.goroutineLists[task.SeqID] = glist
											}
											return widget.Bordered{Color: win.Theme.Palette.Border, Width: 1}.Layout(gtx, func(gtx layout.Context) layout.Dimensions {
												return theme.Tabbed(tabbedState.state, []string{"Goroutines", "Regions", "Logs"}).Layout(win, gtx, func(win *theme.Window, gtx layout.Context) layout.Dimensions {
													gtx.Constraints.Min = gtx.Constraints.Constrain(image.Pt(gtx.Constraints.Max.X, tabbedState.minHeight))
													gtx.Constraints.Max = gtx.Constraints.Constrain(image.Pt(gtx.Constraints.Max.X, 400))

													var dims layout.Dimensions
													switch tabbedState.state.Current {
													case 0:
														if glist.Goroutines.Items == nil {
															glist.SetGoroutines(win, gtx, task.Goroutines)
														}
														dims = glist.Layout(win, gtx)
													case 1:
													case 2:
													}

													// Ensure that the tabbed doesn't shrink in height when switching between tabs.
													if dims.Size.Y > tabbedState.minHeight {
														tabbedState.minHeight = dims.Size.Y
													}
													dims.Size = gtx.Constraints.Constrain(dims.Size)
													return dims
												})
											})
										})
									})
									size := image.Pt(r.Dimensions.Size.X, int(float64(r.Dimensions.Size.Y)*ratio))
									defer clip.Rect{Max: size}.Push(gtx.Ops).Pop()
									r.Layout(win, gtx)
									return layout.Dimensions{Size: size}
								})

							},
						)
					})
				}),
			)
		})
	})
}

// printTask is a debug helper that prints a textual representation of a task to stdout.
func printTask(tr *Trace, task *Task) {
	name := task.Name(tr.Trace)

	var (
		start = "unknown"
		end   = "unknown"
	)
	if task.StartEvent != 0 {
		start = formatTimestamp(nil, tr.Event(task.StartEvent).Ts)
	}
	if task.EndEvent != 0 {
		end = formatTimestamp(nil, tr.Event(task.EndEvent).Ts)
	}

	fmt.Printf("{ID = %d, Name = %q, Goroutines: %v, Logs: %v, NumRegions: %d, Start: %s, End: %s}\n",
		task.ID, name, task.Goroutines, task.Logs, task.NumRegions, start, end)
}

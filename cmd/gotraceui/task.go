package main

import (
	"fmt"

	"honnef.co/go/gotraceui/trace/ptrace"
)

type Task struct {
	// The sequential ID of the ptrace.Task
	ID int
	// The sequential IDs of goroutines that have at least one user region or log message that is part of this
	// task
	Goroutines []int
	NumRegions int
	// The event IDs of user log messages associated with this task
	Logs []ptrace.EventID
}

func (t *Task) PTask(tr *ptrace.Trace) *ptrace.Task {
	return tr.Tasks[t.ID]
}

// printTask is a debug helper that prints a textual representation of a task to stdout.
func printTask(tr *Trace, t *Task) {
	ptask := tr.Trace.Tasks[t.ID]
	name := ptask.Name(tr.Trace)

	var (
		start = "unknown"
		end   = "unknown"
	)
	if ptask.StartEvent != 0 {
		start = formatTimestamp(nil, tr.Event(ptask.StartEvent).Ts)
	}
	if ptask.EndEvent != 0 {
		end = formatTimestamp(nil, tr.Event(ptask.EndEvent).Ts)
	}

	fmt.Printf("{ID = %d, Name = %q, Goroutines: %v, Logs: %v, NumRegions: %d, Start: %s, End: %s}\n",
		ptask.ID, name, t.Goroutines, t.Logs, t.NumRegions, start, end)
}

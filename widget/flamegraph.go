package widget

import (
	"sort"
	"time"
)

type FlameGraph struct {
	Samples []FlamegraphFrame
}

type FlamegraphFrame struct {
	Parent   *FlamegraphFrame
	Name     string
	Duration time.Duration
	Children []FlamegraphFrame
}

type FlamegraphSample []FlamegraphFrame

func (fg *FlameGraph) AddSample(sample FlamegraphSample, root string) {
	if len(sample) == 0 {
		return
	}

	toplevel := FlamegraphFrame{
		Name:     root,
		Duration: sample[0].Duration,
	}

	cur := &toplevel
	for i := range sample {
		child := FlamegraphFrame{
			Name:     sample[i].Name,
			Duration: sample[i].Duration,
		}
		cur.Children = append(cur.Children, child)
		cur = &cur.Children[0]
	}

	fg.Samples = append(fg.Samples, toplevel)
}

func (fg *FlameGraph) Compute() {
	if len(fg.Samples) == 0 {
		fg.Samples = []FlamegraphFrame{
			{
				Name: "",
				// Technically this span has no samples, but that would result in a zero-width span.
				Duration: 1,
			},
		}
		return
	}

	var merge func(root []FlamegraphFrame) []FlamegraphFrame

	merge = func(slice []FlamegraphFrame) []FlamegraphFrame {
		if len(slice) == 0 {
			return nil
		}

		sort.Slice(slice, func(i, j int) bool {
			return slice[i].Name < slice[j].Name
		})
		for i := range slice[:len(slice)-1] {
			frame := &slice[i]
			if frame.Duration == 0 {
				continue
			}

			for j := i + 1; j < len(slice); j++ {
				next := &slice[j]
				if frame.Name != next.Name {
					break
				}
				frame.Children = append(frame.Children, next.Children...)
				frame.Duration += next.Duration
				next.Duration = 0
			}
		}

		compacted := slice[:0]
		for i := range slice {
			if slice[i].Duration > 0 {
				compacted = append(compacted, slice[i])
			}
		}
		slice = compacted

		for i := range slice {
			child := &slice[i]
			child.Children = merge(child.Children)
		}

		return slice
	}

	fg.Samples = merge(fg.Samples)

	// Populate parent pointers
	var dfs func(s, parent *FlamegraphFrame)
	dfs = func(s, parent *FlamegraphFrame) {
		s.Parent = parent
		for i := range s.Children {
			dfs(&s.Children[i], s)
		}
	}
	for i := range fg.Samples {
		dfs(&fg.Samples[i], nil)
	}
}

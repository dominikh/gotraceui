package widget

import (
	"sort"
	"time"
)

type FlameGraph struct {
	Samples []*FlamegraphFrame

	// top-level frames indexed by name
	samples map[string]*FlamegraphFrame
}

type FlamegraphFrame struct {
	Parent   *FlamegraphFrame
	Name     string
	Duration time.Duration
	Children []*FlamegraphFrame

	// immediate children indexed by name
	children map[string]*FlamegraphFrame
}

type FlamegraphSample []FlamegraphFrame

func (fg *FlameGraph) AddSample(sample FlamegraphSample, root string) {
	if len(sample) == 0 {
		return
	}

	toplevel, ok := fg.samples[root]
	if ok {
		toplevel.Duration += sample[0].Duration
	} else {
		toplevel = &FlamegraphFrame{
			Name:     root,
			Duration: sample[0].Duration,
			children: map[string]*FlamegraphFrame{},
		}
		if fg.samples == nil {
			fg.samples = map[string]*FlamegraphFrame{}
		}
		fg.samples[root] = toplevel
	}

	cur := toplevel
	for i := range sample {
		child, ok := cur.children[sample[i].Name]
		if ok {
			child.Duration += sample[i].Duration
		} else {
			child = &FlamegraphFrame{
				Parent:   cur,
				Name:     sample[i].Name,
				Duration: sample[i].Duration,
				children: map[string]*FlamegraphFrame{},
			}
			cur.children[sample[i].Name] = child
		}
		cur = child
	}
}

func (fg *FlameGraph) Compute() {
	if len(fg.samples) == 0 {
		fg.Samples = []*FlamegraphFrame{
			{
				Name: "",
				// Technically this span has no samples, but that would result in a zero-width span.
				Duration: 1,
			},
		}
		return
	}

	var doSort func(frame *FlamegraphFrame)
	doSort = func(frame *FlamegraphFrame) {
		var children []*FlamegraphFrame
		for _, child := range frame.children {
			doSort(child)
			child.children = nil
			children = append(children, child)
		}
		sort.Slice(children, func(i, j int) bool {
			return children[i].Name < children[j].Name
		})
		frame.Children = children
	}

	var samples []*FlamegraphFrame
	for _, frame := range fg.samples {
		doSort(frame)
		samples = append(samples, frame)
	}
	sort.Slice(samples, func(i, j int) bool {
		return samples[i].Name < samples[j].Name
	})
	fg.Samples = samples
	fg.samples = nil
}

package widget

import "sort"

type FlameGraph struct {
	Samples []FlamegraphFrame
}

type FlamegraphFrame struct {
	Name       string
	NumSamples float64
	Children   []FlamegraphFrame
}

type FlamegraphSample []FlamegraphFrame

func (fg *FlameGraph) AddSample(sample FlamegraphSample) {
	if len(sample) == 0 {
		return
	}

	toplevel := FlamegraphFrame{
		Name:       "",
		NumSamples: 1,
		Children: []FlamegraphFrame{
			{
				Name:       sample[0].Name,
				NumSamples: 1,
			},
		},
	}

	cur := &toplevel.Children[0]
	for i := range sample[1:] {
		child := FlamegraphFrame{
			Name:       sample[i+1].Name,
			NumSamples: 1,
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
				NumSamples: 1,
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
			if frame.NumSamples == 0 {
				continue
			}

			for j := i + 1; j < len(slice); j++ {
				next := &slice[j]
				if frame.Name != next.Name {
					break
				}
				frame.Children = append(frame.Children, next.Children...)
				frame.NumSamples += next.NumSamples
				next.NumSamples = 0
			}
		}

		compacted := slice[:0]
		for i := range slice {
			if slice[i].NumSamples > 0 {
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

	if len(fg.Samples) > 1 {
		panic("too many top-level samples")
	}
}

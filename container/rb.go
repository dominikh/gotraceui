package container

import (
	"fmt"
	"io"

	"golang.org/x/exp/constraints"
)

type Direction uint8
type Color bool

const (
	Left  Direction = 0
	Right Direction = 1
)

const (
	Black Color = false
	Red   Color = true
)

type Comparable[T any] interface {
	Compare(T) int
}

type RBTree[K Comparable[K], V any] struct {
	Root     *RBNode[K, V]
	NumNodes int

	Rotated func(node *RBNode[K, V])
}

type RBNode[K Comparable[K], V any] struct {
	Parent   *RBNode[K, V]
	Children [2]*RBNode[K, V]
	Key      K
	Value    V
	color    Color
}

func NewRBNode[K Comparable[K], V any](k K, v V) *RBNode[K, V] {
	return &RBNode[K, V]{
		Key:   k,
		Value: v,
	}
}

func (t *RBTree[K, V]) Search(k K) (node *RBNode[K, V], found bool, dir Direction) {
	if t.Root == nil {
		return nil, false, 0
	}

	x := t.Root
	for {
		switch k.Compare(x.Key) {
		case -1:
			dir = Left
		case 0:
			return x, true, 0
		case 1:
			dir = Right
		}

		child := x.Children[dir]
		if child == nil {
			return x, false, dir
		}
		x = child
	}
}

func (t *RBTree[K, V]) rotate(p *RBNode[K, V], dir Direction) *RBNode[K, V] {
	g := p.Parent
	s := p.Children[1-dir]
	c := s.Children[dir]
	p.Children[1-dir] = c
	if c != nil {
		c.Parent = p
	}
	s.Children[dir] = p
	p.Parent = s
	s.Parent = g
	if g != nil {
		var child Direction
		if p == g.Children[Right] {
			child = Right
		} else {
			child = Left
		}
		g.Children[child] = s
	} else {
		t.Root = s
	}

	if t.Rotated != nil {
		t.Rotated(p)
	}

	return s
}

func (t *RBTree[K, V]) Insert(k K, v V) *RBNode[K, V] {
	if t.Root == nil {
		t.NumNodes++
		n := NewRBNode(k, v)
		t.insert(n, nil, 0)
		return n
	}

	p, ok, dir := t.Search(k)
	if ok {
		p.Value = v
		return p
	} else {
		t.NumNodes++
		n := NewRBNode(k, v)
		t.insert(n, p, dir)
		return n
	}
}

func (t *RBTree[K, V]) insert(n *RBNode[K, V], p *RBNode[K, V], dir Direction) {
	var g *RBNode[K, V]
	var u *RBNode[K, V]

	n.color = Red
	n.Children[Left] = nil
	n.Children[Right] = nil
	n.Parent = p
	if p == nil {
		t.Root = n
		return
	}
	p.Children[dir] = n

	for {
		if p.color == Black {
			return
		}

		g = p.Parent
		if g == nil {
			p.color = Black
			return
		}

		dir = p.childDir()
		u = g.Children[1-dir]
		if u == nil || u.color == Black {
			if n == p.Children[1-dir] {
				t.rotate(p, dir)
				n = p
				p = g.Children[dir]
			}

			t.rotate(g, 1-dir)
			p.color = Black
			g.color = Red
			return
		}

		p.color = Black
		u.color = Black
		g.color = Red
		n = g

		p = n.Parent
		if p == nil {
			break
		}
	}
}

func (n *RBNode[K, V]) childDir() Direction {
	if n.Parent.Children[Right] == n {
		return Right
	} else {
		return Left
	}
}

func (n *RBNode[K, V]) Dot(w io.Writer, meta func(n *RBNode[K, V]) string) {
	p := func(s string) {
		w.Write([]byte(s))
		w.Write([]byte("\n"))
	}
	pf := func(f string, vs ...any) {
		fmt.Fprintf(w, f, vs...)
		w.Write([]byte("\n"))
	}

	var node func(n *RBNode[K, V])
	node = func(n *RBNode[K, V]) {
		if n == nil {
			return
		}

		var c string
		if n.color == Black {
			c = "black"
		} else {
			c = "red"
		}
		label := fmt.Sprintf("%v = %v", n.Key, n.Value)
		if meta != nil {
			label += "\n" + meta(n)
		}
		pf(`p%p [label="%s", color=%s];`, n, label, c)

		for i, child := range n.Children {
			if child == nil {
				pf("p%pc%d [label=nil, style=invis];", n, i)
				pf("p%p -> p%pc%d [style=invis];", n, n, i)
			} else {
				node(child)
				pf("p%p -> p%p;", n, child)
			}
		}

	}

	p("digraph {")
	p("graph [ordering=out];")

	node(n)

	p("}")
}

type Interval[T constraints.Ordered] struct {
	Min, Max T
}

type Value[T constraints.Ordered, V any] struct {
	MaxSubtree T
	Value      V
}

func (ival Interval[T]) Compare(oval Interval[T]) int {
	if ival.Min < oval.Min {
		return -1
	} else if ival.Min > oval.Min {
		return 1
	} else {
		if ival.Max < oval.Max {
			return -1
		} else if ival.Max > oval.Max {
			return 1
		} else {
			return 0
		}
	}
}

func (ival Interval[T]) Overlaps(oval Interval[T]) bool {
	ret := ival.Min <= oval.Max && ival.Max >= oval.Min

	return ret
}

func (ival Interval[T]) SupersetOf(oval Interval[T]) bool {
	return ival.Min <= oval.Min && ival.Max >= oval.Max
}

type IntervalTree[T constraints.Ordered, V any] struct {
	RBTree[Interval[T], Value[T, V]]
}

func (t *IntervalTree[T, V]) Insert(min, max T, value V) *RBNode[Interval[T], Value[T, V]] {
	n := t.RBTree.Insert(Interval[T]{min, max}, Value[T, V]{MaxSubtree: max, Value: value})
	t.updateAug(n)
	return n
}

func (t *IntervalTree[T, V]) updateAug(n *RBNode[Interval[T], Value[T, V]]) {
	if n == nil {
		return
	}

	max := n.Key.Max
	if c := n.Children[0]; c != nil && c.Value.MaxSubtree > max {
		max = c.Value.MaxSubtree
	}
	if c := n.Children[1]; c != nil && c.Value.MaxSubtree > max {
		max = c.Value.MaxSubtree
	}

	n.Value.MaxSubtree = max
	t.updateAug(n.Parent)
}

func (t *IntervalTree[T, V]) Find(
	min T,
	max T,
	out []*RBNode[Interval[T], Value[T, V]],
) []*RBNode[Interval[T], Value[T, V]] {
	return t.find(t.Root, min, max, out)
}

func (t *IntervalTree[T, V]) FindIter(
	min T,
	max T,
	cb func(node *RBNode[Interval[T], Value[T, V]]) bool,
) {
	t.findIter(t.Root, min, max, cb)
}

func (t *IntervalTree[T, V]) find(
	node *RBNode[Interval[T], Value[T, V]],
	min T,
	max T,
	out []*RBNode[Interval[T], Value[T, V]],
) []*RBNode[Interval[T], Value[T, V]] {
	if node == nil {
		return out
	}

	if min > node.Value.MaxSubtree {
		// This node and both subtrees are too small for our start point.
		return out
	}

	out = t.find(node.Children[Left], min, max, out)

	if node.Key.Overlaps(Interval[T]{min, max}) {
		out = append(out, node)
	}

	out = t.find(node.Children[Right], min, max, out)

	return out
}

func (t *IntervalTree[T, V]) findIter(
	node *RBNode[Interval[T], Value[T, V]],
	min T,
	max T,
	cb func(node *RBNode[Interval[T], Value[T, V]]) bool,
) bool {
	if node == nil {
		return false
	}

	if min > node.Value.MaxSubtree {
		// This node and both subtrees are too small for our start point.
		return false
	}

	if t.findIter(node.Children[Left], min, max, cb) {
		return true
	}

	if node.Key.Overlaps(Interval[T]{min, max}) {
		if cb(node) {
			return true
		}
	}

	if t.findIter(node.Children[Right], min, max, cb) {
		return true
	}

	return false
}

func NewIntervalTree[T constraints.Ordered, V any]() *IntervalTree[T, V] {
	t := &IntervalTree[T, V]{}
	t.Rotated = func(node *RBNode[Interval[T], Value[T, V]]) {
		t.updateAug(node)
	}
	return t
}

package main

import (
	"context"
	"fmt"
	"math"
	rtrace "runtime/trace"

	"honnef.co/go/gotraceui/mem"
	"honnef.co/go/gotraceui/trace/ptrace"

	exptrace "honnef.co/go/gotraceui/exptrace"
)

func computeStackTrack(track *Track, cancelled <-chan struct{}) Items[ptrace.Span] {
	defer rtrace.StartRegion(context.Background(), "main.computeStackTrack").End()

	tr := track.parent.cv.trace

	var cpuSamples []ptrace.EventID
	var itSpans []ptrace.Span
	switch c := track.parent.item.(type) {
	case *ptrace.Goroutine:
		cpuSamples = tr.CPUSamplesByG[c.ID]
		itSpans = c.Spans
	case *ptrace.Processor:
		cpuSamples = tr.CPUSamplesByP[c.ID]
		itSpans = c.Spans
	default:
		panic("unreachable")
	}

	it := &samplesAndSpansIterator{
		trace:      tr,
		spans:      itSpans,
		cpuSamples: cpuSamples,
	}

	spans := spanSlicePool.Get()[:0]
	metas := stackSpanMetaSlicePool.Get()[:0]

	var prevFn string
	i := 0
	for {
		if i%20000 == 0 && TryRecv(cancelled) {
			return nil
		}
		idSpan, ok := it.next(true)
		if !ok {
			break
		}

		ev := tr.Event(idSpan.id)
		pcs := tr.Stacks[ev.Stack()]
		state := ptrace.StateStack
		if idSpan.span == nil {
			state = ptrace.StateCPUSample
			pcs = trimSampleRuntimeFrames(pcs, tr)
		}

		if len(pcs) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			pcs = pcs[:64]
		}

		var end exptrace.Time
		if idSpan.span == nil {
			if endIDSpan, ok := it.next(false); ok {
				endEvID := endIDSpan.id
				end = tr.Event(endEvID).Time()
			} else if len(spans) > 0 {
				end = spans[len(spans)-1].End
			} else {
				end = tr.End()
			}
		} else {
			end = idSpan.span.End
		}

		if track.stackLevel >= len(pcs) {
			continue
		}
		pc := pcs[len(pcs)-1-track.stackLevel]

		if len(spans) != 0 {
			// This isn't the first span. Check if we should merge this stack into the previous span.
			prevEnd := last(spans).End
			prevState := last(spans).State
			fn := tr.PCs[pc].Func
			if prevEnd == ev.Time() && prevFn == fn && state == prevState {
				// This is a continuation of the previous span. Merging these can have massive memory usage savings,
				// which is why we do it here and not during display.
				//
				// TODO(dh): make this optional. Merging makes traces easier to read, but not merging makes the resolution of the
				// data more apparent.
				lastPtr(spans).End = end
				if state == ptrace.StateCPUSample {
					lastPtr(metas).num++
				}
			} else {
				// This is a new span
				spans = append(spans, ptrace.Span{
					Start:      ev.Time(),
					End:        end,
					StartEvent: idSpan.id,
					State:      state,
				})
				metas = append(metas, stackSpanMeta{
					pc:  pc,
					num: 1,
				})
				prevFn = tr.PCs[pc].Func
			}
		} else {
			// This is the first span
			spans = append(spans, ptrace.Span{
				Start:      ev.Time(),
				End:        end,
				StartEvent: idSpan.id,
				State:      state,
			})
			metas = append(metas, stackSpanMeta{
				pc:  pc,
				num: 1,
			})
			prevFn = tr.PCs[pc].Func
		}
	}

	return SimpleItems[ptrace.Span, stackSpanMeta]{
		items: spans,
		container: ItemContainer{
			Timeline: track.parent,
			Track:    track,
		},
		metas: metas,
	}
}

type samplesAndSpansIterator struct {
	trace      *Trace
	spans      []ptrace.Span
	cpuSamples []ptrace.EventID
	offSpans   int
	offSamples int
}

type idAndMaybeSpan struct {
	id   ptrace.EventID
	span *ptrace.Span
}

func (it *samplesAndSpansIterator) next(advance bool) (out idAndMaybeSpan, ok bool) {
	for {
		if it.offSpans == len(it.spans) && it.offSamples == len(it.cpuSamples) {
			return idAndMaybeSpan{}, false
		}

		out = idAndMaybeSpan{}
		ok = false

		if it.offSpans < len(it.spans) {
			span := &it.spans[it.offSpans]
			id := span.StartEvent
			if it.offSamples < len(it.cpuSamples) {
				oid := it.cpuSamples[it.offSamples]
				if id <= oid {
					if advance {
						it.offSpans++
					}
					out, ok = idAndMaybeSpan{span: span, id: id}, true
				} else {
					if advance {
						it.offSamples++
					}
					out, ok = idAndMaybeSpan{id: oid}, true
				}
			} else {
				if advance {
					it.offSpans++
				}
				out, ok = idAndMaybeSpan{span: span, id: id}, true
			}
		} else {
			id := it.cpuSamples[it.offSamples]
			if advance {
				it.offSamples++
			}
			out, ok = idAndMaybeSpan{id: id}, true
		}

		if !ok || !advance {
			return
		}

		ev := it.trace.Event(out.id)
		switch ev.Kind() {
		case exptrace.EventStateTransition:
			trans := ev.StateTransition()
			if trans.Resource.Kind == exptrace.ResourceGoroutine {
				from, to := trans.Goroutine()
				if from == exptrace.GoWaiting && to == exptrace.GoRunnable {
					// The stack is in the goroutine that unblocked this one
					continue
				} else if to == exptrace.GoRunning {
					// This event doesn't have a stack
					continue
				}
			}
			return
		case exptrace.EventStackSample:
			return
		default:
			panic(fmt.Sprintf("unhandled kind %s", ev.Kind()))
		}
	}
}

func trimSampleRuntimeFrames(stk []uint64, tr *Trace) []uint64 {
	// CPU samples include two runtime functions at the start of the stack trace that isn't present for stacks
	// collected by the runtime tracer.
	if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Func == "runtime.goexit" {
		stk = stk[:len(stk)-1]
	}
	if len(stk) > 0 && tr.PCs[stk[len(stk)-1]].Func == "runtime.main" {
		stk = stk[:len(stk)-1]
	}
	return stk
}

func addStackTracks[C *ptrace.Goroutine | *ptrace.Processor](tl *Timeline, c C, tr *Trace) {
	var cpuSamples []ptrace.EventID
	var spans []ptrace.Span
	switch c := any(c).(type) {
	case *ptrace.Goroutine:
		if c.Function != nil && c.Function.Func == "runtime.bgsweep" {
			// Go <=1.19 has a lot of spans in runtime.bgsweep, but the stacks are utterly uninteresting, containing only a
			// single frame. Save some memory by not creating stack tracks for this goroutine.
			return
		}
		cpuSamples = tr.CPUSamplesByG[c.ID]
		spans = c.Spans
	case *ptrace.Processor:
		cpuSamples = tr.CPUSamplesByP[c.ID]
		spans = c.Spans
	default:
		panic("unreachable")
	}

	var timeRanges []struct {
		start, end uint64
	}
	it := &samplesAndSpansIterator{
		trace:      tr,
		spans:      spans,
		cpuSamples: cpuSamples,
	}
	processEvent := func(idSpan idAndMaybeSpan) {
		ev := tr.Event(idSpan.id)
		pcs := tr.Stacks[ev.Stack()]

		if idSpan.span == nil {
			pcs = trimSampleRuntimeFrames(pcs, tr)
		}

		if len(pcs) > 64 {
			// Stacks of events have at most 128 frames (actually 126-127 due to a quirk in the runtime's
			// implementation; it captures 128 frames, but then discards the top frame to skip runtime.goexit, and
			// discards the next top frame if gid == 1 to skip runtime.main). Stacks of CPU samples, on the other hand,
			// have at most 64 frames. Always limit ourselves to 64 frames for a consistent result.
			pcs = pcs[:64]
		}

		timeRanges = mem.EnsureLen(timeRanges, len(pcs))

		var end exptrace.Time
		if idSpan.span == nil {
			if endIDAndMaybeSpan, ok := it.next(false); ok {
				endEvID := endIDAndMaybeSpan.id
				end = tr.Event(endEvID).Time()
			} else if len(spans) > 0 {
				end = spans[len(spans)-1].End
			} else {
				end = tr.End()
			}
		} else {
			end = idSpan.span.End
		}

		for i := range pcs {
			if uint64(end) > timeRanges[i].end {
				timeRanges[i].end = uint64(end)
			}
			if math.MaxUint64-uint64(ev.Time()) > timeRanges[i].start {
				timeRanges[i].start = math.MaxUint64 - uint64(ev.Time())
			}
		}
	}
	for {
		idSpan, ok := it.next(true)
		if !ok {
			break
		}
		processEvent(idSpan)
	}

	for i, tsr := range timeRanges {
		track := &Track{
			parent:     tl,
			kind:       TrackKindStack,
			Start:      exptrace.Time(math.MaxUint64 - tsr.start),
			End:        exptrace.Time(tsr.end),
			stackLevel: i,
			spanLabel:  stackSpanLabel,
			// OPT(dh): this allocates a closure for every stack track. it's not even stored in TrackWidget.
			spanTooltip: stackSpanTooltip(i + 1),
			spanColor: func(span *ptrace.Span, tr *Trace) colorIndex {
				if state := span.State; state == statePlaceholder {
					return colorStatePlaceholderStackSpan
				} else {
					return stateColors[state]
				}
			},
			compute: func(track *Track, cancelled <-chan struct{}) Items[ptrace.Span] {
				return computeStackTrack(track, cancelled)
			},
		}
		tl.tracks = append(tl.tracks, track)
	}
}

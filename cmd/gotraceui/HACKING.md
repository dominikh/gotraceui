This document contains notes on the tricky parts of processing runtime traces.

# Syscalls

When user code invokes a syscall, the runtime emits an EvGoSysCall event. If the syscall returns before sysmon finds
the P that's blocked in the syscall, we'll not see any other events. This is a fast path, avoiding a bunch of
scheduling, allowing the goroutine to resume immediately after the syscall returns, having retained its P and M.

If sysmon does find the P, it retakes the P from the M and the runtime emits GoSysBlock, followed by ProcStop. When a
blocking syscall returns, we get an EvGoSysExit event. The EvGoSysExit event gets emitted right before the G resumes
running. However, to maintain accurate timing, the event carries the original timestamp of when the syscall returned,
and the trace parser uses this to reorder the event. However, the fact that a G emits EvGoSysExit means that we'll
also see an EvProcStart before the EvGoSysExit, in preparation for the GoStart that will follow after EvGoSysExit.
This EvProcStart event matters to us because it might be for the same M that the syscall was running on. In that
case, we'll have to adjust some start and end times, so that the EvProcStart and EvGoSysExit don't overlap.
Concretely, the syscall span has to finish before the P span starts.

One implication of sysmon detecting blocked syscalls is that the duration between EvGoSysCall and EvGoSysBlock should
be attributed to the syscall, too. This is the time between starting the syscall and Go figuring out that it's
blocking. However, for Ps, it very much matters if the syscall hasn't been detected as blocking yet. If it hasn't,
the P isn't available for scheduling other Gs; this constitutes a form of latency. Because of that, we shouldn't
simply extend the "blocked syscall" span to include the EvGoSysCall, but it might make sense to display the interval
between the two as its own state.

Sysmon runs at most every 20 μs, but it will run considerably less often if it thinks there's nothing to do, up to 10
ms. In testing, even in busy programs (with GOMAXPROCS restricted to low values), sysmon sometimes decides to sleep
for 5 ms. A loop calling syscall.Nanosleep, sleeping 10 ms each time, will have EvGoSysBlock spans that last from ~10
ms to 5 ms, with the other 5 ms being the time between EvGoSysCall and EvGoSysBlock.

Another issue is that when a syscall returns fast enough we'll not be provided with information on how long the
syscall took. In the worst case, a fast syscall could've run for 10 ms, and to us it'll look like 10 ms of user code.

One special case is syscall.RawSyscall, which doesn't emit any tracing events whatsoever, and AFAIU isn't handled by
sysmon. That means these syscalls can block Ps and Ms, but we have no insight into it happening.

# Trace consistency

Tracing can start and stop in the middle of the program's lifetime, repeatedly. The following corner cases can arise
due to this (not an exhaustive list):

- The user stops tracing before ending a user region or user task. This means we can't rely on all create events
  having valid links to end events.

- The user stops tracing before all goroutines have ended. This happens virtually all the time for the runtime.main
  goroutine.

- User task IDs are not reset between traces.

- The user starts a region before starting tracing, and ends it after starting tracing. runtime/trace handles this
  for us; StartRegion returns a "no-op region" if tracing isn't enabled, ending which doesn't emit any event.

- The user starts tracing, starts a region, stops tracing, starts tracing, stops the region. If we only look at the
  second trace, then we see that the region ends, but we never see its creation.

- The same tracing states as in the previous two examples, but starting and ending tasks instead of regions. For
  tasks, we can see the end of unknown tasks in both cases, because there are no "no-op tasks".

- The user creates a task with a parent task that we didn't see, either because tracing wasn't enabled or because the
  parent was enabled in a previous trace.

# Garbage collection

- The only use of p=1000004 is for GCStart
- GCDone happens on other procs, probably whichever proc was running the background worker that determined that we're done
- A similar thing applies to GCSTWStart and GCSTWDone
- The second GCSTWDone can happen after GCDone

GC can get started by normal user goroutines, when they allocate and detect that the GC trigger condition has been
met. The lucky goroutine will be responsible for running the runtime code that stops the world, sets up write
barriers and so on. It returns to the user's code once the first STW phase is over and the concurrent mark phase has
started. Since multiple goroutines may allocate in parallel, multiple goroutines might detect the GC trigger and
attempt to start GC. All but one will block trying to acquire the semaphore, then not start GC once they unblock.

The GCStart and the first pair of STWStart + STWStop will be sent from that goroutine, but note that these events
form a timeline separate from normal goroutine events. In particular, seeing STWStart on the goroutine doesn't mean
that its previous GoStart has been superseded, and we'll not see another GoStart after we see STWStop.

The second STW phase and GCDone are sent from another goroutine, probably the background mark worker that determined
that we're done.

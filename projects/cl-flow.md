---
layout: page
id: cl-flow
title: cl-flow
parent: projects
---

## Quick overview

Library for asynchronous non-blocking concurrency in Common Lisp. Quite similar
to what green threads are for. In other buzzy words, it is a computation tree
building library mixed with data-flow model for non-blocking, asynchronous and
thread-safe execution.

[GitHub]({{ site.borodust.github }}/cl-flow)

## Documentation
[Getting Started]({% link projects/cl-flow/getting-started.md %})

## Operators

`->` (`flow:atomically`) operator marks atomic block of code that could be
dispatched concurrently - a basic unit of work in `cl-flow`.

`>>` (`flow:serially`) operator encloses forms for them to be executed serially,
but possibly in different threads, returning the value of the last atomic block
or flow.

`~>` (`flow:concurrently`) operator denotes forms that are going to be run in
parallel, returning list of block results in the same order they were specified.

`->>` (`flow:dynamically`) denotes block that generates new flow dynamically
during parent flow execution. In other words, injects (embeds) new dynamically
created flow into current one.

`%>` (`flow:asynchronously`) allows to split the flow and continue or interrupt
its execution at later time by calling `#'continue-flow` or `#'interrupt-flow`
functions from the lexical scope of the current block.

`o>` (`flow:repeatedly`) will short-circuit the flow specified inside the block
and execute it repeatedly in loop until condition returns `nil`. Result from the
last iteration will be passed to the next block.

Code is fully asynchronous and thread-safe with no blocking required (see Memory
Model note below).

Results of previously executed block (denoted by `->`) "flows" into a next code
block and is bound as argument to this next block.


## Compatibility
For thread-safety `cl-flow` library uses lock-free atomic functions not defined
in CL standard and, unfortunately, not widely supported by implementations
either. This said, `cl-flow` is actually tested and known to work in:

* SBCL x86_64
* CCL x86_64

Not tested for, but exptected to work in:

* Lispworks 64-bit

Other implementations should generally work too - only slowly - `cl-flow`
fallbacks to mutexes for synchronization on unsupported platforms.


## Notes
- There's no clearly defined memory model for CL, so there might be issues with cached
  thread-local non-volatile variables/memory.

---
layout: page
title: Getting Started with cl-flow
project: cl-flow
---


* [Setup](#setup)
* [First steps](#first-steps)
* [Concurrency](#concurrency)
* [Shorthand syntax](#shorthand-syntax)
* [Invariants](#invariants)


## Setup

For this library to work, one would need to implement proper flow dispatcher, but, hopefully,
one already exists: [simple-flow-dispatcher](https://github.com/borodust/simple-flow-dispatcher).

Download it into directory quicklisp would be able
to [find it and its dependencies](https://www.quicklisp.org/beta/faq.html#local-project)
([`cl-muth`](https://github.com/borodust/cl-muth)) in. Now, let's load this dispatcher and
`cl-flow` itself.

```common_lisp
(ql:quickload '(:simple-flow-dispatcher :cl-flow))
```

To assist with our journey, let's also import `cl-flow` symbols and define a utility method and
variables to run our flow:

```common_lisp
(use-package :cl-flow)

(defvar *output* *standard-output*)

(defun print-error (e)
  (format *output* "~A" e))

(defvar *dispatcher* (simple-flow-dispatcher:make-simple-dispatcher
                      :threads 4
                      :error-handler #'print-error))

(defun run-flow (flow)
  (run *dispatcher* flow))
```

## First steps

```common_lisp
(run-flow (flow:atomically :default ()
            (print "Hello, flowing World!" *output*)))
```

Looks like a lot is happenning here and, guess what, it is indeed! We just printed a string in
one of the four threads of the `*dispatcher*` asynchronously! To confirm, just add a `#'sleep` call
and you will see that `#'run-flow` returns immediately and string is actually printed a bit later.

```common_lisp
(run-flow (flow:atomically :default ()
            (sleep 1)
            (print "Hello, flowing World!" *output*)))
```

Let's enhance this example with more feaures `cl-flow` provides for a quick overview of the
syntax.


```common_lisp
(run-flow (flow:serially
            (flow:atomically :default ()
              "Hello, flowing World!")
            (flow:atomically :default (argument)
              (print argument *output*))))
```

`(flow:atomically :default ())` form we are going to call a flow block. `:default` value here denotes an
invariant. Blocks with same (by `#'EQ`) invariants are never executed concurrently (in parallel)
for `simple-flow-dispatcher`. Second form in `flow:atomically` block (`()` or `(argument)` here) declares an
argument that could be passed to the flow block from a previos block. And starting from third
form and henceforth is the body of the block that actually does a work we need.

`flow:serially` lists flow blocks that must be executed serially - one after another.

Here, in this example, we specified two atomic blocks to be executed serially with passing a
result of the first block into the second one and printing this result.


## Concurrency
But dispatching serially is no fun. We can do that w/o this crappy overloaded syntax. Let's do
some multithreading!

```common_lisp
(run-flow (flow:serially
            (flow:concurrently
              (flow:atomically :p ()
                "Hello")
              (flow:atomically :q ()
                "parallel")
              (flow:atomically :s ()
                "World!"))
            (flow:atomically :default (result)
              (destructuring-bind (a b c) result
                (format *output* "~A, ~A ~A" a b c)))))
```

Wows! You didn't notice, but we just run three blocks of code in parallel, but also gathered the
results in predictable order! Not unlike how semaphores work only w/o any blocking. By the way,
`flow:concurrently` operator list blocks, that are going to be run in parallel.

Basically, the code above means we scheduled a concurrent block (`flow:concurrently`) to run serially with next
atomic block (`(flow:atomically :default ... )`). In concurrent block, we specified 3 atomic blocks to run in
parallel. Results of those blocks are then gathered into a list and passed to the next block
after `flow:concurrently` one - see `result` argument.

To make sure we indeed ran those blocks in parallel, let's change example a bit:

```common_lisp
(run-flow (flow:serially
            (flow:concurrently
              (flow:atomically :p ()
                (sleep 1)
                (print "Processing hello" *output*)
                "Hello")
              (flow:atomically :q ()
                (sleep 1/2)
                (print "Processing parallel" *output*)
                "parallel")
              (flow:atomically :s ()
                (sleep 1/4)
                (print "Processing world" *output*)
                "World!"))
            (flow:atomically :default (result)
              (destructuring-bind (a b c) result
                (format *output* "~%~A, ~A ~A" a b c)))))
```

No way! We did indeed. And the order of results is preserved. How awesome is that? Yeah, total
crap if one would need to write this heavy syntax trees all the time, I agree. Let's make it a
bit more maintainable.

First, let's extract flows into a couple of functions:

```common_lisp
(defun string-constructing-flow ()
  (flow:concurrently
    (flow:atomically :p ()
      "Hello")
    (flow:atomically :q ()
      "parallel")
    (flow:atomically :s ()
      "World!")))

(defun printing-flow ()
  (flow:atomically :default ((a b c))
    (format *output* "~%~A, ~A ~A" a b c)))
```

And let's run those:

```common_lisp
(run-flow (flow:serially
            (string-constructing-flow)
            (printing-flow)))
```

Woohoo, it worked!

You might have noticed, that flow block in `#'printing-flow` looks a bit different. We specified
`((a b c))` as arguments instead of just `(result)`. Yes! If you wish, you can put destructuring
lambda list as a block argument and `cl-flow` will destructure incoming value for you.


## Shorthand syntax
For this special DSL for building computation trees I personally prefer shorthand syntax
available in `cl-flow`. `->` is a synonym for `flow:atomically`, `flow:serially` can be replaced
with `>>` and so on. We can rewrite one of our examples as such:

```common_lisp
(run-flow (>> (~> (-> :p () "Hello")
                  (-> :q () "parallel")
                  (-> :s () "World!"))
              (-> :default ((a b c))
                (format *output* "~%~A, ~A ~A" a b c))))
```

## Invariants
As already mentioned, they exist to guard atomic blocks from concurrent execution where needed.
One can imagine flow block to be a critical section that is guarded by mutex/lock, only there's
no blocking involved - flow execution is purely non-blocking.

For the sake of experiment, let's define some long running non-thread-safe incrementing function
and required state:

```common_lisp
(defparameter *value* 0)

(defun increment-global-variable ()
  (let ((value *value*))
    (sleep 0.1)
    (setf *value* (1+ value))))
```

And a couple of flows that call this global-state-changing function:

```common_lisp
(defun fully-concurrent-flow ()
  (~> (-> :one ()
        (increment-global-variable))
      (-> :two ()
        (increment-global-variable))
      (-> :three ()
        (increment-global-variable))))


(defun non-concurrent-flow ()
  (~> (-> :one ()
        (increment-global-variable))
      (-> :one ()
        (increment-global-variable))
      (-> :one ()
        (increment-global-variable))))
```

Now, you see those are different only by how invariants are used. `#'fully-concurrent-flow` has
three invariants used: `:one`, `:two` and `:three`, while `#'non-concurrent-flow` is using
`:one` only. Let's see what would happen when we run those!


```common_lisp
(setf *value* 0)
(run-flow (fully-concurrent-flow))
```

Wait a second (or 0.1 of a second to be somewhat precise) and check the value of
`*value*`... `1`. Huh? What happened is that all three blocks started executing concurrently
running `#'increment-global-variable` all at once. So at the same time in different threads
`#'increment-global-variable` cached 0 value of `*value*` in `value` variable and then, after
0.1 second, tried to increment it, all setting `*value*` to 1. That behavior is crappy and
called a concurrent race: 3 same-time function invocations fought for one resource - `*value*`,
and failed to communicate, incorrectly updating the state.

What will happen with another flow we defined? Let's find out.

```common_lisp
(setf *value* 0)
(run-flow (non-concurrent-flow))
```

Hold on a moment (yes, 0.3 of a second, you already guessed it right) and recheck what we have
in `*value*`: `3`. Woah! What happened now? We started executing `#'increment-global-variable`
three times concurrently as in our previous attempt, but dispatcher figured out they cannot be
run as such by checking their invariant, which is `:one` for all of the blocks, so it scheduled
them to run one after another - serially that is, so global state was correctly updated each
time!

In some way, this flow execution was similar to what `flow:serially` does, only the order of
block execution is not guaranteed.

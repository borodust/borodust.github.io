---
layout: page
title: Getting Started with cl-flow
project: cl-flow
---

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

(defvar *dispatcher* (simple-flow-dispatcher:make-simple-dispatcher :threads 4))

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

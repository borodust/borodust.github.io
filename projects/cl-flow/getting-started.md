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
(run-flow (-> :default ()
            (print "Hello, flowing World!" *output*)))
```

Looks like a lot is happenning here and, guess what, it is indeed! We just printed a string in
one of the four threads of the `*dispatcher*` asynchronously! To confirm, just add a `#'sleep` call
and you will see that `#'run-flow` returns immediately and string is actually printed a bit later.

```common_lisp
(run-flow (-> :default ()
            (sleep 1)
            (print "Hello, flowing World!" *output*)))
```

Let's enhance this example with more feaures `cl-flow` provides for a quick overview of the
syntax.


```common_lisp
(run-flow (>> (-> :default ()
                "Hello, flowing World!")
              (-> :default (argument)
                (print argument *output*))))
```

`(-> :default ())` form we are going to call a flow block. `:default` value here denotes an
invariant. Blocks with same (by `#'EQ`) invariants are never executed concurrently (in parallel)
for `simple-flow-dispatcher`. Second form in `->` block (`()` or `(argument)` here) declares an
argument that could be passed to the flow block from a previos block. And starting from third
form and henceforth is the body of the block that actually does a work we need.

`>>` lists flow blocks that must be executed serially - one after another.

Here, in this example, we specified two atomic blocks to be executed serially with passing a
result of the first block into the second one and printing this result.


## Concurrency
But dispatching serially is no fun. We can do that w/o this crappy overloaded syntax. Let's do
some multithreading!

```common_lisp
(run-flow (>> (~> (-> :p ()
                    "Hello")
                  (-> :q ()
                    "parallel")
                  (-> :s ()
                    "World!"))
              (-> :default (result)
                (destructuring-bind (a b c) result
                  (format *output* "~A, ~A ~A" a b c)))))
```

Wows! You didn't notice, but we just run three blocks of code in parallel, but also gathered the
results in predictable order! Not unlike how semaphores work only w/o any blocking. By the way,
`~>` operator list blocks, that are going to be run in parallel.

Basically, the code above means we scheduled a concurrent block (`~>`) to run serially with next
atomic block (`(-> :default ... )`). In concurrent block, we specified 3 atomic blocks to run in
parallel. Results of those blocks are then gathered into a list and passed to the next block
after `~>` one - see `result` argument.

To make sure we indeed ran those blocks in parallel, let's change example a bit:

```common_lisp
(run-flow (>> (~> (-> :p ()
                    (sleep 1)
                    (print "Processing hello" *output*)
                    "Hello")
                  (-> :q ()
                    (sleep 1/2)
                    (print "Processing parallel" *output*)
                    "parallel")
                  (-> :s ()
                    (sleep 1/4)
                    (print "Processing world" *output*)
                    "World!"))
              (-> :default (result)
                (destructuring-bind (a b c) result
                  (format *output* "~%~A, ~A ~A" a b c)))))
```

No way! We did indeed. And the order of results is preserved. How awesome is that? Yeah, total
crap if one would need to write this heavy syntax trees all the time, I agree. Let's make it a
bit more maintainable.

First, let's extract a couple of flows into functions:
```common_lisp
(defun string-constructing-flow ()
  (~> (-> :p ()
        "Hello")
      (-> :q ()
        "parallel")
      (-> :s ()
        "World!")))

(defun printing-flow ()
  (-> :default ((a b c))
    (format *output* "~%~A, ~A ~A" a b c)))
```

And let's run those:

```common_lisp
(run-flow (>> (string-constructing-flow)
              (printing-flow)))
```

Woohoo, it worked!

You might have noticed, that flow block in `#'printing-flow` looks a bit different. We specified
`((a b c))` as arguments instead of just `(result)`. Yes! If you wish, you can put destructuring
lambda list as a block argument and `cl-flow` will destructure incoming value for you.

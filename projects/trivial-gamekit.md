---
layout: page
id: trivial-gamekit
title: trivial-gamekit
parent: projects
---


## Purpose

This library is intended for users who wish to start with game development in Common Lisp as
soon as possible without much configuration hassle.


## Source code

`trivial-gamekit` sources can be found in GitHub
[repository](https://github.com/borodust/trivial-gamekit).


## Requirements

* [Quicklisp](https://www.quicklisp.org)
* OpenGL 3.3+
* x86_64 Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL


## Documentation

* [Getting Started]({% link projects/trivial-gamekit/getting-started.md %})
* [User Manual]({% link projects/trivial-gamekit/manual.md %})
* [Symbol Index]({% link projects/trivial-gamekit/symbol-index.md %})


## Installation and loading

```common_lisp
;; add cl-bodge distribution into quicklisp
;; you need to do this only once per quicklisp installation
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

(ql:quickload :trivial-gamekit)
```


## Example

```common_lisp
(gamekit:defgame example () ())

(defmethod gamekit:draw ((this example))
  (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 240.0 240.0)))

(gamekit:start 'example)
```


## Help

Things break and `trivial-gamekit` is no exception. Feel free to fire an
[issue](https://github.com/borodust/trivial-gamekit/issues) when that happens and
we will try to figure out a solution together!

Also you can find me and awesome `#lispgames` community chatting on `#lispgames` channel at
`irc://chat.freenode.net`.

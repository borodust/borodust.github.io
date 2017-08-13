---
layout: page
id: trivial-gamekit
title: trivial-gamekit
parent: projects
---


## Purpose

This library is intended for users who wish to start with game development in Common Lisp as
soon as possible without much configuration hassle.


## Requirements

* [Quicklisp](https://www.quicklisp.org)
* OpenGL 3.3+
* 64-bit (x86_64) Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL


## Documentation

[Getting Started]({% link projects/trivial-gamekit/getting-started.md %})


## Installation and loading

```common_lisp
;; add cl-bodge distribution into quicklisp
;; you need to do this only once per quicklisp installation
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

(ql:quickload :trivial-gamekit)
```


## Example

```common_lisp
(defclass example (gamekit:gamekit-system) ())

(defmethod gamekit:draw ((this example))
  (gamekit:print-text "Hello, Gamedev!" 240.0 240.0))

(gamekit:start 'example)
```


## Help

Unfortunately, technology behind `trivial-gamekit` is quite fragile, unstable and highly
dependent on the target OS configuration. Things break. Feel free to contact me when that
happens with `trivial-gamekit` system and we will try to figure out something together!

You can find me and awesome `#lispgames` community chatting on `#lispgames` channel at
`irc://chat.freenode.net`.

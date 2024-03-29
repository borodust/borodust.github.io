---
layout: page
id: trivial-gamekit
title: trivial-gamekit
parent: projects
---


## Purpose

This framework is intended for users who wish to start with game development in
Common Lisp as soon as possible without much configuration hassle.

With `trivial-gamekit` you would be able to make simple 2D games: draw basic
geometric forms, images and text, play sounds and listen to mouse, keyboard and
gamepad input.


## Source code

`trivial-gamekit` sources can be found at GitHub
[repository](https://github.com/borodust/trivial-gamekit).


## Requirements

* [Quicklisp](https://www.quicklisp.org)
* OpenGL 2.1 or 3.3+
* x86_64 Windows, GNU/Linux or macOS
* x86_64 SBCL or CCL


## Installation and loading

```common-lisp
;; add cl-bodge distribution into quicklisp
;; you need to do this only once per quicklisp installation
(ql-dist:install-dist "http://bodge.borodust.org/dist/org.borodust.bodge.txt")

(ql:quickload :trivial-gamekit)
```

## Documentation

* [Getting Started]({% link projects/trivial-gamekit/getting-started.md %})
* [User Manual]({% link projects/trivial-gamekit/manual.md %})
* [Advanced Features]({% link projects/trivial-gamekit/advanced.md %})


## Example

```common-lisp
(gamekit:defgame example () ())

(defmethod gamekit:draw ((this example))
  (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 240.0 240.0)))

(gamekit:start 'example)
```

## Projects

[hello-gamekit](https://github.com/borodust/hello-gamekit)
: `trivial-gamekit` example code from [Getting Started]({% link
  projects/trivial-gamekit/getting-started.md %}) guide

[Nature of Code in Common Lisp](https://github.com/mark-gerarts/nature-of-code) by Mark Gerarts
: Exercises and examples from [Nature of Code](https://natureofcode.com/) book

[NOTALONE](https://github.com/borodust/notalone)
: 2D shooter game written for [Lisp Game Jam 2017 (Hard
  Mode)](https://itch.io/jam/lisp-game-jam-2017-hard-mode/rate/186345)

[Ball Z: Second Dimension](https://github.com/borodust/ball-z-2d)
: 2D physics-based puzzle developed for [Lisp Game Jam 2018](https://itch.io/jam/lisp-game-jam-2018/rate/252161)

[NOTALONE: AGAIN](https://borodust.itch.io/notalone-again)
: 2D shooter written for [Lisp Game Jam 2019](https://itch.io/jam/lisp-game-jam-2019/rate/415347)

[Minimal Danmaku Simulator](https://github.com/RyanBurnside/mds) by Ryan ["Pixel_Outlaw"](https://github.com/RyanBurnside) Burnside
: [Lisp Game Jam 2018](https://itch.io/jam/lisp-game-jam-2018/rate/251765) entry in Shoot'em Up genre

[Pong Fight](https://github.com/eliasfeijo/cl-pong-game) by [Elias Feijo](https://github.com/eliasfeijo)
: A simple Pong game made for [Lisp Game Jam 2019](https://itch.io/jam/lisp-game-jam-2019/rate/413733) entry

[Decent Magician Of the Moldy Dungeon](https://codeberg.org/artchad/dmomd/src/commit/0a04f2c4d6592cfa43d9498eb23757d6ad4da963) by [artchad (aka decent-username)](https://codeberg.org/artchad) and [moldybits](https://gitlab.com/moldybits)
: A rogue-like RPG with turn based movement and combat: [Lisp Game Jam
  2019](https://itch.io/jam/lisp-game-jam-2019/rate/415397) entry



## Help

Things break and `trivial-gamekit` is no exception. Feel free to fire an
[issue](https://github.com/borodust/trivial-gamekit/issues) when that happens
and we will try to figure out a solution together!

Also you can find me and awesome `#lispgames` community chatting in `#lispgames`
channel at `irc://libera.chat:6697`.

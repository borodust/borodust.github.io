---
layout: page
title: User Manual
project: trivial-gamekit
---

{% comment %}

Template for function references:
[`#'`](#gamekit-)

{% endcomment %}

## Contents

* [Overview](#overview)
* [Defining a game](#defining-a-game)
* [Math](#math)
* [Locating resources](#locating-resources)
* [Drawing](#drawing)
* [Playing an audio](#playing-an-audio)
* [Listening to input](#listening-to-input)
* [Building a distributable](#building-a-distributable)


## Overview

`trivial-gamekit` is a very simple toolkit for making games in Common Lisp. Its goal is to be as
simple as possible while allowing a user to make a complete game s/he can distribute.

It manages system resources for you by creating and initializing a window, abstracting away
keyboard and mouse input and any other system-dependent configuration. `trivial-gamekit` also
spawns a default game loop for you allowing to hook into it for drawing and other per frame
activities via certain methods described below.

It strives to fully support Common Lisp's interactive development approach where you change your
application on the fly and allowed to gracefully recover from programming errors via restarts
without crashing or rerunning an application.

## Defining a game

`gamekit`'s central idea is a game object you define via [`defgame`](#gamekit-defgame) macro. It
contains all the state `gamekit` needs to run your game. It is instantiated by
[`#'start`](#gamekit-start) which also bootstraps the whole application and opens a window, runs
your game logic, rendering, etc. Only single game object can be run at a time, so subsequently
calling [`#'start`](#gamekit-start) for the same or different game objects is disallowed - you
need to stop game execution. To stop a running game you would need to call
[`#'stop`](#gamekit-stop).

[`#'start`](#gamekit-start) also invokes a default game loop. To hook into it you can use
[`#'act`](#'gamekit-act) and [`#'draw`](#gamekit-draw) methods. [`#'act`](#'gamekit-act) is used
for any activity you need to do per frame except drawing where [`#'draw`](#gamekit-draw) should
be used exclusively for drawing/rendering.

#### Related symbols

{% include_relative ref/defining-a-game.md %}

## Math

For moving objects around, defining their place on a canvas and giving them a color we need a
bit of a math applied. `gamekit` points and positions are represented as two-dimensional vectors
created with [`#'vec2`](#gamekit-vec2). Colors consist of four elements: red, green, blue and
alpha, so they are represented by four-dimensional vecors created via [`#'vec4`](#gamekit-vec4).

Some vector operations are exported: [`#'mult`](#gamekit-) for element-wise multiplication,
[`#'add`](#gamekit-add) for addition, [`#'subt`](#gamekit-subt) for subtraction and
[`#'div`](#gamekit-div) for element-wise division.

Element accessor methods for setting or getting values out of vectors are also exported:
[`#'x`](#gamekit-x) to access first element, [`#'y`](#gamekit-y) for a second,
[`#'z`](#gamekit-z) - third and [`#'w`](#gamekit-w) to access fourth.

#### Related symbols

{% include_relative ref/math.md %}

## Locating resources

Resources are very important for games. Textures, models, sounds, fonts, animations, tiles - you
name it. `gamekit` has several routines prepared for you to ease the handling of those.

First of all, we need to tell `gamekit` where to find resources. `gamekit` resources are
associated with Common Lisp packages. Any package can have associated filesystem directory to
find resources in. Call [`#'register-resource-package`](#gamekit-register-resource-package) as a
toplevel form to bind a directory to a package. Absolute paths required here. Below I'll try to
explain why we need this resource-directory association.

Before diving deeper, lets see how resources are defined. You tell `gamekit` what resources to
load and use via [`#'define-image`](#gamekit-define-image),
[`#'define-sound`](#gamekit-define-sound) or [`#'define-font`](#gamekit-define-font) macros.
First argument is a symbol that would become an identificator for the resource and the second
argument is a relative path to the resource.

Now, when `gamekit` knows absolute path to root and relative paths to resouces, it can locate
and prepare them to use in a game. To find absolute path to a resource `gamekit` looks into its
id and extracts the package (resource identificators must be symbols), then searches for
registered package-directory association and extracts absolute path for the package from there
and finally `gamekit` combines absolute path of a package with resource relative path and can
totally locate it.

This seemingly confusing mechanism was implemented for several games to coexist in one lisp
image. Unless you use keywords or common packages like `:cl-user`, resources are going to be
uniquely identified and each game will load its own resources correctly. Another usage scenario is
an `asdf` system with just a resources and their definitions for sharing between fellow game
developers.

Resources are used by functions that requires them to operate, like
[`#'draw-image`](#gamekit-draw-image), [`#'make-font`](#gamekit-make-font),
[`#'play-sound`](#gamekit-play-sound) and others.

When [`defgame`](#gamekit-defgame) option `:prepare-resources` is set to `nil`, you would need
to request resources manually by calling [`#'prepare-resources`](#gamekit-prepare-resources)
with resource ids you need. Preparation can take a while, so
[`#'prepare-resources`](#gamekit-prepare-resources) asynchonously requests resources and returns
immediately for you to be notified later with [`#'notice-resources`](#gamekit-notice-resources).
This is useful, when you don't want to wait until all assets are loaded and start rendering some
loading or start screen right away and continue with game after all resources are ready.

#### Related symbols

{% include_relative ref/locating-resources.md %}

## Drawing

`gamekit` provides simple to use but versatile drawing API. If you know how HTML5 canvas API is
organized you will find `gamekit`'s one quite similar. Several functions with names starting
with `draw-` exists to help you draw primitives like lines, rectangles, ellipses, arcs, polygons
or even bezier curves. Images can be drawn via [`#'draw-image`](#gamekit-draw-image) and text
with customizable font can be put onto canvas via [`#'draw-text`](#gamekit-draw-text).

Canvas transformation operations are supported too. You can scale, translate, rotate a canvas
with [`#'scale-canvas`](#gamekit-scale-canvas),
[`#'translate-canvas`](#gamekit-translate-canvas) and
[`#'rotate-canvas`](#gamekit-rotate-canvas) accordingly. If you need to keep canvas state for
nested drawing operations you will appreciate presence of
[`#'with-pushed-canvas`](#gamekit-with-pushed-canvas) macro, that keeps canvas state to the
dynamic extent of its body. This means that upon returning from the macro canvas transformation
state will be returned to its original state before the macro and all transformation operations
inside its body were canceled out.

#### Related symbols

{% include_relative ref/drawing.md %}

## Playing an audio

Audio can substantially boost game atmosphere, and `gamekit` is ready to serve you well in this
regard too. [`#'play-sound`](#gamekit-play-sound) will help with getting sounds to reach your
users ears. On the other hand, [`#'stop-sound`](#gamekit-stop-sound) can be used to stop this
process.

#### Related symbols

{% include_relative ref/playing-an-audio.md %}

## Listening to input

There's no game without some sort of interaction with a player. `gamekit` allows you to grab
keyboard and mouse input to listen for player actions. You can pass a callback to
[`#'bind-button`](#gamekit-bind-button) which will be called upon pressing or releasing
keyboard/mouse buttons. Callback passed to [`#'bind-cursor`](#gamekit-bind-cursor) is going to
be invoked when user moves a mouse. Callbacks provided are not stacked together, meaning if you
try to bind multiple callbacks to the same button only last callback is actually going to be
used. Same goes for cursor input.

#### Related symbols

{% include_relative ref/listening-to-input.md %}

## Building a distributable

Sharing a Common Lisp game in a binary form amongst users was always a bit of pain. But fear no
more! `gamekit` includes a mechanism for delivering your game packaged using only a single
function - [`#'deliver`](#gamekit-deliver). It will build an executable, pack all used
resources, copy required foreign libraries used by `trivial-gamekit` and compress all that into
a shippable archive.

For building these packages for `Windows`, `GNU/Linux` and `macOS` with just a single push to a
`github` or `gitlab` respository check out [Advanced Features]({% link
projects/trivial-gamekit/advanced.md %}) section.

#### Related symbols

{% include_relative ref/building-a-distributable.md %}

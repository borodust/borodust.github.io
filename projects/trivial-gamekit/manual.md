---
layout: page
title: User Manual
project: trivial-gamekit
---

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

`gamekit`'s central idea is a game object you define via `defgame` macro. It contains all the
state `gamekit` needs to run your game. It is instantiated by `#'start` which also bootstraps
the whole application and opens a window, runs your game logic, rendering, etc. Only single game
object can be run at a time, so subsequently calling `#'start` for the same or different game
objects is disallowed - you need to stop game execution. To stop a running game you would need
to call `#'stop`.


### Related symbols

*macro* ***`defgame`*** `(name (&rest classes) ((&rest slots) &rest opts))`
{: #gamekit-defgame}

<div class="bodge-docstring" markdown="block">

Defines a game class that can be passed to `#'start` to run a game. `name` is the name of a
class generated. `classes` are names of superclasses, `slots` - standard class slots and `opts`
are class options. So, pretty much standard class definition except it does configure a class in
certain ways specifically for `gamekit` use and allows passing additional options in `opts`
apart from standard `:documentation`, `:default-initargs` and so others.

Additional options that can be passed in `opts` are:

* `:viewport-width` - width of the window and canvas
* `:viewport-height` - height of the window and canvas
* `:viewport-title` - title of the window
* `:prepare-resources` - boolean value that indicates whether `gamekit` should load resources
automatically on startup or if not, user prefers to load them dynamically on request. Defaults
to `t`.

Example:

```common_lisp
(gamekit:defgame notalone ()
;; some game related state
((world :initform (make-instance 'world))
(game-state))
;; options
(:viewport-width 800)
(:viewport-height 600)
(:viewport-title "NOTALONE")
(:prepare-resources nil))
```
</div>


*function* ***`start`*** `(classname &key)`
{: #gamekit-start}

<div class="bodge-docstring" markdown="block">

Bootsraps a game allocating a window and other system resources. Instantiates game object
defined with [`defgame`](#gamekit-defgame) which can be obtained via
[`#'gamekit`](#gamekit-gamekit). Cannot be called twice - [#'stop](#gamekit-stop) should be
called first before running `start` again.

Example:

```common_lisp
(gamekit:start 'notalone)
```
</div>


*function* ***`stop`*** `()`
{: #gamekit-stop}

<div class="bodge-docstring" markdown="block">

Stops currently running game releasing acquired resources.

Example:
```common_lisp
(gamekit:stop)
```
</div>

*function* ***`gamekit`*** `()`
{: #gamekit-gamekit}

<div class="bodge-docstring" markdown="block">
Returns instance of a running game or `nil` if no game is started yet.

Example:
```common_lisp
(gamekit:gamekit)
```
</div>

## Math

## Locating resources

## Drawing

## Playing an audio

## Listening to input

## Building a distributable

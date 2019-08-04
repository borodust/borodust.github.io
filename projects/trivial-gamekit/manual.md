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
* [Playing audio](#playing-audio)
* [Listening to input](#listening-to-input)
* [Building a distributable](#building-a-distributable)
* [Troubleshooting](#troubleshooting)
* [Symbol Index](#symbol-index)


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
need to stop game execution first. To stop a running game you would need to call
[`#'stop`](#gamekit-stop).

You can put game initialization code into
[`#'post-initialize`](#gamekit-post-initialize). Specifically, it is probably a
best place to bind input via [`#'bind-cursor`](#gamekit-bind-cursor) or
[`#'bind-button`](#gamekit-bind-button) or any gamepad-related functions as
described below, but you are free to do any other preparations that require
fully initialized game instance. If you need to release any unmanaged resources
(e.g. foreign memory you allocated during a game or in
[`#'post-initialize`](#gamekit-post-initialize)) to avoid leaks you can override
[`#'pre-destroy`](#gamekit-pre-destroy) function which is called after last game
loop iteration.

[`#'start`](#gamekit-start) also invokes a default game loop. To hook into it
you can use [`#'act`](#gamekit-act) and [`#'draw`](#gamekit-draw)
methods. [`#'act`](#gamekit-act) is used for updaing your game state and
[`#'draw`](#gamekit-draw) should be used exclusively for
drawing/rendering. `:draw-rate` and `:act-rate` options of `defgame` can be used
to specify rate for `#'draw` and `#'act` in invocations per second separately,
meaning if you set `:draw-rate` to 60 your framerate would be set to 60 frames
per second.

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

## Locating resources

Resources are very important for games. Textures, models, sounds, fonts,
animations, tiles - you name it. `gamekit` has several routines prepared for you
to ease the handling of those.

First of all, we need to tell `gamekit` where to find resources via
[`#'define-image`](#gamekit-define-image),
[`#'define-sound`](#gamekit-define-sound) and
[`#'define-font`](#gamekit-define-font) macros. First argument to them is a
symbol that would become an identificator for the resource and the second
argument is a absoulute or relative path to the resource.

For absolute path, be sure to use helper functions like
`asdf:system-relative-pathname` to avoid hardcoding paths to your resources in
the code.

To use relative resource path, you first need to specify resource root
directory. Resource roots are associated with Common Lisp packages. Any package
can have associated filesystem directory to find resources in. Call
[`#'register-resource-package`](#gamekit-register-resource-package) as a
toplevel form to bind a directory to a package. Absolute paths required here.
Once done, you can specify relative path in `define-*` forms and `gamekit` would
merge resource root pathname associated with a package of a symbol (first
argument to `define-*`) with relative pathname you provided.

Resources are used by functions that requires them to operate, like
[`#'draw-image`](#gamekit-draw-image), [`#'make-font`](#gamekit-make-font),
[`#'play-sound`](#gamekit-play-sound) and others.

When [`defgame`](#gamekit-defgame) option `:prepare-resources` is set to `nil`,
you would need to request resources manually by calling
[`#'prepare-resources`](#gamekit-prepare-resources) with resource ids you
need. Preparation can take a while, so
[`#'prepare-resources`](#gamekit-prepare-resources) asynchonously requests
resources and returns immediately for you to be notified later with
[`#'notice-resources`](#gamekit-notice-resources).  This is useful, when you
don't want to wait until all assets are loaded and start rendering some loading
or start screen right away and continue with game after all resources are ready.
If you have a lot of resources and would like to get rid of unneded ones to free
some memory, you can use [`#'dispose-resources`](#gamekit-dispose-resources).

Sometimes you would want to ship a game with custom resources not supported by
`gamekit` directly. To accomplish that, you can use
[`define-text`](#gamekit-define-text) to include text files and
[`define-binary`](#gamekit-define-binary) to include any file basically, which
would be represented as a simple array of bytes. To access those resources you
would need to use [`#'get-text`](#gamekit-get-text) and
[`#'get-binary`](#gamekit-get-binary) correspondingly.

## Drawing

`gamekit` provides simple to use but versatile drawing API. If you know how
HTML5 canvas API is organized you will find `gamekit`'s one quite
similar. Several functions with names starting with `draw-` exists to help you
draw primitives like lines, rectangles, ellipses, arcs, polygons or even bezier
curves. Images can be drawn via [`#'draw-image`](#gamekit-draw-image) and text
with customizable font can be put onto canvas via
[`#'draw-text`](#gamekit-draw-text).

Oftentimes it is useful to know dimensions of the visual resources to position
them correctly. [`#'image-width`](#gamekit-image-width) and
[`#'image-height`](#gamekit-image-height) can help you with retreiving image
dimensions while [`#'calc-text-bounds`](#gamekit-calc-text-bounds) will allow
you to calculate text bounding box.

Canvas transformation operations are supported too. You can scale, translate, rotate a canvas
with [`#'scale-canvas`](#gamekit-scale-canvas),
[`#'translate-canvas`](#gamekit-translate-canvas) and
[`#'rotate-canvas`](#gamekit-rotate-canvas) accordingly. If you need to keep canvas state for
nested drawing operations you will appreciate existence of
[`#'with-pushed-canvas`](#gamekit-with-pushed-canvas) macro, that keeps canvas state to the
dynamic extent of its body. This means that upon returning from the macro canvas transformation
state will be returned to its original state before the macro and all transformation operations
inside its body would be canceled out.

**All** drawing operations should be performed inside [`#'draw`](#gamekit-draw).

Unlike many other drawing APIs `gamekit` uses bottom left corner as an origin (0,0) with y-axis
pointing upwards which is more convenient mathematically.

All origins or positions are represented by two-dimensional (x,y) vectors created via
[`#'vec2`](#gamekit-vec2). Colors are passed around as four-dimensional vectors made with
[`#'vec4`](#gamekit-vec4) consisting of red, green, blue and alpha channels each within 0.0-1.0
range.

## Playing audio

Audio can substantially boost game's atmosphere, and `gamekit` is ready to serve you well in this
regard too. [`#'play-sound`](#gamekit-play-sound) will help with getting sounds to reach your
users ears. On the other hand, [`#'stop-sound`](#gamekit-stop-sound) can be used to stop this
process.

## Listening to input

There's no game without some sort of interaction with a player. `gamekit` allows you to grab
keyboard and mouse input to listen for player actions. You can pass a callback to
[`#'bind-button`](#gamekit-bind-button) which will be called upon pressing or releasing
keyboard/mouse buttons. Callback passed to [`#'bind-cursor`](#gamekit-bind-cursor) is going to
be invoked when user moves a mouse. Callbacks provided are not stacked together, meaning if you
try to bind multiple callbacks to the same button only last callback is actually going to be
invoked. Same goes for cursor input.

`gamekit` also support gamepads and exposing them as a [generic xbox
controllers]({% link public/shared/360_controller.svg %}). You can listen to
gamepads being connected and disconnected with
[`#'bind-any-gamepad`](#gamekit-bind-any-gamepad). Unfortunately, `gamekit`
doesn't have a reliable way to numerate gamepads, so gamepad-related functions
operate on opaque gamepad references and you need to manage player<->gamepad
relationship yourself.

For listening to gamepad buttons you can use
[`#'bind-gamepad-button`](#gamekit-bind-gamepad-button) and
[`#'bind-gamepad-any-button`](#gamekit-bind-gamepad-any-button).
[`#'bind-gamepad-dpad`](#gamekit-bind-gamepad-dpad) can help to catch changes in
d-pad state. [`#'bind-gamepad-stick`](#gamekit-bind-gamepad-stick) and
[`#'bind-gamepad-trigger`](#gamekit-bind-gamepad-trigger) can help tracking
position of right and left sticks and values of left and right triggers
accordingly.

Sometimes `gamekit` wouldn't be able to recognize your gamepad. In this case you
need to specify path to custom controller mapping file in [SDL2
format](https://github.com/gabomdq/SDL_GameControllerDB/blob/master/gamecontrollerdb.txt)
in `BODGE_GAMECONTROLLERDB` environment variable and restart the game.


## Building a distributable

Sharing a Common Lisp game in a binary form amongst users was always a bit of a
pain. But fear no more! `gamekit` includes a mechanism for delivering your game
packaged using only a single function - [`#'deliver`](#gamekit-deliver). It will
build an executable, pack all required resources, copy needed foreign libraries
used by `trivial-gamekit` and compress all that into a shippable archive.

To gain access to this function you need additionally load `:trivial-gamekit/distribution`
system, and then you would be able to find it in `gamekit.distribution` package.

```common-lisp
(ql:quickload :trivial-gamekit/distribution)
```

For building these packages for `Windows`, `GNU/Linux` and `macOS` with just a
single push to a `github` or `gitlab` respository check out [Advanced
Features]({% link projects/trivial-gamekit/advanced.md %}) section.

## Troubleshooting

#### Restarts
Common Lisp is well known for its superior interactive development capabilities,
and when things go left you often can fix problem live without restarting an
image. `gamekit` strives to embrace this approach as much as possible. But
beware, under the hood `gamekit` uses quite an intricate machinery and it still
is possible to break it beyond live repairement.

Important thing to keep in mind: don't invoke `'abort` restart that quit
threads. It's too easy, because `q` button in `slime` or `sly` is bound to quit
action in debugger which invokes most destroying restart possible. It often
works in other applications, but in complex multithreaded environments it can be
disastrous. It's quite hard (if impossible) to restore `gamekit` state after any
of its internal threads are killed. Just don't press `q`.

`sly` and `slime` has other more useful button bindings available: `c` - for
`'continue` restart and `a` for topmost `'abort` restart. `gamekit` binds those
exact restarts to gracefully handle failures. `'continue` will try to skip the
offending block of code and `'abort` will try to shutdown engine gracefully
altogether after which you should be able to start your game with
`#'gamekit:start`. The latter one is not very interactive-friendly obviously,
and should be the last resort before restarting the whole image (lisp process).

Another restart that isn't bound to a key but super useful is
`'rerun-flow-block`. Ivoking it will prompt `gamekit` to rerun a code the error
was in. Fixing the error, reevaluating the code and invoking this restart should
bring your game back to working state. This is the restart you might want to use
the most.

`'skip-flow-block` restart does the same thing as `'continue` - skips the code
with the error.

#### Only one active system of type 'appkit-system is allowed
Invoke `(gamekit:stop)` in REPL and then try running your game again by invoking
`#'gamekit:start` with required params. If that didn't help - reload a lisp
image.


## Symbol Index
{% include_relative ref/symbol-index.md %}

{% include_relative ref/defining-a-game.md %}
{% include_relative ref/math.md %}
{% include_relative ref/locating-resources.md %}
{% include_relative ref/drawing.md %}
{% include_relative ref/playing-audio.md %}
{% include_relative ref/listening-to-input.md %}
{% include_relative ref/building-a-distributable.md %}

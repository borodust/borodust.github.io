---
layout: page
title: Getting Started with trivial-gamekit
project: trivial-gamekit
---


## How to use this guide
Just open up your favorite lisp's REPL [supported]({% link
projects/trivial-gamekit.md %}#requirements) by `trivial-gamekit` and follow
this guide step by step copy-pasting code snippets right into the REPL. Whole
code of this little piece with almost every snippet included can be found at the
[end](#result) of this guide.

There's a [vid on YouTube](https://www.youtube.com/watch?v=iR2Yy5rfHzQ) that
loosely follows this guide. It can help you understand how the development
process with `trivial-gamekit` looks like and how to start development from the
scratch.  The progress in the vid is very slow and unstructured: feel free to
skip sections and rewind back if something stopped to make sense.

## Basics

First we need to install `trivial-gamekit` system. This is quite easy to
accomplish thanks to `Quicklisp`. Check out [this small tip]({% link
projects/trivial-gamekit.md %}#installation-and-loading) on how to do that.

### Starting up

Now, when the system is successfully loaded, let's define a main class that will
manage our application:

```common-lisp
(gamekit:defgame hello-gamekit () ())
```

Yes. That's it. You totally configured an application that can draw onto screen,
play audio and listen for user input. Let's confirm it works. Evaluate in REPL:

```common-lisp
(gamekit:start 'hello-gamekit)
```

Here we go. Canvas for your imagination to go wild onto is now ready!


### Shutting down

You can close `trivial-gamekit`'s window and release acquired system resources
by using your OS UI or with

```common-lisp
(gamekit:stop)
```


### Window configuration

`trivial-gamekit` allows you to configure host window to some degree through
`defgame` options:

```common-lisp
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(gamekit:defgame hello-gamekit () ()
  (:viewport-width *canvas-width*)     ; window's width
  (:viewport-height *canvas-height*)   ; window's height
  (:viewport-title "Hello Gamekit!"))  ; window's title
```

Alrighty, let's bring a window back to continue our endeavor. Evaluate in REPL:

```common-lisp
(gamekit:start 'hello-gamekit)
```

## Drawing

Hopefully, `trivial-gamekit` manages rendering loop for you. To draw anything on
the screen you just need to override `gamekit:draw` generic function:

```common-lisp
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))

(defmethod gamekit:draw ((app hello-gamekit))
  ;; Let's draw a black box in the bottom-left corner
  (gamekit:draw-rect *origin* 100 100 :fill-paint *black*))
```

We can do a couple of observations from this example. First, unlike many other
2D drawing APIs, `trivial-gamekit` uses bottom-left corner as an origin and
y-axis pointing upwards for its 2D coordinate system. Second, colors are
represented by 4-element vectors made with `#'gamekit:vec4` with values for each
element ranging from 0.0 to 1.0 (red/green/blue/alpha) and 2D coordinates are
passed around using 2-element vectors.

While we are at it, it's worth mentioning that 2D canvas by default has exactly
the size we supplied to the `'hello-gamekit` as `:viewport-width` and
`:viewport-height` options. So for this case, bottom-left corner of our canvas
is (0, 0) and top-right corner is (799, 599).

Static black box is anything but exciting. Let's introduce some motion:

```common-lisp
(defvar *current-box-position* (gamekit:vec2 0 0))

(defun real-time-seconds ()
  "Return seconds since certain point of time"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position time)
  "Update position vector depending on the time supplied"
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:x position) (+ 350 (* 100 (cos angle)))
          (gamekit:y position) (+ 250 (* 100 (sin angle))))))

(defmethod gamekit:draw ((app hello-gamekit))
  (update-position *current-box-position* (real-time-seconds))
  (gamekit:draw-rect *current-box-position* 100 100 :fill-paint *black*))
```

Wooosh, it moves!

Functions `#'x`, `#'y`, `#'z` and `#'w` are used to set (via `#'setf`) or get
first, second, third or forth element out of a vector accordingly. Feel free to
tinker with `#'update-position` changing how the position of the box is
calculated. Just paste an updated function back into the REPL and you will see
changes instantly. How awesome is that?

Quite cool, unlike this state-chaning code in our `#'draw` method. `gamekit` has
a special method to separate game-related logic that needs to be done per-frame
from the drawing routine - `#'act`. By default, it is, just like `#'draw`,
called each frame, but in a bit different environment. All non-rendering tasks
should go there.

Gamekit exports several `draw-*` functions that could be useful for 2D
drawing. Let's turn our moving box into a sinus snake:


```common-lisp
(defvar *curve* (make-array 4 :initial-contents (list (gamekit:vec2 300 300)
                                                      (gamekit:vec2 375 300)
                                                      (gamekit:vec2 425 300)
                                                      (gamekit:vec2 500 300))))

(defun update-position (position time)
  (let* ((subsecond (nth-value 1 (truncate time)))
         (angle (* 2 pi subsecond)))
    (setf (gamekit:y position) (+ 300 (* 100 (sin angle))))))


(defmethod gamekit:act ((app hello-gamekit))
  (update-position (aref *curve* 1) (real-time-seconds))
  (update-position (aref *curve* 2) (+ 0.3 (real-time-seconds))))
```

```common-lisp
(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:draw-curve (aref *curve* 0)
                      (aref *curve* 3)
                      (aref *curve* 1)
                      (aref *curve* 2)
                      *black*
                      :thickness 5.0))
```

Well, people won't beleive us it is a snake, so I guess we need to leave a note
for them to not confuse it for bezier curve with animated control points.

```common-lisp
(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:draw-text "A snake that is!" (gamekit:vec2 300 400))
  (gamekit:draw-curve (aref *curve* 0)
                      (aref *curve* 3)
                      (aref *curve* 1)
                      (aref *curve* 2)
                      *black*
                      :thickness 5.0))
```

`#'gamekit:draw-text` allows us to put a text onto the screen.

Anyway, quite a cringy snake, but move it does. We couldn't ask for more.

## Input

Obvisously, we could and we do ask for more: where's all the interactivity much
needed in a game of any kind? So let's put a head of our snake at a place under
the cursor each time left mouse button is clicked.


```common-lisp
(defvar *cursor-position* (gamekit:vec2 0 0))

(gamekit:bind-cursor (lambda (x y)
                       "Save cursor position"
                       (setf (gamekit:x *cursor-position*) x
                             (gamekit:y *cursor-position*) y)))

(gamekit:bind-button :mouse-left :pressed
                     (lambda ()
                       "Copy saved cursor position into snake's head position vector"
                       (let ((head-position (aref *curve* 3)))
                         (setf (gamekit:x head-position) (gamekit:x *cursor-position*)
                               (gamekit:y head-position) (gamekit:y *cursor-position*)))))
```

Just click around in the window to see how that turned out.

Now, for enhanced interactivity, let's move snake's head while left button is
pressed - dragging it along the way.

```common-lisp
(defvar *head-grabbed-p* nil)

(gamekit:bind-cursor (lambda (x y)
                       "When left mouse button is pressed, update snake's head position"
                       (when *head-grabbed-p*
                         (let ((head-position (aref *curve* 3)))
                           (setf (gamekit:x head-position) x
                                 (gamekit:y head-position) y)))))


(gamekit:bind-button :mouse-left :pressed
                     (lambda () (setf *head-grabbed-p* t)))

(gamekit:bind-button :mouse-left :released
                     (lambda () (setf *head-grabbed-p* nil)))
```

Poor snake.

As we can see from examples above, we can bind any action to key or mouse clicks
using `#'bind-button` function. First argument of the function is the button to
track. Possible values are `:mouse-left`, `:mouse-right` and `:mouse-middle` for
mouse buttons, and for keys you can use values such as `:a`, `:b`, `:c`, ...,
`:0`, `:1`, ..., `:f1`, `:f2`, ..., `:escape`, `:enter`, `:tab`, `:backspace`,
`:left`, `:right`, ..., `:space` and many others. Second argument tells the
gamekit which particular button state to assign action to. Available states are
`:pressed`, `:released` and `:repeating`. And finally, last argument is a
function without arguments which will be called when specified key or button
reaches provided state.

To grab a cursor movement one can use `#'bind-cursor` function. Its only
argument represents a function of two arguments - x and y of a mouse cursor
position. The latter is called every time a mouse cursor changes its location.


## Assets

We need a couple of resources prepared to continue with this guide. Download
[this]({% link public/snake-head.png %}) (right click on the link -> save as)
image[^1] and [this]({% link public/snake-grab.ogg %}) sound file[^2] to
`/tmp/hello-gamekit-assets/`. Now let's tell gamekit where to find those with
`#'register-resource-package` function.

```common-lisp
(gamekit:register-resource-package :keyword "/tmp/hello-gamekit-assets/")
```

### Images
One can make quite a complex scene with just primitives like rectangles,
ellipses, lines, curves, etc. But for very intricate objects it is still much
easier to just display a prepared image. `trivial-gamekit` ready to help you
with this too.

First, we need to tell gamekit where it can find our image. We will use
`define-image` for that:

```common-lisp
(gamekit:define-image :snake-head "snake-head.png")
```

By default, upon initialization or when evaluating at runtime (in REPL), gamekit
will load this image using base path you provided with
`#'register-resource-package` of your main class (`hello-gamekit` in this case)
merging it with a relative path specified in a second argument of
`define-image`. If you provide an absolute path, then base path would be
ignored. First argument of that function is used to reference this image in
other places later. `define-image` supports only .png images yet.

No need to tell `gamekit` anything else - your image should have been already
loaded.

To put an image onto the screen you can use `#'gamekit:draw-image`. It has two
arguments. First is the coords where image origin (its bottom-left corner) will
be put. And the second one tells which image gamekit should use. Let's improve
our `#'draw` method using it:

```common-lisp
(defmethod gamekit:draw ((app hello-gamekit))
  (gamekit:print-text "A snake that is!" 300 400)
  (gamekit:draw-curve (aref *curve* 0)
                      (aref *curve* 3)
                      (aref *curve* 1)
                      (aref *curve* 2)
                      *black*
                      :thickness 5.0)
  ;; let's center image position properly first
  (let ((head-image-position (gamekit:subt (aref *curve* 3) (gamekit:vec2 32 32))))
    ;; then draw it where it belongs
    (gamekit:draw-image head-image-position :snake-head)))
```

A face appears!

### Sounds

`trivial-gamekit` can help you with tricking not only player eyes, but ears too.
First, let's inform the `gamekit` where it can locate a sound with `define-audio`
macro. It supports a couple audio formats including `.ogg` (Ogg/Vorbis), `.flac`
and `.wav`.

```common-lisp
(gamekit:define-sound :snake-grab "snake-grab.ogg")
```

For playing a sound `#'gamekit:play` function is used. Let's play this sound
when we grabbing snake's head:

```common-lisp
(gamekit:bind-button :mouse-left :pressed
                     (lambda ()
                       (gamekit:play :snake-grab)
                       (setf *head-grabbed-p* t)))
```

Try to grab a snake's head now. Yeah, I like it too.


## Result

All snippets combined could be found in `hello-gamekit` GitHub
[repository](https://github.com/borodust/hello-gamekit). Clone it or fork it and
start playing with `trivial-gamekit` any moment!



***
[^1]: Original [image](http://www.clipartpanda.com/clipart_images/snake-cartoon-black-and-white-29075943)
[^2]: Original [sound](https://freesound.org/people/Tomlija/sounds/105419/)

*macro* ***`defgame`*** `(name (&rest classes) &body ((&rest slots) &rest opts))`
{: #gamekit-defgame}
<div class="bodge-docstring" markdown="block">
Defines a game class that can be passed to [`#'start`](#gamekit-start) to run
a game. `name` is the name of a class generated. `classes` are names of
superclasses, `slots` - standard class slots and `opts` are class options. So,
pretty much standard class definition except it does configure a class in
certain ways specifically for `gamekit` use and allows passing additional
options in `opts` apart from standard `:documentation`, `:default-initargs` and
so others.

Additional options that can be passed in `opts` are:

* `:viewport-width` - width of the window and canvas
* `:viewport-height` - height of the window and canvas
* `:viewport-title` - title of the window
* `:prepare-resources` - boolean value that indicates whether `gamekit` should
load resources automatically on startup or if not, user prefers to load them dynamically on request. Defaults to `t`.

Example:

```common-lisp
(gamekit:defgame example ()
  ;; some game related state
  ((world :initform (make-instance 'world))
   (game-state))
  ;; options
  (:viewport-width 800)
  (:viewport-height 600)
  (:viewport-title "EXAMPLE")
  (:prepare-resources nil))
```
</div>

*function* ***`start`*** `(classname &key (log-level info) (opengl-version '(3 3)) samples blocking viewport-resizable (viewport-decorated
                                                                                              t) (autoscaled
                                                                                                  t) swap-interval properties)`
{: #gamekit-start}
<div class="bodge-docstring" markdown="block">
Bootsraps a game allocating a window and other system resources. Instantiates
game object defined with [`defgame`](#gamekit-defgame) which can be obtained via
[`#'gamekit`](#gamekit-gamekit). Cannot be called twice -
[`#'stop`](#gamekit-stop) should be called first before running `start` again.

Example:

```common-lisp
(gamekit:start 'example)
```
</div>

*function* ***`stop`*** `(&key blocking)`
{: #gamekit-stop}
<div class="bodge-docstring" markdown="block">
Stops currently running game releasing acquired resources.

Example:
```common-lisp
(gamekit:stop)
```
</div>

*function* ***`gamekit`*** `()`
{: #gamekit-gamekit}
<div class="bodge-docstring" markdown="block">
Returns instance of a running game or `nil` if no game is started yet.

Example:
```common-lisp
 (gamekit:gamekit)
```
</div>

*generic* ***`post-initialize`*** `(system)`
{: #gamekit-post-initialize}
<div class="bodge-docstring" markdown="block">
This function is called after game instance is fully initialized, right
before main game loop starts its execution. Put initialization code for your
application into method of this function. For example, it would be logical to
bind input via [`#'bind-cursor`](#gamekit-bind-cursor) or
[`#'bind-button`](#gamekit-bind-button) here.

Example:
```common-lisp
(defmethod gamekit:post-initialize ((this example))
  (init-game)
  (bind-input))
```
</div>

*generic* ***`pre-destroy`*** `(system)`
{: #gamekit-pre-destroy}
<div class="bodge-docstring" markdown="block">
This function is called just before shutting down a game instance for you to
free all acquired resources and do any other clean up procedures.

Example:
```common-lisp
 (defmethod gamekit:pre-destroy ((this example))
   (release-foreign-memory)
   (stop-threads))
```
</div>

*generic* ***`act`*** `(system)`
{: #gamekit-act}
<div class="bodge-docstring" markdown="block">
Called every game loop iteration for user to add any per-frame behavior to
the game. NOTE: all drawing operations should be performed in
[`#'draw`](#gamekit-draw) method.

Example:
```common-lisp
 (defmethod gamekit:act ((this example))
   (report-fps))
```
</div>

*generic* ***`draw`*** `(system)`
{: #gamekit-draw}
<div class="bodge-docstring" markdown="block">
Called every game loop iteration for frame rendering.
All drawing operations should be performed in this method.

Example:
```common-lisp
 (defmethod gamekit:draw ((this example))
   (gamekit:draw-text "Hello, Gamedev!" (gamekit:vec2 10 10)))
```
</div>

*function* ***`viewport-width`*** `()`
{: #gamekit-viewport-width}
<div class="bodge-docstring" markdown="block">
Returns width of a gamekit viewport (window) if there's an active gamekit
instance (started via [`#'start`](#gamekit-start)) or nil otherwise.

Example:

```common-lisp
  (gamekit:viewport-width)
```
</div>

*function* ***`viewport-height`*** `()`
{: #gamekit-viewport-height}
<div class="bodge-docstring" markdown="block">
Returns height of a gamekit viewport (window) if there's an active gamekit
instance (started via [`#'start`](#gamekit-start)) or nil otherwise.

Example:

```common-lisp
  (gamekit:viewport-height)
```
</div>


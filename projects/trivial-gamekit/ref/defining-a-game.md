*macro* ***`defgame`*** `(name (&rest classes) &body ((&rest slots) &rest opts))`
{: #gamekit-defgame}
<div class="bodge-docstring" markdown="block">
Defines a game class that can be passed to [`#'start`](#gamekit-start) to run a game. `name` is
the name of a class generated. `classes` are names of superclasses, `slots` - standard class
slots and `opts` are class options. So, pretty much standard class definition except it does
configure a class in certain ways specifically for `gamekit` use and allows passing additional
options in `opts` apart from standard `:documentation`, `:default-initargs` and so others.

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

*function* ***`start`*** `(classname &key (log-level info) (opengl-version '(3 3)) blocking)`
{: #gamekit-start}
<div class="bodge-docstring" markdown="block">
Bootsraps a game allocating a window and other system resources. Instantiates game object
defined with [`defgame`](#gamekit-defgame) which can be obtained via
[`#'gamekit`](#gamekit-gamekit). Cannot be called twice - [`#'stop`](#gamekit-stop) should be
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

*generic* ***`act`*** `(system)`
{: #gamekit-act}
<div class="bodge-docstring" markdown="block">

</div>

*generic* ***`draw`*** `(system)`
{: #gamekit-draw}
<div class="bodge-docstring" markdown="block">

</div>


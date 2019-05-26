*function* ***`bind-button`*** `(key state action)`
{: #gamekit-bind-button}
<div class="bodge-docstring" markdown="block">
Binds `action` to specified `key` `state`. When key state changes to the one specified,
action callback is invoked with no arguments. `#'bind-button` function should be
called when there's active game exists started earlier with
[`#'start`](#gamekit-start). `state` can be either `:pressed`, `:released` or
`:repeating`.

Actions are not stacked together and would be overwritten for the same key and state.

Can only be called when gamekit instance is active (started).

Supported keys:
```common-lisp
  :space :apostrophe :comma :minus :period :slash
  :0 :1 :2 :3 :4 :5 :6 :7 :8 :9
  :semicolon :equal
  :a :b :c :d :e :f :g :h :i :j :k :l :m
  :n :o :p :q :r :s :t :u :v :w :x :y :z
  :left-bracket :backslash :right-bracket
  :grave-accent :world-1 :world-2
  :escape :enter :tab :backspace :insert :delete
  :right :left :down :up
  :page-up :page-down :home :end
  :caps-lock :scroll-lock :num-lock :print-screen :pause
  :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12
  :f13 :f14 :f15 :f16 :f17 :f18 :f19 :f20 :f21 :f22 :f23 :f24 :f25
  :keypad-0 :keypad-1 :keypad-2 :keypad-3 :keypad-4
  :keypad-5 :keypad-6 :keypad-7 :keypad-8 :keypad-9
  :keypad-decimal :keypad-divide :keypad-multiply
  :keypad-subtract :keypad-add :keypad-enter :keypad-equal
  :left-shift :left-control :left-alt :left-super
  :right-shift :right-control :right-alt :right-super
  :menu

  :mouse-left :mouse-right :mouse-middle
```

Example
```common-lisp
(gamekit:bind-button :enter :pressed
                     (lambda ()
                       (start-game-for *player*)))
```
</div>

*function* ***`bind-cursor`*** `(action)`
{: #gamekit-bind-cursor}
<div class="bodge-docstring" markdown="block">
Binds action callback to a cursor movement event. Everytime user moves a
cursor callback will be called with x and y of cursor coordinates within the
same coordinate system canvas is defined in: bottom left corner as (0,0) origin
and y-axis pointing upwards.

Actions doesn't stack together and would be overwritten each time
`#'bind-cursor` is called.

Can only be called when gamekit instance is active (started).

Example:
```common-lisp
 (gamekit:bind-cursor (lambda (x y)
                        (shoot-to x y)))
```
</div>

*function* ***`bind-any-gamepad`*** `(action)`
{: #gamekit-bind-any-gamepad}
<div class="bodge-docstring" markdown="block">
Binds `action` to a gamepad connection and disconnection events. Once one of
those events happen, `action` is called with two arguments: `gamepad` - opaque
reference to a gamepad that will be supplied as an argument to other
gamepad-related actions, and `state` - which can be either `:connected` or
`:disconnected` to catch connection and disconnection of a gamepad accordingly.

If there were gamepads already connected before call to `#'bind-any-gamepad`,
`action` is called for each one of those upon invocation.

Example:
```common-lisp
 (gamekit:bind-any-gamepad (lambda (gamepad state)
                             (if (eq :connected state)
                                 (add-player-for-gamepad gamepad)
                                 (pause-game-and-wait-for-player gamepad))))
```
</div>

*function* ***`bind-gamepad-button`*** `(button state action)`
{: #gamekit-bind-gamepad-button}
<div class="bodge-docstring" markdown="block">
Binds `action` to specified gamepad's `button` `state`.  When button state
changes to the one specified, action callback is invoked with gamepad opaque
reference as an argument. `state` can be either `:pressed` or `:released`.

Actions are not stacked together and would be overwritten for the same button and state.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Gamekit's gamepad is a generic xbox controller with the same layout of controls.

Supported buttons:
```common-lisp
  :a :b :x :y
  :left-bumper :right-bumper
  :start :back :guide
  :left-thumb :right-thumb
```

Example
```common-lisp
 (gamekit:bind-gamepad-button :start :pressed
                              (lambda (gamepad)
                                (declare (ignore gamepad))
                                (start-game)))
```
</div>

*function* ***`bind-gamepad-any-button`*** `(action)`
{: #gamekit-bind-gamepad-any-button}
<div class="bodge-docstring" markdown="block">
Binds `action` to all buttons of gamepads. When any button state of any
gamepad changes, action callback is invoked with gamepad opaque reference as a
first argument, gamepad's button as a second and button's state as a third argument.
See [`#'bind-gamepad-button`](#gamekit-bind-gamepad-button) for available button
values and states.

Actions are not stacked together and would be overwritten on each function invocation.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-any-button (lambda (gamepad button state)
                                    (when (and (eq button :start) (eq state :pressed))
                                      (join-party (make-player-for-gamepad gamepad)))))
```
</div>

*function* ***`bind-gamepad-dpad`*** `(state action)`
{: #gamekit-bind-gamepad-dpad}
<div class="bodge-docstring" markdown="block">
Binds `action` to gamepad's dpad. When dpad state changes, action callback is
invoked with gamepad opaque reference as a first argument and new dpad state as a
second.

Dpad states:
```common-lisp
  :up :down :left :right
  :left-up :left-down
  :right-up :right-down
  :centered
```

Actions are not stacked together and would be overwritten for the same dpad
state.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-state :up (lambda (gamepad)
                                   (declare (ignore gamepad))
                                   (jump *player*)))
```
</div>

*function* ***`bind-gamepad-stick`*** `(stick action)`
{: #gamekit-bind-gamepad-stick}
<div class="bodge-docstring" markdown="block">
Binds `action` to gamepad's left or right stick. When position of the
specified stick changes, action callback is invoked with `gamepad` opaque
reference as a first argument, position's `x` and `y` as second and third
arguments. `x` and `y` values are in [-1;1] range: stick up (0;1), stick
down (0;-1), stick left (-1;0) and stick right (1;0).

Sticks: `:left` and `:right`.

Actions are not stacked together and would be overwritten for the same stick.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-stick :left (lambda (gamepad x y)
                                     (declare (ignore gamepad))
                                     (move-player *player* x y)))
```
</div>

*function* ***`bind-gamepad-trigger`*** `(trigger action)`
{: #gamekit-bind-gamepad-trigger}
<div class="bodge-docstring" markdown="block">
Binds `action` to gamepad's left or right triggers. When value of the
specified trigger changes, action callback is invoked with `gamepad` opaque
reference as a first argument and new trigger value as second. Trigger values
are in [0;1] range.

Triggers: `:left` and `:right`.

Actions are not stacked together and would be overwritten for the same trigger.

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example
```common-lisp
 (gamekit:bind-gamepad-trigger :right (lambda (gamepad value)
                                        (declare (ignore gamepad))
                                        (setf (shot-power *player*) value)))
```
</div>


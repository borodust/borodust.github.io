*function* ***`bind-button`*** `(key state action)`
{: #gamekit-bind-button}
<div class="bodge-docstring" markdown="block">
Binds `action` to specified `key` `state`. When key state changes to the one specified,
action callback is invoked with no arguments. `#'bind-button` function should be called when
there's active game exists started earlier with [`#'start`](#gamekit-start). `state` can be either
`:pressed`, `:released` or `:repeating`.

Actions are not stacked together and would be overwritten for the same key and state.

Supported keys:
```common_lisp
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
```common_lisp
(gamekit:bind-button :enter :pressed
                     (lambda ()
                       (start-game-for *player*)))
```
</div>

*function* ***`bind-cursor`*** `(action)`
{: #gamekit-bind-cursor}
<div class="bodge-docstring" markdown="block">
Binds action callback to a cursor movement event. Everytime user moves a cursor callback will
be called with x and y of cursor coordinates within the same coordinate system canvas is defined
in: bottom left corner as (0,0) origin and y-axis pointing upwards.

Actions doesn't stack together and would be overwritten each time `#'bind-cursor` is called.

Example:
```common_lisp
(gamekit:bind-cursor (lambda (x y)
                       (shoot-to x y)))
```
</div>


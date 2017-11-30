*function* ***`play-sound`*** `(sound-id &key looped-p)`
{: #gamekit-play-sound}
<div class="bodge-docstring" markdown="block">
Plays a sound defined earlier with [`define-sound`](#gamekit-define-sound). Pass `t` to
`:looped-p` key to play sound in a loop.

Example:
```common_lisp
(gamekit:play-sound 'example-package::blop
                    :looped-p t)
```
</div>

*function* ***`stop-sound`*** `(sound-id)`
{: #gamekit-stop-sound}
<div class="bodge-docstring" markdown="block">
Stops a playing sound by provided sound id.

Example:
```common_lisp
(gamekit:stop-sound 'example-package::blop)
```
</div>


---
layout: post
title: trivial-gamekit's future
excerpt_separator: <!-- excerpt -->
---

[Lisp Game Jam 2019](https://itch.io/jam/lisp-game-jam-2019) is over and there
are quite a number of submissions based on
[`trivial-gamekit`](https://github.com/borodust/trivial-gamekit) this time:
* [Decent Magician Of the Moldy Dungeon](https://itch.io/jam/lisp-game-jam-2019/rate/415397)
* [NOTALONE: AGAIN](https://itch.io/jam/lisp-game-jam-2019/rate/415347)
* [Pong Fight](https://itch.io/jam/lisp-game-jam-2019/rate/413733)
* [Split-Shot](https://itch.io/jam/lisp-game-jam-2019/rate/413829)
* [Lispout](https://itch.io/jam/lisp-game-jam-2019/rate/410429)

Thanks to everyone who participated and I'm especially grateful to authors of
above games for their trust in `gamekit`. Any jam is a stress and having solid
tools during this time is very important. Thank you for giving `gamekit` a
chance!

I've received a couple questions about `trivial-gamekit` future and what
direction it is heading.

Here's my answer.

<!-- excerpt -->

With latest additions to `gamekit` (gamepad support and customizable rendering
and logic update rates) I think its API is rich enough while still quite simple
to use. Backward compatibility is a very important goal: no change will be made
in public `gamekit` API that would break existing projects based on it.

All this means it is unlikely `gamekit` itself would see any substantial API
changes in the future. I think it serves its purpose very well right now and
will in the future. In no sense this means `gamekit` is abandoned - if there are
bugs or you think it still misses an essential feature - fire an issue or
contact me directly and we will resolve all those problems together.

At this point you might have thought that this is a bit sad news - `gamekit` has
so much potential. You are right! And here goes new chapter in `trivial-gamekit`
development :)

Instead of extending `gamekit` API indefinitely (making it less and less
suitable for simple games, much harder to understand and derailing it farther
away from its initial goals), new features, experimental or not, are going to be
implemented as plugins!

One day while looking into ways to handle complexity in `gamekit`-based games I
discovered it actually is quite extendable. Just inherit a class in `defgame`
form with special functionality you need, hook into `gamekit` instance methods
(`#'draw`, `#'act`, `#'post-initialize` and `#'pre-destroy`), provide some
`:default-initargs` and you can go far in extending its functionality.

So here I present a new way of extending `gamekit` - _kinda_ plugins. And first
of the kind I found useful for jam games:

[trivial-gamekit-fistmachine](https://github.com/borodust/trivial-gamekit-fistmachine)
: Simple finite-state machine implementation for handling state transitions in
  `gamekit`-based games

[trivial-gamekit-input-handler](https://github.com/borodust/trivial-gamekit-input-handler)
: Utility class for switching and controlling input in `trivial-gamekit` - handy
  for enabling and disabling different control schemes at any point of time,
  especially when transitioning between game states (menu->play->pause->play etc)

[trivial-gamekit-postproc](https://github.com/borodust/trivial-gamekit-postproc)
: Shader-based post-processing in `gamekit`! Also allows you to change
  resolution of the underlying framebuffer, meaning you can render your game
  undersampled (true unblurred 256×224 resolution on HiDPI displays anyone?) or
  supersampled (8k on FullHD displays - let the world burn in gpu heat)

More to come! :)

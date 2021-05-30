---
layout: post
title: :claw honing - Beta milestone and alien-works
excerpt_separator: <div id="excerpt"></div>
---

Long time no see my C++ autowrapping rants. But several bindings later, `:claw`
has reached the stage where it is ready to leave a garage and see the outside
world. Since my last post, some approaches were revised, though the autowrapping
process is still not fully cemented yet. I wouldn't expect `:claw` to be out of
beta for at least a year. That doesn't mean it is unusable, but rather I cannot
guarantee a stable interface and a trivial procedure for setting it up.

In other big news, `:alien-works` system got
[all](https://github.com/borodust/alien-works/tree/b3cd069c0dfddf75fb0cf8f77065a08a537011cc#foundation)
required foreign libraries wrapped and integrated, including some complex and
peculiar C++ ones ([Skia](https://github.com/google/skia) 👀). Next step is to
write a game, based on `:alien-works` framework, to see how much lispification
of autowrapped systems is possible without loosing any performance and what is
required for a solid game delivery.

<div id="excerpt"></div>

## claw

`:claw` had roots in `cl-autowrap`, but at the current state it has nothing in
common with it, except for both being an automatic bindings generators for
foreign libraries.

`:claw`, unlike `:cl-autowrap`, doesn't use intermediate representation for
foreign code (`cl-autowrap` uses JSON descriptors) and directly generates Common
Lisp
[sources](https://github.com/borodust/claw-sdl/blob/115adfd0d0814b133fedfd927ff7e730b77c047d/bindings/x86_64-pc-linux-gnu.lisp)
you can load[^1]. Generated sources doesn't have any dependency on `:claw` system
itself.

For C bindings, those sources contain pure
[`CFFI`](https://common-lisp.net/project/cffi/) definitions and no funky
home-grown DSL. So to load them you only need to ensure `CFFI` is present in an
image. Conveniently, `:claw` generates minimal `ASDF` definition[^2] to load
bindings and everything required.

`CFFI` doesn't support C++ idiosyncrasies and home-grown DSL to describe C++
constructs is, unfortunately, required. Main problem - C++ function overloading
and non-standardized name mangling. Function overloading in particular is not
supported by Common Lisp (for the best, probably). Hence
[`IFFI`](https://github.com/borodust/claw/blob/e1c36fcfec9cf7bce47ab4b3cfe6f8ee19d5722f/iffi.asd)
system was born. `IFFI` is really just a tiny layer over `CFFI` to simplify
calling C++ functions and working with classes.

Another significant difference between `cl-autowrap` and `:claw` is that the
latter includes feature to generate C shim to call foreign code that is not
directly invocable from Lisp image. E.g. to call C functions that pass structs
by value more or less reliably, you often need to use `libffi` (some CL
implementation support that case natively, but this feature is far from being
common). That's what `cl-autowrap` does in this situation - fallbacks on
`libffi`. My primary goal with `:claw` is to wrap libraries to use in tight game
loops - `libffi` call overhead is atrocious, that's a full stop for me - you
can't use `libffi`-backed functions in those loops. Instead, `:claw` generates C
shim -
[library](https://github.com/borodust/claw-sdl/blob/115adfd0d0814b133fedfd927ff7e730b77c047d/src/lib/adapter.x86_64-pc-linux-gnu.c)
in portable C that calls into actual library where needed. For C libraries, C
shim helps to call those functions that use struct-by-value arguments. For C++
libraries, well, C shim helps to call anything, because calling any C++ directly
is not supported on most Common Lisp implementations (not sure, but maybe
`clasp` can do something useful). This introduces minimal to no overhead,
depending on what kind of shim you want - `:claw` can generate C shims for both
dynamic and static linking with target library. Just in case, for C libraries,
because `:claw` uses native `CFFI`, you can use `cffi-libffi` out of the box and
don't bother with C shims at all.

Performance is of paramount importance for `:claw`-generated bindings. `:claw`
generates thinniest of possible wrappers with minimal lispification by design.

## alien-works

`:alien-works` passed proof-of-concept stage, demonstrating that many (any?)
game-related foreign libraries, C and C++ alike, can be used together with
`:claw` from Common Lisp without life-sucking effort of writing or rewriting
everything manually. This should save me some man-decades of development time,
giving access to quality "industry standard" libraries and performance while
staying within Common Lisp comfort zone.

[Filament](https://github.com/google/filament) from Common Lisp:
<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts allow-popups" src="https://peertube.social/videos/embed/0fafcb0f-20c9-411f-8919-31c0c0725b96" frameborder="0" allowfullscreen></iframe>

[Skia](https://github.com/google/skia) drawing into a texture which is then rendered with Filament:
<iframe width="560" height="315" sandbox="allow-same-origin allow-scripts allow-popups" src="https://peertube.social/videos/embed/723669b8-f3f1-47ac-9769-b8b2b66ab953" frameborder="0" allowfullscreen></iframe>

## What's next

I need to mainline `:claw` `cxx` branch and write initial documentation, so
people interested could engage with the tool. Existing `:claw`-based wrappers
will need some love adapting to new interface.


`:alien-works` needs something more than a proof-of-concept and that would be a
shippable game. It can be simple, but it must be done. That should take care of
revealing quirks that needs to be smoothed out before the system can actually be
used as a foundation for any game. This also should help with figuring out a
level of lispification that can be introduced over foreign foundation
`:alien-works` is based on without sacrificing any performance.


[^1]: It also uses different means to parse C/C++ headers -
    [libresect](https://github.com/borodust/libresect) library and Common Lisp
    [bindings](https://github.com/borodust/cl-resect) for it instead of
    [`c2ffi`](https://github.com/rpav/c2ffi). It is based on `libclang` which
    has much more stable interface than `llvm/clang` C++ internals and,
    theoretically, you don't need to update it much (if at all) with every new
    `clang` release. `libresect` allows inspecting C/C++ definitions directly
    from Common Lisp.


[^2]: e.g. for
    [SDL2](https://github.com/borodust/claw-sdl/blob/115adfd0d0814b133fedfd927ff7e730b77c047d/claw-sdl-bindings.asd) -
    `:claw-utils` dependency here is explicitly requested by me in [wrapper
    descriptor](https://github.com/borodust/claw-sdl/blob/115adfd0d0814b133fedfd927ff7e730b77c047d/src/claw.lisp#L14)
    and not required

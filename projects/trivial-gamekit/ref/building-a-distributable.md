*function* ***`deliver`*** `(system-name game-class &key build-directory (zip *zip*) (lisp *lisp*))`
{: #gamekit-deliver}
<div class="bodge-docstring" markdown="block">
Builds an executable, serializes resources and packs required foreign libraries into a .zip
archive for distribution. `system-name` is a name of `asdf` system of your application and
`game-class` is a game class defined with [`defgame`](#gamekit-defgame) (the one that could be
passed to [`#'start`](#gamekit-start) to start your game). By default, it builds all artifacts
into `build/` directory relative to `system-name` system path, but you can pass any other path
to `:build-directory` key argument to put target files into it instead.

This routine uses `zip` and `lisp` ('sbcl' [Steel Bank Common Lisp](http://sbcl.org) is the
default) to build a distributable package on various platforms. If those executables are not on
your system's `PATH`, you would need to provide absolute paths to them via `:zip` and `:lisp`
key arguments accordingly.

You can load this function into an image via `:trivial-gamekit/distribution` system.

Example:
```common-lisp
(ql:quickload :trivial-gamekit/distribution)
(gamekit.distribution:deliver :example-asdf-system 'example-package::example
                              :build-directory "/tmp/example-game/"
                              :zip "/usr/bin/zip"
                              :lisp "/usr/bin/sbcl")
```
</div>

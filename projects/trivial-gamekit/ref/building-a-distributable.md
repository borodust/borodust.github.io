*function* ***`deliver`*** `(system-name game-class &key build-directory (zip *zip*) (sbcl *sbcl*))`
{: #gamekit-deliver}
<div class="bodge-docstring" markdown="block">
Builds an executable, serializes resources and packs required foreign libraries into a .zip
archive for distribution. `system-name` is a name of `asdf` system of your application and
`game-class` is a game class defined with [`defgame`](#gamekit-defgame) (the one that could be
passed to [`#'start`](#gamekit-start) to start your game). By default, it builds all artifacts
into `build/` directory relative to `system-name` system path, but you can pass any other path
to `:build-directory` key argument to put target files into it instead.

This routine uses `zip` and `sbcl` ([Steel Bank Common Lisp](http://sbcl.org)) to build a
distributable package on various platforms. If those executables are not on your system's
`PATH`, you would need to provide absolute paths to them via `:zip` and `:sbcl` key arguments
accordingly.

Example:
```common_lisp
(gamekit.distribution:deliver :example-asdf-system 'example-package::example
                              :build-directory "/tmp/example-game/"
                              :zip "/usr/bin/zip"
                              :sbcl "/usr/bin/sbcl")
```
</div>


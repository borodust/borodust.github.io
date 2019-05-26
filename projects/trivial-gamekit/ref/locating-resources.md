*function* ***`register-resource-package`*** `(package-name path)`
{: #gamekit-register-resource-package}
<div class="bodge-docstring" markdown="block">
Associates resource package with filesystem path. For proper resource
handling it is recommended to put it as a top-level form, so resources could be
located at load-time.

First argument, a package name, must be a valid Common Lisp package name that
could be used to locate package via #'find-package. Second argument is a
filesystem path to a directory where resources can be found.

Example:
```common-lisp
 (gamekit:register-resource-package :example-package
                                    "/home/gamdev/example-game/assets/")
```
</div>

*macro* ***`define-image`*** `(name path &key use-nearest-interpolation)`
{: #gamekit-define-image}
<div class="bodge-docstring" markdown="block">
Registers image resource by name that can be used by
[`#'draw-image`](#gamekit-draw-image) later. Second argument is a valid path to
the resource.  Only .png images are supported at this moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-image 'example-package::logo "images/logo.png")
```
</div>

*macro* ***`define-sound`*** `(name path)`
{: #gamekit-define-sound}
<div class="bodge-docstring" markdown="block">
Registers sound resource by name that can be used by [`#'play-sound`](#gamekit-play-sound) later.
Second argument is a valid path to the resource.  Formats supported: .wav,
.ogg (Vorbis), .flac, .aiff.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-sound 'example-package::blop "sounds/blop.ogg")
```
</div>

*macro* ***`define-font`*** `(name path)`
{: #gamekit-define-font}
<div class="bodge-docstring" markdown="block">
Registers font resource by name that can be passed to [`#'make-font`](#gamekit-make-font) later.
Second argument is a valid path to the resource. Only .ttf format is supported
at this moment.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-font 'example-package::noto-sans "fonts/NotoSans-Regular.ttf")
```
</div>

*macro* ***`define-text`*** `(name path &key encoding)`
{: #gamekit-define-text}
<div class="bodge-docstring" markdown="block">
Registers text resource by name that can be retrieved with [`#'get-text`](#gamekit-get-text) later.
Second argument is a valid path to the resource. You can specify encoding via
`:encoding` keywrod argument. `:utf-8` is used by default.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-text 'example-package::example-text "dialog.txt" :encoding :utf-8)
```
</div>

*macro* ***`define-binary`*** `(name path)`
{: #gamekit-define-binary}
<div class="bodge-docstring" markdown="block">
Registers binary resource by name that can be retrieved with [`#'get-binary`](#gamekit-get-binary) later.
Second argument is a valid path to the resource.

Name must be a symbol. Package of that symbol and its associated path (via
[`#'register-resource-package`](#gamekit-register-resource-package)) will be
used to locate the resource, if relative path is given as an argument to this
macro.

Example:
```common-lisp
 (gamekit:define-binary 'example-package::example-blob "blob.data")
```
</div>

*function* ***`make-font`*** `(font-id size)`
{: #gamekit-make-font}
<div class="bodge-docstring" markdown="block">
Makes a font instance that can be later passed to [`#'draw-text`](#gamekit-draw-text) to
customize text looks. `font-id` must be a valid resource name previously registered with
[`define-font`](#gamekit-define-font). Second argument is a font size in pixels.

Example:
```common-lisp
 (gamekit:make-font 'example-package::noto-sans 32)
```
</div>

*function* ***`prepare-resources`*** `(&rest resource-names)`
{: #gamekit-prepare-resources}
<div class="bodge-docstring" markdown="block">
Loads and prepares resources for later usage asynchronously. `resource-names`
should be symbols used previously registered with `define-*` macros.

This function returns immediately. When resources are ready for use
[`#'notice-resources`](#gamekit-notice-resources) will be called with names that
were passed to this function.

`gamekit` by default will try to load and prepare all registered resources on
startup which might take a substantial time, but then you don't need to call
#'prepare-resources yourself. If you prefer load resources on demand and have a
faster startup time, pass nil to :prepare-resources option of a
[`defgame`](#gamekit-defgame) macro which will disable startup resource
autoloading.

Example:
```common-lisp
 (gamekit:prepare-resources 'example-package::noto-sans
                            'example-package::blop
                            'example-package::logo)
```
</div>

*generic* ***`notice-resources`*** `(game &rest resource-names)`
{: #gamekit-notice-resources}
<div class="bodge-docstring" markdown="block">
Called when resource names earlier requested with
[`#'prepare-resources`](#gamekit-prepare-resources) which indicates those
resources are ready to be used.

Override this generic function to know when resources are ready.

Example:
```common-lisp
 (defmethod gamekit:notice-resources ((this example) &rest resource-names)
   (declare (ignore resource-names))
   (gamekit:play-sound 'example-package::blop)
   (show-start-screen))
```
</div>

*function* ***`get-text`*** `(resource-id)`
{: #gamekit-get-text}
<div class="bodge-docstring" markdown="block">
Get text resource (a string) by id. `resource-id` must be a valid resource id
previously registered with [`'define-text`](#gamekit-define-text).

```common-lisp
 (gamekit:get-text 'example-package::example-text)
```
</div>

*function* ***`get-binary`*** `(resource-id)`
{: #gamekit-get-binary}
<div class="bodge-docstring" markdown="block">
Get binary resource (a byte vector) by id. `resource-id` must be a valid
resource id previously registered with [`'define-binary`](#gamekit-define-binary).

```common-lisp
 (gamekit:get-binary 'example-package::example-blob)
```
</div>


*function* ***`draw-line`*** `(origin end paint &key (thickness 1.0))`
{: #gamekit-draw-line}
<div class="bodge-docstring" markdown="block">
Draws a line starting from coordinates passed as first argument to
coordinates in second parameter. Third parameter is a color to draw a line
with. `:thickness` is a scalar floating point value controlling pixel-width of a
line.

Example:
```common-lisp
 (gamekit:draw-line (gamekit:vec2 8 5) (gamekit:vec2 32 11)
                   (gamekit:vec4 1 0.5 0 1)
                   :thickness 1.5)
```
</div>

*function* ***`draw-curve`*** `(origin end ctrl0 ctrl1 paint &key (thickness 1.0))`
{: #gamekit-draw-curve}
<div class="bodge-docstring" markdown="block">
Draws a bezier curve from coordinates passed as first argument to coordinates
in second parameter with two control points in third and fourth parameters
accordingly. Fifth argument is a curve's color. `:thickness` is a scalar
floating point value controlling pixel-width of a curve.

Example:
```common-lisp
 (gamekit:draw-line (gamekit:vec2 8 5) (gamekit:vec2 32 11)
                   (gamekit:vec2 0 5) (gamekit:vec2 32 0)
                   (gamekit:vec4 1 0.5 0 1)
                   :thickness 1.5)
```
</div>

*function* ***`draw-rect`*** `(origin w h &key (fill-paint nil) (stroke-paint nil) (thickness 1.0) (rounding
                                                                      0.0))`
{: #gamekit-draw-rect}
<div class="bodge-docstring" markdown="block">
Draws a rectangle with origin passed in first argument, width and height -
second and third arguments accordingly. `:fill-paint` key is a color to fill
insides of a rectangle with. If you pass color to `:stroke-paint`, edges of the
rectangle are going to be struck with it. `:thickness` controls pixel width of
struck edges. Use `:rounding` in pixels to round rectangle corners.

Example:
```common-lisp
 (gamekit:draw-rect (gamekit:vec2 0 0) 314 271
                   :fill-paint (gamekit:vec4 1 0.75 0.5 0.5)
                   :stroke-paint (gamekit:vec4 0 0 0 1)
                   :rounding 5.0)
```
</div>

*function* ***`draw-circle`*** `(center radius &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))`
{: #gamekit-draw-circle}
<div class="bodge-docstring" markdown="block">
Draws a circle with center in first argument and radius in second argument.
Provide color with `:fill-paint` paramater to fill the inner area of the circle
with. If `:stroke-paint` color is provided, circle's border is going to be
struck with it. `:thickness` controls pixel width of struck border.

Example:
```common-lisp
 (gamekit:draw-circle (gamekit:vec2 100 500) 3/4
                     :fill-paint (gamekit:vec4 1 1 1 1)
                     :stroke-paint (gamekit:vec4 0 0 0 1)
                     :thickness 3)
```
</div>

*function* ***`draw-ellipse`*** `(center x-radius y-radius &key (fill-paint nil) (stroke-paint nil) (thickness
                                                                    1.0))`
{: #gamekit-draw-ellipse}
<div class="bodge-docstring" markdown="block">
Draws an ellipse with center provided in first argument, x and y radii as
second and thrid arguments accordingly. Pass a color as `:fill-paint` paramater
to fill the inner area of the ellipse with. If `:stroke-paint` color is
provided, ellipse's border will be struck with it. `:thickness` controls pixel
width of struck border.

Example:
```common-lisp
 (gamekit:draw-ellipse (gamekit:vec2 128 128) 16 32
                      :fill-paint (gamekit:vec4 0 0 0 1)
                      :stroke-paint (gamekit:vec4 1 1 1 1)
                      :thickness 1.1)
```
</div>

*function* ***`draw-arc`*** `(center radius a0 a1 &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))`
{: #gamekit-draw-arc}
<div class="bodge-docstring" markdown="block">
Draws an arc from `a0` to `a1` angles (in radians) with center passed in
first argument and radius in second. If provided, color in `:fill-paint` will be
used to fill the area under an arc confined between a circle's curve and a line
connecting angle points. `:fill-paint` and `:stroke-paint` colors are, if
provided, used to fill insides and stroke arc's border correspondingly.

Example:
```common-lisp
 (gamekit:draw-arc (gamekit:vec2 256 256) 128
                  (/ pi 4) (* (/ pi 2) 1.5)
                  :fill-paint (gamekit:vec4 0.25 0.5 0.75 1)
                  :stroke-paint (gamekit:vec4 0.75 0.5 0.25 1)
                  :thickness 2.0)
```
</div>

*function* ***`draw-polygon`*** `(vertices &key (fill-paint nil) (stroke-paint nil) (thickness 1.0))`
{: #gamekit-draw-polygon}
<div class="bodge-docstring" markdown="block">
Draws a polygon connecting list of vertices provided in first
argument. `:fill-paint` is a color to fill insides of a polygon and
`:stroke-paint` color is used to stroke polygon edges. `:thickness` controls
pixel-width of a stroke.

Example:
```common-lisp
 (gamekit:draw-polygon (list (gamekit:vec2 10 10) (gamekit:vec2 20 20)
                            (gamekit:vec2 30 20) (gamekit:vec2 20 10))
                      :fill-paint (gamekit:vec4 0.25 0.5 0.75 1)
                      :stroke-paint (gamekit:vec4 0.75 0.5 0.25 1)
                      :thickness 3.0)
```
</div>

*function* ***`draw-polyline`*** `(points paint &key (thickness 1.0))`
{: #gamekit-draw-polyline}
<div class="bodge-docstring" markdown="block">
Draws a polyline connecting list of vertices provided in first
argument. Second argument is a color to stroke a line with. `:thickness`
controls pixel width of a line.

Example:
```common-lisp
 (gamekit:draw-polyline (list (gamekit:vec2 10 10) (gamekit:vec2 20 20)
                             (gamekit:vec2 30 20) (gamekit:vec2 20 10))
                       (gamekit:vec4 0.75 0.5 0.25 1)
                       :thickness 3.0)
```
</div>

*function* ***`draw-image`*** `(position image-id &key origin width height)`
{: #gamekit-draw-image}
<div class="bodge-docstring" markdown="block">
Draws an image at coordinates specified in first argument. Second argument is
`image-id` used in [`#'define-image`](#gamekit-define-image) earlier. Optional
`:origin` key is a point within image to start drawing from, if you want to
render only a part of image. `:width` and `:height` keys tell width and height
of a subimage to draw. They are optional and could be skipped to draw a subimage
with full height and width available.

Example:
```common-lisp
 (gamekit:draw-image (gamekit:vec2 314 271) 'example-package::logo
                    :origin (gamekit:vec2 0 0)
                    :width 320
                    :height 240)
```
</div>

*function* ***`draw-text`*** `(string origin &key (fill-color *black*) (font *font*))`
{: #gamekit-draw-text}
<div class="bodge-docstring" markdown="block">
Draws text on the canvas starting at coordinates passed as second argument.
Use `:fill-color` key parameter to change text's color. To change a font, pass
object created with [`#'make-font`](#gamekit-make-font) via `:font` parameter.

Example:
```common-lisp
 (gamekit:draw-text "Hello, Gamekit!" (gamekit:vec2 11 23)
                   :fill-color (gamekit:vec4 0 0 0 1)
                   :font (gamekit:make-font 'example-package::noto-sans 32))
```
</div>

*function* ***`translate-canvas`*** `(x y)`
{: #gamekit-translate-canvas}
<div class="bodge-docstring" markdown="block">
Moves drawing origin to the specified position making the latter a new
origin. All following draw operations will be affected by this change unless
wrapped with [`with-pushed-canvas`](#gamekit-with-pushed-canvas) macro.

Example:
```common-lisp
 (gamekit:translate-canvas 100 500)
```
</div>

*function* ***`rotate-canvas`*** `(angle)`
{: #gamekit-rotate-canvas}
<div class="bodge-docstring" markdown="block">
Rotates current canvas for specified number of radians. All following drawing
operations will be affected by this change unless wrapped with
[`with-pushed-canvas`](#gamekit-with-pushed-canvas) macro.

Example:
```common-lisp
 (gamekit:rotate-canvas (/ pi 2))
```
</div>

*function* ***`scale-canvas`*** `(x y)`
{: #gamekit-scale-canvas}
<div class="bodge-docstring" markdown="block">
Scales current canvas by x and y axes accordingly. All following drawing
operations will be affected by this change unless wrapped with
[`with-pushed-canvas`](#gamekit-with-pushed-canvas) macro.

Example:
```common-lisp
 (gamekit:scale-canvas 0.5 1.5)
```
</div>

*macro* ***`with-pushed-canvas`*** `(nil &body body)`
{: #gamekit-with-pushed-canvas}
<div class="bodge-docstring" markdown="block">
Saves current canvas transformations (translations, rotations, scales) before
entering its body and restores previous transformations upon exit from the
body. All transformation operations within this macro don't affect outer canvas
transformations outside of a body of this macro.

Example:
```common-lisp
 (gamekit:translate-canvas 400 300)
 (gamekit:with-pushed-canvas ()
   (gamekit:rotate-canvas (/ pi 4)))
```
</div>

*function* ***`image-width`*** `(image-id)`
{: #gamekit-image-width}
<div class="bodge-docstring" markdown="block">
Returns width of an image by its id (defined with
 [`#'define-image`](#gamekit-define-image)).

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example:
```common-lisp
 (gamekit:image-width 'example-package::logo)
```
</div>

*function* ***`image-height`*** `(image-id)`
{: #gamekit-image-height}
<div class="bodge-docstring" markdown="block">
Returns height of an image by its id (defined with
 [`#'define-image`](#gamekit-define-image)).

Can only be called when gamekit instance is active (started via
[`#'start`](#gamekit-start)).

Example:
```common-lisp
 (gamekit:image-height 'example-package::logo)
```
</div>

*function* ***`calc-text-bounds`*** `(text &optional (font *font*))`
{: #gamekit-calc-text-bounds}
<div class="bodge-docstring" markdown="block">
Calculates text bounds with the font provided or the default one otherwise and returns
several values: origin as vec2, width, height and calculated advance

Example:
```common-lisp
(gamekit:calc-text-bounds "hello there")
```
</div>


*function* ***`draw-line`*** `(origin end paint &key (thickness 1.0) (canvas *canvas*))`
{: #gamekit-draw-line}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-curve`*** `(origin end ctrl0 ctrl1 paint &key (thickness 1.0) (canvas *canvas*))`
{: #gamekit-draw-curve}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-rect`*** `(origin w h &key (fill-paint nil) (stroke-paint nil) (thickness 1.0) (rounding
                                                                      0.0) (canvas
                                                                            *canvas*))`
{: #gamekit-draw-rect}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-circle`*** `(center radius &key (fill-paint nil) (stroke-paint nil) (thickness 1.0) (canvas
                                                                         *canvas*))`
{: #gamekit-draw-circle}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-ellipse`*** `(center x-radius y-radius &key (fill-paint nil) (stroke-paint nil) (thickness
                                                                    1.0) (canvas
                                                                          *canvas*))`
{: #gamekit-draw-ellipse}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-arc`*** `(center radius a0 a1 &key (fill-paint nil) (stroke-paint nil) (thickness 1.0) (canvas
                                                                               *canvas*))`
{: #gamekit-draw-arc}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-polygon`*** `(vertices &key (fill-paint nil) (stroke-paint nil) (thickness 1.0) (canvas
                                                                    *canvas*))`
{: #gamekit-draw-polygon}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-polyline`*** `(points paint &key (thickness 1.0) (canvas *canvas*))`
{: #gamekit-draw-polyline}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-image`*** `(position image-id &key origin width height)`
{: #gamekit-draw-image}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`draw-text`*** `(string origin &key (fill-color *black*) (font *font*))`
{: #gamekit-draw-text}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`translate-canvas`*** `(x y &key (canvas *canvas*))`
{: #gamekit-translate-canvas}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`rotate-canvas`*** `(angle &key (canvas *canvas*))`
{: #gamekit-rotate-canvas}
<div class="bodge-docstring" markdown="block">

</div>

*function* ***`scale-canvas`*** `(x y &key (canvas *canvas*))`
{: #gamekit-scale-canvas}
<div class="bodge-docstring" markdown="block">

</div>

*macro* ***`with-pushed-canvas`*** `((&optional canvas) &body body)`
{: #gamekit-with-pushed-canvas}
<div class="bodge-docstring" markdown="block">

</div>


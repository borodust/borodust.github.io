*function* ***`vec2`*** `(&optional (x 0.0) (y 0.0))`
{: #gamekit-vec2}
<div class="bodge-docstring" markdown="block">
Makes a two-dimensional vector.

Example:
```common_lisp
(gamekit:vec2 0 0)
```
</div>

*function* ***`vec3`*** `(&optional (x 0.0) (y 0.0) (z 0.0))`
{: #gamekit-vec3}
<div class="bodge-docstring" markdown="block">
Makes a three-dimensional vector.

Example:
```common_lisp
(gamekit:vec3 1 1 2)
```
</div>

*function* ***`vec4`*** `(&optional (x 0.0) (y 0.0) (z 0.0) (w 0.0))`
{: #gamekit-vec4}
<div class="bodge-docstring" markdown="block">
Makes a four-dimensional vector.

Example:
```common_lisp
(gamekit:vec4 1 1 2 3)
```
</div>

*function* ***`mult`*** `(arg0 &rest args)`
{: #gamekit-mult}
<div class="bodge-docstring" markdown="block">
Element-wise multiplication. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:mult 2 (gamekit:vec2 1 1) 0.5)
```
</div>

*function* ***`add`*** `(arg0 &rest args)`
{: #gamekit-add}
<div class="bodge-docstring" markdown="block">
Element-wise addition. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:add 1 (gamekit:vec2 1 1) -1)
```
</div>

*function* ***`subt`*** `(arg0 &rest args)`
{: #gamekit-subt}
<div class="bodge-docstring" markdown="block">
Element-wise subtraction. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:subt 1 (gamekit:vec2 1 1) (gamekit:vec2 -1 -1))
```
</div>

*function* ***`div`*** `(arg0 &rest args)`
{: #gamekit-div}
<div class="bodge-docstring" markdown="block">
Element-wise division. Accepts both vectors and scalars.

Example:
```common_lisp
(gamekit:div (gamekit:vec2 1 1) 2 (gamekit:vec2 0.5 0.5))
```
</div>

*function* ***`x`*** `(vec)`
{: #gamekit-x}
<div class="bodge-docstring" markdown="block">
Reads first element of a vector.

Example:
```common_lisp
(gamekit:x (gamekit:vec2 1 1))
```
</div>

*function* ***`(setf x)`*** `(value vec)`
{: #gamekit-setf-x}
<div class="bodge-docstring" markdown="block">
Stores first element of a vector.

Example:
```common_lisp
(setf (gamekit:x (gamekit:vec2 1 1)) 0)
```
</div>

*function* ***`y`*** `(vec)`
{: #gamekit-y}
<div class="bodge-docstring" markdown="block">
Reads second element of a vector.

Example:
```common_lisp
(gamekit:y (gamekit:vec2 1 1))
```
</div>

*function* ***`(setf y)`*** `(value vec)`
{: #gamekit-setf-y}
<div class="bodge-docstring" markdown="block">
Stores second element of a vector.

Example:
```common_lisp
(setf (gamekit:y (gamekit:vec2 1 1)) 0)
```
</div>

*function* ***`z`*** `(vec)`
{: #gamekit-z}
<div class="bodge-docstring" markdown="block">
Reads third element of a vector.

Example:
```common_lisp
(gamekit:z (gamekit:vec4 1 1 2 3))
```
</div>

*function* ***`(setf z)`*** `(value vec)`
{: #gamekit-setf-z}
<div class="bodge-docstring" markdown="block">
Stores third element of a vector.

Example:
```common_lisp
(setf (gamekit:z (gamekit:vec4 1 1 2 3)) 0)
```
</div>

*function* ***`w`*** `(vec)`
{: #gamekit-w}
<div class="bodge-docstring" markdown="block">
Reads fourth element of a vector.

Example:
```common_lisp
(gamekit:w (gamekit:vec4 1 1 2 3))
```
</div>

*function* ***`(setf w)`*** `(value vec)`
{: #gamekit-setf-w}
<div class="bodge-docstring" markdown="block">
Stores fourth element of a vector.

Example:
```common_lisp
(setf (gamekit:w (gamekit:vec4 1 1 2 3)) 0)
```
</div>


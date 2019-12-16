module Playground.Transformation exposing (Transformation, apply, identity, makeRotate, makeScale, makeTranslate, scale, toGL, transform)

import Math.Vector2
import Math.Vector4


type alias Transformation =
    { a11 : Float
    , a12 : Float
    , a13 : Float
    , a21 : Float
    , a22 : Float
    , a23 : Float
    }


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Transformation
identity =
    { a11 = 1
    , a12 = 0
    , a13 = 0
    , a21 = 0
    , a22 = 1
    , a23 = 0
    }


{-| Matrix multiplcation: a \* b
-}
apply : Transformation -> Transformation -> Transformation
apply a b =
    { a11 = a.a11 * b.a11 + a.a12 * b.a21
    , a12 = a.a11 * b.a12 + a.a12 * b.a22
    , a13 = a.a11 * b.a13 + a.a12 * b.a23 + a.a13
    , a21 = a.a21 * b.a11 + a.a22 * b.a21
    , a22 = a.a21 * b.a12 + a.a22 * b.a22
    , a23 = a.a21 * b.a13 + a.a22 * b.a23 + a.a23
    }


transform : Float -> Float -> Float -> Float -> Float -> Transformation
transform tx ty sx sy angle =
    { a11 = cos angle * sx
    , a12 = sin angle * -sy
    , a13 = tx
    , a21 = sin angle * sx
    , a22 = cos angle * sy
    , a23 = ty
    }


scale : Float -> Float -> Transformation -> Transformation
scale sx sy b =
    { a11 = sx * b.a11
    , a12 = sx * b.a12
    , a13 = sx * b.a13
    , a21 = sy * b.a21
    , a22 = sy * b.a22
    , a23 = sy * b.a23
    }



--https://mrl.nyu.edu/~dzorin/ig04/lecture05/lecture05.pdf


{-| Creates a transformation matrix for rotation in radians.
-}
makeRotate : Float -> Transformation
makeRotate angle =
    Transformation (cos angle) (sin angle * -1) 0 (sin angle) (cos angle) 0


{-| Creates a transformation matrix for scaling each of the x and y by the amount.
-}
makeScale : Float -> Float -> Transformation
makeScale sx sy =
    Transformation sx 0 0 0 sy 0


{-| Creates a transformation matrix for translating each of the x, y, and z axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate : Float -> Float -> Transformation
makeTranslate tx ty =
    Transformation 1 0 tx 0 1 ty


toGL : Transformation -> ( Math.Vector4.Vec4, Math.Vector2.Vec2 )
toGL { a11, a12, a13, a21, a22, a23 } =
    ( Math.Vector4.fromRecord { x = a11, y = a12, z = a21, w = a22 }, Math.Vector2.fromRecord { x = a13, y = a23 } )

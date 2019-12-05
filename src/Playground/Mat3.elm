module Playground.Mat3 exposing (Mat3, identity, makeRotate, makeScale, makeTranslate, mul, toGL)

import Math.Vector2
import Math.Vector4


type alias Mat3 =
    { a11 : Float
    , a12 : Float
    , a13 : Float
    , a21 : Float
    , a22 : Float
    , a23 : Float
    , a31 : Float
    , a32 : Float
    , a33 : Float
    }


{-| A matrix with all 0s, except 1s on the diagonal.
-}
identity : Mat3
identity =
    { a11 = 1
    , a12 = 0
    , a13 = 0
    , a21 = 0
    , a22 = 1
    , a23 = 0
    , a31 = 0
    , a32 = 0
    , a33 = 1
    }


{-| Matrix multiplcation: a \* b
-}
mul : Mat3 -> Mat3 -> Mat3
mul a b =
    { a11 = a.a11 * b.a11 + a.a12 * b.a21 + a.a13 * b.a31
    , a12 = a.a11 * b.a12 + a.a12 * b.a22 + a.a13 * b.a32
    , a13 = a.a11 * b.a13 + a.a12 * b.a23 + a.a13 * b.a33
    , a21 = a.a21 * b.a11 + a.a22 * b.a21 + a.a23 * b.a31
    , a22 = a.a21 * b.a12 + a.a22 * b.a22 + a.a23 * b.a32
    , a23 = a.a21 * b.a13 + a.a22 * b.a23 + a.a23 * b.a33
    , a31 = a.a31 * b.a11 + a.a32 * b.a21 + a.a33 * b.a31
    , a32 = a.a31 * b.a12 + a.a32 * b.a22 + a.a33 * b.a32
    , a33 = a.a31 * b.a13 + a.a32 * b.a23 + a.a33 * b.a33
    }



--https://mrl.nyu.edu/~dzorin/ig04/lecture05/lecture05.pdf


{-| Creates a transformation matrix for rotation in radians.
-}
makeRotate : Float -> Mat3
makeRotate angle =
    Mat3 (cos angle) (sin angle * -1) 0 (sin angle) (cos angle) 0 0 0 1


{-| Creates a transformation matrix for scaling each of the x and y by the amount.
-}
makeScale : Float -> Float -> Mat3
makeScale sx sy =
    Mat3 sx 0 0 0 sy 0 0 0 1


{-| Creates a transformation matrix for translating each of the x, y, and z axes by the amount given in the corresponding element of the 3-element vector.
-}
makeTranslate tx ty =
    Mat3 1 0 tx 0 1 ty 0 0 1


toGL { a11, a12, a13, a21, a22, a23, a31, a32, a33 } =
    ( Math.Vector4.fromRecord { x = a11, y = a12, z = a21, w = a22 }, Math.Vector2.fromRecord { x = a13, y = a23 } )

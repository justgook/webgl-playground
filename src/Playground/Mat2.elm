module Playground.Mat2 exposing (Mat2, makeRotate, makeScale, mul)


type alias Mat2 =
    { x : Float, y : Float, z : Float, w : Float }


{-| Matrix multiplcation: a \* b
-}
mul : Mat2 -> Mat2 -> Mat2
mul m n =
    { x = m.x * n.x + m.y * n.z
    , y = m.x * n.y + m.y * n.w
    , z = m.z * n.x + m.w * n.z
    , w = m.z * n.y + m.w * n.w
    }


{-| Creates a transformation matrix for rotation in radians.
-}
makeRotate : Float -> Mat2
makeRotate angle =
    Mat2 (cos angle) (sin angle * -1) (sin angle) (cos angle)


{-| Creates a transformation matrix for scaling each of the x and y by the amount.
-}
makeScale : Float -> Float -> Mat2
makeScale sx sy =
    Mat2 sx 0 0 sy

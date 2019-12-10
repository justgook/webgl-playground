module Playground.Internal exposing (CustomCustom(..), Form(..), Number, Shape(..), initShape)

import Math.Vector2
import Math.Vector3
import Math.Vector4
import WebGL
import WebGL.Texture as Texture


type Shape
    = Shape
        { x : Number
        , y : Number
        , a : Number
        , sx : Number
        , sy : Number
        , o : Number
        , form : Form
        }


type Form
    = Polygon Color (List ( Number, Number ))
    | Custom CustomCustom
    | Group (List Shape)


type CustomCustom
    = CustomEnd Number Number Render
    | CustomTextured String (Texture.Texture -> Shape)


type alias Color =
    Math.Vector3.Vec3


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float


initShape : Form -> Shape
initShape form =
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = form }


type alias Render =
    Math.Vector2.Vec2
    -> Math.Vector4.Vec4
    -> Float
    -> WebGL.Entity

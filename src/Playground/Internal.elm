module Playground.Internal exposing (Form(..), Number, Shape(..))

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
    = Form Number Number Render
    | Textured String (Texture.Texture -> Shape)
    | Group (List Shape)


type alias Color =
    Math.Vector3.Vec3


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float


type alias Render =
    Math.Vector2.Vec2
    -> Math.Vector4.Vec4
    -> Float
    -> WebGL.Entity

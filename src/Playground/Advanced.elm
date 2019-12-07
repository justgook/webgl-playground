module Playground.Advanced exposing
    ( scaleX, scaleY
    , custom, useTexture, Render, Opacity, ScaleRotateSkew, Translate
    )

{-|


# Customize Shapes

@docs scaleX, scaleY


# Create Shape

Advanced user section, for usage in packages, or custom shader creation.

@docs custom, useTexture, Render, Opacity, ScaleRotateSkew, Translate

-}

import Math.Vector2
import Math.Vector4
import Playground.Internal exposing (CustomCustom(..), Form(..), Number, Shape(..), initShape)
import WebGL
import WebGL.Texture as Texture


{-| Make a shape **horizontally** bigger or smaller.
Also can be used to flip object:

    sprite 20 27 0 "images/mario.png"
        |> scaleX -1

-}
scaleX : Number -> Shape -> Shape
scaleX sx (Shape shape) =
    Shape { shape | sx = shape.sx * sx }


{-| Make a shape **vertically** bigger or smaller.
Also can be used to flip object:

    sprite 20 27 0 "images/mario.png"
        |> scaleX -1

-}
scaleY : Number -> Shape -> Shape
scaleY sy (Shape shape) =
    Shape { shape | sy = shape.sy * sy }


{-| Create your own shape

    redRect =
        (\translation transformation opacity ->
            WebGL.entity
                rectVertexShader
                fillFragment
                clipPlate
                { color = Math.Vector4.vec4 1 0 0 opacity
                , translation = translation
                , transformation = transformation
                }
        )
            |> custom

-}
custom : Number -> Number -> Render -> Shape
custom width height fn =
    initShape (Custom (CustomEnd width height fn))


{-| Get texture for your custom shader

    useTexture "images/mario/stand/left.gif" <|
        \t ->
            custom 32 32 <|
                myCustomRender t

-}
useTexture : String -> (Texture.Texture -> Shape) -> Shape
useTexture url fn =
    initShape (Custom (CustomTextured url fn))


{-| Function used in [`custom`](#custom)

    redRect : Render
    redRect translation transformation opacity =
        WebGL.entity
            rectVertexShader
            fillFragment
            clipPlate
            { color = Math.Vector4.vec4 1 0 0 opacity
            , translation = translation
            , transformation = transformation
            }

-}
type alias Render =
    Translate
    -> ScaleRotateSkew
    -> Opacity
    -> WebGL.Entity


{-| Vec2 representing part of transform matrix for [`custom`](#custom)

    | 1 0 x |
    | 0 1 y |
    | 0 0 1 |

-}
type alias Translate =
    Math.Vector2.Vec2


{-| Vec4 representing part of transform matrix for [`custom`](#custom)

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Math.Vector4.Vec4


{-| -}
type alias Opacity =
    Float

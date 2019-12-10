module Playground.Render exposing
    ( Render, Opacity, ScaleRotateSkew, Translate
    , rect, circle, image, ngon, tile, sprite, spriteWithColor
    , entitySettings
    )

{-|


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate


# Renders

@docs rect, circle, image, ngon, tile, sprite, spriteWithColor


# Settings

@docs entitySettings

-}

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4)
import Playground.Shader exposing (fragCircle, fragFill, fragImage, fragImageColor, fragNgon, mesh, meshTriangle, vertImage, vertNone, vertRect, vertSprite, vertTile, vertTriangle)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)


{-| Function used in [`custom`](#custom)

    redRect : Render
    redRect uP uT opacity =
        WebGL.entity
            rectVertexShader
            fillFragment
            clipPlate
            { color = Math.Vector4.vec4 1 0 0 opacity
            , uP = uP
            , uT = uT
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
    Vec2


{-| Vec4 representing part of transform matrix for [`custom`](#custom)

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Vec4


{-| -}
type alias Opacity =
    Float


{-| Rectangle render
-}
rect : Vec3 -> Render
rect color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertNone
        fragFill
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| Render circle or ellipse
-}
circle : Vec3 -> Render
circle color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertRect
        fragCircle
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| -}
ngon : Float -> Vec3 -> Render
ngon n color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertRect
        fragNgon
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , n = n
        , uT = uT
        }


{-| -}
image : Texture -> Vec2 -> Render
image image_ imageSize uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertImage
        fragImage
        mesh
        { uP = uP
        , uT = uT
        , image = image_
        , imageSize = imageSize
        }


{-| Render sprite from asymmetrical sprite sheet

Same as [`sprite`](#sprite), but with color blending.

-}
spriteWithColor : Texture -> Vec2 -> Vec4 -> Vec3 -> Render
spriteWithColor t imgSize uv color translate scaleRotateSkew opacity =
    WebGL.entityWith
        entitySettings
        vertSprite
        fragImageColor
        mesh
        { uP = translate
        , uT = scaleRotateSkew
        , image = t
        , imageSize = imgSize
        , uUV = uv
        , color = setAlpha color opacity
        }


{-| Render sprite from asymmetrical sprite sheet.

Sprites can be placed anywhere in tileset and each have different size

-}
sprite : Texture -> Vec2 -> Vec4 -> Render
sprite image_ imageSize uv translate scaleRotateSkew opacity =
    WebGL.entityWith
        entitySettings
        vertSprite
        fragImage
        mesh
        { uP = translate
        , uT = scaleRotateSkew
        , image = image_
        , imageSize = imageSize
        , uUV = uv
        }


{-| Render tile from symmetrical tileset.

All tiles is fixed size and placed in grid

-}
tile : Texture -> Vec2 -> Vec2 -> Float -> Render
tile spriteSheet spriteSize imageSize index translate scaleRotateSkew opacity =
    WebGL.entityWith
        entitySettings
        vertTile
        fragImage
        mesh
        { translation = translate
        , transformation = scaleRotateSkew
        , index = index
        , spriteSize = spriteSize
        , image = spriteSheet
        , imageSize = imageSize
        }


renderTriangle : { b | color : Vec4 } -> WebGL.Entity
renderTriangle =
    WebGL.entityWith
        entitySettings
        vertTriangle
        fragFill
        meshTriangle


{-| -}
entitySettings : List Setting
entitySettings =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    , WebGL.colorMask True True True False
    ]


setAlpha c =
    c |> Math.Vector3.toRecord |> (\c1 -> Math.Vector4.vec4 c1.x c1.y c1.z)

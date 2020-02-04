module Playground.Render exposing
    ( triangle, rect, circle, image, ngon, tile, sprite, spriteWithColor
    , defaultEntitySettings
    , tileWithColor
    )

{-|


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate


# Renders

@docs triangle, rect, circle, image, ngon, tile, sprite, spriteWithColor


# Settings

@docs defaultEntitySettings

-}

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Playground.Advanced exposing (Render)
import Playground.Shader as Shader
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Texture exposing (Texture)


{-| Rectangle render

Example [Playground.rectangle](Playground#rectangle):

    rectangle : Color -> Number -> Number -> Shape
    rectangle color width height =
        rect color |> Advanced.custom width height

-}
rect : Vec3 -> Render
rect color uP uT opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertNone
        Shader.fragFill
        Shader.mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| Render circle or ellipse

Example [Playground.oval](Playground#oval):

    oval : Color -> Number -> Number -> Shape
    oval color width height =
        rect color |> Advanced.custom width height

-}
circle : Vec3 -> Render
circle color uP uT opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertRect
        Shader.fragCircle
        Shader.mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| Render

    hexagon : Color -> Number -> Shape
    hexagon color radius =
        ngon 6 color |> Advanced.custom width height

-}
ngon : Float -> Vec3 -> Render
ngon n color uP uT opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertRect
        Shader.fragNgon
        Shader.mesh
        { color = setAlpha color opacity
        , uP = uP
        , n = n
        , uT = uT
        }


{-| -}
image : Texture -> Vec2 -> Render
image image_ imageSize uP uT opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertImage
        Shader.fragImage
        Shader.mesh
        { uP = uP
        , uT = uT
        , uImg = image_
        , uImgSize = imageSize
        , uA = opacity
        }


{-| Render sprite from asymmetrical sprite sheet

Same as [`sprite`](#sprite), but with color blending.

-}
spriteWithColor : Texture -> Vec2 -> Vec3 -> Vec4 -> Render
spriteWithColor t imgSize color uv translate scaleRotateSkew opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertSprite
        Shader.fragImageColor
        Shader.mesh
        { uP = translate
        , uT = scaleRotateSkew
        , uImg = t
        , uImgSize = imgSize
        , uUV = uv
        , color = setAlpha color opacity
        }


{-| Render sprite from asymmetrical sprite sheet.

Sprites can be placed anywhere in tileset and each have different size

-}
sprite : Texture -> Vec2 -> Vec4 -> Render
sprite image_ imageSize uv translate scaleRotateSkew opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertSprite
        Shader.fragImage
        Shader.mesh
        { uP = translate
        , uT = scaleRotateSkew
        , uA = opacity
        , uImg = image_
        , uImgSize = imageSize
        , uUV = uv
        }


{-| Render tile from symmetrical tileset.

Same as [`tile`](#tile), but with color blending.

-}
tileWithColor : Texture -> Vec2 -> Vec2 -> Vec3 -> Float -> Vec2 -> Vec4 -> Float -> WebGL.Entity
tileWithColor spriteSheet spriteSize imageSize color index translate scaleRotateSkew opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertTile
        Shader.fragImageColor
        Shader.mesh
        { uP = translate
        , uT = scaleRotateSkew
        , index = index
        , spriteSize = spriteSize
        , uImg = spriteSheet
        , uImgSize = imageSize
        , uA = opacity
        , color = setAlpha color opacity
        }


{-| Render tile from symmetrical tileset.

All tiles is fixed size and placed in grid

-}
tile : Texture -> Vec2 -> Vec2 -> Float -> Render
tile spriteSheet spriteSize imageSize index translate scaleRotateSkew opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertTile
        Shader.fragImage
        Shader.mesh
        { uP = translate
        , uT = scaleRotateSkew
        , index = index
        , spriteSize = spriteSize
        , uImg = spriteSheet
        , uImgSize = imageSize
        , uA = opacity
        }


{-| Render triangle
-}
triangle : Vec3 -> ( Vec2, Vec2, Vec2 ) -> Render
triangle color ( vert0, vert1, vert2 ) translate scaleRotateSkew opacity =
    WebGL.entityWith
        defaultEntitySettings
        Shader.vertTriangle
        Shader.fragFill
        Shader.meshTriangle
        { uP = translate
        , uT = scaleRotateSkew
        , vert0 = vert0
        , vert1 = vert1
        , vert2 = vert2
        , color = setAlpha color opacity
        }


{-| -}
defaultEntitySettings : List Setting
defaultEntitySettings =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    , WebGL.colorMask True True True False
    ]


setAlpha c =
    c |> Math.Vector3.toRecord |> (\c1 -> Math.Vector4.vec4 c1.x c1.y c1.z)

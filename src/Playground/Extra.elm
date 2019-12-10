module Playground.Extra exposing
    ( scaleX, scaleY
    , tile
    )

{-|


# Customize Shapes

@docs scaleX, scaleY


# Shapes

@docs tile

-}

import Math.Vector2 exposing (vec2)
import Playground exposing (Number, Shape)
import Playground.Advanced exposing (custom, useTexture)
import Playground.Internal exposing (CustomCustom(..), Form(..), Number, Shape(..))
import Playground.Render as Render exposing (Render)
import WebGL.Texture


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


{-| Show a piece of an sprite sheet.

For example, if your animation is on the first row, it has 7 frames, and you want to loop it
every 5 seconds you can write:

    sprite width height (spin (1.0 / 6.0) * 7 // 360) "sprites.png"

-}
tile : Number -> Number -> Int -> String -> Shape
tile tileW tileH index atlas =
    useTexture atlas <|
        \t ->
            custom tileW tileH <| Render.tile t (vec2 tileW tileH) (size t) (toFloat index)


triangle =
    ""



--
--{-| -}
--tileMap : Number -> Number -> String -> String -> Shape
--tileMap tileW tileH atlas lut =
--    useTexture lut
--        (\lutTexture ->
--            let
--                ( lutW, lutH ) =
--                    WebGL.Texture.size lutTexture
--                        |> Tuple.mapBoth toFloat toFloat
--            in
--            useTexture atlas
--                (\atlasTexture ->
--                    (\translation transformation opacity ->
--                        LowLevel.rectOld
--                            { color = Math.Vector4.vec4 1 0 0 opacity
--                            , translation = translation
--                            , transformation = transformation
--                            , atlasSize = WebGL.Texture.size atlasTexture
--                            , atlas = atlasTexture
--                            , lut = lutTexture
--                            }
--                    )
--                        |> custom (lutW * tileW) (lutH * tileH)
--                )
--        )


size t =
    WebGL.Texture.size t |> (\( w, h ) -> vec2 (toFloat w) (toFloat h))

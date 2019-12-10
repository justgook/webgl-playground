module Playground.Extra exposing (sprite)

{-|

@docs sprite

-}

import Math.Vector2 exposing (vec2)
import Playground exposing (Number, Shape)
import Playground.Advanced exposing (custom, entitySettings, mesh, useTexture)
import WebGL
import WebGL.Texture


{-| Show a piece of an sprite sheet.

For example, if your animation is on the first row, it has 7 frames, and you want to loop it
every 5 seconds you can write:

    sprite width height (spin (1.0 / 5.0) * 7 // 360) "sprites.png"

-}
sprite : Number -> Number -> Int -> String -> Shape
sprite tileW tileH index atlas =
    useTexture atlas <|
        \t ->
            custom tileW tileH <|
                \translation transformation opacity ->
                    renderSprite
                        { translation = translation
                        , transformation = transformation
                        , index = toFloat index
                        , spriteSize = vec2 tileW tileH
                        , image = t
                        , imageSize = size t
                        }


size t =
    WebGL.Texture.size t |> (\( w, h ) -> vec2 (toFloat w) (toFloat h))


renderSprite =
    WebGL.entityWith
        entitySettings
        verSprite
        fragSprite
        mesh



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


verSprite =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 transformation;
            uniform vec2 translation;
            uniform float index;
            uniform vec2 spriteSize;
            uniform vec2 imageSize;
            varying vec2 uv;
            void main () {
                vec2 ratio = spriteSize / imageSize;
                float row = floor(index * ratio.x);
                float column = index - row * (ratio.x);
                vec2 offset = vec2(column, row) * ratio;
                uv = (aP * .5 + 0.5) * ratio + offset;
                gl_Position = vec4(aP * mat2(transformation) + translation, 0.5, 1.0);
            }
        |]


fragSprite =
    --(2i + 1)/(2N) Pixel perfect center
    --diffuseColor
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform vec2 imageSize;
        uniform sampler2D image;

        void main () {
            vec2 pixel = (floor(uv * imageSize) + 0.5) / imageSize;
            gl_FragColor = texture2D(image, pixel);
//            gl_FragColor = vec4(0,0,1.,1.);
        }
    |]

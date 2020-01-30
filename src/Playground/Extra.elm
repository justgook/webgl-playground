module Playground.Extra exposing
    ( scaleX, scaleY
    , tile, sprite
    , tilemap
    )

{-|


# Customize Shapes

@docs scaleX, scaleY


# Shapes

@docs tile, sprite


# Batch renders

@docs tilemap

-}

import Math.Vector2 exposing (vec2)
import Math.Vector4 exposing (Vec4)
import Playground exposing (Number, Shape)
import Playground.Advanced exposing (Render, custom, useTexture)
import Playground.Batch.Tilemap
import Playground.Internal exposing (Form(..), Number, Shape(..))
import Playground.Render as Render
import WebGL.Texture


{-| Make a shape **horizontally** bigger or smaller.
Also can be used to flip object:

    tile 20 27 0 "character.png" 1
        |> scaleX -1

-}
scaleX : Number -> Shape -> Shape
scaleX sx (Shape shape) =
    Shape { shape | sx = shape.sx * sx }


{-| Make a shape **vertically** bigger or smaller.
Also can be used to flip object:

    tile 20 27 0 "character.png" 1
        |> scaleY -1

-}
scaleY : Number -> Shape -> Shape
scaleY sy (Shape shape) =
    Shape { shape | sy = shape.sy * sy }


{-| Show tile from symmetrical tileset.

All tiles is fixed size and placed in grid, where top left is 0 index, and goes incrementally left down.

Example: your have 3x3 tileset (each tile 16x24px)

    | 0 1 2 |
    | 3 4 5 |
    | 6 7 8 |

to draw tile that is first on second row

    tile 16 24 "sprites.png" 3

-}
tile : Number -> Number -> String -> Int -> Shape
tile tileW tileH atlas index =
    useTexture atlas <|
        \t ->
            custom tileW tileH <| Render.tile t (vec2 tileW tileH) (size t) (toFloat index)


{-| Show sprite from asymmetrical sprite sheet.

Sprites can be placed anywhere in tileset and each have different size

-}
sprite : Number -> Number -> String -> Vec4 -> Shape
sprite tileW tileH atlas uv =
    useTexture atlas <|
        \t ->
            custom tileW tileH <| Render.sprite t (size t) uv


{-| -}
tilemap : Float -> Float -> String -> String -> Shape
tilemap =
    Playground.Batch.Tilemap.tilemap


size : WebGL.Texture.Texture -> Math.Vector2.Vec2
size t =
    WebGL.Texture.size t |> (\( w, h ) -> vec2 (toFloat w) (toFloat h))

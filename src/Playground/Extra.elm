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

All tiles is fixed size and placed in grid, where top left is **0** index, and goes incrementally left down.

Example: your have 3x3 tileset (each tile 16x24px)

    | 0 1 2 |
    | 3 4 5 |
    | 6 7 8 |

to draw tile that is first on second row

    tile 16 24 "sprites.png" 3

-}
tile : Number -> Number -> String -> Int -> Shape
tile tileW tileH tileset index =
    useTexture tileset <|
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


{-| Show tilemap from symmetrical tileset and a corresponding lookup table stored as a texture.

Example: this lookup table is used to draw a T-shaped platform

    | 2 2 2 |
    | 0 1 0 |
    | 0 1 0 |

which in turn uses this 3x3 tileset with each tile 16x24px

    | 1 2 3 |
    | 4 5 6 |
    | 7 8 9 |

**Note:** tileset indexing start from **1** when used in lookup table

    tilemap 16 24 "sprites.png" "lookuptable.png"


## Why

For tiny maps `tile` function is enough. However, when the game map grows in size performance issues creep in. The underlying issue is that for each `tile` the WebGL rendering engine uses what is called an [Entity][1].
WebGL can handle a few thousands of such entities thus having a map with 100x100 tiles means to draw 10.000 entities for each frame - that’s way too much for WebGL.


## How it works

To avoid performance issues the idea is to draw a single WebGL `Entity` for each `tilemap` call by pushing the composition of the map down the rendering pipeline.

To do that we need to pass to playground both the tileset and a 2D array of tile indices. The latter that will be used to look-up the correct tile.

You can visualize the lookup table like those mini-maps you see on video games HUD’s. Each lookup table pixel represents a tile in the final tilemap, while the color _value_ of that pixel is an index telling which tile to pick from the tileset.

Notice that a fully black but transparent pixel (`0x00000000`) means "no tile here" and nothing is rendered. Unlike `tile`, this makes the lookup table indices to start from **1**.

All tiles are fixed size and aligned to a grid, with indices increasing left to right and top to bottom.

More details about this rendering trick can be found in [Brandon Jones’ blog][2].
[1]: <https://package.elm-lang.org/packages/elm-community/webgl/latest/WebGL#Entity>
[2]: <https://blog.tojicode.com/2012/07/sprite-tile-maps-on-gpu.html>

-}
tilemap : Number -> Number -> String -> String -> Shape
tilemap =
    Playground.Batch.Tilemap.tilemap


size : WebGL.Texture.Texture -> Math.Vector2.Vec2
size t =
    WebGL.Texture.size t |> (\( w, h ) -> vec2 (toFloat w) (toFloat h))

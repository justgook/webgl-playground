module Playground.Extra exposing
    ( tile, sprite
    , tilemap
    )

{-|


# Shapes

@docs tile, sprite


# Batch renders

@docs tilemap

-}

import Math.Vector2 exposing (vec2)
import Math.Vector4 exposing (Vec4, vec4)
import Playground exposing (Color, Shape)
import Playground.Extra.Tilemap as Tilemap
import Playground.Render as Render
import WebGL.Shape2d exposing (Form(..), Render, Shape2d(..))
import WebGL.Texture exposing (Texture)


{-| Show tile from a tileset.

All tiles are fixed size and placed into a grid, where the _first tile has a 0 index_
increasing left to right and top to bottom.

Example: having a 3x3 tileset with each tile of 16x24 pixels

    | 0 1 2 |
    | 3 4 5 |
    | 6 7 8 |

this draws the first tile of the second row

    tile 16 24 "sprites.png" 3

-}
tile : Float -> Float -> String -> Int -> Shape
tile tileW tileH tileset index =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured tileset <|
                \t ->
                    Shape2d { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Form tileW tileH <| Render.tile t (vec2 tileW tileH) (size t) (toFloat index) }
        }


{-| Show sprite from a sprite sheet.

Sprites can be placed anywhere in the sprite sheet and each can have different sizes.

Example: this draws a sprite of 16x24 pixels taking it from a sprite sheet,
starting at position `16,0` up to _including_ pixels at `31,23`

    sprite "sprites.png" { xmin = 16, xmax = 31, ymin = 0, ymax = 23 }

-}
sprite : String -> { xmin : Float, xmax : Float, ymin : Float, ymax : Float } -> Shape
sprite atlas { xmin, xmax, ymin, ymax } =
    let
        w =
            abs (xmax - xmin) + 1

        h =
            abs (ymax - ymin) + 1
    in
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured atlas <|
                \t ->
                    let
                        ( tW_, tH_ ) =
                            WebGL.Texture.size t

                        tW =
                            toFloat tW_

                        tH =
                            toFloat tH_

                        uv =
                            vec4 (xmin / tW) (1 - ymin / tH - (h / tH)) (w / tW) (h / tH)
                    in
                    Shape2d { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Form w h <| Render.sprite t (vec2 tW tH) uv }
        }


{-| Show tilemap from a tileset and a corresponding lookup table stored as a texture.

For example, this lookup table is used to draw a T-shaped platform:

    | 2 2 2 |
    | 0 1 0 |
    | 0 1 0 |

which in turn uses this 3x3 tileset with each tile 16x24px.

    | 1 2 3 |
    | 4 5 6 |
    | 7 8 9 |

Finally, the function is used as follows: 

    tilemap 16 24 "sprites.png" "lookuptable.png"

**Note:** tileset indexing starts from 1 when used in lookup table, since 0 is used to communicate "no tile here".

## Why

For tiny maps `tile` function is enough. However, when the game map grows in size performance issues creep in.
The underlying issue is that for each `tile` the WebGL rendering engine uses what is called an [Entity][1].
WebGL can handle a few thousands of such entities thus having a map with 100x100 tiles means to draw 10.000
entities for each frame - that’s way too much for WebGL.


## How it works

To avoid performance issues the idea is to draw a single WebGL `Entity` for each `tilemap` call by pushing
the composition of the map down the rendering pipeline.

To do that we need to pass to playground both the tileset and a 2D array of tile indices. The latter will
be used to look-up the correct tile.

You can visualize the lookup table like those mini-maps you see on video games HUD. Each lookup table pixel
represents a tile in the final tilemap, while the color _value_ of that pixel is an index telling which tile
to pick from the tileset.

All tiles are fixed size and placed into a grid, with indices increasing left to right and top to bottom. Notice
that a fully black but transparent pixel (`0x00000000`) means "no tile here" and nothing is rendered.
Hence, unlike `tile` function, this makes the lookup table indices to _start from 1_.

More details about this rendering technique can be found in [Brandon Jones’ blog][2].

[1]: https://package.elm-lang.org/packages/elm-community/webgl/latest/WebGL#Entity
[2]: https://blog.tojicode.com/2012/07/sprite-tile-maps-on-gpu.html

-}
tilemap : Float -> Float -> String -> String -> Shape
tilemap =
    Tilemap.tilemap


size : WebGL.Texture.Texture -> Math.Vector2.Vec2
size t =
    WebGL.Texture.size t |> (\( w, h ) -> vec2 (toFloat w) (toFloat h))

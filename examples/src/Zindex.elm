module Zindex exposing (main, view)

import Extra.Mario
import Playground exposing (..)
import Playground.Extra exposing (..)


main : Program () (Playground ()) Msg
main =
    picture view


view : List Shape
view =
    [ square red 50 |> move -15 0 |> moveZ 5
    , mario 0 |> moveY 25 |> moveZ 10 |> scale 2
    , square blue 50 |> moveZ 1
    , square yellow 50 |> move 15 10 |> moveZ 3 |> fade 0.5
    , rectangle black 150 50 |> move 20 5
    ]


mario =
    tile 20 27 Extra.Mario.spriteSheet

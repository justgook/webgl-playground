module Zindex exposing (main, view)

import Playground exposing (..)


main : Program () (Playground ()) Msg
main =
    picture view


view : List Shape
view =
    [ square red 50 |> move -15 0 |> moveZ 5
    , square blue 50 |> moveZ 1
    , square yellow 50 |> move 15 10 |> moveZ 3 |> fade 0.5
    , rectangle black 150 50 |> move 20 5
    ]

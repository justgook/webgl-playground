module Main exposing (main)

import Render.Playground exposing (..)


main =
    picture
        --[ rectangle yellow 20 200
        --    |> rotate 90
        --    |> moveX 100
        --, circle blue 40
        --, circle red 140 |> rotate 90
        [ rectangle red 2 1000
        , rectangle red 1000 2
        , rectangle red 100 2 |> moveY 100
        , rectangle red 100 2 |> moveY -100
        , rectangle red 2 100 |> moveX 100
        , rectangle red 2 100 |> moveX -100
        , cross
            |> moveX 100
            |> rotate 45

        --, rectangle brown 40 200
        --    |> moveX 100
        --    |> rotate 45
        ]


cross =
    group
        [ rectangle green 40 200
        , rectangle brown 40 200
            |> rotate 90
        ]

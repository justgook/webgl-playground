module Image exposing (main)

import Playground exposing (..)


main =
    picture
        [ rectangle red 2 1000
        , rectangle red 1000 2
        , rectangle red 100 2 |> moveY 100
        , rectangle red 100 2 |> moveY -100
        , rectangle red 2 100 |> moveX 100
        , rectangle red 2 100 |> moveX -100
        , cross
            |> moveX 100
            |> rotate 45
        ]


cross =
    group
        [ rectangle green 40 200
        , rectangle brown 40 200
            |> rotate 90
        ]

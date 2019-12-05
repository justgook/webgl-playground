module Mouse exposing (main)

import Playground exposing (..)


main =
    game view update ()


view computer memory =
    [ circle lightPurple 30
        |> moveX computer.mouse.x
        |> moveY computer.mouse.y
        |> fade
            (if computer.mouse.down then
                0.2

             else
                1
            )
    , rectangle red 2 1000
    , rectangle red 1000 2
    , rectangle red 100 2 |> moveY 100
    , rectangle red 100 2 |> moveY -100
    , rectangle red 2 100 |> moveX 100
    , rectangle red 2 100 |> moveX -100
    ]


update computer memory =
    memory

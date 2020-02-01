module Mouse exposing (Memory, init, main, update, view)

import Playground exposing (..)


main : Program () (Game Memory) Msg
main =
    game view update ()


type alias Memory =
    ()


init : Memory
init =
    ()


view : Computer -> Memory -> List Shape
view computer _ =
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


update : Computer -> Memory -> Memory
update _ memory =
    memory

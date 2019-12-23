module Extra.Shmup exposing (main)

import Extra.Shmup.Sprite exposing (sheet)
import Playground exposing (..)


main =
    animation <|
        \time ->
            [ [ sheet.enemyBlue1 |> rotate 180 |> moveX -130 ]
                |> group
                |> rotate (spin -3 time)
            , [ sheet.enemyRed4 |> moveX -240 ]
                |> group
                |> rotate (spin 5 time)
            , [ sheet.enemyGreen3 |> moveX -240 ]
                |> group
                |> rotate 180
                |> rotate (spin 5 time)
            ]


type alias Object =
    { x : Number
    , y : Number
    , ax : Number
    , ay : Number
    , vx : Number
    , vy : Number
    }

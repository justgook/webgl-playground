module Main exposing (main)

import Array
import Math.Vector4
import Playground exposing (..)
import Playground.Advanced exposing (..)
import Playground.Extra exposing (..)


main =
    animation <|
        \time ->
            let
                frame =
                    toFrac 0.75 time |> (*) 8 |> floor

                dir =
                    toFrac 3 time
                        |> (*) 2
                        |> floor
                        |> (*) 2
                        |> (+) -1
                        |> toFloat
            in
            [ rectangle red 2 1000
            , rectangle red 1000 2
            , rectangle red 100 2 |> moveY 100
            , rectangle red 100 2 |> moveY -100
            , rectangle red 2 100 |> moveX 100
            , rectangle red 2 100 |> moveX -100
            , sprite 20 27 (getFrame frame) "images/mario.png"
                |> scale 3
                |> scaleX dir
            ]


cross =
    group
        [ rectangle green 40 200
        , rectangle brown 40 200
            |> rotate 90
        ]


frames =
    Array.fromList [ 0, 1, 2, 1, 0, 3, 4, 3 ]


getFrame i =
    Array.get i frames |> Maybe.withDefault 0

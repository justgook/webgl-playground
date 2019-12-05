module Circles exposing (main)

import Playground exposing (..)



--https://ellie-app.com/7nQrJm4vRKWa1


main =
    animation view


view time =
    [ [ ring 70 darkGreen 5
            |> rotate (spin 20 time)
      , ring 140 green 9
            |> rotate (spin 25 time)
      , ring 210 lightGreen 11
            |> rotate (spin 15 time)
      ]
        |> group

    --|> scale 3
    ]



--main =
--    picture
--        [ [ ring 70 darkGreen 5
--          , ring 140 green 9
--          , ring 210 lightGreen 11
--          ]
--            |> group
--        ]


ring radius color circles =
    let
        angles =
            List.range 1 circles
                |> List.map ((*) (360 // circles))
    in
    group
        (List.map
            (\angle ->
                c1 radius color
                    |> rotate (toFloat angle)
            )
            angles
        )


c1 radius color =
    group
        [ circle color 30
            |> moveUp radius
        , image 70 70 "images/mario/stand/left.gif"
            |> moveUp radius
        ]

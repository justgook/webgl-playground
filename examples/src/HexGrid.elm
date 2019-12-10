module HexGrid exposing (main)

--https://ellie-app.com/7pVThGKMKFPa1

import Playground exposing (..)


main =
    game view update initialModel


initialModel =
    { r = 30, d = 1 }


explanation computer { r, d } =
    [ "radius: " ++ String.fromFloat r ++ " Use Up/Down Arrow Keys" |> words white |> moveY (computer.screen.top - 20) |> scale 2
    , "distance: " ++ String.fromFloat d ++ " Use Left/Right Arrow Keys" |> words white |> moveY (computer.screen.top - 52) |> scale 2
    ]


hexGrid ({ r, d } as model) =
    let
        horizontalDistance =
            2 * r * cos (degrees 30) + d

        verticalDistance =
            r + sin (degrees 30) * r + (d / cos (degrees 30))

        hexagonAt ( i, j ) =
            hexagon black r
                |> moveX (toFloat i * horizontalDistance)
                |> moveY (toFloat j * verticalDistance)
                |> shiftIfEven j

        shiftIfEven j =
            if modBy 2 j == 0 then
                moveX (horizontalDistance / 2)

            else
                identity

        cartesianProduct l1 l2 =
            List.concatMap (\i -> List.map (\j -> ( i, j )) l2) l1
    in
    List.map hexagonAt (cartesianProduct (List.range -2 2) (List.range -2 2))


view computer model =
    explanation computer model ++ hexGrid model


update computer { r, d } =
    { r = r + toY computer.keyboard
    , d = max 1 (d + toX computer.keyboard)
    }

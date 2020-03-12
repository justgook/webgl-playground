module HexGrid exposing (Memory, init, main, update, view)

import Playground exposing (..)



--https://ellie-app.com/7XqHHHr8J7ka1


type alias Memory =
    { r : Float, d : Float }


main : Program () (Playground Memory) Msg
main =
    game view update init


init : Memory
init =
    { r = 30, d = 1 }


explanation computer { r, d } =
    [ "radius: " ++ String.fromFloat r ++ " Use Up/Down Arrow Keys" |> words black |> moveY (computer.screen.top - 20)
    , "distance: " ++ String.fromFloat d ++ " Use Left/Right Arrow Keys" |> words black |> moveY (computer.screen.top - 40)
    ]


hexGrid ({ r, d } as model) =
    let
        horizontalDistance =
            2 * r * cos (degrees 30) + d

        verticalDistance =
            cos (degrees 30) * (2 * r * cos (degrees 30) + d)

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
    { r = max 1 (r + toY computer.keyboard)
    , d = max 1 (d + toX computer.keyboard)
    }

module HexGrid exposing (Memory, init, main, update, view)

--https://ellie-app.com/7pVThGKMKFPa1

import Playground exposing (..)


type alias Memory =
    { r : Float, d : Float }


main : Program () (Game Memory) Msg
main =
    game view update init


init : Memory
init =
    { r = 30, d = 1 }


view : Computer -> Memory -> List Shape
view computer model =
    explanation computer model ++ hexGrid model


update : Computer -> Memory -> Memory
update computer { r, d } =
    { r = r + toY computer.keyboard
    , d = max 1 (d + toX computer.keyboard)
    }


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

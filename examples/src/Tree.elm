module Tree exposing (Memory, init, main, update, view)

--https://ellie-app.com/7nVtFgNfSK4a1

import Playground exposing (..)


type alias Memory =
    { w : Float
    , h : Float
    , s : Float
    , t : Float
    }


main : Program () (Playground Memory) Msg
main =
    game view update init


init : Memory
init =
    { h = 200, s = 40, t = 31, w = 80 }


view : Computer -> Memory -> List Shape
view computer { w, h, s, t } =
    [ fractalTree w h s t colors 10
        |> move -(w / 2) -(computer.screen.height / 3)
    ]


update : Computer -> Memory -> Memory
update computer { w, h, s, t } =
    { w = w + toX computer.keyboard
    , h = h + toY computer.keyboard
    , s = w / 2 + w * 0.5 * computer.mouse.x / computer.screen.width
    , t = 100 * computer.mouse.y / computer.screen.height
    }


colors =
    [ brown, orange, yellow, red, purple, green, blue, darkGreen, lightPurple, darkCharcoal ]


fractalTree w h s t colorList n =
    let
        currentColor =
            List.head colorList |> Maybe.withDefault charcoal

        restColors =
            List.tail colorList |> Maybe.withDefault colors

        toDegrees alpha =
            alpha / pi * 180

        hypotenus e f =
            sqrt (e ^ 2 + f ^ 2)

        ( wLeft, wRight ) =
            ( hypotenus s t
            , hypotenus (w - s) t
            )

        baseRect =
            rectangle currentColor w h
                |> move (w / 2) (h / 2)

        children =
            if n == 0 then
                []

            else
                [ fractalTree w h s t restColors (n - 1)
                    |> scale (wLeft / w)
                    |> rotate (toDegrees (asin (t / wLeft)))
                    |> moveUp h
                , fractalTree w h s t restColors (n - 1)
                    |> scale (wRight / w)
                    |> rotate -(toDegrees (asin (t / wRight)))
                    |> move s (t + h)
                ]
    in
    group (baseRect :: children)

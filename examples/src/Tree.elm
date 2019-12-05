module Tree exposing (main)

--https://ellie-app.com/7nVtFgNfSK4a1

import Playground exposing (..)


main =
    game view update initialModel


colors =
    [ brown, orange, yellow, red, purple, green, blue, darkGreen, lightPurple, darkCharcoal ]


initialModel =
    { w = 80, h = 200, s = 0, t = 0 }


view computer { w, h, s, t } =
    [ fractalTree w h s t colors 10
        |> move -(w / 2) -(computer.screen.height / 3)
    ]


update computer { w, h, s, t } =
    { w = w + toX computer.keyboard
    , h = h + toY computer.keyboard
    , s = w / 2 + w * 0.5 * computer.mouse.x / computer.screen.width
    , t = 100 * computer.mouse.y / computer.screen.height
    }


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

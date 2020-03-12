module Extra.Font exposing (main, view)

import Extra.Font.Glyph exposing (glyph)
import Playground exposing (..)


main : Program () (Playground ()) Msg
main =
    picture view


view : List Shape
view =
    [ glyph.a
        --|> rotate (spin 3 time)
        --|> moveX (wave -100 100 5 time)
        |> scale 15
    ]

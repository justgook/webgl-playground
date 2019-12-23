module Extra.Font exposing (main)

import Extra.Font.Glyph exposing (glyph)
import Playground exposing (..)


main =
    animation <|
        \time ->
            [ glyph.a

            --|> rotate (spin 3 time)
            --|> moveX (wave -100 100 5 time)
            --|> scale 15
            ]

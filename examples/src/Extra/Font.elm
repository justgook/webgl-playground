module Extra.Font exposing (main, view)

import Extra.Font.Glyph exposing (wordsConfig)
import Playground exposing (..)
import WebGL.Shape2d.Util as Util


main : Program () (Playground ()) Msg
main =
    animation view


sentence : String
sentence =
    "The quick brown fox jumps over the lazy dog"


view time =
    [ Util.msdfFont (wave 1 20 9 time) wordsConfig (rgb 0 0 0) (sentence ++ " (Source Code Pro)")
        |> move -463 -16
        |> List.singleton
        |> group
        |> rotate (spin 8 time)
        |> scale (wave 0.5 20 9 time)
    ]

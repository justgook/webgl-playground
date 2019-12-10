module Font exposing (main)

import Playground exposing (..)


main =
    picture
        [ aaaa
            |> group
            |> scale 3
        ]


aaaa =
    [ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" |> words red
    , "abcdefghijklmnopqrstuvwxyz" |> words red
    , "0123456789" |> words red
    , "!\"#$%&'()*+,-./" |> words red
    , ":;<=>?[\\]^_`{|}~" |> words red
    ]
        |> List.indexedMap (\i a -> a |> moveDown (i * 16 |> toFloat))

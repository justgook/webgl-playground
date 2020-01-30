module Isometric exposing (main)

import Playground exposing (..)


main =
    picture
        [ text
            |> group
            |> scale 3
        ]


text =
    [ "Just Placeholder" |> words red
    , "http://clintbellanger.net/articles/isometric_math/" |> words red
    ]
        |> List.indexedMap (\i a -> a |> moveDown (i * 16 |> toFloat))



--http://clintbellanger.net/articles/isometric_math/

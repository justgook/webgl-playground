module Extra.Chibi exposing (..)

import Playground exposing (..)



--https://forums.rpgmakerweb.com/index.php?threads/mv-added-resource-generators-a-face-for-your-hero.37958/
--http://schlangan.free.fr/mvxgen.html
--https://sanderfrenken.github.io/Universal-LPC-Spritesheet-Character-Generator/
--http://gaurav.munjal.us/Universal-LPC-Spritesheet-Character-Generator/
--http://lpc.opengameart.org/static/lpc-style-guide/assets.html
--https://chibi.center/


main =
    picture
        [ text
            |> group
            |> scale 3
        ]


text =
    [ "Just Placeholder" |> words black
    ]
        |> List.indexedMap (\i a -> a |> moveDown (i * 16 |> toFloat))

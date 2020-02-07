module Extra.Jump.Sprite exposing (idle, run)

import Extra.Jump.Direction exposing (Direction(..))
import Playground exposing (group, move)
import Playground.Extra exposing (scaleX, sprite)


run dir frame_ =
    let
        frame =
            round (frame_ / 5)
                |> modBy 8
    in
    sprites.body.run frame
        |> (case dir of
                North ->
                    identity

                NorthEast ->
                    flip (::) [ sprites.gunuf.run frame ]
                        >> group

                East ->
                    flip (::) [ sprites.gunf.run frame ]
                        >> group

                SouthEast ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gundf.jump
                        ]
                            |> group

                South ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gund.jump
                        ]
                            |> group

                SouthWest ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gundf.jump
                        ]
                            |> group
                            |> scaleX -1

                West ->
                    flip (::) [ sprites.gunf.run frame ]
                        >> group
                        >> scaleX -1

                NorthWest ->
                    flip (::) [ sprites.gunuf.run frame ]
                        >> group
                        >> scaleX -1

                Neither ->
                    identity
           )


idle dir frame_ =
    let
        frame =
            round (frame_ / 5)
                |> modBy 20
    in
    sprites.body.idle frame
        |> (case dir of
                North ->
                    flip (::) [ sprites.gunu.idle frame ]
                        >> group

                NorthEast ->
                    flip (::) [ sprites.gunuf.idle frame ]
                        >> group

                East ->
                    flip (::) [ sprites.gunf.idle frame ]
                        >> group

                SouthEast ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gundf.jump
                        ]
                            |> group

                South ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gund.jump
                        ]
                            |> group

                SouthWest ->
                    \_ ->
                        [ sprites.body.jump
                        , sprites.gundf.jump
                        ]
                            |> group
                            |> scaleX -1

                West ->
                    flip (::) [ sprites.gunf.idle frame ]
                        >> group
                        >> scaleX -1

                NorthWest ->
                    flip (::) [ sprites.gunuf.idle frame ]
                        >> group
                        >> scaleX -1

                Neither ->
                    identity
           )


image =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAADsAAAA8CAMAAADv/jI5AAAAAXNSR0IArs4c6QAAADxQTFRFAAAATD4NnHoBWUUA1qMC3aGJ98+3kmpaj08p9OH5AAAARSUSzotgr3ZSDCMzTWd5NzExU1NTdnZ2ra2tSRBLOAAAABR0Uk5TAP////////////////////////+64WOpAAACdElEQVRIia2WiXKrMAxFgYC7hBph/v9f61W+ko2TN6+aaYKWK2TDcTpNf2MzX8wY1Q76OTjP65wvlmVOCnY4s+Yy1C2rNx98LMnACZklO2suY2kJzvF6XdeXDkhFxbJ5e+HgbbFiM9627WPkgBYr4rW3oVO1GPzczNe3Md9fY6eYrti2Mtm9A1pRIYa4cUBbg889efvYKSYrfvZoP2OHtbIi29ipWgja4zhs+LTaeUsbVTZ/phZRG1LhT2hFMFYrSzGbTaU6wf8x5lez36DP1Y85G8CMB4FCPzecC2lLIbLSCY1WqU0ny1IYvOW3QR9OllRhbvldNPr63LjnN3oSI3mY3CLLjZQWDpMhv7GR1L6HbGmE2mbMG5g/OzPLyQYw54zWwq26MB+QEdrS0DSEH5HfxKV9dvDlhnuj9eAW+uI5cORm1eyRyxvCEUtxDoiotaXhK5jbjFWzSOsi2ykTuAPLeA4QUae5hBQJe9RjgMgJLaMPkCJhgja6qOoC+0B71TJh8bp09TMbM/FkK/50Z0iRMJEhd1JcM84pIUXCRCZoL0dqTgkpEoaZdN9JzSkhRcIwQ+eZNktMIyFFwjDjtWmrxDQS0ue+M8wiU7ZZTCMh9VAxzF18xTQSUgRS4ZtXjNNYASkAqfAtu/VsaGcEK535GEgZ/4BOR3HJ8V+D8ks/aWsTdF6s9YkRvY1Ruu9NMlXoSDVjGoRjR0qv+QSdCaArIR3xu+ecoyvuob82HG8G9CEZM2ewfF+/JGfS9K6j1TPHZeT3zQ91podIzvH7OzReR9DGdfq7nu3OdLV5PK8KD5DSK3Rd74lJfoeV9Db6H6wn/QXjZzr/yq55/wAAAABJRU5ErkJggg=="


sprites =
    { body =
        { run =
            \i ->
                get i
                    ( sprite image { xmin = 14, xmax = 25, ymin = 1, ymax = 16 } |> move -1 9
                    , [ sprite image { xmin = 27, xmax = 38, ymin = 18, ymax = 32 } |> move -1 9.5
                      , sprite image { xmin = 27, xmax = 38, ymin = 1, ymax = 16 } |> move -1 8
                      , sprite image { xmin = 1, xmax = 10, ymin = 36, ymax = 50 } |> move -1 7.5
                      , sprite image { xmin = 13, xmax = 23, ymin = 35, ymax = 49 } |> move -0.5 8.5
                      , sprite image { xmin = 39, xmax = 49, ymin = 35, ymax = 49 } |> move -0.5 9.5
                      , sprite image { xmin = 40, xmax = 50, ymin = 18, ymax = 33 } |> move -0.5 8
                      , sprite image { xmin = 1, xmax = 11, ymin = 19, ymax = 34 } |> move -1.5 8
                      ]
                    )
        , jump = sprite image { xmin = 27, xmax = 37, ymin = 34, ymax = 49 } |> move -1.5 10
        , idle =
            \i ->
                get i
                    ( sprite image { xmin = 40, xmax = 51, ymin = 1, ymax = 16 } |> move -1 9
                    , [ sprite image { xmin = 14, xmax = 25, ymin = 18, ymax = 33 } |> move -1 9
                      , sprite image { xmin = 14, xmax = 25, ymin = 18, ymax = 33 } |> move -1 9
                      , sprite image { xmin = 14, xmax = 25, ymin = 18, ymax = 33 } |> move -1 9
                      , sprite image { xmin = 1, xmax = 12, ymin = 1, ymax = 17 } |> move -1 9.5
                      ]
                    )
        }
    , gundf =
        { jump = sprite image { xmin = 23, xmax = 31, ymin = 51, ymax = 58 } |> move 2.5 6
        }
    , gund =
        { jump = sprite image { xmin = 51, xmax = 58, ymin = 46, ymax = 54 } |> move 1 4.5
        }
    , gunf =
        { run =
            \i ->
                get i
                    ( sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
                    , [ sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 6
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 4
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 4
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 2 4
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 2 5
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 2 5
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 2 4
                      ]
                    )
        , jump =
            sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
        , idle =
            \i ->
                get i
                    ( sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 4
                    , [ sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
                      , sprite image { xmin = 12, xmax = 21, ymin = 51, ymax = 58 } |> move 1 5
                      ]
                    )
        }
    , gunu =
        { run =
            \i ->
                get i
                    ( sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 3 11
                    , [ sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 3 12
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 3 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 3 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 11
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      ]
                    )
        , jump = sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 9
        , idle =
            \i ->
                get i
                    ( sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 9
                    , [ sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 10
                      , sprite image { xmin = 51, xmax = 58, ymin = 35, ymax = 44 } |> move 4 9
                      ]
                    )
        }
    , gunuf =
        { run =
            \i ->
                get i
                    ( sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 8.5
                    , [ sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 9.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 7.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 4 7.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 4 7.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 4 8.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 4 8.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 4 7.5
                      ]
                    )
        , jump =
            sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 9.5
        , idle =
            \i ->
                get i
                    ( sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 7.5
                    , [ sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 8.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 8.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 8.5
                      , sprite image { xmin = 33, xmax = 40, ymin = 51, ymax = 59 } |> move 3 8.5
                      ]
                    )
        }
    }


flip : (c -> b -> a) -> b -> c -> a
flip fn a b =
    fn b a


get : Int -> ( a, List a ) -> a
get i ( x, xs ) =
    if i < 0 then
        x

    else
        case i of
            0 ->
                x

            _ ->
                List.drop (i - 1) xs
                    |> List.head
                    |> Maybe.withDefault x



{- TEXTURE PACKER

   data.frames.reduce((result, item) => {
       const name = item.filename.match(/([^:]*):([^ ]*).*/);
       name[1] = name[1].toLocaleLowerCase();
       result[name[1]] = result[name[1]] || {};
       result[name[1]][name[2]] = result[name[1]][name[2]] || [];
       const { x, y, w, h } = item.frame;
       const { x: sx, y: sy } = item.spriteSourceSize;
       const { w: sw, h: sh } = item.sourceSize;
       result[name[1]][name[2]].push(
           `sprite image { xmin = ${x}, xmax = ${x + w - 1}, ymin = ${y}, ymax = ${y + h - 1} }  |> move ${w * 0.5 + sx - (sw * 0.5)} ${h * 0.5 + (sh - h - sy)}`
       );
       return result;
   }, {})

-}

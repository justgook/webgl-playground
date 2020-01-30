module Main exposing (main)

import Browser
import Browser.Events
import GitHubCorner
import Playground exposing (..)
import Playground.Advanced exposing (..)


main : Program () (Game Model) Msg
main =
    Browser.document
        { init = init
        , view =
            \memory ->
                { title = "Playground Demo Page"
                , body =
                    [ GitHubCorner.topRight "https://github.com/justgook/webgl-playground"
                    , playground.view memory
                    ]
                }
        , update = playground.update
        , subscriptions =
            \m ->
                [ subscriptions.all
                , Browser.Events.onResize resize
                ]
                    |> Sub.batch
        }


init : () -> ( Game Model, Cmd Msg )
init =
    \flags -> playground.init


viewMemory : Computer -> Model -> List Shape
viewMemory { screen } memory =
    let
        cols =
            4

        w =
            screen.width / cols - 20

        rows =
            toFloat (List.length memory) / cols |> ceiling

        h =
            ((screen.height - 70) / toFloat rows - 20)
                |> min w

        y =
            screen.top - 0.5 * h - 70

        x =
            screen.left + 0.5 * w + 10
    in
    List.indexedMap
        (\i a ->
            let
                xx =
                    remainderBy cols i |> toFloat
            in
            [ rectangle blue w h
            , words white a
                |> scale (h / 14 * 0.2)
            ]
                |> group
                |> move (x + (w + 20) * xx)
                    (y - toFloat (i // cols) * (h + 20))
        )
        memory
        |> (::) (words white "WebGL Playground" |> scale 3 |> move (screen.left + 225) (screen.top - 35))


updateMemory computer memory =
    memory


initialMemory : Model
initialMemory =
    [ "Circles"
    , "Clock"
    , "Font"
    , "HexGrid"
    , "Isometric"
    , "Main"
    , "Mouse"
    , "Polygon"
    , "Shmup"
    , "Polygon2"
    , "Tree"
    , "Vectors"
    , "Jump(E)"
    , "Mario(E)"
    , "MSDF(E)"
    ]


calcGrid : Int -> Float -> Float -> List a -> List { x : Float, y : Float }
calcGrid cols_ width height l =
    let
        cols =
            toFloat cols_

        w =
            width / cols - 20

        rows =
            toFloat (List.length l) / cols |> ceiling

        h =
            ((height - 70) / toFloat rows - 20)
                |> min w

        y =
            height * 0.5 - 0.5 * h - 70

        x =
            width * -0.5 + 0.5 * w + 10
    in
    List.indexedMap
        (\i a ->
            { x = x + (w + 20) * (remainderBy cols_ i |> toFloat)
            , y = y - toFloat (i // cols_) * (h + 20)
            }
        )
        l


type alias Model =
    List String


playground =
    embed viewMemory updateMemory initialMemory

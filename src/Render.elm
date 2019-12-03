module Render exposing (toHtml)

import Html exposing (Attribute, Html)
import WebGL exposing (Entity)


toHtml : List (Attribute msg) -> List Entity -> Html msg
toHtml =
    WebGL.toHtmlWith webGLOption


webGLOption : List WebGL.Option
webGLOption =
    [ WebGL.alpha False
    , WebGL.depth 1
    , WebGL.clearColor (29 / 255) (33 / 255) (45 / 255) 1
    ]

module Playground.Extra.Font exposing (tileFont)

import Math.Vector2 exposing (vec2)
import Math.Vector3 exposing (Vec3)
import Playground.Render as Render
import WebGL.Shape2d exposing (Form(..), Shape2d(..))
import WebGL.Texture as Texture exposing (Texture)


tileFont : { a | charW : Float, charH : Float, src : String, getIndex : Char -> Float } -> Vec3 -> String -> Shape2d
tileFont { charW, charH, src, getIndex } color string =
    Shape2d
        { x = 0
        , y = 0
        , z = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured src
                (\t ->
                    let
                        ( imgW, imgH ) =
                            t
                                |> Texture.size
                                |> Tuple.mapBoth toFloat toFloat

                        imgSize =
                            Math.Vector2.vec2 imgW imgH

                        toChar =
                            char t imgSize color charW charH

                        output =
                            String.toList string
                                |> List.foldl (outputFold toChar getIndex charW charH)
                                    { chars = [], x = charW, y = charH, width = 0 }
                    in
                    Shape2d
                        { x = max output.x output.width * -0.5
                        , y = output.y * -0.5 + 0.5 * -charH
                        , z = 0
                        , a = 0
                        , sx = 1
                        , sy = 1
                        , o = 1
                        , form = Group output.chars
                        }
                )
        }


char : Texture -> Math.Vector2.Vec2 -> Vec3 -> Float -> Float -> Float -> Float -> Float -> Shape2d
char spriteSheet imageSize color w h x y index =
    Shape2d
        { x = x
        , y = y
        , z = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form w h <| Render.tileWithColor spriteSheet (vec2 w h) imageSize color index
        }


outputFold : (Float -> Float -> a -> b) -> (Char -> a) -> Float -> Float -> Char -> { c | chars : List b, x : Float, y : Float, width : Float } -> { chars : List b, x : Float, y : Float, width : Float }
outputFold toChar getIndex w h c { chars, x, y, width } =
    if c == '\n' then
        { chars = chars
        , x = w
        , y = y - h
        , width = max width x
        }

    else
        { chars = toChar x y (getIndex c) :: chars, x = x + w, y = y, width = width }

module Playground.Extra.Tilemap exposing (tilemap)

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector4 exposing (Vec4)
import Playground.Render exposing (defaultEntitySettings)
import Playground.Shader as Shader
import WebGL exposing (Shader)
import WebGL.Shape2d exposing (Form(..), Render, Shape2d(..))
import WebGL.Texture exposing (Texture)


tilemap : Float -> Float -> String -> String -> Shape2d
tilemap tileW tileH tileset lut =
    Shape2d
        { x = 0
        , y = 0
        , z = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured tileset
                (\tileset_ ->
                    Shape2d
                        { x = 0
                        , y = 0
                        , z = 0
                        , a = 0
                        , sx = 1
                        , sy = 1
                        , o = 1
                        , form =
                            Textured lut
                                (\lut_ ->
                                    let
                                        ( w1, h1 ) =
                                            WebGL.Texture.size tileset_
                                                |> Tuple.mapBoth toFloat toFloat

                                        ( w2, h2 ) =
                                            WebGL.Texture.size lut_
                                                |> Tuple.mapBoth toFloat toFloat
                                    in
                                    Shape2d
                                        { x = 0
                                        , y = 0
                                        , z = 0
                                        , a = 0
                                        , sx = 1
                                        , sy = 1
                                        , o = 1
                                        , form =
                                            Form
                                                (w2 * tileW)
                                                (h2 * tileH)
                                                (\translate scaleRotateSkew z opacity ->
                                                    WebGL.entityWith
                                                        defaultEntitySettings
                                                        Shader.vertImage
                                                        fragTilemap
                                                        Shader.mesh
                                                        { uP = translate
                                                        , uT = scaleRotateSkew
                                                        , uA = opacity
                                                        , uTileSize = vec2 tileW tileH
                                                        , uAtlas = tileset_
                                                        , uAtlasSize = vec2 w1 h1
                                                        , uLut = lut_
                                                        , uLutSize = vec2 w2 h2
                                                        , z = z
                                                        }
                                                )
                                        }
                                )
                        }
                )
        }


fragTilemap =
    --http://media.tojicode.com/webgl-samples/tilemap.html
    [glsl|
precision mediump float;
varying vec2 uv;
uniform sampler2D uAtlas;
uniform sampler2D uLut;
uniform vec2 uAtlasSize;
uniform vec2 uLutSize;
uniform vec2 uTileSize;
uniform float uA;

float color2float(vec4 color) {

    return
    color.a * 255.0
    + color.b * 256.0 * 255.0
    + color.g * 256.0 * 256.0 * 255.0
    + color.r * 256.0 * 256.0 * 256.0 * 255.0;
    }

/**
 * Returns accurate MOD when arguments are approximate integers.
 */
float modI(float a,float b) {
    float m=a-floor((a+0.5)/b)*b;
    return floor(m+0.5);
}

void main () {
   vec2 point = floor(uv * uLutSize);
   vec2 offset = fract(uv * uLutSize);

   //(2i + 1)/(2N) Pixel center
   vec2 coordinate = (point + 0.5) / uLutSize;
   float index = color2float(texture2D(uLut, coordinate));
   if (index <= 0.0) discard;
   vec2 grid = uAtlasSize / uTileSize;
   // tile indexes in uAtlas starts from zero, but in lut zero is used for
   // "none" placeholder
   vec2 tile = vec2(modI((index - 1.), grid.x), int(index - 1.) / int(grid.x));
   // inverting reading botom to top
   tile.y = grid.y - tile.y - 1.;
   vec2 fragmentOffsetPx = floor(offset * uTileSize);
   //(2i + 1)/(2N) Pixel center
   vec2 pixel = (floor(tile * uTileSize + fragmentOffsetPx) + 0.5) / uAtlasSize;
   gl_FragColor = texture2D(uAtlas, pixel);
   gl_FragColor.a *= float(index != 0.);
   if(gl_FragColor.a <= 0.025) discard;
}
    |]

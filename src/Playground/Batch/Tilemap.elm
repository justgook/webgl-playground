module Playground.Batch.Tilemap exposing (tilemap)

import Math.Vector2 exposing (vec2)
import Playground.Advanced exposing (Render, custom, useTexture)
import Playground.Internal exposing (Shape)
import Playground.Render exposing (defaultEntitySettings)
import Playground.Shader as Shader
import WebGL
import WebGL.Texture


tilemap : Float -> Float -> String -> String -> Shape
tilemap tileW tileH tileset lut =
    useTexture tileset
        (\tileset_ ->
            useTexture lut
                (\lut_ ->
                    let
                        ( w1, h1 ) =
                            WebGL.Texture.size tileset_
                                |> Tuple.mapBoth toFloat toFloat

                        ( w2, h2 ) =
                            WebGL.Texture.size lut_
                                |> Tuple.mapBoth toFloat toFloat
                    in
                    custom
                        (w2 * tileW)
                        (h2 * tileH)
                        (\translate scaleRotateSkew opacity ->
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
                                }
                        )
                )
        )



--http://www.catalinzima.com/2010/07/my-technique-for-the-shader-based-dynamic-2d-shadows/


fragTilemap =
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
float modI(float a, float b) {
   float m = a - floor((a + 0.5) / b) * b;
   return floor(m + 0.5);
}
void main () {
   vec2 point = uv * uLutSize;
   vec2 look = floor(point);
   //(2i + 1)/(2N) Pixel center
   vec2 coordinate = (look + 0.5) / uLutSize;
   float uIndex = color2float(texture2D(uLut, coordinate));
   vec2 grid = uAtlasSize / uTileSize;
   // tile indexes in uAtlas starts from zero, but in lut zero is used for
   // "none" placeholder
   vec2 tile = vec2(modI((uIndex - 1.), grid.x), int(uIndex - 1.) / int(grid.x));
   // inverting reading botom to top
   tile.y = grid.y - tile.y - 1.;
   vec2 fragmentOffsetPx = floor((point - look) * uTileSize);
   //(2i + 1)/(2N) Pixel center
   vec2 pixel = (floor(tile * uTileSize + fragmentOffsetPx) + 0.5) / uAtlasSize;
   gl_FragColor = texture2D(uAtlas, pixel);
   gl_FragColor.a *= float(uIndex > 0.);
   gl_FragColor.rgb *= gl_FragColor.a;

}
    |]

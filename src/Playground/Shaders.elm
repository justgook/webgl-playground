module Playground.Shaders exposing (circle, clipPlate, entitySettings, image, imagePixelFragment, ngon, rect, uvVertex)

import Math.Vector2
import Math.Vector4
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL exposing (Setting)
import WebGL.Settings.Blend as Blend


rect :
    { a
        | color : Math.Vector4.Vec4
        , transformation : Math.Vector4.Vec4
        , translation : Math.Vector2.Vec2
    }
    -> WebGL.Entity
rect =
    WebGL.entityWith
        entitySettings
        rectVertexShader
        fillFragment
        clipPlate


circle :
    { a
        | color : Math.Vector4.Vec4
        , transformation : Math.Vector4.Vec4
        , translation : Math.Vector2.Vec2
    }
    -> WebGL.Entity
circle =
    WebGL.entityWith
        entitySettings
        clipSpaceVertex
        circleFragment
        clipPlate


ngon : { a | color : Math.Vector4.Vec4, n : Float, transformation : Math.Vector4.Vec4, translation : Math.Vector2.Vec2 } -> WebGL.Entity
ngon =
    WebGL.entityWith
        entitySettings
        clipSpaceVertex
        nGonFragment
        clipPlate


image =
    WebGL.entityWith
        entitySettings
        uvVertex
        imagePixelFragment
        clipPlate


type alias TileUV =
    Math.Vector4.Vec4


entitySettings : List Setting
entitySettings =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    , WebGL.colorMask True True True False
    ]


clipPlate : Mesh Plate
clipPlate =
    WebGL.triangleStrip
        [ Plate (Math.Vector2.vec2 -1 -1)
        , Plate (Math.Vector2.vec2 -1 1)
        , Plate (Math.Vector2.vec2 1 -1)
        , Plate (Math.Vector2.vec2 1 1)
        ]


plate : Mesh Plate
plate =
    WebGL.triangleStrip
        [ Plate (Math.Vector2.vec2 0 1)
        , Plate (Math.Vector2.vec2 1 1)
        , Plate (Math.Vector2.vec2 0 0)
        , Plate (Math.Vector2.vec2 1 0)
        ]


rectVertexShader =
    [glsl|
        precision mediump float;
        attribute vec2 aP;
        uniform vec4 transformation;
        uniform vec2 translation;
        void main () {
            gl_Position = vec4(aP * mat2(transformation) + translation, 0., 1.0);
        }
    |]


fillFragment =
    --https://thndl.com/square-shaped-shaders.html
    [glsl|
        precision mediump float;
        uniform vec4 color;

        void main () {
            gl_FragColor = color;

        }
    |]


uvVertex =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 transformation;
            uniform vec2 translation;
            varying vec2 uv;
            void main () {
                uv = aP * .5 + 0.5;
                gl_Position = vec4(aP * mat2(transformation) + translation, 0., 1.0);
            }
        |]


clipSpaceVertex =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 transformation;
            uniform vec2 translation;
            varying vec2 uv;
            void main () {
                uv = aP;
                gl_Position = vec4(aP * mat2(transformation) + translation, 0., 1.0);
            }
        |]


circleFragment =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        varying vec2 uv;
        void main () {
            gl_FragColor = color;
            gl_FragColor.a *= smoothstep(0.01,0.04,1.-length(uv));
        }
    |]


nGonFragment =
    --https://thebookofshaders.com/07/
    [glsl|
        precision mediump float;
        uniform vec4 color;
        uniform float n;
        varying vec2 uv;

        void main () {
            float angle = 0.523599;
            float a = atan(uv.x,uv.y) + angle;
            float b = 6.28319 / n;
            float f = smoothstep(0.859,.86,cos(floor(.5 + a/b)*b-a)*length(uv));
            gl_FragColor = color;
            gl_FragColor.a -= f;
        }
    |]


imageFragment =
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform sampler2D image;

        void main () {
            gl_FragColor = texture2D(image, uv);
            //gl_FragColor.xyz=czm_saturation(gl_FragColor.xyz, 1.0);
        }
    |]


imageSaturationFragment =
    --https://github.com/AnalyticalGraphicsInc/cesium/blob/master/Source/Shaders/Builtin/Functions/saturation.glsl
    -- * @example
    -- * vec3 greyScale = saturation(color, 0.0);
    -- * vec3 doubleSaturation = saturation(color, 2.0);
    -- */
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform sampler2D image;
        uniform float adjustment;
        vec3 saturation(vec3 rgb, float adj) {
            // Algorithm from Chapter 16 of OpenGL Shading Language
            const vec3 W = vec3(0.2125, 0.7154, 0.0721);
            vec3 intensity = vec3(dot(rgb, W));
            return mix(intensity, rgb, adj);
        }
        void main () {
            gl_FragColor = texture2D(image, uv);
            gl_FragColor.xyz=saturation(gl_FragColor.xyz, adjustment);
        }
    |]


imagePixelFragment =
    --(2i + 1)/(2N) Pixel perfect center
    --diffuseColor
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform vec2 imageSize;
        uniform sampler2D image;

        void main () {
            vec2 pixel = (floor(uv * imageSize) + 0.5) / imageSize;
            gl_FragColor = texture2D(image, pixel);

        }
    |]


rotSprite =
    --https://discover.therookies.co/2019/08/13/unity-masterclass-how-to-set-up-your-project-for-pixel-perfect-retro-8-bit-games/
    --https://en.wikipedia.org/wiki/Pixel-art_scaling_algorithms#RotSprite
    [glsl|

        precision mediump float;
        varying vec2 uv;
        uniform vec2 imageSize;
        uniform sampler2D image;

        void main () {
            vec2 pixel = (floor(uv * imageSize) + 0.5) / imageSize;
            gl_FragColor = texture2D(image, pixel);

        }
    |]


{-| FILTERS !!! <https://dev.to/lesnitsky/webgl-month-day-9-image-filters-5g8e>
-}
sepia =
    [glsl|
  vec4 sepia(vec4 color) {
      vec3 sepiaColor = vec3(112, 66, 20) / 255.0;
     return vec4(
         mix(color.rgb, sepiaColor, 0.4),
         color.a
     );
  }
  |]


hue =
    --https://github.com/AnalyticalGraphicsInc/cesium/blob/master/Source/Shaders/Builtin/Functions/hue.glsl
    [glsl|
vec3 czm_hue(vec3 rgb, float adjustment)
{
    const mat3 toYIQ = mat3(0.299,     0.587,     0.114,
                            0.595716, -0.274453, -0.321263,
                            0.211456, -0.522591,  0.311135);
    const mat3 toRGB = mat3(1.0,  0.9563,  0.6210,
                            1.0, -0.2721, -0.6474,
                            1.0, -1.107,   1.7046);

    vec3 yiq = toYIQ * rgb;
    float hue = atan(yiq.z, yiq.y) + adjustment;
    float chroma = sqrt(yiq.z * yiq.z + yiq.y * yiq.y);

    vec3 color = vec3(yiq.x, chroma * cos(hue), chroma * sin(hue));
    return toRGB * color;
}
    |]


type alias Varyings =
    { uv : Math.Vector2.Vec2 }


type alias Plate =
    { aP : Math.Vector2.Vec2 }

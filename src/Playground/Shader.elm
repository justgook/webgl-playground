module Playground.Shader exposing
    ( vertNone, vertRect, vertImage, vertSprite, vertTile, vertTriangle
    , fragFill, fragCircle, fragNgon, fragImage, fragImageColor
    , mesh, meshTriangle
    )

{-|


# Vertex Shaders

@docs vertNone, vertRect, vertImage, vertSprite, vertTile, vertTriangle


# Fragment Shaders

@docs fragFill, fragCircle, fragNgon, fragImage, fragImageColor


# Mesh

@docs mesh, meshTriangle

-}

import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector4 exposing (Vec4)
import WebGL exposing (Mesh, Shader)
import WebGL.Texture exposing (Texture)


{-| -}
vertTriangle : Shader { a | i : Float } { b | uP : Vec2, uT : Vec4, vert0 : Vec2, vert1 : Vec2, vert2 : Vec2 } {}
vertTriangle =
    --http://in2gpu.com/2014/11/24/creating-a-triangle-in-opengl-shader/
    [glsl|
    precision mediump float;
    attribute float i;
    uniform vec2 vert0;
    uniform vec2 vert1;
    uniform vec2 vert2;
    uniform vec4 uT;
    uniform vec2 uP;
    void main () {
     vec2 aP;
     if (i == 0.) {
        aP = vert0;
     } else if (i == 1.) {
        aP = vert1;
     } else if (i == 2.) {
        aP = vert2;
     }
     gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
    }
    |]



-- Vertex Shaders


{-| -}
vertSprite : Shader { a | aP : Vec2 } { b | uP : Vec2, uT : Vec4, uUV : Vec4 } { uv : Vec2 }
vertSprite =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 uT;
            uniform vec2 uP;
            varying vec2 uv;
            uniform vec4 uUV;
            void main () {
                vec2 aP_ = aP * .5 + 0.5;
                uv =  uUV.xy + (aP_ * uUV.zw);
                gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
            }
        |]


{-| -}
vertImage : Shader { a | aP : Vec2 } { b | uP : Vec2, uT : Vec4 } { uv : Vec2 }
vertImage =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 uT;
            uniform vec2 uP;
            varying vec2 uv;
            void main () {
                uv = aP * .5 + 0.5;
                gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
            }
        |]


{-| -}
vertNone : Shader { a | aP : Vec2 } { b | uP : Vec2, uT : Vec4 } {}
vertNone =
    [glsl|
        precision mediump float;
        attribute vec2 aP;
        uniform vec4 uT;
        uniform vec2 uP;
        void main () {
            gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
        }
    |]


{-| -}
vertRect : Shader { a | aP : Vec2 } { b | uP : Vec2, uT : Vec4 } { uv : Vec2 }
vertRect =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 uT;
            uniform vec2 uP;
            varying vec2 uv;
            void main () {
                uv = aP;
                gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
            }
        |]


{-| -}
vertTile :
    Shader { a | aP : Vec2 }
        { b
            | uImgSize : Vec2
            , index : Float
            , spriteSize : Vec2
            , uP : Vec2
            , uT : Vec4
        }
        { uv : Vec2 }
vertTile =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 uT;
            uniform vec2 uP;
            uniform float index;
            uniform vec2 spriteSize;
            uniform vec2 uImgSize;
            varying vec2 uv;
            void main () {
                vec2 ratio = spriteSize / uImgSize;
                float row = floor(index * ratio.x);
                float column = index - row * (ratio.x);
                vec2 offset = vec2(column, row) * ratio;
                uv = (aP * .5 + 0.5) * ratio + offset;
                gl_Position = vec4(aP * mat2(uT) + uP, 0.5, 1.0);
            }
        |]



--Fragment Shaders


{-| -}
fragImage : Shader a { b | uImg : Texture, uImgSize : Vec2 } { uv : Vec2 }
fragImage =
    --(2i + 1)/(2N) Pixel perfect center
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform vec2 uImgSize;
        uniform sampler2D uImg;

        void main () {
            vec2 pixel = (floor(uv * uImgSize) + 0.5) / uImgSize;
            gl_FragColor = texture2D(uImg, pixel);
        }
    |]


{-| -}
fragImageColor : Shader a { b | color : Vec4, uImg : Texture, uImgSize : Vec2 } { uv : Vec2 }
fragImageColor =
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform vec2 uImgSize;
        uniform sampler2D uImg;
        uniform vec4 color;
        void main () {
            vec2 pixel = (floor(uv * uImgSize) + 0.5) / uImgSize;
            gl_FragColor = texture2D(uImg, pixel) * color;
        }
    |]


{-| -}
fragFill : Shader a { b | color : Vec4 } {}
fragFill =
    [glsl|
        precision mediump float;
        uniform vec4 color;

        void main () {
            gl_FragColor = color;

        }
    |]


{-| -}
fragCircle : Shader a { b | color : Vec4 } { uv : Vec2 }
fragCircle =
    [glsl|
        precision mediump float;
        uniform vec4 color;
        varying vec2 uv;
        void main () {
            gl_FragColor = color;
            gl_FragColor.a *= smoothstep(0.01,0.04,1.-length(uv));
        }
    |]


{-| -}
fragNgon : Shader a { b | color : Vec4, n : Float } { uv : Vec2 }
fragNgon =
    --https://thebookofshaders.com/07/
    --https://thndl.com/square-shaped-shaders.html
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



---MESHES


{-| -}
meshTriangle : WebGL.Mesh { i : Float }
meshTriangle =
    WebGL.triangleStrip
        [ { i = 0 }
        , { i = 1 }
        , { i = 2 }
        ]


{-| -}
mesh : Mesh { aP : Vec2 }
mesh =
    WebGL.triangleStrip
        [ { aP = vec2 -1 -1 }
        , { aP = vec2 -1 1 }
        , { aP = vec2 1 -1 }
        , { aP = vec2 1 1 }
        ]



---FUTURE DEVELOPMENT ---


fragImageSaturation =
    --https://github.com/AnalyticalGraphicsInc/cesium/blob/master/Source/Shaders/Builtin/Functions/saturation.glsl
    -- * @example
    -- * vec3 greyScale = saturation(color, 0.0);
    -- * vec3 doubleSaturation = saturation(color, 2.0);
    -- */
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform sampler2D uImg;
        uniform float adjustment;
        vec3 saturation(vec3 rgb, float adj) {
            // Algorithm from Chapter 16 of OpenGL Shading Language
            const vec3 W = vec3(0.2125, 0.7154, 0.0721);
            vec3 intensity = vec3(dot(rgb, W));
            return mix(intensity, rgb, adj);
        }
        void main () {
            gl_FragColor = texture2D(uImg, uv);
            gl_FragColor.xyz=saturation(gl_FragColor.xyz, adjustment);
        }
    |]


rotSprite =
    --https://discover.therookies.co/2019/08/13/unity-masterclass-how-to-set-up-your-project-for-pixel-perfect-retro-8-bit-games/
    --https://en.wikipedia.org/wiki/Pixel-art_scaling_algorithms#RotSprite
    [glsl|

        precision mediump float;
        varying vec2 uv;
        uniform vec2 uImgSize;
        uniform sampler2D uImg;

        void main () {
            vec2 pixel = (floor(uv * uImgSize) + 0.5) / uImgSize;
            gl_FragColor = texture2D(uImg, pixel);

        }
    |]


{-| FILTERS !!! <https://dev.to/lesnitsky/webgl-month-day-9-uImg-filters-5g8e>
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

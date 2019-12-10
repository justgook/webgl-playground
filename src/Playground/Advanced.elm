module Playground.Advanced exposing
    ( scaleX, scaleY
    , custom, useTexture
    , rect, circle, image, ngon, char
    , Render, Opacity, ScaleRotateSkew, Translate
    , entitySettings, mesh
    )

{-|


# Customize Shapes

@docs scaleX, scaleY


# Create Shape

Advanced user section, for usage in packages, or custom shader creation.

@docs custom, useTexture


# Renders

@docs rect, circle, image, ngon, char


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate


# Defaults

@docs entitySettings, mesh

-}

import Math.Vector2 exposing (Vec2)
import Math.Vector3 exposing (Vec3)
import Math.Vector4 exposing (Vec4, vec4)
import Playground.Font.GoodNeighbors as Font
import Playground.Internal exposing (CustomCustom(..), Form(..), Number, Shape(..), initShape)
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL exposing (Setting)
import WebGL.Settings.Blend as Blend
import WebGL.Texture as Texture exposing (Texture)


{-| Make a shape **horizontally** bigger or smaller.
Also can be used to flip object:

    sprite 20 27 0 "images/mario.png"
        |> scaleX -1

-}
scaleX : Number -> Shape -> Shape
scaleX sx (Shape shape) =
    Shape { shape | sx = shape.sx * sx }


{-| Make a shape **vertically** bigger or smaller.
Also can be used to flip object:

    sprite 20 27 0 "images/mario.png"
        |> scaleX -1

-}
scaleY : Number -> Shape -> Shape
scaleY sy (Shape shape) =
    Shape { shape | sy = shape.sy * sy }


{-| Create your own shape

    redRect =
        (\uP uT opacity ->
            WebGL.entity
                rectVertexShader
                fillFragment
                clipPlate
                { color = Math.Vector4.vec4 1 0 0 opacity
                , uP = uP
                , uT = uT
                }
        )
            |> custom

-}
custom : Number -> Number -> Render -> Shape
custom width height fn =
    initShape (Custom (CustomEnd width height fn))


{-| Get texture for your custom shader

    useTexture "images/mario/stand/left.gif" <|
        \t ->
            custom 32 32 <|
                myCustomRender t

-}
useTexture : String -> (Texture.Texture -> Shape) -> Shape
useTexture url fn =
    initShape (Custom (CustomTextured url fn))


{-| Function used in [`custom`](#custom)

    redRect : Render
    redRect uP uT opacity =
        WebGL.entity
            rectVertexShader
            fillFragment
            clipPlate
            { color = Math.Vector4.vec4 1 0 0 opacity
            , uP = uP
            , uT = uT
            }

-}
type alias Render =
    Translate
    -> ScaleRotateSkew
    -> Opacity
    -> WebGL.Entity


{-| Vec2 representing part of transform matrix for [`custom`](#custom)

    | 1 0 x |
    | 0 1 y |
    | 0 0 1 |

-}
type alias Translate =
    Math.Vector2.Vec2


{-| Vec4 representing part of transform matrix for [`custom`](#custom)

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Math.Vector4.Vec4


{-| -}
type alias Opacity =
    Float


{-| Rectangle render
-}
rect : Math.Vector3.Vec3 -> Render
rect color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertRect
        fragFill
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| Render circle or ellipse
-}
circle : Math.Vector3.Vec3 -> Render
circle color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertClip
        fragCircle
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , uT = uT
        }


{-| -}
ngon : Float -> Vec3 -> Render
ngon n color uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertClip
        fragNgon
        mesh
        { color = setAlpha color opacity
        , uP = uP
        , n = n
        , uT = uT
        }


{-| -}
image : Texture -> Vec2 -> Render
image image_ imageSize uP uT opacity =
    WebGL.entityWith
        entitySettings
        vertUV
        fragImagePX
        mesh
        { uP = uP
        , uT = uT
        , image = image_
        , imageSize = imageSize
        }


{-| Render single character
-}
char : Vec3 -> Vec4 -> Texture -> Vec2 -> Render
char color uv image_ imageSize translate scaleRotateSkew opacity =
    WebGL.entityWith
        entitySettings
        vertChar
        fragChar
        mesh
        { uP = translate
        , uT = scaleRotateSkew
        , image = image_
        , imageSize = imageSize
        , uUV = uv
        , color = setAlpha color opacity
        }


vertChar =
    [glsl|
            precision mediump float;
            attribute vec2 aP;
            uniform vec4 uT;
            uniform vec2 uP;
            varying vec2 uv;
            uniform vec4 uUV;
            void main () {
                vec2 aP_ = aP * .5 + 0.5;
                float ratio = 1.0 / 850.0;
                uv =  uUV.xy + (aP_ * uUV.zw);
                gl_Position = vec4(aP * mat2(uT) + uP, 0.5, 1.0);

            }
        |]


{-| -}
entitySettings : List Setting
entitySettings =
    [ Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
    , WebGL.colorMask True True True False
    ]


{-| -}
mesh : Mesh Plate
mesh =
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


vertRect =
    [glsl|
        precision mediump float;
        attribute vec2 aP;
        uniform vec4 uT;
        uniform vec2 uP;
        void main () {
            gl_Position = vec4(aP * mat2(uT) + uP, 0., 1.0);
        }
    |]


fragFill =
    --https://thndl.com/square-shaped-shaders.html
    [glsl|
        precision mediump float;
        uniform vec4 color;

        void main () {
            gl_FragColor = color;

        }
    |]


vertUV =
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


vertClip =
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


fragNgon =
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


fragImage =
    [glsl|
        precision mediump float;
        varying vec2 uv;
        uniform sampler2D image;

        void main () {
            gl_FragColor = texture2D(image, uv);
            //gl_FragColor.xyz=czm_saturation(gl_FragColor.xyz, 1.0);
        }
    |]


fragImageSaturation =
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


fragImagePX =
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
//            if (gl_FragColor.a < 0.1)
//            gl_FragColor = vec4( 0, 0, 1., 1.);
        }
    |]


fragChar =
    --(2i + 1)/(2N) Pixel perfect center
    --diffuseColor
    [glsl|
                precision mediump float;
                varying vec2 uv;
                uniform vec2 imageSize;
                uniform sampler2D image;
                uniform vec4 color;
                void main () {
                    vec2 pixel = (floor(uv * imageSize) + 0.5) / imageSize;
                    gl_FragColor = color * texture2D(image, pixel);
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


setAlpha c =
    c |> Math.Vector3.toRecord |> (\c1 -> Math.Vector4.vec4 c1.x c1.y c1.z)

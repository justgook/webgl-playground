module Render.LowLevel exposing (circle, ngon, rect)

import Math.Vector2
import Math.Vector4
import WebGL exposing (Mesh, Shader)
import WebGL.Settings as WebGL exposing (Setting)
import WebGL.Settings.Blend as Blend


rect =
    WebGL.entityWith
        entitySettings
        rectVertexShader
        fillFragment
        clipPlate


circle =
    WebGL.entityWith
        entitySettings
        uvVertex
        circleFragment
        clipPlate


ngon =
    WebGL.entityWith
        entitySettings
        uvVertex
        nGonFragment
        clipPlate


type alias TileUV =
    Math.Vector4.Vec4


entitySettings : List Setting
entitySettings =
    [ WebGL.cullFace WebGL.front
    , Blend.add Blend.srcAlpha Blend.oneMinusSrcAlpha
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



--rectVertexShader =
--    [glsl|
--        precision mediump float;
--        attribute vec2 aP;
--        uniform vec4 transformation;
--        uniform vec2 translation;
--        void main () {
--            mat3 delme = mat3(
--             transformation.x, transformation.y, translation.x,
--             transformation.z, transformation.w, translation.y,
--             0,0,1
--             );
--             vec3 aaa = vec3(aP, 1.);
--             vec3 result = aaa * delme;
--            gl_Position = vec4(result, 1.0);
--        }
--    |]


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
            varying vec2 uv;
            uniform vec2 translation;
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
            float angle = 0.0;
            float a = atan(uv.x,uv.y) + angle;
            float b = 6.28319 / n;
            float f = smoothstep(.5,.51, cos(floor(.5 + a/b)*b-a)*length(uv));
            gl_FragColor = color;
            gl_FragColor.a -= f;
        }
    |]


type alias Varyings =
    { uv : Math.Vector2.Vec2 }


type alias Plate =
    { aP : Math.Vector2.Vec2 }

module Main exposing (main)

import Shmup


main =
    Shmup.main



--
--fullScreen =
--    (\uP uT opacity ->
--        WebGL.entity
--            verFullScreen
--            fragFill
--            mesh
--            { color = Math.Vector4.vec4 1 0 0 opacity
--            }
--    )
--        |> custom 200 200
--
--
--verFullScreen =
--    [glsl|
--        precision mediump float;
--        attribute vec2 aP;
--        void main () {
--            gl_Position = vec4(aP, 0., 1.0);
--        }
--    |]

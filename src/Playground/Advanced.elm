module Playground.Advanced exposing (custom, useTexture)

{-| Advanced user section, for usage in packages, or custom Shape creation.

@docs custom, useTexture

-}

import Playground.Internal exposing (Form(..), Number, Shape(..))
import Playground.Render exposing (Opacity, Render)
import WebGL.Texture as Texture exposing (Texture)


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
custom width height render =
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Form width height render }


{-| Get texture for your custom Shape

    useTexture "image.png" <|
        \t ->
            custom 32 32 <|
                myCustomRender t

-}
useTexture : String -> (Texture.Texture -> Shape) -> Shape
useTexture url fn =
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Textured url fn }

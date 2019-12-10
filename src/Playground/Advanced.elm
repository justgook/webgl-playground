module Playground.Advanced exposing (custom, useTexture)

{-| Advanced user section, for usage in packages, or custom shader creation.

@docs custom, useTexture

-}

import Playground.Internal exposing (CustomCustom(..), Form(..), Number, Shape(..), initShape)
import Playground.Render exposing (Render)
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

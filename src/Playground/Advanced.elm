module Playground.Advanced exposing
    ( custom, useTexture
    , Render, Opacity, ScaleRotateSkew, Translate
    , embed, subscriptions, resize, cacheTextures
    )

{-| Advanced user section, for:

1.  packages authors (if you like create addons)
2.  custom Shape creation
3.  embedding into other app


# Shape

@docs custom, useTexture


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate


# Embedding

@docs embed, subscriptions, resize, cacheTextures

-}

import Html exposing (Html)
import Math.Vector2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Playground.Internal as Internal exposing (Form(..), Game(..), Number, Shape(..))
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture)


{-| Create your own shape

    redRect w h =
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
            |> custom w h

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



------ Embed Stuff ------


{-| When you need advanced application or just need to playground be part of your app
-}
embed :
    (Internal.Computer -> memory -> List Shape)
    -> (Internal.Computer -> memory -> memory)
    -> memory
    ->
        { init : ( Internal.Game memory, Cmd Internal.Msg )
        , view : Internal.Game memory -> Html a
        , update : Internal.Msg -> Internal.Game memory -> ( Internal.Game memory, Cmd Internal.Msg )
        }
embed viewMemory updateMemory initialMemory =
    let
        { init, update } =
            Internal.embed viewMemory updateMemory initialMemory

        view (Game { computer, entities }) =
            Internal.embedViewWrap computer.screen entities
    in
    { init = init
    , view = view
    , update = update
    }


{-| Playground have different subscriptions:

  - `picture` uses only `subscriptions.resize`
  - `animation` uses only `subscriptions.resize` and `subscriptions.time`
  - `game` uses all (shortcut `subscriptions.all`)

When embedding playground you can choose what to use:

  - `keys` - listen to keyboard events (keyUp and keyDown) and updates
  - `time` - updates `Computer.time` each frame
  - `click` - listen on mouse movement `Computer.mouse.down/click`
  - `mouse` - listen on mouse movement `Computer.mouse.x/y`
  - `resize` - listen on window resize and updates `Computer.screen`

-}
subscriptions :
    { keys : Sub Internal.Msg
    , time : Sub Internal.Msg
    , click : Sub Internal.Msg
    , mouse : Sub Internal.Msg
    , resize : Sub Internal.Msg
    , all : Sub Internal.Msg
    }
subscriptions =
    Internal.subscriptions


{-| Embedded target is be part of your application,
most of time it will not take whole screen,
so it is up to you decide on dimensions, and pass it to update function as Msg
-}
resize : Int -> Int -> Internal.Msg
resize =
    Internal.resize


{-| All textures start loading only after first time request in `view`,
with slow network that can take some time,
but for more advanced games and more smooth transition you can cache textures for later use,
so when you need show new image - texture already there.
-}
cacheTextures : List ( String, Texture ) -> Internal.Msg
cacheTextures =
    Internal.setTexture


{-| Create Your own Render and use it in [`Advanced.custom`](Playground-Advanced#custom) to create [`Shape`](Playground#Shape)

    redRect : Render
    redRect uP uT opacity =
        WebGL.entityWith
            defaultEntitySettings
            vertNone
            fragFill
            mesh
            { color = setAlpha color opacity
            , uP = uP
            , uT = uT
            }

-}
type alias Render =
    Translate
    -> ScaleRotateSkew
    -> Opacity
    -> WebGL.Entity


{-| Vec2 representing part of transform matrix for [`Advanced.custom`](Playground-Advanced#custom)

    | 1 0 x |
    | 0 1 y |
    | 0 0 1 |

-}
type alias Translate =
    Vec2


{-| Vec4 representing part of transform matrix for [`Advanced.custom`](Playground-Advanced#custom)

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Vec4


{-| -}
type alias Opacity =
    Float

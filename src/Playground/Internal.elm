module Playground.Internal exposing
    ( Computer
    , Keyboard
    , Mouse
    , Msg(..)
    , Playground(..)
    , Screen
    , TextureManager
    , Time
    , create
    , gotTextures
    , mouseClick
    , mouseDown
    , requestTexture
    , subscriptions
    , toScreen
    , updateKeyboard
    , viewWrap
    )

{-| -}

import Browser.Dom as Dom
import Browser.Events as E
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as H
import Json.Decode as D
import Set exposing (Set)
import Task
import Time
import WebGL exposing (Entity)
import WebGL.Shape2d exposing (Shape2d, toEntities)
import WebGL.Texture as Texture exposing (Texture, nonPowerOfTwoOptions)


create :
    (Computer -> memory -> List Shape2d)
    -> (Computer -> memory -> memory)
    -> memory
    ->
        { init : ( Playground memory, Cmd Msg )
        , view : Playground memory -> List Entity
        , update : Msg -> Playground memory -> ( Playground memory, Cmd Msg )
        }
create viewMemory updateMemory initialMemory =
    let
        init =
            ( Playground
                { visibility = E.Visible
                , memory = initialMemory
                , textures = { done = Dict.empty, loading = Set.empty }
                , computer = initialComputer
                , entities = []
                }
            , Cmd.batch
                [ Task.perform GotViewport Dom.getViewport
                , Task.perform InitTime Time.now
                ]
            )

        view (Playground { entities }) =
            entities
    in
    { init = init
    , view = view
    , update = gameUpdate viewMemory updateMemory
    }


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = { now = 0, delta = 0 }
    }


requestTexture : Set String -> TextureManager -> ( TextureManager, Cmd Msg )
requestTexture missing textures =
    missing
        |> Set.foldl
            (\url (( { done, loading }, acc2 ) as acc) ->
                if Set.member url loading then
                    acc

                else
                    ( { loading = Set.insert url loading, done = done }
                    , (Texture.loadWith textureOption url
                        |> Task.map (Tuple.pair url)
                        |> Task.mapError (Tuple.pair url)
                        |> Task.attempt GotTexture
                      )
                        :: acc2
                    )
            )
            ( textures, [] )
        |> Tuple.mapSecond Cmd.batch


textureOption : Texture.Options
textureOption =
    { magnify = Texture.linear
    , minify = Texture.linear
    , horizontalWrap = Texture.clampToEdge
    , verticalWrap = Texture.clampToEdge
    , flipY = True
    }


gotTextures : Result ( String, Texture.Error ) ( String, Texture ) -> TextureManager -> TextureManager
gotTextures r ({ done, loading } as textures) =
    case r of
        Ok ( name, t ) ->
            { done =
                Dict.insert name t done
            , loading = Set.remove name loading
            }

        Err ( name, err ) ->
            textures


gameUpdate : (Computer -> memory -> List Shape2d) -> (Computer -> memory -> memory) -> Msg -> Playground memory -> ( Playground memory, Cmd Msg )
gameUpdate viewMemory updateMemory msg (Playground ({ visibility, memory, textures, computer } as model)) =
    case msg of
        Tick time ->
            let
                now =
                    Time.posixToMillis time

                d =
                    now - computer.time.now
            in
            gameTick viewMemory updateMemory <| { model | computer = { computer | time = { now = now, delta = d } } }

        GotViewport { viewport } ->
            ( Playground { model | computer = { computer | screen = toScreen viewport.width viewport.height } }
            , Cmd.none
            )

        Resized newScreen ->
            ( Playground { model | computer = { computer | screen = newScreen } }
            , Cmd.none
            )

        KeyChanged isDown key ->
            ( Playground { model | computer = { computer | keyboard = updateKeyboard isDown key computer.keyboard } }
            , Cmd.none
            )

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            ( Playground { model | computer = { computer | mouse = mouseMove x y computer.mouse } }
            , Cmd.none
            )

        MouseClick ->
            ( Playground { model | computer = { computer | mouse = mouseClick True computer.mouse } }
            , Cmd.none
            )

        MouseButton isDown ->
            ( Playground { model | computer = { computer | mouse = mouseDown isDown computer.mouse } }
            , Cmd.none
            )

        VisibilityChanged vis ->
            ( Playground
                { model
                    | visibility = vis
                    , computer =
                        { computer
                            | time = { now = computer.time.now, delta = 0 }
                            , keyboard = emptyKeyboard
                            , mouse = Mouse computer.mouse.x computer.mouse.y False False
                        }
                }
            , Cmd.none
            )

        GotTexture r ->
            ( Playground
                { model
                    | textures = gotTextures r model.textures
                    , computer = { computer | time = { now = computer.time.now, delta = 0 } }
                }
            , Cmd.none
            )

        InitTime time ->
            ( Playground { model | computer = { computer | time = { now = Time.posixToMillis time, delta = 0 } } }, Cmd.none )


gameTick viewMemory updateMemory ({ computer, memory, textures } as model) =
    let
        newMemory =
            updateMemory computer memory

        newComputer =
            if computer.mouse.click then
                { computer | mouse = mouseClick False computer.mouse }

            else
                computer

        ( entities, missing ) =
            toEntities textures.done computer.screen (viewMemory newComputer newMemory)
    in
    if Set.isEmpty missing then
        ( Playground { model | entities = entities, computer = newComputer, memory = newMemory }
        , Cmd.none
        )

    else
        requestTexture missing textures
            |> Tuple.mapFirst
                (\loadingTextures ->
                    Playground
                        { model
                            | entities = entities
                            , textures = loadingTextures
                            , computer = newComputer
                            , memory = newMemory
                        }
                )



-- RENDER


canvasStyle : Html msg
canvasStyle =
    Html.node "style" [] [ Html.text "canvas {position: absolute; top: 0; right:0; bottom: 0; left: 0; } " ]


viewWrap : Screen -> List Entity -> List (Html msg)
viewWrap screen entities =
    [ canvasStyle
    , WebGL.toHtmlWith webGLOption
        [ H.width (round screen.width)
        , H.height (round screen.height)
        ]
        entities
    ]


webGLOption : List WebGL.Option
webGLOption =
    [ WebGL.alpha False
    , WebGL.depth 1
    , WebGL.clearColor 1 1 1 1
    ]



-- MOUSE HELPERS


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }



-- KEYBOARD HELPERS


emptyKeyboard : Keyboard
emptyKeyboard =
    { up = False
    , down = False
    , left = False
    , right = False
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.empty
    }


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    let
        keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    in
    case key of
        "Space" ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "ShiftLeft" ->
            { keyboard | keys = keys, shift = isDown }

        "ShiftRight" ->
            { keyboard | keys = keys, shift = isDown }

        "Backspace" ->
            { keyboard | keys = keys, backspace = isDown }

        "ArrowUp" ->
            { keyboard | keys = keys, up = isDown }

        "ArrowDown" ->
            { keyboard | keys = keys, down = isDown }

        "ArrowLeft" ->
            { keyboard | keys = keys, left = isDown }

        "ArrowRight" ->
            { keyboard | keys = keys, right = isDown }

        _ ->
            { keyboard | keys = keys }



-- SCREEN HELPERS


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }



-- SUBSCRIPTIONS


subscriptions :
    { keys : Sub Msg
    , time : Sub Msg
    , visibility : Sub Msg
    , click : Sub Msg
    , mouse : Sub Msg
    , resize : Sub Msg
    }
subscriptions =
    { keys =
        [ E.onKeyUp (D.map (KeyChanged False) (D.field "code" D.string))
        , E.onKeyDown
            (D.field "repeat" D.bool
                |> D.andThen
                    (\repeat ->
                        if repeat then
                            D.fail ""

                        else
                            D.field "code" D.string
                                |> D.map (KeyChanged True)
                    )
            )
        ]
            |> Sub.batch
    , time = E.onAnimationFrame Tick
    , visibility = E.onVisibilityChange VisibilityChanged
    , click =
        [ E.onClick (D.succeed MouseClick)
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False))
        ]
            |> Sub.batch
    , mouse = E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
    , resize = E.onResize (\w h -> Resized <| toScreen (toFloat w) (toFloat h))
    }


type Playground memory
    = Playground
        { visibility : E.Visibility
        , memory : memory
        , textures : TextureManager
        , computer : Computer
        , entities : List Entity
        }


type alias TextureManager =
    { done : Dict String Texture
    , loading : Set String
    }


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | InitTime Time.Posix
    | GotViewport Dom.Viewport
    | Resized Screen
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool
    | GotTexture (Result ( String, Texture.Error ) ( String, Texture ))


type alias Time =
    { now : Int, delta : Int }


type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


type alias Screen =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    }


type alias Keyboard =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : Set String
    }


type alias Mouse =
    { x : Float
    , y : Float
    , down : Bool
    , click : Bool
    }

module Playground.Internal exposing
    ( Computer
    , Form(..)
    , Game(..)
    , Mouse
    , Msg(..)
    , Number
    , Shape(..)
    , TextureData(..)
    , TextureManager
    , Time(..)
    , create
    , embedViewWrap
    , game
    , gotTextures
    , render
    , requestTexture
    , resize
    , setTexture
    , subscriptions
    , subscriptions_
    , toScreen
    , viewWrap
    )

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as H
import Json.Decode as D
import Math.Vector2
import Math.Vector3
import Math.Vector4
import Playground.Transformation as Trans exposing (Transformation)
import Set exposing (Set)
import Task
import Time
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (Texture, nonPowerOfTwoOptions)


type Shape
    = Shape
        { x : Number
        , y : Number
        , a : Number
        , sx : Number
        , sy : Number
        , o : Number
        , form : Form
        }


type Form
    = Form Number Number Render
    | Textured String (Texture -> Shape)
    | Group (List Shape)


type alias Color =
    Math.Vector3.Vec3


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float


type alias Render =
    Math.Vector2.Vec2
    -> Math.Vector4.Vec4
    -> Float
    -> WebGL.Entity


{-| -}
game : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> memory -> Program () (Game memory) Msg
game viewMemory updateMemory initialMemory =
    let
        { init, update } =
            create viewMemory updateMemory initialMemory

        subs (Game { visibility, computer }) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    subscriptions.all

        view (Game { computer, entities }) =
            { title = "Playground"
            , body = viewWrap computer.screen entities
            }
    in
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subs
        }


create :
    (Computer -> memory -> List Shape)
    -> (Computer -> memory -> memory)
    -> memory
    ->
        { init : ( Game memory, Cmd Msg )
        , view : Game memory -> List Entity
        , update : Msg -> Game memory -> ( Game memory, Cmd Msg )
        }
create viewMemory updateMemory initialMemory =
    let
        init =
            ( Game
                { visibility = E.Visible
                , memory = initialMemory
                , textures = Dict.empty
                , computer = initialComputer
                , entities = []
                }
            , Task.perform GotViewport Dom.getViewport
            )

        view (Game { entities }) =
            entities
    in
    { init = init
    , view = view
    , update = gameUpdate viewMemory updateMemory
    }


type alias TextureManager =
    Dict String TextureData


type TextureData
    = Loading
    | Success { texture : Texture, size : Math.Vector2.Vec2 }
    | Fail Texture.Error


type Game memory
    = Game
        { visibility : E.Visibility
        , memory : memory
        , textures : TextureManager
        , computer : Computer
        , entities : List Entity
        }


requestTexture : List String -> TextureManager -> ( TextureManager, Cmd Msg )
requestTexture missing textures =
    missing
        |> List.foldl
            (\url ( acc1, acc2 ) ->
                ( Dict.insert url Loading acc1
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



--|> Tuple.mapSecond
--    (Task.sequence >> Task.attempt GotTexture)


textureOption : Texture.Options
textureOption =
    { nonPowerOfTwoOptions
        | magnify = Texture.linear
        , minify = Texture.linear

        --, horizontalWrap = Texture.mirroredRepeat
        --, verticalWrap = Texture.mirroredRepeat
    }


gotTextures : Result ( String, Texture.Error ) ( String, Texture ) -> TextureManager -> TextureManager
gotTextures r textures =
    case r of
        Ok ( name, t ) ->
            ({ texture = t
             , size =
                Texture.size t
                    |> (\( w, h ) -> Math.Vector2.vec2 (toFloat w) (toFloat h))
             }
                |> Success
                |> Dict.insert name
            )
                textures

        Err ( name, err ) ->
            Dict.insert name (Fail err) textures


gameUpdate : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> Msg -> Game memory -> ( Game memory, Cmd Msg )
gameUpdate viewMemory updateMemory msg (Game ({ visibility, memory, textures, computer } as model)) =
    case msg of
        Tick time ->
            let
                (Time timeWas _) =
                    computer.time

                d =
                    Time.posixToMillis time - Time.posixToMillis timeWas

                newModel =
                    { model
                        | memory = updateMemory computer memory
                        , computer =
                            if computer.mouse.click then
                                { computer | time = Time time d, mouse = mouseClick False computer.mouse }

                            else
                                { computer | time = Time time d }
                    }

                --TODO move that after all updates
                ( entities, missing ) =
                    render computer.screen textures (viewMemory newModel.computer newModel.memory)
            in
            case missing of
                [] ->
                    ( Game { newModel | entities = entities }, Cmd.none )

                _ ->
                    requestTexture missing textures
                        |> Tuple.mapFirst (\loadingTextures -> Game { newModel | entities = entities, textures = loadingTextures })

        GotViewport { viewport } ->
            ( Game { model | computer = { computer | screen = toScreen viewport.width viewport.height } }, Cmd.none )

        Resized newScreen ->
            ( Game { model | computer = { computer | screen = newScreen } }, Cmd.none )

        KeyChanged isDown key ->
            ( Game { model | computer = { computer | keyboard = updateKeyboard isDown key computer.keyboard } }, Cmd.none )

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            ( Game { model | computer = { computer | mouse = mouseMove x y computer.mouse } }, Cmd.none )

        MouseClick ->
            ( Game { model | computer = { computer | mouse = mouseClick True computer.mouse } }, Cmd.none )

        MouseButton isDown ->
            ( Game { model | computer = { computer | mouse = mouseDown isDown computer.mouse } }, Cmd.none )

        VisibilityChanged vis ->
            ( { model
                | visibility = vis
                , computer =
                    { computer
                        | keyboard = emptyKeyboard
                        , mouse = Mouse computer.mouse.x computer.mouse.y False False
                    }
              }
                |> Game
            , Cmd.none
            )

        GotTexture r ->
            ( Game { model | textures = gotTextures r model.textures }, Cmd.none )



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


embedViewWrap : Screen -> List Entity -> Html msg
embedViewWrap screen entities =
    WebGL.toHtmlWith webGLOption
        [ H.width (round screen.width)
        , H.height (round screen.height)
        ]
        entities


render : Screen -> TextureManager -> List Shape -> ( List Entity, List String )
render screen textures shapes =
    List.foldr (renderShape screen textures Trans.identity 1) ( [], Set.empty ) shapes
        |> Tuple.mapSecond Set.toList


renderShape : Screen -> TextureManager -> Transformation -> Float -> Shape -> ( List Entity, Set String ) -> ( List Entity, Set String )
renderShape screen textures parent parentOpacity (Shape { x, y, a, sx, sy, o, form }) (( entities, missing ) as acc) =
    let
        createTrans tx ty sx_ sy_ a_ =
            Trans.transform tx ty sx_ sy_ a_
                |> Trans.apply parent

        opacity =
            o * parentOpacity
    in
    case form of
        Form width height fn ->
            let
                ( t1, t2 ) =
                    createTrans (x * 2) (y * 2) (width * sx) (height * sy) a
                        |> Trans.scale (1 / screen.width) (1 / screen.height)
                        |> Trans.toGL
            in
            ( fn t2 t1 opacity :: entities, missing )

        Textured src fn ->
            case ( Set.member src missing, Dict.get src textures ) of
                ( _, Just (Success { texture, size }) ) ->
                    renderShape screen textures (createTrans (x * 2) (y * 2) sx sy a) opacity (fn texture) acc

                ( False, Nothing ) ->
                    ( entities, Set.insert src missing )

                _ ->
                    acc

        Group shapes ->
            shapes
                |> List.foldr (renderShape screen textures (createTrans (x * 2) (y * 2) sx sy a) opacity) acc


webGLOption : List WebGL.Option
webGLOption =
    [ WebGL.alpha False
    , WebGL.depth 1
    , WebGL.clearColor 1 1 1 1
    ]


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = Time (Time.millisToPosix 0) 0
    }



-- SUBSCRIPTIONS


subscriptions_ :
    { keys : Sub Msg
    , time : Sub Msg
    , visibility : Sub Msg
    , click : Sub Msg
    , mouse : Sub Msg
    , resize : Sub Msg
    }
subscriptions_ =
    { keys =
        [ E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown
            (D.field "repeat" D.bool
                |> D.andThen
                    (\repeat ->
                        if repeat then
                            D.fail ""

                        else
                            D.field "key" D.string
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
    , resize = E.onResize resize
    }


subscriptions :
    { keys : Sub Msg
    , time : Sub Msg
    , click : Sub Msg
    , mouse : Sub Msg
    , resize : Sub Msg
    , all : Sub Msg
    }
subscriptions =
    { keys = subscriptions_.keys
    , time = subscriptions_.time
    , click = subscriptions_.click
    , mouse = subscriptions_.mouse
    , resize = subscriptions_.resize
    , all =
        [ subscriptions_.keys
        , subscriptions_.time
        , subscriptions_.visibility
        , subscriptions_.click
        , subscriptions_.mouse
        , subscriptions_.resize
        ]
            |> Sub.batch
    }



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


resize : Int -> Int -> Msg
resize w h =
    Resized (toScreen (toFloat w) (toFloat h))


setTexture : ( String, Texture ) -> Msg
setTexture =
    Ok >> GotTexture



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
        " " ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "Shift" ->
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


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Dom.Viewport
    | Resized Screen
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool
    | GotTexture (Result ( String, Texture.Error ) ( String, Texture ))


type Time
    = Time Time.Posix Int


type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


type alias Screen =
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
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
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }

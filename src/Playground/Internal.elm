module Playground.Internal exposing
    ( Msg(..)
    , Playground(..)
    , PlaygroundModel
    , gameUpdate
    , initModel
    , mouseSubscription
    , render
    , requestScreen
    , update
    )

import Browser.Dom as Dom
import Browser.Events as E
import Json.Decode as D
import Task
import WebGL exposing (Entity)
import WebGL.Shape2d as Shape2d exposing (Keyboard, Screen, Time)
import WebGL.Shape2d.TexturedShape as TexturedShape exposing (TextureLoader(..), TexturedShape)
import WebGL.Texture as Texture exposing (Texture)


{-| Playground State
-}
type Playground memory
    = Playground (PlaygroundModel memory)


type alias PlaygroundModel memory =
    Shape2d.Model Screen { memory : memory, computer : Computer }


type Msg
    = MouseMsg (Mouse -> Mouse)
    | KeyboardMsg (Keyboard -> Keyboard)
    | ScreenMsg Screen
    | TimeMsg (Time -> Time)
    | Textures (TextureLoader String -> TextureLoader String)


mouseSubscription : Sub (Mouse -> Mouse)
mouseSubscription =
    Sub.batch
        [ E.onMouseDown
            (D.map2 (\x y mouse -> { mouse | down = True, x = x, y = y })
                (D.field "pageX" D.float)
                (D.field "pageY" D.float)
            )
        , E.onMouseUp
            (D.map2 (\x y mouse -> { mouse | down = False, click = True, x = x, y = y })
                (D.field "pageX" D.float)
                (D.field "pageY" D.float)
            )
        , E.onMouseMove
            (D.map2 (\x y mouse -> { mouse | x = x, y = y })
                (D.field "pageX" D.float)
                (D.field "pageY" D.float)
            )
        ]


type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }


type alias Mouse =
    { x : Float
    , y : Float
    , down : Bool
    , click : Bool
    }


initModel : memory -> PlaygroundModel memory
initModel memory =
    { textures = Shape2d.textureManager
    , entities = []
    , screen = Shape2d.toScreen 300 300
    , memory = memory
    , computer =
        { mouse = { x = 0, y = 0, down = False, click = False }
        , keyboard = Shape2d.initKeyboard
        , screen = Shape2d.toScreen 300 300
        , time = { now = 0, delta = 0 }
        }
    }


gameUpdate :
    (PlaygroundModel memory -> List (TexturedShape String))
    -> (Computer -> memory -> memory)
    -> Msg
    -> Playground memory
    -> ( Playground memory, Cmd Msg )
gameUpdate view updater msg model =
    let
        (Playground mm) =
            parseMsg msg model

        mmm =
            { mm | memory = updater mm.computer mm.memory }

        ( entities, textures, cmd ) =
            render view (Playground mmm)
    in
    ( Playground { mmm | entities = entities, textures = textures }, cmd )


parseMsg : Msg -> Playground memory -> Playground memory
parseMsg msg (Playground ({ computer } as model)) =
    case msg of
        TimeMsg fn ->
            if computer.mouse.click then
                let
                    mouse =
                        computer.mouse
                in
                Playground
                    { model
                        | computer =
                            { computer
                                | time = fn computer.time
                                , mouse = { mouse | click = False }
                            }
                    }

            else
                Playground { model | computer = { computer | time = fn computer.time } }

        MouseMsg fn ->
            let
                mouse =
                    fn computer.mouse
            in
            Playground
                { model
                    | computer =
                        { computer
                            | mouse =
                                { mouse
                                    | x = computer.screen.left + mouse.x
                                    , y = computer.screen.top - mouse.y
                                }
                        }
                }

        KeyboardMsg fn ->
            Playground { model | computer = { computer | keyboard = fn computer.keyboard } }

        ScreenMsg screen ->
            Playground { model | screen = screen, computer = { computer | screen = screen } }

        Textures fn ->
            Playground { model | textures = fn model.textures }


render : (PlaygroundModel memory -> List (TexturedShape String)) -> Playground memory -> ( List Entity, TextureLoader String, Cmd Msg )
render viewFn (Playground model) =
    let
        ( entities, TextureLoader textures ) =
            viewFn model
                |> TexturedShape.toEntities model.screen model.textures

        ( loader, missing ) =
            textures.extract ()

        cmd2 =
            case missing of
                [] ->
                    Cmd.none

                _ ->
                    List.map load missing
                        |> Task.sequence
                        |> Task.attempt
                            (\r ->
                                case r of
                                    Ok value ->
                                        Textures
                                            (\tt ->
                                                List.foldl (\( k, v ) (TextureLoader acc) -> acc.insert k v) tt value
                                            )

                                    Err err ->
                                        Textures identity
                            )
    in
    ( entities, loader, cmd2 )


update :
    (PlaygroundModel memory -> List (TexturedShape String))
    -> Msg
    -> Playground memory
    -> ( Playground memory, Cmd Msg )
update view msg model =
    let
        ((Playground mm) as mmm) =
            parseMsg msg model

        ( entities, textures, cmd ) =
            render view mmm
    in
    ( Playground { mm | entities = entities, textures = textures }, cmd )


textureOption : Texture.Options
textureOption =
    { magnify = Texture.linear
    , minify = Texture.linear
    , horizontalWrap = Texture.clampToEdge
    , verticalWrap = Texture.clampToEdge
    , flipY = True
    }


load : String -> Task.Task Texture.Error ( String, Texture )
load src =
    Texture.loadWith textureOption src
        |> Task.map (\t -> ( src, t ))


requestScreen : Cmd Msg
requestScreen =
    Dom.getViewport
        |> Task.map
            (\{ scene } -> ScreenMsg (Shape2d.toScreen scene.width scene.height))
        |> Task.attempt (Result.withDefault (ScreenMsg (Shape2d.toScreen 100 100)))

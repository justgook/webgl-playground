module Computer exposing (main, subscriptions)

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Dict exposing (Dict)
import Html.Attributes exposing (height, width)
import Json.Decode as D
import Math.Vector3 exposing (vec3)
import Playground exposing (..)
import Playground.Internal exposing (mouseClick, mouseDown, toScreen, updateKeyboard)
import Set exposing (Set)
import Task
import Time
import WebGL
import WebGL.Shape2d exposing (..)
import WebGL.Texture as Texture exposing (Texture)


type alias Model =
    { computer : Computer
    , entities : List WebGL.Entity
    , textures : { done : Dict String Texture, loading : Set String }
    }


type Msg
    = Computer Computer
    | Texture String Texture
    | TextureFail Texture.Error


view : Model -> List Shape
view _ =
    [ [ rectangle (vec3 1 0 0) 30 30
      , rectangle (vec3 0 1 0) 30 30 |> move 5 5
      , rectangle (vec3 0 0 1) 30 30 |> move 10 10
      , words blue "hello"
      ]
        |> group
    ]


main : Program () Model Msg
main =
    Browser.element
        { init =
            \_ ->
                ( initModel
                , Task.perform (\{ scene } -> { initialComputer | screen = toScreen scene.width scene.height }) Dom.getViewport
                    |> Cmd.map Computer
                )
        , view = viewWrap
        , update = update view
        , subscriptions =
            \model ->
                [ subscriptions.resize model.computer
                , subscriptions.time model.computer
                , subscriptions.mouse model.computer
                , subscriptions.click model.computer
                , subscriptions.keys model.computer
                ]
                    |> Sub.batch
                    |> Sub.map Computer
        }


update view_ msg ({ textures } as model) =
    case msg of
        Computer c ->
            let
                computer =
                    unClick model.computer c

                ( entities, missing ) =
                    view_ model
                        |> WebGL.Shape2d.toEntities textures.done
                            { width = computer.screen.width
                            , height = computer.screen.height
                            }

                loading =
                    Set.diff missing textures.loading
            in
            ( { model
                | computer = computer
                , entities = entities
                , textures = { textures | loading = Set.union loading textures.loading }
              }
            , loading
                |> Set.foldl (\url -> (::) (getTexture url)) []
                |> Cmd.batch
            )

        Texture url t ->
            ( { model
                | textures =
                    { textures
                        | loading = Set.remove url textures.loading
                        , done = Dict.insert url t textures.done
                    }
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


initModel : Model
initModel =
    { computer = initialComputer
    , textures = { done = Dict.empty, loading = Set.empty }
    , entities = []
    }


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard =
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
    , screen = toScreen 600 600
    , time = { now = 0, delta = 0 }
    }


viewWrap { computer, entities } =
    entities
        |> WebGL.toHtmlWith [ WebGL.alpha False, WebGL.depth 1, WebGL.clearColor 1 1 1 1 ]
            [ width (round computer.screen.width), height (round computer.screen.height) ]


subscriptions :
    { click : Computer -> Sub Computer
    , keys : Computer -> Sub Computer
    , mouse : Computer -> Sub Computer
    , resize : Computer -> Sub Computer
    , time : Computer -> Sub Computer
    }
subscriptions =
    { keys =
        \computer ->
            [ E.onKeyUp (D.map (\k -> { computer | keyboard = updateKeyboard False k computer.keyboard }) (D.field "key" D.string))
            , E.onKeyDown (D.map (\k -> { computer | keyboard = updateKeyboard True k computer.keyboard }) (D.field "key" D.string))
            ]
                |> Sub.batch
    , time =
        \computer ->
            E.onAnimationFrame
                (\time ->
                    let
                        now =
                            Time.posixToMillis time

                        d =
                            now - computer.time.now
                    in
                    { computer | time = { now = now, delta = d } }
                )
    , click =
        \computer ->
            [ E.onClick (D.succeed { computer | mouse = mouseClick True computer.mouse })
            , E.onMouseDown (D.succeed { computer | mouse = mouseDown True computer.mouse })
            , E.onMouseUp (D.succeed { computer | mouse = mouseDown False computer.mouse })
            ]
                |> Sub.batch
    , mouse =
        \computer ->
            E.onMouseMove
                (D.map2
                    (\pageX pageY ->
                        let
                            x =
                                computer.screen.left + pageX

                            y =
                                computer.screen.top - pageY

                            mouse =
                                computer.mouse
                        in
                        { computer | mouse = { mouse | x = x, y = y } }
                    )
                    (D.field "pageX" D.float)
                    (D.field "pageY" D.float)
                )
    , resize = \computer -> E.onResize (\w h -> { computer | screen = toScreen (toFloat w) (toFloat h) })
    }


unClick : Computer -> Computer -> Computer
unClick was computer =
    if was.mouse.click then
        { computer | mouse = mouseClick False computer.mouse }

    else
        computer


getTexture : String -> Cmd Msg
getTexture url =
    Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        Texture url t

                    Err e ->
                        TextureFail e
            )

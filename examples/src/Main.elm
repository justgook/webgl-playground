module Main exposing (main)

import Array exposing (Array)
import Browser
import Circles
import Clock
import Extra.Font
import Extra.JumpGun as JumpGun
import Extra.Mario as Mario
import Font
import GitHubCorner
import HexGrid
import Mouse
import MulticolorPolygon
import Playground exposing (..)
import Playground.Internal exposing (embed, subscriptions)
import Polygon
import Set
import Shmup
import Tree
import Vectors


config =
    { cols = 4
    , padding = 10
    , topOffset = 70
    }


animationTime =
    60


main : Program () (Playground Memory) Msg
main =
    Browser.document
        { init = \_ -> playground.init
        , view =
            \memory ->
                { title = "WebGL Playground Demo"
                , body =
                    [ GitHubCorner.topRight "https://github.com/justgook/webgl-playground"
                    , playground.view memory
                    ]
                }
        , update = playground.update
        , subscriptions =
            \_ ->
                [ subscriptions.keys
                , subscriptions.time
                , subscriptions.visibility
                , subscriptions.click
                , subscriptions.mouse
                , subscriptions.resize
                ]
                    |> Sub.batch
        }


viewMemory : Computer -> Memory -> List Shape
viewMemory ({ screen, time } as computer) memory =
    (case memory of
        Init _ ->
            []

        Menu menu ->
            viewMenu menu time

        Open item _ ->
            viewExample item.example.run computer

        Opening item left menu ->
            let
                t =
                    left / animationTime

                p =
                    t |> bezier 1 0.1 1 -1

                s =
                    t |> bezier 0.1 1 0.1 1
            in
            [ viewMenu menu time
                |> group
                |> fade t
            , viewExample item.example.run computer
                |> group
                |> scaleX ((1 - (menu.w / screen.width)) * (1 - s) + (menu.w / screen.width))
                |> scaleY ((1 - (menu.h / screen.height)) * (1 - s) + (menu.h / screen.height))
                |> move (item.x * p) (item.y * p)
                |> fade (1 - t)
            ]

        Closing item left menu ->
            let
                t =
                    left / animationTime

                p =
                    1 - t |> bezier 0.1 1 0.1 1

                s =
                    t |> bezier 1 0 1 0
            in
            [ viewMenu menu time |> group
            , viewExample item.example.run computer
                |> group
                |> scaleX ((1 - (menu.w / screen.width)) * s + (menu.w / screen.width))
                |> scaleY ((1 - (menu.h / screen.height)) * s + (menu.h / screen.height))
                |> move (item.x * p) (item.y * p)
                |> fade (bezier 0 1 0 1 t)
            ]
    )
        |> (::) (words black "WebGL Playground" |> scale 3 |> move (screen.left + 256 * 3 / 2) (screen.top - 35))


viewExample : Run -> Computer -> List Shape
viewExample example ({ screen } as computer) =
    ((case example of
        AnimationExample fn ->
            fn computer.time

        PictureExample shape ->
            shape

        GameExample a ->
            viewGame a computer
     )
        |> (::) (rectangle lightGray screen.width screen.height)
    )
        ++ [ words orange "Press Escape to go back to menu"
                |> moveY (screen.bottom + 30)
                |> scale 2
           ]


viewGame : GameExample -> Computer -> List Shape
viewGame a computer =
    case a of
        MouseGame { view, update, memory } ->
            view computer memory

        TreeGame { view, update, memory } ->
            view computer memory

        HexGridGame { view, update, memory } ->
            view computer memory

        ShmupGame { view, update, memory } ->
            view computer memory

        VectorsGame { view, update, memory } ->
            view computer memory

        MarioGame { view, update, memory } ->
            view computer memory

        JumpGunGame { view, update, memory } ->
            view computer memory


viewMenu : Grid -> Time -> List Shape
viewMenu { grid, w, h, hover } time =
    grid
        |> indexedFoldlArray
            (\i { example, x, y } ->
                [ Maybe.map
                    (\s ->
                        [ rectangle blue w h
                        , image (w - 10) (h - 10) s
                        ]
                            |> group
                    )
                    example.img
                    |> Maybe.withDefault (rectangle blue w h)
                , [ words darkCharcoal example.name |> move 2 -2
                  , words white example.name
                  ]
                    |> group
                    |> moveY (h * -0.5 + 30)
                    |> scale (h / 16 * 0.15)
                    |> applyIf (i == hover) (moveY (wave 3 -3 1 time))
                ]
                    |> group
                    |> move x y
                    |> (::)
            )
            []



------ UPDATE


updateMemory : Computer -> Memory -> Memory
updateMemory ({ screen, mouse, keyboard } as computer) memory =
    case memory of
        Init names ->
            calcGrid config screen names
                |> Menu

        Menu menu ->
            if mouse.click then
                Array.get (getHoverIndex computer menu) menu.grid
                    |> Maybe.map (\item -> Opening item animationTime menu)
                    |> Maybe.withDefault memory

            else
                { menu | hover = getHoverIndex computer menu }
                    |> Menu

        Opening ({ example } as menuItem) left menu ->
            if left > 0 then
                case example.run of
                    GameExample a ->
                        Opening { menuItem | example = { example | run = GameExample (updateGame a computer) } } (left - 1) menu

                    _ ->
                        Opening menuItem (left - 1) menu

            else
                Open menuItem menu

        Open ({ example } as menuItem) menu ->
            if Set.member "Escape" keyboard.keys then
                Closing menuItem animationTime menu

            else
                case example.run of
                    GameExample a ->
                        Open { menuItem | example = { example | run = GameExample (updateGame a computer) } } menu

                    _ ->
                        memory

        Closing i left menu ->
            if left > 0 then
                Closing i (left - 1) menu

            else
                Menu menu


updateGame : GameExample -> Computer -> GameExample
updateGame a computer =
    case a of
        MouseGame ({ view, update, memory } as info) ->
            MouseGame { info | memory = update computer memory }

        TreeGame ({ view, update, memory } as info) ->
            TreeGame { info | memory = update computer memory }

        HexGridGame ({ view, update, memory } as info) ->
            HexGridGame { info | memory = update computer memory }

        ShmupGame ({ view, update, memory } as info) ->
            ShmupGame { info | memory = update computer memory }

        VectorsGame ({ view, update, memory } as info) ->
            VectorsGame { info | memory = update computer memory }

        MarioGame ({ view, update, memory } as info) ->
            MarioGame { info | memory = update computer memory }

        JumpGunGame ({ view, update, memory } as info) ->
            JumpGunGame { info | memory = update computer memory }


initialMemory : Memory
initialMemory =
    Array.fromList
        [ { img = Just "Circles.png", name = "Circles", run = AnimationExample Circles.view }
        , { img = Just "Clock.png", name = "Clock", run = AnimationExample Clock.view }
        , { img = Just "Font.png", name = "Font", run = PictureExample Font.view }
        , { img = Just "HexGrid.png", name = "HexGrid", run = GameExample <| HexGridGame { view = HexGrid.view, update = HexGrid.update, memory = HexGrid.init } }
        , { img = Just "Mouse.png", name = "Mouse", run = GameExample <| MouseGame { view = Mouse.view, update = Mouse.update, memory = Mouse.init } }
        , { img = Just "Polygon.png", name = "Polygon", run = PictureExample Polygon.view }
        , { img = Just "Shmup.png", name = "Shmup", run = GameExample <| ShmupGame { view = Shmup.view, update = Shmup.update, memory = Shmup.init } }
        , { img = Just "Polygon2.png", name = "Polygon2", run = PictureExample MulticolorPolygon.view }
        , { img = Just "Tree.png", name = "Tree", run = GameExample <| TreeGame { view = Tree.view, update = Tree.update, memory = Tree.init } }
        , { img = Just "Vectors.png", name = "Vectors", run = GameExample <| VectorsGame { view = Vectors.view, update = Vectors.update, memory = Vectors.init } }
        , { img = Just "JumpGun.png", name = "JumpGun(E)", run = GameExample <| JumpGunGame { view = JumpGun.view, update = JumpGun.update, memory = JumpGun.init } }
        , { img = Just "Mario.png", name = "Mario(E)", run = GameExample <| MarioGame { view = Mario.view, update = Mario.update, memory = Mario.init } }
        , { img = Just "MSDF.png", name = "MSDF(E)", run = PictureExample Extra.Font.view }
        ]
        |> Init


type Run
    = AnimationExample (Time -> List Shape)
    | PictureExample (List Shape)
    | GameExample GameExample


type GameExample
    = MouseGame (GameData Mouse.Memory)
    | TreeGame (GameData Tree.Memory)
    | HexGridGame (GameData HexGrid.Memory)
    | ShmupGame (GameData Shmup.Memory)
    | VectorsGame (GameData Vectors.Memory)
    | MarioGame (GameData Mario.Memory)
    | JumpGunGame (GameData JumpGun.Memory)


type alias GameData memory =
    { view : Computer -> memory -> List Shape
    , update : Computer -> memory -> memory
    , memory : memory
    }


getHoverIndex { mouse, screen } a =
    let
        x =
            ((mouse.x - screen.left - a.padding) / (a.w + a.padding))
                |> floor

        y =
            ((screen.top - mouse.y - a.topOffset) / (a.h + a.padding))
                |> floor
    in
    x + y * a.cols


calcGrid : { a | cols : Int, padding : Float, topOffset : Float } -> Screen -> Array Example -> Grid
calcGrid config_ { width, height } l =
    let
        cols =
            toFloat config_.cols

        w =
            (width - config_.padding) / cols - config_.padding

        rows =
            toFloat (Array.length l) / cols |> ceiling

        h =
            ((height - config_.topOffset) / toFloat rows - config_.padding)
                |> min w

        y =
            height * 0.5 - 0.5 * h - config_.topOffset

        x =
            width * -0.5 + 0.5 * w + config_.padding
    in
    { grid =
        Array.indexedMap
            (\i name ->
                { x = x + (w + config_.padding) * (remainderBy config_.cols i |> toFloat)
                , y = y - toFloat (i // config_.cols) * (h + config_.padding)
                , example = name
                }
            )
            l
    , w = w
    , h = h
    , padding = config_.padding
    , topOffset = config_.topOffset
    , cols = config_.cols
    , hover = -1
    }


type Memory
    = Init (Array Example)
    | Menu Grid
    | Opening MenuItem Float Grid
    | Closing MenuItem Float Grid
    | Open MenuItem Grid


type alias Example =
    { name : String
    , run : Run
    , img : Maybe String
    }


type alias Grid =
    { grid : Array MenuItem
    , w : Float
    , h : Float
    , padding : Float
    , topOffset : Float
    , cols : Int
    , hover : Int
    }


type alias MenuItem =
    { x : Float
    , y : Float
    , example : Example
    }


playground =
    embed viewMemory updateMemory initialMemory


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world


indexedFoldlArray : (Int -> a -> b -> b) -> b -> Array a -> b
indexedFoldlArray func acc list =
    let
        step : a -> ( Int, b ) -> ( Int, b )
        step x ( i, thisAcc ) =
            ( i + 1, func i x thisAcc )
    in
    Tuple.second (Array.foldl step ( 0, acc ) list)


type alias Easing =
    Float -> Float


{-| <https://package.elm-lang.org/packages/elm-community/easing-functions/latest/Ease#bezier>
-}
bezier : Float -> Float -> Float -> Float -> Easing
bezier x1 y1 x2 y2 time =
    let
        lerp from to v =
            from + (to - from) * v

        pair interpolate ( a0, b0 ) ( a1, b1 ) v =
            ( interpolate a0 a1 v, interpolate b0 b1 v )

        casteljau ps =
            case ps of
                [ ( _, y ) ] ->
                    y

                xs ->
                    List.map2 (\x y -> pair lerp x y time) xs (Maybe.withDefault [] (List.tail xs))
                        |> casteljau
    in
    casteljau [ ( 0, 0 ), ( x1, y1 ), ( x2, y2 ), ( 1, 1 ) ]

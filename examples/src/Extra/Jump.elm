module Extra.Jump exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Extra.Jump.Collision as Collision
import Extra.Jump.Direction as Direction exposing (Direction)
import Extra.Jump.Sprite as Sprite
import Playground exposing (..)


config =
    { acc = 0.5
    , friction = -0.125
    , gravity = vec2 0 -0.5
    , jump = 9
    , viewScale = 3
    }


main =
    game view update Init


pxSnap =
    round >> toFloat


view computer m =
    case m of
        Play ({ player } as memory) ->
            [ (if player.v.x > 0.5 || player.v.x < -0.5 then
                Sprite.run player.dir player.frame

               else
                Sprite.idle player.dir player.frame
              )
                |> move (pxSnap player.p.x) (pxSnap player.p.y)
            , debug computer memory
            ]
                |> group
                |> move (-player.p.x * config.viewScale) (-16 * config.viewScale)
                |> scale config.viewScale
                |> List.singleton

        Init ->
            []


update computer memory =
    case memory of
        Play m ->
            m
                |> updateMovement computer
                |> simulate computer
                |> crossScreen computer
                |> animatePlayer computer
                |> Play

        Init ->
            Play (initGame computer)


animatePlayer computer ({ player } as memory) =
    let
        ( x, y ) =
            toXY computer.keyboard

        dir =
            Direction.fromRecord { x = x, y = y }
    in
    if dir /= Direction.Neither && dir /= player.dir then
        { memory | player = { player | frame = 0, dir = dir } }

    else
        { memory | player = { player | frame = player.frame + 1 } }


crossScreen { screen } ({ player } as memory) =
    if player.p.x * config.viewScale > screen.right then
        { memory
            | player = { player | p = Vec2.setX (screen.left / config.viewScale) player.p }
        }

    else if player.p.x * config.viewScale < screen.left then
        { memory
            | player = { player | p = Vec2.setX (screen.right / config.viewScale) player.p }
        }

    else if player.p.y * config.viewScale < screen.bottom then
        { memory
            | player = { player | p = Vec2.setY (screen.top / config.viewScale) player.p }
        }

    else if player.p.y * config.viewScale > screen.top then
        { memory
            | player = { player | p = Vec2.setY (screen.bottom / config.viewScale) player.p }
        }

    else
        memory


updateMovement { keyboard } ({ player } as memory) =
    let
        x =
            if keyboard.left then
                -config.acc

            else if keyboard.right then
                config.acc

            else
                0

        ( acc, v ) =
            if keyboard.space && (player.contact.y < -0.5) then
                ( vec2 x config.jump, Vec2.setY 0 player.v )

            else
                ( vec2 x 0, player.v )
    in
    { memory | player = { player | acc = acc, v = v } }


simulate computer ({ player } as memory) =
    let
        acc =
            player.acc
                |> Vec2.add (Vec2.scale config.friction player.v)
                |> Vec2.add config.gravity
                |> roundVec

        forceApplied =
            { player
                | v = player.v |> Vec2.add (Vec2.scale 1.5 acc)
                , contact = zero
            }

        newPlayer =
            List.foldl Collision.lineCircle forceApplied memory.static
    in
    { memory
        | player =
            { newPlayer
                | p = Vec2.add newPlayer.p newPlayer.v
                , v =
                    applyIf (newPlayer.v == forceApplied.v) (Vec2.add (Vec2.scale -0.5 acc)) newPlayer.v
                        |> roundVec
            }
    }


type Phase
    = Init
    | Play
        { player : Player
        , static : List { p1 : Vec2, p2 : Vec2 }
        }


type alias Level =
    ()


zero =
    Vec2.vec2 0 0


initGame { screen } =
    { player = initPlayer -10 90
    , static =
        [ { p1 = { x = 98, y = -40 }, p2 = { x = 91, y = 140 } }
        , { p1 = { x = 50, y = -40 }, p2 = { x = 50, y = 140 } }
        , { p1 = { x = -50, y = -10 }, p2 = { x = -120, y = -10 } }
        , { p1 = { x = -120, y = 10 }, p2 = { x = -90, y = 10 } }
        , { p1 = { x = 120, y = -40 }, p2 = { x = -300, y = -40 } }
        , { p1 = { x = 15, y = 10 }, p2 = { x = -90, y = 10 } }
        , { p1 = { x = 25, y = 10 }, p2 = { x = -20, y = -40 } }
        , { p1 = { x = 120, y = 10 }, p2 = { x = 20, y = 10 } }
        , { p1 = { x = 204, y = 52 }, p2 = { x = 100, y = 10 } }
        , { p1 = { x = 160, y = -80 }, p2 = { x = 120, y = -80 } }

        --{ p1 = { x = -90, y = 50 }, p2 = { x = -160, y = 72 } }
        --, { p1 = { x = -90, y = 72 }, p2 = { x = -160, y = 50 } }
        ]
    }


initPlayer : Number -> Number -> Player
initPlayer x y =
    { p = vec2 x y
    , v = zero
    , acc = { x = 0, y = 0 }
    , r = 9
    , dir = Direction.East
    , frame = 0
    , contact = { x = 0, y = 0 }
    , debug = [ { p1 = zero, p2 = zero } ]
    }


type alias Player =
    { p : Vec2
    , v : Vec2
    , acc : Vec2
    , r : Number
    , dir : Direction
    , frame : Number
    , contact : Vec2
    , debug : List { p1 : Vec2, p2 : Vec2 }
    }


roundVec { x, y } =
    { x = toFloat (round (x * precision)) / precision, y = toFloat (round (y * precision)) / precision }


precision =
    1.0e10


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world


debug computer ({ player, static } as memory) =
    List.foldl
        (\{ p1, p2 } acc ->
            let
                w =
                    Vec2.sub p1 p2 |> Vec2.length

                a =
                    atan2 (p2.y - p1.y) (p2.x - p1.x) / pi * 180
            in
            ([ rectangle yellow w 2
             , triangle yellow 5 |> rotate 180
             , circle yellow 3 |> moveX (w / -2)
             ]
                |> group
                |> move ((p1.x + p2.x) / 2) ((p1.y + p2.y) / 2)
                |> rotate a
                |> fade 0.5
            )
                :: acc
        )
        (List.indexedMap (\i a -> drawSegment a (randomColor i) (randomColor i) (randomColor i)) player.debug)
        static
        |> group


drawSegment =
    drawSegment_ True True


drawSegment_ start_ end_ { p1, p2 } c1 c2 c3 =
    let
        w =
            Vec2.sub p1 p2 |> Vec2.length

        a =
            atan2 (p2.y - p1.y) (p2.x - p1.x) / pi * 180

        addon =
            if end_ then
                [ triangle c2 3
                    |> move p2.x p2.y
                    |> rotate a
                    |> rotate -90
                ]

            else
                []

        start =
            if start_ then
                [ circle c1 2 |> move p1.x p1.y
                ]

            else
                []
    in
    [ rectangle c3 w 1
        |> move ((p1.x + p2.x) / 2) ((p1.y + p2.y) / 2)
        |> rotate a
    ]
        ++ start
        ++ addon
        |> group


randomColor i =
    let
        getAt : Int -> List a -> Maybe a
        getAt idx xs =
            if idx < 0 then
                Nothing

            else
                List.head <| List.drop idx xs
    in
    [ rgb 130 178 182
    , rgb 226 23 49
    , rgb 57 54 134
    , rgb 204 2 181
    , rgb 18 187 231
    , rgb 8 144 133
    , rgb 129 8 16
    , rgb 156 230 146
    , rgb 5 77 37
    , rgb 72 116 216
    , rgb 196 223 15
    ]
        |> getAt (modBy 11 i)
        |> Maybe.withDefault (rgb 196 223 15)

module Extra.JumpGun exposing (Memory, init, main, update, view)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Extra.Jump.Collision as Collision
import Extra.Jump.Direction as Direction exposing (Direction)
import Extra.Jump.Sprite as Sprite
import Extra.Jump.TileMap exposing (fullscreen, level1)
import Playground exposing (..)


config =
    { acc = 0.5
    , friction = vec2 -0.125 -0.05
    , gravity = vec2 0 -0.4
    , jump = 7
    , viewScale = 3
    }


main : Program () (Game Memory) Msg
main =
    game view update Init


init : Memory
init =
    Init


pxSnap =
    round >> toFloat


view : Computer -> Memory -> List Shape
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
                |> andFold memory.bullets (\bullet -> (::) (bullet.shape computer.time |> move bullet.x bullet.y))
                |> (::) level1
                |> (::) fullscreen
                |> group
                |> move (-player.p.x * config.viewScale) (-16 * config.viewScale)
                |> scale config.viewScale
                --|> scale (wave 3 5 1 computer.time)
                |> List.singleton

        Init ->
            []


update : Computer -> Memory -> Memory
update computer memory =
    case memory of
        Play m ->
            m
                |> updateMovement computer
                --|> crossScreen computer
                |> animatePlayer computer
                |> updateBullet computer
                |> updateShoot computer
                |> simulate
                |> Play

        Init ->
            Play (initGame computer)


simulate memory =
    { memory | player = Collision.simulate config memory.player memory.static }


animatePlayer computer ({ player } as memory) =
    let
        ( x, y ) =
            toXY computer.keyboard

        dir =
            { x = x, y = y }
                |> applyIf (player.contact.y < 0 && y < 0) (Vec2.setY 0)
                |> Direction.fromRecord
    in
    if dir /= Direction.Neither && dir /= player.dir then
        { memory | player = { player | frame = 0, dir = dir } }

    else if
        dir
            == Direction.Neither
            && player.contact.y
            < 0
            && (player.dir == Direction.SouthEast || player.dir == Direction.South || player.dir == Direction.SouthWest)
    then
        { memory
            | player =
                { player
                    | dir =
                        case player.dir of
                            Direction.SouthEast ->
                                Direction.East

                            Direction.SouthWest ->
                                Direction.West

                            _ ->
                                Direction.East
                }
        }

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
            if keyboard.left && not keyboard.shift then
                -config.acc

            else if keyboard.right && not keyboard.shift then
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


updateBullet computer memory =
    memory.bullets
        |> List.foldl
            (\mob bullets ->
                let
                    y =
                        mob.y + mob.v.y

                    x =
                        mob.x + mob.v.x
                in
                if
                    (y > computer.screen.top)
                        || (y < computer.screen.bottom)
                        || (x > memory.player.p.x + computer.screen.right)
                        || (x < computer.screen.left + memory.player.p.x)
                then
                    bullets

                else
                    { mob | y = y, x = x } :: bullets
            )
            []
        |> (\bullets -> { memory | bullets = bullets })


shoot weapon ({ player } as memory) =
    let
        dirRecord =
            Direction.toRecord memory.player.dir

        v =
            dirRecord
                |> Vec2.mul weapon.bullet.v

        bullet =
            { x = memory.player.p.x + weapon.bullet.x
            , y = memory.player.p.y + weapon.bullet.y
            , r = weapon.bullet.r
            , a = weapon.bullet.a
            , v = v
            , shape = weapon.bullet.shape
            }
    in
    { memory
        | bullets = bullet :: memory.bullets
        , player =
            { player
                | acc =
                    Vec2.negate dirRecord |> Vec2.mul (vec2 3 9)
            }
    }


updateShoot computer state =
    List.foldl
        (\weapon ( acc, model ) ->
            if computer.keyboard.shift && weapon.counter <= 0 then
                ( { weapon | counter = weapon.interval } :: acc, shoot weapon model )

            else if weapon.counter > 0 then
                ( { weapon | counter = weapon.counter - 1 } :: acc, model )

            else
                ( weapon :: acc, model )
        )
        ( [], state )
        state.weapon
        |> (\( a, b ) -> { b | weapon = a })


type Memory
    = Init
    | Play
        { player : Player
        , static : List { p1 : Vec2, p2 : Vec2 }
        , bullets : List Bullet
        , weapon :
            List
                { counter : Int
                , interval : Int
                , bullet : Bullet
                }
        }


type alias Bullet =
    { x : Number
    , y : Number
    , r : Number
    , a : Number
    , v : Vec2
    , shape : Time -> Shape
    }


type alias Level =
    ()


zero =
    Vec2.vec2 0 0


initGame { screen } =
    { player = initPlayer -160 90
    , static =
        [ { p1 = { x = -3000, y = -40 }, p2 = { x = -3000, y = 140 } }
        , { p1 = { x = 90, y = 140 }, p2 = { x = 90, y = -40 } }
        , { p1 = { x = 120, y = -40 }, p2 = { x = -3000, y = -40 } }

        ----
        , { p1 = { x = -80, y = -24 }, p2 = { x = -88, y = -24 } }
        , { p1 = { x = -80, y = 8 }, p2 = { x = -88, y = 8 } }
        , { p1 = { x = -80, y = 40 }, p2 = { x = -88, y = 40 } }
        , { p1 = { x = -80, y = 72 }, p2 = { x = -88, y = 72 } }
        , { p1 = { x = -112, y = 72 }, p2 = { x = -120, y = 72 } }
        , { p1 = { x = -76, y = 108 }, p2 = { x = -76, y = -40 } }
        , { p1 = { x = 76, y = 104 }, p2 = { x = -82, y = 104 } }
        , { p1 = { x = -124, y = 56 }, p2 = { x = -124, y = 120 } }
        , { p1 = { x = -120, y = 112 }, p2 = { x = -224, y = 112 } }

        --
        , { p1 = { x = -136, y = 16 }, p2 = { x = -176, y = 16 } }
        , { p1 = { x = -176, y = 56 }, p2 = { x = -120, y = 56 } }

        --, { p1 = { x = -78, y = 8 }, p2 = { x = -88, y = 8 } }
        ]
    , bullets = []

    --, explosions = []
    , weapon =
        [ { counter = 5
          , interval = 15
          , bullet =
                { x = 3
                , y = 0
                , r = 10
                , a = 30
                , v = vec2 4 4
                , shape = \_ -> group [ circle red 2, circle white 1 ]
                }
          }
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


andFold l fn acc =
    List.foldr fn acc l


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

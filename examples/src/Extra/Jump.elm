module Extra.Jump exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Extra.Jump.Collision as Collision
import Extra.Jump.Sprite exposing (sheet)
import Playground exposing (..)
import Playground.Extra exposing (scaleX)


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
            [ sheet.f4
                --|> move (pxSnap player.p.x) (pxSnap player.p.y)
                |> move (pxSnap player.p.x) (pxSnap player.p.y)
                |> applyIf (player.v.x < 0) (scaleX -1)
            , debug computer memory
            ]
                |> group
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
                |> Play

        Init ->
            Play (initGame computer)


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

        acc =
            if keyboard.space && (player.contact.y < 0) then
                vec2 x config.jump

            else
                vec2 x 0
    in
    { memory | player = { player | acc = acc } }


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
            List.foldl
                (\wall ->
                    if wall.p1.x == wall.p2.x then
                        playerVsVerticalWall wall

                    else
                        playerVsHorizontalWall wall
                )
                forceApplied
                memory.static
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


intersectionVec2 p1 p2 p3 p4 =
    Collision.intersection p1.x p1.y p2.x p2.y p3.x p3.y p4.x p4.y


playerVsVerticalWall wall player =
    let
        pTop =
            Vec2.add player.p (vec2 (player.hurt + slopeFix) 0)

        pBottom =
            Vec2.add player.p (vec2 (-player.hurt - slopeFix) 0)

        hitTop =
            intersectionVec2 pTop (Vec2.add pTop player.v) wall.p1 wall.p2

        hitBottom =
            intersectionVec2 pBottom (Vec2.add pBottom player.v) wall.p1 wall.p2
    in
    getTU hitTop hitBottom
        |> Maybe.map
            (\( t, u ) ->
                let
                    restV =
                        Vec2.scale (1 - t) player.v

                    zeroedWall =
                        Vec2.sub wall.p2 wall.p1

                    onLeftSide =
                        isLeft zeroedWall zero restV

                    calcRest =
                        if onLeftSide then
                            scalarProjection restV zeroedWall

                        else
                            restV
                in
                { player
                    | v =
                        player.v
                            |> Vec2.scale t
                            |> Vec2.add calcRest
                            |> Vec2.add
                                (if hitBottom /= Nothing then
                                    vec2 slopeFix 0

                                 else
                                    vec2 -slopeFix 0
                                )
                    , contact = applyIf onLeftSide (Vec2.add (getLeftNormal zeroedWall)) player.contact
                }
            )
        |> Maybe.withDefault player


slopeFix =
    0.5


playerVsHorizontalWall wall player =
    let
        pTop =
            Vec2.add player.p (vec2 0 (player.hurt + slopeFix))

        pBottom =
            Vec2.add player.p (vec2 0 (-player.hurt - slopeFix))

        hitTop =
            intersectionVec2 pTop (Vec2.add pTop player.v) wall.p1 wall.p2

        hitBottom =
            intersectionVec2 pBottom (Vec2.add pBottom player.v) wall.p1 wall.p2
    in
    if isLeft wall.p1 wall.p2 player.p && isLeft wall.p1 wall.p2 pTop && isLeft wall.p1 wall.p2 pBottom then
        (getTU hitTop hitBottom
            |> Maybe.map
                (\( t, u ) ->
                    let
                        restV =
                            Vec2.scale (1 - t) player.v

                        zeroedWall =
                            Vec2.sub wall.p2 wall.p1

                        isVelocityLeft =
                            isLeft zeroedWall zero restV

                        calcRest =
                            if isVelocityLeft then
                                scalarProjection restV zeroedWall

                            else
                                restV
                    in
                    { player
                        | v =
                            player.v
                                |> Vec2.scale t
                                |> Vec2.add calcRest
                                |> Vec2.add
                                    (if hitBottom /= Nothing then
                                        vec2 0 slopeFix

                                     else
                                        vec2 0 -slopeFix
                                    )
                        , contact = applyIf isVelocityLeft (Vec2.add (getLeftNormal zeroedWall)) player.contact
                    }
                )
            |> Maybe.withDefault player
        )
            |> (\p ->
                    { p
                        | debug =
                            [ { p1 = player.p
                              , p2 = Vec2.add player.p (player.v |> Vec2.scale debugFactor)
                              }
                            , { p1 = p.p
                              , p2 = Vec2.add p.p (p.v |> Vec2.scale debugFactor)
                              }
                            ]
                    }
               )

    else
        player


debugFactor =
    5


getLeftNormal vec =
    let
        { x, y } =
            vec
                |> Vec2.normalize
    in
    vec2 -y x


isLeft a b c =
    (b.x - a.x) * (c.y - a.y) < (b.y - a.y) * (c.x - a.x)


getTU a b =
    case ( a, b ) of
        ( Nothing, Nothing ) ->
            Nothing

        ( Just a_, Nothing ) ->
            Just a_

        ( Nothing, Just b_ ) ->
            Just b_

        ( Just a_, Just b_ ) ->
            Just (min a_ b_)


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
    let
        half =
            screen.height / config.viewScale * -0.5

        stairs =
            List.range 1 (screen.height / config.viewScale / 20 |> round)
                |> List.foldl
                    (\i acc ->
                        let
                            y =
                                toFloat i * 20 + half
                        in
                        if y > 50 || y < -50 then
                            { p1 = { x = 20, y = y }, p2 = { x = -20, y = y } } :: acc

                        else
                            acc
                    )
                    []
    in
    { player = initPlayer -100 -32
    , static =
        [ { p1 = { x = 70, y = -40 }, p2 = { x = screen.left / config.viewScale, y = -40 } }
        , { p1 = { x = -50, y = -10 }, p2 = { x = -120, y = -10 } }
        , { p1 = { x = -120, y = 10 }, p2 = { x = -90, y = 10 } }
        , { p1 = { x = -120, y = 10 }, p2 = { x = screen.left / config.viewScale, y = 10 } }
        , { p1 = { x = 19.5, y = 10 }, p2 = { x = -90, y = 10 } }
        , { p1 = { x = 22, y = 10 }, p2 = { x = -20, y = -40 } }
        , { p1 = { x = 120, y = 10 }, p2 = { x = 20, y = 10 } }
        , { p1 = { x = 200, y = 50 }, p2 = { x = 100, y = 10 } }
        , { p1 = { x = screen.right / config.viewScale + 1, y = 50 }, p2 = { x = 200, y = 50 } }
        , { p1 = { x = 20, y = 50 }, p2 = { x = screen.left / config.viewScale - 1, y = 50 } }

        ---
        , { p1 = { x = 20, y = screen.bottom / config.viewScale }, p2 = { x = -20, y = screen.bottom / config.viewScale } }
        , { p1 = { x = 160, y = -80 }, p2 = { x = 120, y = -80 } }
        ]
            ++ stairs
    }


initPlayer : Number -> Number -> Player
initPlayer x y =
    { p = vec2 x y
    , v = zero
    , acc = { x = 0, y = 0 }
    , hurt = 7
    , contact = { x = 0, y = 0 }
    , debug = [ { p1 = zero, p2 = zero } ]
    }


type alias Player =
    { p : Vec2
    , v : Vec2
    , acc : Vec2
    , hurt : Number
    , contact : Vec2
    , debug : List { p1 : Vec2, p2 : Vec2 }
    }


scalarProjection : Vec2 -> Vec2 -> Vec2
scalarProjection a b =
    Vec2.scale (Vec2.dot a b / Vec2.lengthSquared b) b


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
    --([ words green "HurtBox"
    --    |> moveY -10
    -- , words yellow "PushBox"
    --    |> moveY -24
    -- , words red "HitBox"
    --    |> moveY -38
    -- , words purple "GrabBox"
    --    |> moveY -52
    -- ]
    --    |> group
    --    |> moveY (computer.screen.top * 0.2)
    --    |> scale 0.4
    --)
    --    ::
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
        ([-- drawSegment { p1 = player.p, p2 = Vec2.add player.p originalV } green green green
         ]
            ++ List.indexedMap (\i a -> drawSegment a (randomColor i) (randomColor i) (randomColor i)) player.debug
        )
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

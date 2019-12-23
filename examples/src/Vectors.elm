module Vectors exposing (main)

import AltMath.Vector2 as Vec2 exposing (Vec2, vec2)
import Playground exposing (..)


intersectionSegment : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Maybe ( Float, Float )
intersectionSegment x1 y1 x2 y2 x3 y3 x4 y4 =
    let
        den =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in
    if den == 0 then
        Nothing

    else
        let
            t =
                ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / den

            u =
                -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / den
        in
        if (t >= 0) && (t <= 1) && (u >= 0) && (u <= 1) then
            Just ( t, u )

        else
            Nothing


main =
    game view update Init


view computer phase =
    case phase of
        LineLine ({ l1, l2 } as memory) ->
            case memory.intersection of
                Nothing ->
                    [ drawSegment l1
                        (applyIf (memory.active == P1) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P2) (\_ -> lightBlue) blue)
                        blue
                    , drawSegment memory.l2
                        (applyIf (memory.active == P3) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P4) (\_ -> lightBlue) blue)
                        blue
                    ]

                Just ( t, u ) ->
                    let
                        l1P2 =
                            l1.p1
                                |> Vec2.sub l1.p2
                                |> Vec2.scale t
                                |> Vec2.add l1.p1

                        l2P2 =
                            l2.p1
                                |> Vec2.sub l2.p2
                                |> Vec2.scale u
                                |> Vec2.add l2.p1

                        project =
                            scalarProjection (Vec2.sub l2.p2 l2P2) (Vec2.sub l1.p1 l1.p2)
                    in
                    [ drawSegment_ True
                        False
                        { l1 | p2 = l1P2 }
                        (applyIf (memory.active == P1) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P2) (\_ -> lightBlue) blue)
                        red
                    , drawSegment_ False
                        True
                        { l1 | p1 = l1P2, p2 = l1.p2 }
                        (applyIf (memory.active == P1) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P2) (\_ -> lightBlue) blue)
                        red
                        |> fade 0.4
                    , drawSegment_ True
                        False
                        { l2 | p2 = l2P2 }
                        (applyIf (memory.active == P3) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P4) (\_ -> lightBlue) blue)
                        blue
                    , drawSegment_ False
                        True
                        { l2 | p1 = l2P2, p2 = l2.p2 }
                        (applyIf (memory.active == P3) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P4) (\_ -> lightBlue) blue)
                        red
                        |> fade 0.4
                    , drawSegment_ False
                        True
                        { l2 | p1 = l2P2, p2 = Vec2.add l2P2 project }
                        (applyIf (memory.active == P3) (\_ -> lightBlue) blue)
                        (applyIf (memory.active == P4) (\_ -> lightBlue) blue)
                        blue

                    --|> fade 0.4
                    ]

        Init ->
            []


scalarProjection : Vec2 -> Vec2 -> Vec2
scalarProjection a b =
    --    let
    --        normB =
    --            Vec2.normalize b
    --    in
    --    Vec2.scale (Vec2.dot a normB) normB
    -- not using `Math.sqrt`
    Vec2.scale (Vec2.dot a b / Vec2.lengthSquared b) b


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
                [ triangle c2 10
                    |> move p2.x p2.y
                    |> rotate a
                    |> rotate -90
                ]

            else
                []

        start =
            if start_ then
                [ circle c1 5 |> move p1.x p1.y
                ]

            else
                []
    in
    [ rectangle c3 w 4
        |> move ((p1.x + p2.x) / 2) ((p1.y + p2.y) / 2)
        |> rotate a
    ]
        ++ start
        ++ addon
        |> group


update { mouse } phase =
    case phase of
        LineLine ({ l1, l2 } as memory) ->
            (if not mouse.down && memory.active /= None then
                { memory | active = None }

             else if mouse.down then
                case memory.active of
                    P1 ->
                        { memory | l1 = { l1 | p1 = vec2 mouse.x mouse.y } }

                    P2 ->
                        { memory | l1 = { l1 | p2 = vec2 mouse.x mouse.y } }

                    P3 ->
                        { memory | l2 = { l2 | p1 = vec2 mouse.x mouse.y } }

                    P4 ->
                        { memory | l2 = { l2 | p2 = vec2 mouse.x mouse.y } }

                    None ->
                        if mousePointTest 10 mouse memory.l1.p1 then
                            { memory | active = P1 }

                        else if mousePointTest 10 mouse memory.l1.p2 then
                            { memory | active = P2 }

                        else if mousePointTest 10 mouse memory.l2.p1 then
                            { memory | active = P3 }

                        else if mousePointTest 10 mouse memory.l2.p2 then
                            { memory | active = P4 }

                        else
                            { memory | active = None }

             else
                memory
            )
                |> (\m -> { m | intersection = intersectionSegment m.l1.p1.x m.l1.p1.y m.l1.p2.x m.l1.p2.y m.l2.p1.x m.l2.p1.y m.l2.p2.x m.l2.p2.y })
                |> LineLine

        Init ->
            LineLine initLineLine


mousePointTest : Float -> { a | x : Float, y : Float } -> Vec2 -> Bool
mousePointTest tolerance mouse point =
    (mouse.x > point.x - tolerance)
        && (mouse.x < point.x + tolerance)
        && (mouse.y > point.y - tolerance)
        && (mouse.y < point.y + tolerance)


type Phase
    = Init
    | LineLine
        { l1 : Segment
        , l2 : Segment
        , active : ActivePoint
        , intersection : Maybe ( Float, Float )
        }


type ActivePoint
    = P1
    | P2
    | P3
    | P4
    | None


initLineLine =
    { l1 = { p1 = vec2 0 0, p2 = vec2 -30 100 }
    , l2 = { p1 = vec2 30 0, p2 = vec2 -30 -80 }
    , active = None
    , intersection = Nothing
    }


type alias Segment =
    { p1 : Vec2, p2 : Vec2 }


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world

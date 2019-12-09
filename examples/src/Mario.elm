module Mario exposing (main)

import Array
import Playground exposing (..)
import Playground.Advanced exposing (scaleX)
import Playground.Extra exposing (sprite)



-- MAIN


main =
    game view
        update
        { x = 0
        , y = 0
        , vx = 0
        , vy = 0
        , dir = 1
        }



-- VIEW


view computer mario =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height

        b =
            computer.screen.bottom
    in
    [ rectangle (rgb 174 238 238) w h
    , rectangle (rgb 74 163 41) w 100
        |> moveY b
    , sprite 20 27 (getFrame mario computer.time) "assets/mario.png"
        |> scale 2
        |> scaleX mario.dir
        |> move mario.x (b + 76 + mario.y)
    ]


getFrame mario time =
    let
        frame =
            toFrac 0.75 time |> (*) 8 |> floor
    in
    if mario.y > 0 then
        5

    else if mario.vx /= 0 then
        Array.get frame run |> Maybe.withDefault 0

    else
        0



-- UPDATE


update computer mario =
    let
        dt =
            1.666

        vx =
            toX computer.keyboard

        vy =
            if mario.y == 0 then
                if computer.keyboard.up then
                    5

                else
                    0

            else
                mario.vy - dt / 8

        x =
            mario.x + dt * vx * 2

        y =
            mario.y + dt * vy
    in
    { x = x
    , y = max 0 y
    , vx = vx
    , vy = vy
    , dir =
        if vx == 0 then
            mario.dir

        else if vx < 0 then
            1

        else
            -1
    }


run =
    Array.fromList [ 0, 1, 2, 1, 0, 3, 4, 3 ]

module Extra.Mario exposing (Memory, init, main, update, view)

import Array exposing (Array)
import Playground exposing (..)
import Playground.Extra exposing (scaleX, tile)



-- MAIN


main : Program () (Game Memory) Msg
main =
    game view update init


init : Memory
init =
    { x = 0
    , y = 0
    , vx = 0
    , vy = 0
    , dir = 1
    }


type alias Memory =
    { x : Number
    , y : Number
    , vx : Number
    , vy : Float
    , dir : Number
    }



-- VIEW


view : Computer -> Memory -> List Shape
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
    , tile 20 27 spriteSheet (getFrame mario computer.time)
        |> scale 2
        |> scaleX mario.dir
        |> move mario.x (b + 76 + mario.y)
    ]


getFrame : Memory -> Time -> number
getFrame mario time =
    let
        frame =
            now time // 60 |> remainderBy 8
    in
    if mario.y > 0 then
        5

    else if mario.vx /= 0 then
        Array.get frame run |> Maybe.withDefault 0

    else
        0



-- UPDATE


update : Computer -> Memory -> Memory
update computer mario =
    let
        dt =
            toFloat (delta computer.time) / 10

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


run : Array number
run =
    Array.fromList [ 0, 1, 2, 1, 0, 3, 4, 3 ]


spriteSheet : String
spriteSheet =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAHgAAAAbCAMAAACA2rsUAAAAAXNSR0IArs4c6QAAADNQTFRFAAAAEgkELg0AdgACUS0TAyVivAADq2c6+PYv2I1lEEWhFJfbko6G/bKIxse19PXnAAAA2GphuwAAABF0Uk5TAP////////////////////8QFUChAAACFklEQVRIib2WiW6EMAxEKTloKCT+/6+tHedwIFSmlWqJbdeZ8TzCgliWXEC1/H+BidGYS7QW5i/QkLxPHqN/AaOHvjeBy3uZrIXRQ9+amJgLo7tdC6OGnjTBcGF0s2thXkBPmrQJIe9DD1bC6KFnTdyFr/MrhbCuYqIK5oVu0sRgCJBODA6hKVUwL3STJoTzPPFY1/UUwSoYtW5KGKg+MDk0sxZGDT1rfpQ6EectjB561uRYmauG0ermzdIVDTXMO921uYAzxuEx3PEqGKHjHj4kktDBT4SY6/3u8B4bHmh3GMA7Akv2AEzV5SbEFDGNWYwR0pl5L7nDw/AOQ0P5IdB6kKIbzJASiQqMGDkxYzAWy0TvBgMHDeUnXw+uuuKm+d53GB8TPJkX+Nz3TxLGeEiYiDUEx+rtZ3wUaD4XOmU4BHRECzyZcVfRSzm4JGFihmwZOBLJ2Oou0FiEyRm7MXUHaQQ8mnmvOUfCDMGwWYchOM7Q9btA40fNyDDAl8/hT5xGPpjJ7XJy22pwtVHfGWDbrHVkRqe0uh2sxQwR7IC0LMv2B3PJce5oudkHLh74x0IPLmWtTKYYC/lGqleliNu+PJjJ7dwgpFE4QOjITO8Qdy+ubFndaThIiOZmoBhre6eM4pWtBfOpi14xQ127BA3fJ2aKoUO4y6hR1v8RwQx8e+m9kczM0OuifHpZF80HyQ/v+HnpGwFKQ0StZOtsAAAAAElFTkSuQmCC"

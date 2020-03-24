module Playground exposing
    ( picture, animation, game
    , Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon
    , words
    , image
    , move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY
    , rotate, fade, scale, scaleX, scaleY, flipY, flipX
    , group
    , Time, spin, wave, zigzag
    , Computer, Mouse, Screen, Keyboard, toX, toY, toXY
    , Color, rgb, red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    , Playground, Msg
    )

{-|


# Playgrounds

@docs picture, animation, game


# Shapes

@docs Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon


# Words

@docs words


# Images

@docs image


# Move Shapes

@docs move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY


# Customize Shapes

@docs rotate, fade, scale, scaleX, scaleY, flipY, flipX


# Groups

@docs group


# Time

@docs Time, spin, wave, zigzag


# Computer

@docs Computer, Mouse, Screen, Keyboard, toX, toY, toXY


# Colors

@docs Color, rgb, red, orange, yellow, green, blue, purple, brown


### Light Colors

@docs lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown


### Dark Colors

@docs darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown


### Shades of Grey

@docs white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black


### Alternate Spellings of Gray

@docs lightGray, gray, darkGray


### Helpers Types

@docs Playground, Msg

-}

import Browser
import Browser.Dom as Dom
import Browser.Events
import Dict exposing (Dict)
import Math.Vector2 exposing (vec2)
import Math.Vector3
import Playground.Extra.Font exposing (tileFont)
import Playground.Font.SimpleMood as SimpleMood
import Playground.Internal as Internal exposing (Msg(..), Playground(..), TextureManager, create, subscriptions, viewWrap)
import Playground.Polygon exposing (signedArea, triangulate)
import Playground.Render as Render
import Set exposing (Set)
import Task exposing (Task)
import Time
import WebGL exposing (Entity)
import WebGL.Shape2d exposing (Form(..), Shape2d(..), toEntities)
import WebGL.Texture as Texture



-- PICTURE


{-| Make a picture! Here is a picture of a triangle with an eyeball:

    import Playground exposing (..)

    main =
        picture
            [ triangle green 150
            , circle white 40
            , circle black 10
            ]

-}
picture : List Shape -> Program () (Playground ()) Msg
picture shapes =
    let
        { init, update } =
            create (\_ _ -> shapes) (\_ m -> m) ()

        view (Playground { computer, entities }) =
            { title = "Playground"
            , body = viewWrap computer.screen entities
            }
    in
    Browser.document
        { init =
            \_ ->
                init
                    |> Tuple.mapSecond (\cmd -> [ Task.perform Tick Time.now, cmd ] |> Cmd.batch)
        , view = view
        , update =
            \msg m ->
                case msg of
                    GotTexture _ ->
                        ( update msg m |> Tuple.first, Task.perform Tick Time.now )

                    _ ->
                        update msg m
        , subscriptions = \_ -> Internal.subscriptions.resize
        }



-- COMPUTER


{-| When writing a [`game`](#game), you can look up all sorts of information
about your computer:

  - [`Mouse`](#Mouse) - Where is the mouse right now?
  - [`Keyboard`](#Keyboard) - Are the arrow keys down?
  - [`Screen`](#Screen) - How wide is the screen?
  - [`Time`](#Time) - What time is it right now?

So you can use expressions like `computer.mouse.x` and `computer.keyboard.enter`
in games where you want some mouse or keyboard interaction.

-}
type alias Computer =
    { mouse : Mouse
    , keyboard : Keyboard
    , screen : Screen
    , time : Time
    }



-- MOUSE


{-| Figure out what is going on with the mouse.

You could draw a circle around the mouse with a program like this:

    import Playground exposing (..)

    main =
        game view update 0

    view computer memory =
        [ circle yellow 40
            |> moveX computer.mouse.x
            |> moveY computer.mouse.y
        ]

    update computer memory =
        memory

You could also use `computer.mouse.down` to change the color of the circle
while the mouse button is down.

-}
type alias Mouse =
    { x : Float
    , y : Float
    , down : Bool
    , click : Bool
    }



-- KEYBOARD


{-| Figure out what is going on with the keyboard.

If someone is pressing the UP and RIGHT arrows, you will see a value like this:

    { up = True
    , down = False
    , left = False
    , right = True
    , space = False
    , enter = False
    , shift = False
    , backspace = False
    , keys = Set.fromList [ "ArrowUp", "ArrowRight" ]
    }

So if you want to move a character based on arrows, you could write an update
like this:

    update computer y =
        if computer.keyboard.up then
            y + 1

        else
            y

Check out [`toX`](#toX) and [`toY`](#toY) which make this even easier!

**Note:** The `keys` set will be filled with the `code` of all keys which are
down right now. So you will see things like `"KeyA"`, `"KeyB"`, `"KeyC"`, `"Digit1"`, `"Digit2"`,
`"Space"`, and `"ControlLeft"` in there.
For example, the code is `"KeyQ"` for the `Q` key on a QWERTY layout keyboard,
but the same code value also represents the `'` key on Dvorak keyboards and the `A` key on AZERTY keyboards.

Check out [this list][list] to see the
names used for all the different keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/code/code_values
[member]: /packages/elm/core/latest/Set#member

-}
type alias Keyboard =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    , enter : Bool
    , shift : Bool
    , backspace : Bool
    , keys : Set.Set String
    }


{-| Turn the LEFT and RIGHT arrows into a Float.

    toX { left = False, right = False, ... } == 0
    toX { left = True , right = False, ... } == -1
    toX { left = False, right = True , ... } == 1
    toX { left = True , right = True , ... } == 0

So to make a square move left and right based on the arrow keys, we could say:

    import Playground exposing (..)

    main =
        game view update 0

    view computer x =
        [ square green 40
            |> moveX x
        ]

    update computer x =
        x + toX computer.keyboard

-}
toX : Keyboard -> Float
toX keyboard =
    (if keyboard.right then
        1

     else
        0
    )
        - (if keyboard.left then
            1

           else
            0
          )


{-| Turn the UP and DOWN arrows into a Float.

    toY { up = False, down = False, ... } == 0
    toY { up = True , down = False, ... } == 1
    toY { up = False, down = True , ... } == -1
    toY { up = True , down = True , ... } == 0

This can be used to move characters around in games just like [`toX`](#toX):

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square blue 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

-}
toY : Keyboard -> Float
toY keyboard =
    (if keyboard.up then
        1

     else
        0
    )
        - (if keyboard.down then
            1

           else
            0
          )


{-| If you just use `toX` and `toY`, you will move diagonal too fast. You will go
right at 1 pixel per update, but you will go up/right at 1.41421 pixels per
update.

So `toXY` turns the arrow keys into an `(x,y)` pair such that the distance is
normalized:

    toXY { up = True , down = False, left = False, right = False, ... } == (1, 0)
    toXY { up = True , down = False, left = False, right = True , ... } == (0.707, 0.707)
    toXY { up = False, down = False, left = False, right = True , ... } == (0, 1)

Now when you go up/right, you are still going 1 pixel per update.

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square green 40
            |> move x y
        ]

    update computer ( x, y ) =
        let
            ( dx, dy ) =
                toXY computer.keyboard
        in
        ( x + dx, y + dy )

-}
toXY : Keyboard -> ( Float, Float )
toXY keyboard =
    let
        x =
            toX keyboard

        y =
            toY keyboard
    in
    if x /= 0 && y /= 0 then
        ( x / squareRootOfTwo, y / squareRootOfTwo )

    else
        ( x, y )


squareRootOfTwo : Float
squareRootOfTwo =
    sqrt 2



-- SCREEN


{-| Get the dimensions of the screen. If the screen is 800 by 600, you will see
a value like this:

    { width = 800
    , height = 600
    , top = 300
    , left = -400
    , right = 400
    , bottom = -300
    }

This can be nice when used with [`moveY`](#moveY) if you want to put something
on the bottom of the screen, no matter the dimensions.

-}
type alias Screen =
    { width : Float
    , height : Float
    , top : Float
    , left : Float
    , right : Float
    , bottom : Float
    }



-- TIME


{-| The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).

`Time` is defined as:

    type alias Time = 
        { now: Int
        , delta: Int
        }

Where `now` is the number of milliseconds since 1970 January 1 at 00:00:00 UTC, 
and `delta` is the number of milliseconds since the previous animation frame.

-}
type alias Time =
    Internal.Time


{-| Create an angle that cycles from 0 to 360 degrees over time.

Here is an [`animation`](#animation) with a spinning triangle:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ triangle orange 50
            |> rotate (spin 8 time)
        ]

It will do a full rotation once every eight seconds. Try changing the `8` to
a `2` to make it do a full rotation every two seconds. It moves a lot faster!

-}
spin : Float -> Time -> Float
spin period time =
    360 * toFrac period time


{-| Smoothly wave between two Floats.

Here is an [`animation`](#animation) with a circle that resizes:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ circle lightBlue (wave 50 90 7 time)
        ]

The radius of the circle will cycles between 50 and 90 every seven seconds.
It kind of looks like it is breathing.

-}
wave : Float -> Float -> Float -> Time -> Float
wave lo hi period time =
    lo + (hi - lo) * (1 + cos (turns (toFrac period time))) / 2


{-| Zig zag between two Floats.

Here is an [`animation`](#animation) with a rectangle that tips back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ rectangle lightGreen 20 100
            |> rotate (zigzag -20 20 4 time)
        ]

It gets rotated by an angle. The angle cycles from -20 degrees to 20 degrees
every four seconds.

-}
zigzag : Float -> Float -> Float -> Time -> Float
zigzag lo hi period time =
    lo + (hi - lo) * abs (2 * toFrac period time - 1)


{-| -}
toFrac : Float -> Time -> Float
toFrac period { now } =
    let
        ms =
            now

        p =
            period * 1000
    in
    toFloat (modBy (round p) ms) / p



-- ANIMATION


{-| Create an animation!

Once you get comfortable using [`picture`](#picture) to layout shapes, you can
try out an `animation`. Here is square that zigzags back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square blue 40
            |> moveX (zigzag -100 100 2 time)
        ]

We need to define a `view` to make our animation work.

Within `view` we can use functions like [`spin`](#spin), [`wave`](#wave),
and [`zigzag`](#zigzag) to move and rotate our shapes.

-}
animation : (Time -> List Shape) -> Program () (Playground ()) Msg
animation viewFrame =
    let
        { init, update } =
            create (\{ time } _ -> viewFrame time) (\_ m -> m) ()

        view (Playground { computer, entities }) =
            { title = "Playground"
            , body = viewWrap computer.screen entities
            }

        subs (Playground { visibility, computer }) =
            case visibility of
                Browser.Events.Hidden ->
                    Browser.Events.onVisibilityChange VisibilityChanged

                Browser.Events.Visible ->
                    Sub.batch
                        [ Internal.subscriptions.resize
                        , Internal.subscriptions.time
                        , Internal.subscriptions.visibility
                        ]
    in
    Browser.document
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subs
        }



-- GAME


{-| Create a game!

Once you get comfortable with [`animation`](#animation), you can try making a
game with the keyboard and mouse. Here is an example of a green square that
just moves to the right:

    import Playground exposing (..)

    main =
        game view update 0

    view computer offset =
        [ square green 40
            |> moveRight offset
        ]

    update computer offset =
        offset + 0.03

This shows the three important parts of a game:

1.  `memory` - makes it possible to store information. So with our green square,
    we save the `offset` in memory. It starts out at `0`.
2.  `view` - lets us say which shapes to put on screen. So here we move our
    square right by the `offset` saved in memory.
3.  `update` - lets us update the memory. We are incrementing the `offset` by
    a tiny amount on each frame.

The `update` function is called about 60 times per second, so our little
changes to `offset` start to add up pretty quickly!

This game is not very fun though! Making a `game` also gives you access to the
[`Computer`](#Computer), so you can use information about the [`Mouse`](#Mouse)
and [`Keyboard`](#Keyboard) to make it interactive! So here is a red square that
moves based on the arrow keys:

    import Playground exposing (..)

    main =
        game view update ( 0, 0 )

    view computer ( x, y ) =
        [ square red 40
            |> move x y
        ]

    update computer ( x, y ) =
        ( x + toX computer.keyboard
        , y + toY computer.keyboard
        )

Notice that in the `update` we use information from the keyboard to update the
`x` and `y` values. These building blocks let you make pretty fancy games!

-}
game : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> memory -> Program () (Playground memory) Msg
game viewMemory updateMemory initialMemory =
    let
        { init, update } =
            create viewMemory updateMemory initialMemory

        subs (Playground { visibility, computer }) =
            case visibility of
                Browser.Events.Hidden ->
                    Browser.Events.onVisibilityChange VisibilityChanged

                Browser.Events.Visible ->
                    Sub.batch
                        [ subscriptions.keys
                        , subscriptions.time
                        , subscriptions.visibility
                        , subscriptions.click
                        , subscriptions.mouse
                        , subscriptions.resize
                        ]

        view (Playground { computer, entities }) =
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


{-| Playground State
-}
type alias Playground memory =
    Internal.Playground memory


{-| -}
type alias Msg =
    Internal.Msg



-- SHAPES


{-| Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!

-}
type alias Shape =
    Shape2d


{-| Make circles:

    dot =
        circle red 10

    sun =
        circle yellow 300

You give color and then the radius. So the higher the Float, the larger
the circle.

-}
circle : Color -> Float -> Shape
circle color radius =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.circle color)
        }


{-| Make ovals:

    football =
        oval brown 200 100

You give the color, and then the width and height. So our `football` example is 200 pixels wide and 100 pixels tall

-}
oval : Color -> Float -> Float -> Shape
oval color width height =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form width height (Render.circle color)
        }


{-| Make squares. Here are two squares combined to look like an empty box:

    import Playground exposing (..)

    main =
        picture
            [ square purple 80
            , square white 60
            ]

The Float you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.

-}
square : Color -> Float -> Shape
square color n =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form n n (Render.rect color)
        }


{-| Make rectangles. This example makes a red cross:

    import Playground exposing (..)

    main =
        picture
            [ rectangle red 20 60
            , rectangle red 60 20
            ]

You give the color, width, and then height. So the first shape is vertical
part of the cross, the thinner and taller part.

-}
rectangle : Color -> Float -> Float -> Shape
rectangle color width height =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form width height (Render.rect color)
        }


{-| Make triangles. So if you wanted to draw the Egyptian pyramids, you could
do a simple version like this:

    import Playground exposing (..)

    main =
        picture
            [ triangle darkYellow 200
            ]

The `Float` is the "radius", so the distance from the center to each point of
the pyramid is `200`. Pretty big!

-}
triangle : Color -> Float -> Shape
triangle color radius =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.ngon 3 color)
        }


{-| Make pentagons:

    import Playground exposing (..)

    main =
        picture
            [ pentagon darkGrey 100
            ]

You give the color and then the radius. So the distance from the center to each
of the five points is 100 pixels.

-}
pentagon : Color -> Float -> Shape
pentagon color radius =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.ngon 5 color)
        }


{-| Make hexagons:

    import Playground exposing (..)

    main =
        picture
            [ hexagon lightYellow 50
            ]

The Float is the radius, the distance from the center to each point.

If you made more hexagons, you could [`move`](#move) them around to make a
honeycomb pattern!

-}
hexagon : Color -> Float -> Shape
hexagon color radius =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1.75
        , sy = 1.75
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.ngon 6 color)
        }


{-| Make octagons:

    import Playground exposing (..)

    main =
        picture [ octagon red 100 ]

You give the color and radius, so each point of this stop sign is 100 pixels
from the center.

-}
octagon : Color -> Float -> Shape
octagon color radius =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.ngon 8 color)
        }


{-| Make any shape you want! Here is a very thin triangle:

    import Playground exposing (..)

    main =
        picture
            [ polygon red [ ( -10, -20 ), ( 0, 100 ), ( 10, -20 ) ]
            ]

**Note:** If you [`rotate`](#rotate) a polygon, it will always rotate around
`(0,0)`. So it is best to build your shapes around that point, and then use
[`move`](#move) or [`group`](#group) so that rotation makes more sense.

-}
polygon : Color -> List ( Float, Float ) -> Shape
polygon color points =
    (if signedArea points < 0 then
        List.reverse points

     else
        points
    )
        |> triangulate
        |> List.map
            (\( ( p1x, p1y ), ( p2x, p2y ), ( p3x, p3y ) ) ->
                polygonTriangle color ( vec2 p1x p1y, vec2 p2x p2y, vec2 p3x p3y )
            )
        |> group


polygonTriangle color data =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form 2 2 (Render.triangle color data)
        }


{-| Add some image from the internet:

    import Playground exposing (..)

    main =
        picture
            [ image 96 96 "https://elm-lang.org/images/turtle.gif"
            ]

You provide the width, height, and then the URL of the image you want to show.

-}
image : Float -> Float -> String -> Shape
image width height src =
    Shape2d
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured src
                (\t ->
                    Shape2d
                        { x = 0
                        , y = 0
                        , a = 0
                        , sx = 1
                        , sy = 1
                        , o = 1
                        , form =
                            t
                                |> Texture.size
                                |> (\( w, h ) -> Math.Vector2.vec2 (toFloat w) (toFloat h))
                                |> Render.image t
                                |> Form width height
                        }
                )
        }


{-| Show some words!

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello! How are you?"
            ]

You can use [`scale`](#scale) to make the words bigger or smaller.

-}
words : Color -> String -> Shape
words =
    tileFont wordsConfig


wordsConfig =
    { charW = 16
    , charH = 16
    , src = SimpleMood.image
    , getIndex = SimpleMood.letters
    }


{-| Put shapes together so you can [`move`](#move) and [`rotate`](#rotate)
them as a group. Maybe you want to put a bunch of stars in the sky:

    import Playground exposing (..)

    main =
        picture
            [ star
                |> move 100 100
                |> rotate 5
            , star
                |> move -120 40
                |> rotate 20
            , star
                |> move 80 -150
                |> rotate 32
            , star
                |> move -90 -30
                |> rotate -16
            ]

    star =
        group
            [ triangle yellow 20
            , triangle yellow 20
                |> rotate 180
            ]

-}
group : List Shape -> Shape
group shapes =
    Shape2d { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Group shapes }



-- TRANSFORMS


{-| Move a shape by some Float of pixels:

    import Playground exposing (..)

    main =
        picture
            [ square red 100
                |> move -60 60
            , square yellow 100
                |> move 60 60
            , square green 100
                |> move 60 -60
            , square blue 100
                |> move -60 -60
            ]

-}
move : Float -> Float -> Shape -> Shape
move dx dy (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | x = x + dx, y = y + dy }


{-| Move a shape up by some Float of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:

    import Playground exposing (..)

    main =
        picture
            [ rectangle brown 40 200
            , circle green 100
                |> moveUp 180
            ]

-}
moveUp : Float -> Shape -> Shape
moveUp =
    moveY


{-| Move a shape down by some Float of pixels. So if you wanted to put the sky
above the ground, you could move the sky up and the ground down:

    import Playground exposing (..)

    main =
        picture
            [ rectangle lightBlue 200 100
                |> moveUp 50
            , rectangle lightGreen 200 100
                |> moveDown 50
            ]

-}
moveDown : Float -> Shape -> Shape
moveDown dy (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | y = y - dy }


{-| Move shapes to the left.

    import Playground exposing (..)

    main =
        picture
            [ circle yellow 10
                |> moveLeft 80
                |> moveUp 30
            ]

-}
moveLeft : Float -> Shape -> Shape
moveLeft dx (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | x = x - dx }


{-| Move shapes to the right.

    import Playground exposing (..)

    main =
        picture
            [ square purple 20
                |> moveRight 80
                |> moveDown 100
            ]

-}
moveRight : Float -> Shape -> Shape
moveRight =
    moveX


{-| Move the `x` coordinate of shape by some amount. Here is a square that
moves back and forth:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square purple 20
            |> moveX (wave 4 -200 200 time)
        ]

Using `moveX` feels a bit nicer here because the movement may be positive or negative.

-}
moveX : Float -> Shape -> Shape
moveX dx (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | x = x + dx }


{-| Move the `y` coordinate of shape by some amount. Maybe you want to make grass along the bottom of the screen:

    import Playground exposing (..)

    main =
        game view update 0

    update computer memory =
        memory

    view computer count =
        [ rectangle green computer.screen.width 100
            |> moveY computer.screen.bottom
        ]

Using `moveY` feels a bit nicer when setting things relative to the bottom or
top of the screen, since the values are negative sometimes.

-}
moveY : Float -> Shape -> Shape
moveY dy (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | y = y + dy }


{-| Make a shape bigger or smaller. So if you wanted some [`words`](#words) to
be larger, you could say:

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello, nice to see you!"
                |> scale 3
            ]

-}
scale : Float -> Shape -> Shape
scale ns (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | sx = sx * ns, sy = sy * ns }


{-| Make a shape _horizontally_ bigger or smaller.
Also, passing a negative value mirrors a shape:

    tile 20 27 0 "character.png" 1
        |> scaleX -1

**Note**: If you just need to mirror a shape horizontally use `flipX`.

-}
scaleX : Float -> Shape -> Shape
scaleX sx (Shape2d shape) =
    Shape2d { shape | sx = shape.sx * sx }


{-| Make a shape _vertically_ bigger or smaller.
Also, passing a negative value mirrors a shape:

    tile 20 27 0 "character.png" 1
        |> scaleY -1

**Note**: If you just need to mirror a shape vertically use `flipY`.

-}
scaleY : Float -> Shape -> Shape
scaleY sy (Shape2d shape) =
    Shape2d { shape | sy = shape.sy * sy }


{-| Mirror shape horizontally.

    tile 20 27 0 "character.png" 1
        |> flipX

-}
flipX : Shape -> Shape
flipX (Shape2d shape) =
    Shape2d { shape | sx = shape.sx * -1 }


{-| Mirror shape vertically.

    tile 20 27 0 "character.png" 1
        |> flipY

-}
flipY : Shape -> Shape
flipY (Shape2d shape) =
    Shape2d { shape | sy = shape.sy * -1 }


{-| Rotate shapes in degrees.

    import Playground exposing (..)

    main =
        picture
            [ words black "These words are tilted!"
                |> rotate 10
            ]

The degrees go _counter-clockwise_ to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).

-}
rotate : Float -> Shape -> Shape
rotate da (Shape2d ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape2d { shape | a = a + degrees da }


{-| Fade a shape. This lets you make shapes see-through or even completely
invisible. Here is a shape that fades in and out:

    import Playground exposing (..)

    main =
        animation view

    view time =
        [ square orange 30
        , square blue 200
            |> fade (zigzag 0 1 3 time)
        ]

The Float has to be between `0` and `1`, where `0` is totally transparent
and `1` is completely solid.

-}
fade : Float -> Shape -> Shape
fade o (Shape2d shape) =
    Shape2d { shape | o = o }



-- COLOR


{-| Represents a color.

The colors below, like `red` and `green`, come from the [Tango palette][tango].
It provides a bunch of aesthetically reasonable colors. Each color comes with a
light and dark version, so you always get a set like `lightYellow`, `yellow`,
and `darkYellow`.

[tango]: https://en.wikipedia.org/wiki/Tango_Desktop_Project

-}
type alias Color =
    Math.Vector3.Vec3


{-| -}
lightYellow : Color
lightYellow =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#fce94f"


{-| -}
yellow : Color
yellow =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#edd400"


{-| -}
darkYellow : Color
darkYellow =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#c4a000"


{-| -}
lightOrange : Color
lightOrange =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#fcaf3e"


{-| -}
orange : Color
orange =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#f57900"


{-| -}
darkOrange : Color
darkOrange =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#ce5c00"


{-| -}
lightBrown : Color
lightBrown =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#e9b96e"


{-| -}
brown : Color
brown =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#c17d11"


{-| -}
darkBrown : Color
darkBrown =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#8f5902"


{-| -}
lightGreen : Color
lightGreen =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#8ae234"


{-| -}
green : Color
green =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#73d216"


{-| -}
darkGreen : Color
darkGreen =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#4e9a06"


{-| -}
lightBlue : Color
lightBlue =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#729fcf"


{-| -}
blue : Color
blue =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#3465a4"


{-| -}
darkBlue : Color
darkBlue =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#204a87"


{-| -}
lightPurple : Color
lightPurple =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#ad7fa8"


{-| -}
purple : Color
purple =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#75507b"


{-| -}
darkPurple : Color
darkPurple =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#5c3566"


{-| -}
lightRed : Color
lightRed =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#ef2929"


{-| -}
red : Color
red =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#cc0000"


{-| -}
darkRed : Color
darkRed =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#a40000"


{-| -}
lightGrey : Color
lightGrey =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#eeeeec"


{-| -}
grey : Color
grey =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#d3d7cf"


{-| -}
darkGrey : Color
darkGrey =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#babdb6"


{-| -}
lightCharcoal : Color
lightCharcoal =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#888a85"


{-| -}
charcoal : Color
charcoal =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#555753"


{-| -}
darkCharcoal : Color
darkCharcoal =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#2e3436"


{-| -}
white : Color
white =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#ffffff"


{-| -}
black : Color
black =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#000000"



-- ALTERNATE SPELLING GREYS


{-| -}
lightGray : Color
lightGray =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#eeeeec"


{-| -}
gray : Color
gray =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#d3d7cf"


{-| -}
darkGray : Color
darkGray =
    Maybe.withDefault (Math.Vector3.vec3 0 0 0) <| hexColor2Vec3 "#babdb6"



-- CUSTOM COLORS


{-| RGB stands for Red-Green-Blue. With these three parts, you can create any
color you want. For example:

    brightBlue =
        rgb 18 147 216

    brightGreen =
        rgb 119 244 8

    brightPurple =
        rgb 94 28 221

Each Float needs to be between 0 and 255.

It can be hard to figure out what Floats to pick, so try using a color picker
like [paletton] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/

-}
rgb : Float -> Float -> Float -> Color
rgb r g b =
    Math.Vector3.vec3 (toFloat (colorClamp r) / 255) (toFloat (colorClamp g) / 255) (toFloat (colorClamp b) / 255)


colorClamp : Float -> Int
colorClamp n =
    clamp 0 255 (round n)


hexColor2Vec3 : String -> Maybe Math.Vector3.Vec3
hexColor2Vec3 str =
    let
        withoutHash =
            if String.startsWith "#" str then
                String.dropLeft 1 str

            else
                str
    in
    case String.toList withoutHash of
        [ r1, r2, g1, g2, b1, b2 ] ->
            maybeMap6
                (\a b c d e f ->
                    Math.Vector3.vec3 ((a * 16 + b) / 255) ((c * 16 + d) / 255) ((e * 16 + f) / 255)
                )
                (intFromHexChar r1)
                (intFromHexChar r2)
                (intFromHexChar g1)
                (intFromHexChar g2)
                (intFromHexChar b1)
                (intFromHexChar b2)

        _ ->
            Nothing


intFromHexChar : Char -> Maybe Float
intFromHexChar s =
    case s of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing


maybeMap6 : (a -> b -> c -> d -> e -> f -> value) -> Maybe a -> Maybe b -> Maybe c -> Maybe d -> Maybe e -> Maybe f -> Maybe value
maybeMap6 func ma mb mc md me mf =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    case mc of
                        Nothing ->
                            Nothing

                        Just c ->
                            case md of
                                Nothing ->
                                    Nothing

                                Just d ->
                                    case me of
                                        Nothing ->
                                            Nothing

                                        Just e ->
                                            case mf of
                                                Nothing ->
                                                    Nothing

                                                Just f ->
                                                    Just (func a b c d e f)

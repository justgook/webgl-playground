module Playground exposing
    ( picture, animation, game
    , Shape, circle, oval, square, rectangle, triangle, pentagon, hexagon, octagon, polygon
    , words
    , image
    , move, moveUp, moveDown, moveLeft, moveRight, moveX, moveY
    , scale, rotate, fade
    , group
    , Time, spin, wave, zigzag, toFrac
    , Computer, Mouse, Screen, Keyboard, toX, toY, toXY
    , Color, rgb, red, orange, yellow, green, blue, purple, brown
    , lightRed, lightOrange, lightYellow, lightGreen, lightBlue, lightPurple, lightBrown
    , darkRed, darkOrange, darkYellow, darkGreen, darkBlue, darkPurple, darkBrown
    , white, lightGrey, grey, darkGrey, lightCharcoal, charcoal, darkCharcoal, black
    , lightGray, gray, darkGray
    , Number
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

@docs scale, rotate, fade


# Groups

@docs group


# Time

@docs Time, spin, wave, zigzag, toFrac


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


### Numbers

@docs Number

-}

import Browser
import Browser.Dom as Dom
import Browser.Events as E
import Dict exposing (Dict)
import Html
import Html.Attributes as H
import Json.Decode as D
import Math.Vector2 exposing (vec2)
import Math.Vector3
import Math.Vector4 exposing (vec4)
import Playground.Font.GoodNeighbors as Font
import Playground.Internal exposing (Form(..), Number, Shape(..))
import Playground.Mat3 as Mat3
import Playground.Polygon exposing (signedArea, triangulate)
import Playground.Render as Render
import Set exposing (Set)
import Task exposing (Task)
import Time
import WebGL exposing (Entity)
import WebGL.Texture as Texture exposing (nonPowerOfTwoOptions)



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
picture : List Shape -> Program () Picture Msg
picture shapes =
    let
        init () =
            ( Picture (toScreen 600 600) Dict.empty []
            , Task.perform (\{ scene } -> Resized (toScreen scene.width scene.height)) Dom.getViewport
            )

        view (Picture screen _ entities) =
            { title = "Playground"
            , body = [ viewWrap screen entities ]
            }

        update msg ((Picture screen textures entities) as model) =
            (case msg of
                Resized newScreen ->
                    Picture newScreen textures entities

                GotTexture r ->
                    Picture screen (gotTextures r textures) entities

                _ ->
                    model
            )
                |> renderPicture shapes

        subscriptions _ =
            E.onResize toResized
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


renderPicture : List Shape -> Picture -> ( Picture, Cmd Msg )
renderPicture shapes (Picture screen textures _) =
    let
        ( entities, missing ) =
            render screen textures shapes

        ( newTextures, cmd ) =
            case missing of
                [] ->
                    ( textures, Cmd.none )

                _ ->
                    requestTexture missing textures
    in
    ( Picture screen newTextures entities, cmd )


type Picture
    = Picture Screen TextureManager (List Entity)



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
    { x : Number
    , y : Number
    , down : Bool
    , click : Bool
    }


{-| A number like `1` or `3.14` or `-120`.
-}
type alias Number =
    Float



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

**Note:** The `keys` set will be filled with the name of all keys which are
down right now. So you will see things like `"a"`, `"b"`, `"c"`, `"1"`, `"2"`,
`"Space"`, and `"Control"` in there. Check out [this list][list] to see the
names used for all the different special keys! From there, you can use
[`Set.member`][member] to check for whichever key you want. E.g.
`Set.member "Control" computer.keyboard.keys`.

[list]: https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/key/Key_Values
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


{-| Turn the LEFT and RIGHT arrows into a number.

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
toX : Keyboard -> Number
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


{-| Turn the UP and DOWN arrows into a number.

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
toY : Keyboard -> Number
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
toXY : Keyboard -> ( Number, Number )
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


squareRootOfTwo : Number
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
    { width : Number
    , height : Number
    , top : Number
    , left : Number
    , right : Number
    , bottom : Number
    }



-- TIME


{-| The current time.

Helpful when making an [`animation`](#animation) with functions like
[`spin`](#spin), [`wave`](#wave), and [`zigzag`](#zigzag).

-}
type Time
    = Time Time.Posix


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
spin : Number -> Time -> Number
spin period time =
    360 * toFrac period time


{-| Smoothly wave between two numbers.

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
wave : Number -> Number -> Number -> Time -> Number
wave lo hi period time =
    lo + (hi - lo) * (1 + cos (turns (toFrac period time))) / 2


{-| Zig zag between two numbers.

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
zigzag : Number -> Number -> Number -> Time -> Number
zigzag lo hi period time =
    lo + (hi - lo) * abs (2 * toFrac period time - 1)


{-| -}
toFrac : Float -> Time -> Float
toFrac period (Time posix) =
    let
        ms =
            Time.posixToMillis posix

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
animation : (Time -> List Shape) -> Program () Animation Msg
animation viewFrame =
    let
        init () =
            ( Animation E.Visible (toScreen 600 600) Dict.empty [] (Time (Time.millisToPosix 0))
            , Task.perform GotViewport Dom.getViewport
            )

        view (Animation _ screen _ entities _) =
            { title = "Playground"
            , body = [ viewWrap screen entities ]
            }

        update =
            animationUpdate viewFrame

        subscriptions (Animation visibility _ _ _ _) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    animationSubscriptions
    in
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Animation
    = Animation E.Visibility Screen TextureManager (List Entity) Time


animationSubscriptions : Sub Msg
animationSubscriptions =
    Sub.batch
        [ E.onResize toResized
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        ]


toResized : Int -> Int -> Msg
toResized w h =
    Resized (toScreen (toFloat w) (toFloat h))


animationUpdate : (Time -> List Shape) -> Msg -> Animation -> ( Animation, Cmd Msg )
animationUpdate viewFrame msg ((Animation v screen textures entities t) as state) =
    case msg of
        Tick time ->
            let
                ( newEntities, missing ) =
                    render screen textures (viewFrame (Time time))
            in
            case missing of
                [] ->
                    ( Animation v screen textures newEntities (Time time), Cmd.none )

                _ ->
                    requestTexture missing textures
                        |> Tuple.mapFirst (\loadingTextures -> Animation v screen loadingTextures newEntities (Time time))

        VisibilityChanged vis ->
            ( Animation vis screen textures entities t, Cmd.none )

        GotViewport { viewport } ->
            ( Animation v (toScreen viewport.width viewport.height) textures entities t, Cmd.none )

        Resized newScreen ->
            ( Animation v newScreen textures entities t, Cmd.none )

        KeyChanged _ _ ->
            ( state, Cmd.none )

        MouseMove _ _ ->
            ( state, Cmd.none )

        MouseClick ->
            ( state, Cmd.none )

        MouseButton _ ->
            ( state, Cmd.none )

        GotTexture r ->
            ( Animation v screen (gotTextures r textures) entities t, Cmd.none )



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
game : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> memory -> Program () (Game memory) Msg
game viewMemory updateMemory initialMemory =
    let
        init () =
            ( Game
                { visibility = E.Visible
                , memory = initialMemory
                , textures = Dict.empty
                , computer = initialComputer
                , entities = []
                }
            , Task.perform GotViewport Dom.getViewport
            )

        view (Game { memory, textures, computer, entities }) =
            { title = "Playground"
            , body = [ viewWrap computer.screen entities ]
            }

        subscriptions (Game { visibility }) =
            case visibility of
                E.Hidden ->
                    E.onVisibilityChange VisibilityChanged

                E.Visible ->
                    gameSubscriptions
    in
    Browser.document
        { init = init
        , view = view
        , update = gameUpdate viewMemory updateMemory
        , subscriptions = subscriptions
        }


initialComputer : Computer
initialComputer =
    { mouse = Mouse 0 0 False False
    , keyboard = emptyKeyboard
    , screen = toScreen 600 600
    , time = Time (Time.millisToPosix 0)
    }



-- SUBSCRIPTIONS


gameSubscriptions : Sub Msg
gameSubscriptions =
    Sub.batch
        [ E.onResize toResized
        , E.onKeyUp (D.map (KeyChanged False) (D.field "key" D.string))
        , E.onKeyDown (D.map (KeyChanged True) (D.field "key" D.string))
        , E.onAnimationFrame Tick
        , E.onVisibilityChange VisibilityChanged
        , E.onClick (D.succeed MouseClick)
        , E.onMouseDown (D.succeed (MouseButton True))
        , E.onMouseUp (D.succeed (MouseButton False))
        , E.onMouseMove (D.map2 MouseMove (D.field "pageX" D.float) (D.field "pageY" D.float))
        ]



-- GAME HELPERS


type Game memory
    = Game
        { visibility : E.Visibility
        , memory : memory
        , textures : TextureManager
        , computer : Computer
        , entities : List Entity
        }


type alias TextureManager =
    Dict String TextureData


type TextureData
    = Loading
    | Success { texture : Texture.Texture, size : Math.Vector2.Vec2 }


type Msg
    = KeyChanged Bool String
    | Tick Time.Posix
    | GotViewport Dom.Viewport
    | Resized Screen
    | VisibilityChanged E.Visibility
    | MouseMove Float Float
    | MouseClick
    | MouseButton Bool
    | GotTexture (Result Texture.Error (List ( String, Texture.Texture )))


gameUpdate : (Computer -> memory -> List Shape) -> (Computer -> memory -> memory) -> Msg -> Game memory -> ( Game memory, Cmd Msg )
gameUpdate viewMemory updateMemory msg (Game ({ visibility, memory, textures, computer } as model)) =
    case msg of
        Tick time ->
            let
                newModel =
                    { model
                        | memory = updateMemory computer memory
                        , computer =
                            if computer.mouse.click then
                                { computer | time = Time time, mouse = mouseClick False computer.mouse }

                            else
                                { computer | time = Time time }
                    }

                --TODO move that after all updates
                ( entities, missing ) =
                    render computer.screen textures (viewMemory newModel.computer newModel.memory)
            in
            case missing of
                [] ->
                    ( Game { newModel | entities = entities }, Cmd.none )

                _ ->
                    requestTexture missing textures
                        |> Tuple.mapFirst (\loadingTextures -> Game { newModel | entities = entities, textures = loadingTextures })

        GotViewport { viewport } ->
            ( Game { model | computer = { computer | screen = toScreen viewport.width viewport.height } }, Cmd.none )

        Resized newScreen ->
            ( Game { model | computer = { computer | screen = newScreen } }, Cmd.none )

        KeyChanged isDown key ->
            ( Game { model | computer = { computer | keyboard = updateKeyboard isDown key computer.keyboard } }, Cmd.none )

        MouseMove pageX pageY ->
            let
                x =
                    computer.screen.left + pageX

                y =
                    computer.screen.top - pageY
            in
            ( Game { model | computer = { computer | mouse = mouseMove x y computer.mouse } }, Cmd.none )

        MouseClick ->
            ( Game { model | computer = { computer | mouse = mouseClick True computer.mouse } }, Cmd.none )

        MouseButton isDown ->
            ( Game { model | computer = { computer | mouse = mouseDown isDown computer.mouse } }, Cmd.none )

        VisibilityChanged vis ->
            ( { model
                | visibility = vis
                , computer =
                    { computer
                        | keyboard = emptyKeyboard
                        , mouse = Mouse computer.mouse.x computer.mouse.y False False
                    }
              }
                |> Game
            , Cmd.none
            )

        GotTexture r ->
            ( Game { model | textures = gotTextures r model.textures }, Cmd.none )


requestTexture : List String -> TextureManager -> ( TextureManager, Cmd Msg )
requestTexture missing textures =
    missing
        |> List.foldl
            (\url ( acc1, acc2 ) ->
                let
                    textureName =
                        stripTextureUrl url
                in
                ( Dict.insert textureName Loading acc1
                , (Texture.loadWith textureOption textureName
                    |> Task.map (Tuple.pair textureName)
                  )
                    :: acc2
                )
            )
            ( textures, [] )
        |> Tuple.mapSecond (Task.sequence >> Task.attempt GotTexture)


gotTextures : Result error (List ( String, Texture.Texture )) -> TextureManager -> TextureManager
gotTextures r textures =
    case r of
        Ok texturesList ->
            List.foldl
                (\( name, t ) ->
                    { texture = t
                    , size =
                        Texture.size t
                            |> (\( w, h ) -> Math.Vector2.vec2 (toFloat w) (toFloat h))
                    }
                        |> Success
                        |> Dict.insert name
                )
                textures
                texturesList

        Err _ ->
            textures


stripTextureUrl a =
    a


setIfNotExists : String -> TextureManager -> TextureManager
setIfNotExists a =
    Dict.update a (Maybe.withDefault Loading >> Just)



-- SCREEN HELPERS


toScreen : Float -> Float -> Screen
toScreen width height =
    { width = width
    , height = height
    , top = height / 2
    , left = -width / 2
    , right = width / 2
    , bottom = -height / 2
    }



-- MOUSE HELPERS


mouseClick : Bool -> Mouse -> Mouse
mouseClick bool mouse =
    { mouse | click = bool }


mouseDown : Bool -> Mouse -> Mouse
mouseDown bool mouse =
    { mouse | down = bool }


mouseMove : Float -> Float -> Mouse -> Mouse
mouseMove x y mouse =
    { mouse | x = x, y = y }



-- KEYBOARD HELPERS


emptyKeyboard : Keyboard
emptyKeyboard =
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


updateKeyboard : Bool -> String -> Keyboard -> Keyboard
updateKeyboard isDown key keyboard =
    let
        keys =
            if isDown then
                Set.insert key keyboard.keys

            else
                Set.remove key keyboard.keys
    in
    case key of
        " " ->
            { keyboard | keys = keys, space = isDown }

        "Enter" ->
            { keyboard | keys = keys, enter = isDown }

        "Shift" ->
            { keyboard | keys = keys, shift = isDown }

        "Backspace" ->
            { keyboard | keys = keys, backspace = isDown }

        "ArrowUp" ->
            { keyboard | keys = keys, up = isDown }

        "ArrowDown" ->
            { keyboard | keys = keys, down = isDown }

        "ArrowLeft" ->
            { keyboard | keys = keys, left = isDown }

        "ArrowRight" ->
            { keyboard | keys = keys, right = isDown }

        _ ->
            { keyboard | keys = keys }



-- SHAPES


{-| Shapes help you make a `picture`, `animation`, or `game`.

Read on to see examples of [`circle`](#circle), [`rectangle`](#rectangle),
[`words`](#words), [`image`](#image), and many more!

-}
type alias Shape =
    Playground.Internal.Shape


{-| Make circles:

    dot =
        circle red 10

    sun =
        circle yellow 300

You give a color and then the radius. So the higher the number, the larger
the circle.

-}
circle : Color -> Number -> Shape
circle color radius =
    Shape
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

You give the color, and then the width and height. So our `football` example
is 200 pixels wide and 100 pixels tall.

-}
oval : Color -> Number -> Number -> Shape
oval color width height =
    Shape
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

The number you give is the dimension of each side. So that purple square would
be 80 pixels by 80 pixels.

-}
square : Color -> Number -> Shape
square color n =
    Shape
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
rectangle : Color -> Number -> Number -> Shape
rectangle color width height =
    Shape
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

The number is the "radius", so the distance from the center to each point of
the pyramid is `200`. Pretty big!

-}
triangle : Color -> Number -> Shape
triangle color radius =
    Shape
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
pentagon : Color -> Number -> Shape
pentagon color radius =
    Shape
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

The number is the radius, the distance from the center to each point.

If you made more hexagons, you could [`move`](#move) them around to make a
honeycomb pattern!

-}
hexagon : Color -> Number -> Shape
hexagon color radius =
    Shape
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form (radius * 2) (radius * 2) (Render.ngon 6 color)
        }


{-| Make octogons:

    import Playground exposing (..)

    main =
        picture [ octagon red 100 ]

You give the color and radius, so each point of this stop sign is 100 pixels
from the center.

-}
octagon : Color -> Number -> Shape
octagon color radius =
    Shape
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
            [ polygon [ ( -10, -20 ), ( 0, 100 ), ( 10, -20 ) ]
            ]

**Note:** If you [`rotate`](#rotate) a polygon, it will always rotate around
`(0,0)`. So it is best to build your shapes around that point, and then use
[`move`](#move) or [`group`](#group) so that rotation makes more sense.

-}
polygon : Color -> List ( Number, Number ) -> Shape
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
    Shape
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
            [ image 96 96 "https://elm-lang.org/assets/turtle.gif"
            ]

You provide the width, height, and then the URL of the image you want to show.

-}
image : Number -> Number -> String -> Shape
image width height src =
    Shape
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured src
                (\t ->
                    Shape
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
words color string =
    let
        config =
            Font.config
    in
    Shape
        { x = 0
        , y = 0
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form =
            Textured config.image
                (\t ->
                    let
                        ( imgW, imgH ) =
                            t
                                |> Texture.size
                                |> Tuple.mapBoth toFloat toFloat

                        imgSize =
                            Math.Vector2.vec2 imgW imgH

                        ( chars, width ) =
                            String.toList string
                                |> List.foldl
                                    (\a ( l, w ) ->
                                        let
                                            c =
                                                config.letters a

                                            uv =
                                                vec4 (c.x / imgW) (c.y / imgH) (c.w / imgW) (c.h / imgH)
                                        in
                                        ( char color t imgSize c (w + 0.5 * c.w) c.o uv :: l, w + c.w + config.spacing )
                                    )
                                    ( [], 0 )
                    in
                    Shape { x = (width - config.spacing) * -0.5, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Group chars }
                )
        }


char color t imgSize { w, h } x y uv =
    Shape
        { x = x
        , y = y
        , a = 0
        , sx = 1
        , sy = 1
        , o = 1
        , form = Form w h (Render.spriteWithColor t imgSize color uv)
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
    Shape { x = 0, y = 0, a = 0, sx = 1, sy = 1, o = 1, form = Group shapes }



-- TRANSFORMS


{-| Move a shape by some number of pixels:

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
move : Number -> Number -> Shape -> Shape
move dx dy (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | x = x + dx, y = y + dy }


{-| Move a shape up by some number of pixels. So if you wanted to make a tree
you could move the leaves up above the trunk:

    import Playground exposing (..)

    main =
        picture
            [ rectangle brown 40 200
            , circle green 100
                |> moveUp 180
            ]

-}
moveUp : Number -> Shape -> Shape
moveUp =
    moveY


{-| Move a shape down by some number of pixels. So if you wanted to put the sky
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
moveDown : Number -> Shape -> Shape
moveDown dy (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | y = y - dy }


{-| Move shapes to the left.

    import Playground exposing (..)

    main =
        picture
            [ circle yellow 10
                |> moveLeft 80
                |> moveUp 30
            ]

-}
moveLeft : Number -> Shape -> Shape
moveLeft dx (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | x = x - dx }


{-| Move shapes to the right.

    import Playground exposing (..)

    main =
        picture
            [ square purple 20
                |> moveRight 80
                |> moveDown 100
            ]

-}
moveRight : Number -> Shape -> Shape
moveRight =
    moveX


{-| Move the `x` coordinate of a shape by some amount. Here is a square that
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
moveX : Number -> Shape -> Shape
moveX dx (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | x = x + dx }


{-| Move the `y` coordinate of a shape by some amount. Maybe you want to make
grass along the bottom of the screen:

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
moveY : Number -> Shape -> Shape
moveY dy (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | y = y + dy }


{-| Make a shape bigger or smaller. So if you wanted some [`words`](#words) to
be larger, you could say:

    import Playground exposing (..)

    main =
        picture
            [ words black "Hello, nice to see you!"
                |> scale 3
            ]

-}
scale : Number -> Shape -> Shape
scale ns (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | sx = sx * ns, sy = sy * ns }


{-| Rotate shapes in degrees.

    import Playground exposing (..)

    main =
        picture
            [ words black "These words are tilted!"
                |> rotate 10
            ]

The degrees go **counter-clockwise** to match the direction of the
[unit circle](https://en.wikipedia.org/wiki/Unit_circle).

-}
rotate : Number -> Shape -> Shape
rotate da (Shape ({ x, y, a, sx, sy, o, form } as shape)) =
    Shape { shape | a = a + da }


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

The number has to be between `0` and `1`, where `0` is totally transparent
and `1` is completely solid.

-}
fade : Number -> Shape -> Shape
fade o (Shape shape) =
    Shape { shape | o = o }



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

Each number needs to be between 0 and 255.

It can be hard to figure out what numbers to pick, so try using a color picker
like [paletton] to find colors that look nice together. Once you find nice
colors, click on the color previews to get their RGB values.

[paletton]: http://paletton.com/

-}
rgb : Number -> Number -> Number -> Color
rgb r g b =
    Math.Vector3.vec3 (toFloat (colorClamp r) / 255) (toFloat (colorClamp g) / 255) (toFloat (colorClamp b) / 255)


colorClamp : Number -> Int
colorClamp number =
    clamp 0 255 (round number)



-- RENDER


viewWrap : Screen -> List Entity -> Html.Html msg
viewWrap screen =
    WebGL.toHtmlWith webGLOption
        [ H.style "position" "absolute"
        , H.style "top" "0"
        , H.style "left" "0"
        , H.style "width" "100%"
        , H.style "height" "100%"
        , H.width (round screen.width)
        , H.height (round screen.height)
        ]


render : Screen -> TextureManager -> List Shape -> ( List Entity, List String )
render screen textures shapes =
    List.foldr (renderShape screen textures Mat3.identity) ( [], Set.empty ) shapes
        |> Tuple.mapSecond Set.toList


webGLOption : List WebGL.Option
webGLOption =
    [ WebGL.alpha False
    , WebGL.depth 1

    --, WebGL.clearColor (29 / 255) (33 / 255) (45 / 255) 1
    , WebGL.clearColor 1 1 1 1
    ]


textureOption : Texture.Options
textureOption =
    { nonPowerOfTwoOptions
        | magnify = Texture.linear
        , minify = Texture.linear
    }


roundAngle a =
    let
        --fmod a b =
        --    a - b * toFloat (floor (a / b))
        trigPrecision =
            1000000000
    in
    (round a * trigPrecision |> toFloat) / trigPrecision


createMat3_ tx ty sx sy a_ =
    let
        t =
            Mat3.makeTranslate tx ty

        a =
            a_ * pi / 180

        r =
            Mat3.makeRotate a

        s_ =
            Mat3.makeScale sx sy
    in
    Mat3.mul t (Mat3.mul r s_)


setAlpha c =
    c |> Math.Vector3.toRecord |> (\c1 -> vec4 c1.x c1.y c1.z)


renderShape : Screen -> TextureManager -> Mat3.Mat3 -> Shape -> ( List Entity, Set String ) -> ( List Entity, Set String )
renderShape screen textures parent (Shape { x, y, a, sx, sy, o, form }) (( entities, missing ) as acc) =
    let
        createMat3 tx ty sx_ sy_ a_ =
            createMat3_ tx ty sx_ sy_ a_
                |> Mat3.mul parent

        newWay tx ty sx_ sy_ a_ =
            createMat3 tx ty sx_ sy_ a_
                |> Mat3.mul (Mat3.makeScale (1 / screen.width) (1 / screen.height))
                |> Mat3.toGL
    in
    case form of
        Form width height fn ->
            let
                ( t1, t2 ) =
                    newWay
                        (x * 2)
                        (y * 2)
                        (width * sx)
                        (height * sy)
                        a
            in
            ( fn t2 t1 o
                :: entities
            , missing
            )

        Textured src fn ->
            let
                name =
                    stripTextureUrl src
            in
            case ( Set.member name missing, Dict.get name textures ) of
                ( _, Just (Success { texture, size }) ) ->
                    renderShape screen textures (createMat3 (x * 2) (y * 2) sx sy a) (fn texture) acc

                ( False, Nothing ) ->
                    ( entities, Set.insert src missing )

                _ ->
                    acc

        Group shapes ->
            shapes
                |> List.foldr (renderShape screen textures (createMat3 (x * 2) (y * 2) sx sy a)) acc


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

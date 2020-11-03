# WebGL Playground
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fjustgook%2Fwebgl-playground.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Fjustgook%2Fwebgl-playground?ref=badge_shield)


Fork of [evancz/elm-playground](https://package.elm-lang.org/packages/evancz/elm-playground/latest/), but with [WebGL](https://package.elm-lang.org/packages/elm-explorations/webgl/latest/) backend.

Create pictures, animations, and games with Elm!

This is the package I wanted when I was learning programming. Start by putting shapes on screen and work up to making games. I hope this package will be fun for a broad range of ages and backgrounds!


## Pictures

A picture is a list of shapes. For example, this picture combines a brown rectangle and a green circle to make a tree:

```elm
import Playground exposing (..)

main =
  picture
    [ rectangle brown 40 200
    , circle green 100
        |> moveUp 100
    ]
```

Play around to get familiar with all the different shapes and transformations in the library.


## Animations

An animation is a list of shapes that changes over time. For example, here is a spinning triangle:

```elm
import Playground exposing (..)

main =
  animation view

view time =
  [ triangle orange 50
      |> rotate (spin 8 time)
  ]
```

It will do a full spin every 8 seconds.

Maybe try making a car with spinning octogons as wheels? Try using [`wave`](https://package.elm-lang.org/packages/evancz/elm-playground/latest/Playground#wave) to move things back-and-forth? Try using [`zigzag`](https://package.elm-lang.org/packages/evancz/elm-playground/latest/Playground#zigzag) to fade things in-and-out?


## Games

A game lets you use input from the mouse and keyboard to change your picture. For example, here is a square that moves around based on the arrow keys:

```elm
import Playground exposing (..)

main =
  game view update (0,0)

view computer (x,y) =
  [ square blue 40
      |> move x y
  ]

update computer (x,y) =
  ( x + toX computer.keyboard
  , y + toY computer.keyboard
  )
```

Every game has three important parts:

1. `memory` - Store information. Our examples stores `(x,y)` coordinates.
2. `update` - Update the memory based on mouse movements, key presses, etc. Our example moves the `(x,y)` coordinate around based on the arrow keys.
3. `view` - Turn the memory into a picture. Our example just shows one blue square at the `(x,y)` coordinate we have been tracking in memory.

When you start making fancier games, you will store fancier things in memory. There is a lot of room to develop your programming skills here: Making lists, using records, creating custom types, etc.

I started off trying to make Pong, then worked on games like Breakout and Space Invaders as I learned more and more. It was really fun, and I hope it will be for you as well!
# Changelog

# 3.0.1 -> 4.0.0

  1. `Playground.Advanced` move to own package [`WebGL.Shape2d`](https://package.elm-lang.org/packages/justgook/webgl-shape/latest/) [example](https://raw.githubusercontent.com/justgook/webgl-playground/master/examples/src/Embedded.elm)
  1. `Playground.Keyboard.keys` now is filled with the `event.code` instead of `event.key`
  1. `Playground.delta computer.time` - `computer.time.delta`
  1. `Playground.now computer.time` - `computer.time.now`
  1. `Playground.Extra.scaleX` moved to `Playground.scaleX`
  1. `Playground.Extra.scaleY` moved to `Playground.scaleY`
  

  ### Type changes:

  1. Was `Playground.Game` - now is  `Playground.Playground`
  1. Was `Playground.Animation` - now is  `Playground.Playground ()`
  1. Was `Playground.Picture` - now is  `Playground.Playground ()`
  1. `Number` - removed - just use `Float`
  
  


## License
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Fjustgook%2Fwebgl-playground.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Fjustgook%2Fwebgl-playground?ref=badge_large)
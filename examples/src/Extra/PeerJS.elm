module Extra.PeerJS exposing (..)

import Browser
import Playground exposing (..)
import Playground.Advanced exposing (..)


type Message
    = Playground Msg
    | Port (Memory -> Memory)


type alias Memory =
    { p1 : Number
    , p2 : Number
    , x : Number
    , y : Number
    , score : ( Int, Int )
    , connected : Bool
    }


viewMemory { screen } { p1, p2, x, y } =
    [ rectangle black 10 100 |> move (screen.left + 20) p1
    , rectangle black 10 100 |> move (screen.right - 20) p2
    , circle red 15 |> move x y
    ]


updateMemory computer memory =
    memory


initialMemory =
    { p1 = 0
    , p2 = 0
    , x = 0
    , y = 0
    , score = ( 0, 0 )
    , connected = False
    }


main : Program () (Game Memory) Message
main =
    Browser.document
        { init = \_ -> playground.init |> Tuple.mapSecond (Cmd.map Playground)
        , view =
            \model ->
                { title = "WebGL Playground Demo"
                , body = [ playground.view model ]
                }
        , update =
            \msg model ->
                case msg of
                    Playground m ->
                        playground.update m model |> Tuple.mapSecond (Cmd.map Playground)

                    Port fn ->
                        ( edit (\_ -> fn) model, Cmd.none )
        , subscriptions = \_ -> subscriptions.all |> Sub.map Playground
        }


playground =
    embed viewMemory updateMemory initialMemory

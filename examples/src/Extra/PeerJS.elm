port module Extra.PeerJS exposing (..)

import Browser
import Json.Decode as D
import Playground exposing (..)
import Playground.Shape2d exposing (..)


port connect : (Bool -> msg) -> Sub msg


port p1Data : (Bool -> msg) -> Sub msg


port p2Data : ({ p1 : Number, p2 : Number, x : Number, y : Number, score : { p1 : Int, p2 : Int } } -> msg) -> Sub msg


port p2Send : Bool -> Cmd msg


port p1Send : { p1 : Number, p2 : Number, x : Number, y : Number, score : { p1 : Int, p2 : Int } } -> Cmd msg


connectToMsg : Bool -> Message
connectToMsg a =
    if a then
        Port (\m -> { m | connection = P1 })

    else
        Port (\m -> { m | connection = P2 })


type ID a
    = Int


p1DataToMsg a =
    Port (\m -> { m | p2 = clamp -0.5 0.5 <| m.p2 + bool2Number a * 0.025 })


modelToP1Send m_ =
    let
        ( _, m ) =
            get m_
    in
    { p1 = m.p1
    , p2 = m.p2
    , x = m.x
    , y = m.y
    , score = m.score
    }


bool2Number a =
    if a then
        1

    else
        -1


p2DataToMsg a =
    Port (\m -> { m | p1 = a.p1, p2 = a.p2, x = a.x, y = a.y, score = a.score })


type Message
    = Playground Msg
    | Port (Memory -> Memory)


type Connection
    = P1
    | P2
    | Waiting
    | Fail


type alias Memory =
    { p1 : Number
    , p2 : Number
    , x : Number
    , y : Number
    , score : { p1 : Int, p2 : Int }
    , connection : Connection
    }


viewMemory : Computer -> Memory -> List Shape
viewMemory { screen, time } { p1, p2, x, y, connection } =
    [ rectangle black 10 100 |> move (screen.left + 20) (p1 * screen.height)
    , rectangle black 10 100 |> move (screen.right - 20) (p2 * screen.height)
    , circle red 15 |> move x y
    , case connection of
        P1 ->
            words black "Master"
                |> move 0 (screen.top - 40)

        P2 ->
            words black "Slave"
                |> move 0 (screen.top - 40)

        _ ->
            words black "Connecting"
                |> scale 3
                |> move 0 (screen.top - wave 40 60 1 time)
    ]


updateMemory { keyboard, screen } memory =
    case memory.connection of
        P1 ->
            { memory
                | p1 = clamp -0.5 0.5 <| memory.p1 + toY keyboard * 0.025
            }

        _ ->
            memory


initialMemory : Memory
initialMemory =
    { p1 = 0
    , p2 = 0
    , x = 0
    , y = 0
    , score = { p1 = 0, p2 = 0 }
    , connection = Waiting
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
                case ( msg, get model ) of
                    ( Playground m, ( computer, memory ) ) ->
                        playground.update m model
                            |> Tuple.mapSecond
                                (case memory.connection of
                                    P1 ->
                                        Cmd.map Playground
                                            >> List.singleton
                                            >> (::) (p1Send (modelToP1Send model))
                                            >> Cmd.batch

                                    P2 ->
                                        if computer.keyboard.up then
                                            Cmd.map Playground
                                                >> List.singleton
                                                >> (::) (p2Send True)
                                                >> Cmd.batch

                                        else if computer.keyboard.down then
                                            Cmd.map Playground
                                                >> List.singleton
                                                >> (::) (p2Send False)
                                                >> Cmd.batch

                                        else
                                            Cmd.map Playground

                                    _ ->
                                        Cmd.map Playground
                                )

                    ( Port fn, _ ) ->
                        ( edit (\_ -> fn) model, Cmd.none )
        , subscriptions =
            \_ ->
                [ subscriptions.all |> Sub.map Playground
                , connect connectToMsg
                , p1Data p1DataToMsg
                , p2Data p2DataToMsg
                ]
                    |> Sub.batch
        }


playground =
    embed viewMemory updateMemory initialMemory

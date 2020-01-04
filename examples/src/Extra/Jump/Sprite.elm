module Extra.Jump.Sprite exposing (idle, run, sheet)

import Extra.Jump.Direction as Direction exposing (Direction(..))
import Math.Vector4 exposing (vec4)
import Playground exposing (moveX, moveY)
import Playground.Extra exposing (scaleX, sprite)


run dir frame_ =
    let
        frame =
            round (frame_ / 5)
    in
    case dir of
        North ->
            [ sheet.u0, sheet.u1, sheet.u2, sheet.u3, sheet.u4, sheet.u5, sheet.u6, sheet.u7 ]
                |> getAt (modBy 8 frame)
                |> Maybe.withDefault sheet.u10

        NorthEast ->
            [ sheet.fu0, sheet.fu1, sheet.fu2, sheet.fu3, sheet.fu4, sheet.fu5, sheet.fu6, sheet.fu7 ]
                |> getAt (modBy 8 frame)
                |> Maybe.withDefault sheet.fu0

        East ->
            [ sheet.f0, sheet.f1, sheet.f2, sheet.f3, sheet.f4, sheet.f5, sheet.f6, sheet.f7 ]
                |> getAt (modBy 8 frame)
                |> Maybe.withDefault sheet.f7

        SouthEast ->
            sheet.fd9

        South ->
            sheet.d

        SouthWest ->
            sheet.fd9
                |> scaleX -1

        West ->
            [ sheet.f0, sheet.f1, sheet.f2, sheet.f3, sheet.f4, sheet.f5, sheet.f6, sheet.f7 ]
                |> getAt (modBy 8 frame)
                |> Maybe.withDefault sheet.f10
                |> scaleX -1

        NorthWest ->
            [ sheet.fu0, sheet.fu1, sheet.fu2, sheet.fu3, sheet.fu4, sheet.fu5, sheet.fu6, sheet.fu7 ]
                |> getAt (modBy 8 frame)
                |> Maybe.withDefault sheet.fu10
                |> scaleX -1

        Neither ->
            sheet.u1


idle dir frame_ =
    let
        get =
            getAt (modBy 20 (round (frame_ / 5)))
    in
    case dir of
        North ->
            [ sheet.u9, sheet.u10, sheet.u11, sheet.u12, sheet.u13 ]
                |> get
                |> Maybe.withDefault sheet.u10

        NorthEast ->
            [ sheet.fu10, sheet.fu13 ]
                |> get
                |> Maybe.withDefault sheet.fu9

        East ->
            [ sheet.f10, sheet.f13 ]
                |> get
                |> Maybe.withDefault sheet.f9

        SouthEast ->
            sheet.fd9

        South ->
            sheet.d

        SouthWest ->
            sheet.fd9
                |> scaleX -1

        West ->
            [ sheet.f10, sheet.f13 ]
                |> get
                |> Maybe.withDefault sheet.f9
                |> scaleX -1

        NorthWest ->
            [ sheet.fu10, sheet.fu13 ]
                |> get
                |> Maybe.withDefault sheet.fu9
                |> scaleX -1

        Neither ->
            sheet.u1


sheet =
    { u0 = sprite 14 16 image <| vec4 0.004347826086956522 0.7763157894736842 0.06086956521739131 0.21052631578947367
    , u1 = sprite 14 15 image <| vec4 0.07391304347826087 0.7894736842105262 0.06086956521739131 0.19736842105263158
    , u2 = sprite 14 16 image <| vec4 0.14347826086956522 0.7763157894736842 0.06086956521739131 0.21052631578947367
    , u3 = sprite 13 15 image <| vec4 0.21304347826086956 0.7894736842105262 0.05652173913043478 0.19736842105263158
    , u4 = sprite 14 15 image <| vec4 0.2782608695652174 0.7894736842105262 0.06086956521739131 0.19736842105263158
    , u5 = sprite 14 15 image <| vec4 0.34782608695652173 0.7894736842105262 0.06086956521739131 0.19736842105263158
    , u6 = sprite 14 16 image <| vec4 0.41739130434782606 0.7763157894736842 0.06086956521739131 0.21052631578947367
    , u7 = sprite 15 16 image <| vec4 0.48695652173913045 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u8 = sprite 15 16 image <| vec4 0.5608695652173913 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u9 = moveX 1 <| sprite 15 16 image <| vec4 0.6347826086956522 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u10 = moveX 1 <| sprite 15 16 image <| vec4 0.7086956521739131 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u11 = moveX 1 <| sprite 15 16 image <| vec4 0.782608695652174 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u12 = moveX 1 <| sprite 15 16 image <| vec4 0.8565217391304348 0.7763157894736842 0.06521739130434782 0.21052631578947367
    , u13 = moveX 1 <| moveY 0.5 <| sprite 15 17 image <| vec4 0.9304347826086956 0.763157894736842 0.06521739130434782 0.2236842105263158
    , fu0 = sprite 14 16 image <| vec4 0.004347826086956522 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu1 = sprite 14 15 image <| vec4 0.07391304347826087 0.5394736842105263 0.06086956521739131 0.19736842105263158
    , fu2 = sprite 14 16 image <| vec4 0.14347826086956522 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu3 = sprite 14 15 image <| vec4 0.21304347826086956 0.5394736842105263 0.06086956521739131 0.19736842105263158
    , fu4 = sprite 14 15 image <| vec4 0.2826086956521739 0.5394736842105263 0.06086956521739131 0.19736842105263158
    , fu5 = sprite 14 15 image <| vec4 0.3521739130434783 0.5394736842105263 0.06086956521739131 0.19736842105263158
    , fu6 = sprite 14 16 image <| vec4 0.4217391304347826 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu7 = sprite 15 16 image <| vec4 0.49130434782608695 0.5263157894736843 0.06521739130434782 0.21052631578947367
    , fu8 = sprite 14 16 image <| vec4 0.5652173913043478 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu9 = moveX 0.5 <| sprite 14 16 image <| vec4 0.6347826086956522 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu10 = moveX 0.5 <| sprite 14 16 image <| vec4 0.7043478260869566 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu11 = moveX 0.5 <| sprite 14 16 image <| vec4 0.7739130434782608 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu12 = moveX 0.5 <| sprite 14 16 image <| vec4 0.8434782608695652 0.5263157894736843 0.06086956521739131 0.21052631578947367
    , fu13 = moveX 0.5 <| moveY 0.5 <| sprite 14 17 image <| vec4 0.9130434782608695 0.513157894736842 0.06086956521739131 0.2236842105263158
    , f0 = moveY 1 <| sprite 13 16 image <| vec4 0.004347826086956522 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f1 = moveY 1.5 <| sprite 13 15 image <| vec4 0.06956521739130435 0.2894736842105262 0.05652173913043478 0.19736842105263158
    , f2 = sprite 13 16 image <| vec4 0.13478260869565217 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f3 = moveX 0.5 <| moveY -0.5 <| sprite 12 15 image <| vec4 0.2 0.2894736842105262 0.05217391304347826 0.19736842105263158
    , f4 = moveX 1 <| sprite 13 16 image <| vec4 0.2608695652173913 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f5 = moveX 1 <| sprite 13 16 image <| vec4 0.32608695652173914 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f6 = moveX 1 <| sprite 13 16 image <| vec4 0.391304347826087 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f7 = moveX 0.5 <| sprite 14 16 image <| vec4 0.45652173913043476 0.2763157894736842 0.06086956521739131 0.21052631578947367
    , f8 = sprite 13 17 image <| vec4 0.5260869565217391 0.26315789473684204 0.05652173913043478 0.2236842105263158
    , f9 = moveY -0.5 <| sprite 13 17 image <| vec4 0.5913 0.2632 0.05652 0.2237
    , f10 = sprite 13 16 image <| vec4 0.6565217391304348 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f11 = sprite 13 16 image <| vec4 0.7217391304347827 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f12 = sprite 13 16 image <| vec4 0.7869565217391304 0.2763157894736842 0.05652173913043478 0.21052631578947367
    , f13 = moveY 0.5 <| sprite 13 17 image <| vec4 0.8521739130434782 0.26315789473684204 0.05652173913043478 0.2236842105263158
    , fd9 = sprite 14 16 image <| vec4 0.004347826086956522 0.02631578947368418 0.06086956521739131 0.21052631578947367
    , d = sprite 12 18 image <| vec4 0.07391304347826087 0 0.05217391304347826 0.23684210526315788
    }


getAt : Int -> List a -> Maybe a
getAt idx xs =
    if idx < 0 then
        Nothing

    else
        List.head <| List.drop idx xs


image =
    "data:image/png;base64,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"

module Playground.Polygon exposing
    ( triangulate
    , signedArea
    )

{-|

@docs triangulate

-}

--https://github.com/zaidan/earclipper/blob/master/src/Earclipper/EarClipping.hs


{-| Triangulate given polygon and return a list of triangles.
It is assumed that the given polygon's points are given anticlockwise.
First and last point in the list don't need to be identical.
-}
triangulate : List Point -> List Triangle
triangulate =
    triangulateEar 0


{-| Ear clipping triangulation
-}
triangulateEar : Int -> List Point -> List Triangle
triangulateEar lastEar points =
    case points of
        [] ->
            []

        [ _ ] ->
            []

        [ _, _ ] ->
            []

        [ a, x, c ] ->
            [ ( a, x, c ) ]

        a :: x :: c :: xs ->
            let
                --TODO get rid of size in each step
                size =
                    3 + List.length xs
            in
            if lastEar > 2 * size then
                [ ( a, x, c ) ]

            else
                let
                    noPointInTriangle =
                        not <| isAnyPointInTriangle ( a, x, c ) xs

                    earFound =
                        convex && noPointInTriangle

                    convex =
                        isConvex a x c
                in
                if earFound then
                    -- TODO make me tail safe
                    ( a, x, c ) :: triangulateEar 0 ([ a ] ++ [ c ] ++ xs)

                else
                    triangulateEar (lastEar + 1) ([ x, c ] ++ xs ++ [ a ])


{-| Check if given point is in given triangle.

Triangle is assumed to be given clockwise.

-}
pointInTriangle : Triangle -> Point -> Bool
pointInTriangle ( ( ax, ay ), ( bx, by ), ( cx, cy ) ) ( px, py ) =
    let
        b0 =
            (bx - ax) * (cy - ay) - (cx - ax) * (by - ay)

        b1 =
            ((bx - px) * (cy - py) - (cx - px) * (by - py)) / b0

        b2 =
            ((cx - px) * (ay - py) - (ax - px) * (cy - py)) / b0

        b3 =
            1.0 - b1 - b2
    in
    if b0 == 0 then
        False

    else
        (b1 > 0) && (b2 > 0) && (b3 > 0)


{-| Check if any given point in list is in the given triangle.

E.g.

    isAnyPointInTriangle [ ( 0, 0 ), ( 0, 1 ), ( 1, 0 ) ] [ ( 0.1, 0.1 ), ( 0.2, 0.2 ) ] -- True

-}
isAnyPointInTriangle : Triangle -> List Point -> Bool
isAnyPointInTriangle triangle =
    List.any (pointInTriangle triangle)


{-| Check if given points are convex.
Points need to be given clockwise, e.g.
-}
isConvex : Point -> Point -> Point -> Bool
isConvex ( p1x, p1y ) ( px, py ) ( p2x, p2y ) =
    (p1x - px) * (p2y - py) - (p1y - py) * (p2x - px) < 0


type alias Triangle =
    ( Point, Point, Point )


type alias Point =
    ( Float, Float )


{-| Compute the signed area of a simple polygon. The the vertices are in
clockwise order, the signed area will be negative, if the vertices are given
in counter clockwise order, the area will be positive.
-}
signedArea : List Point -> Float
signedArea points =
    case points of
        first :: _ ->
            signedArea_ first points 0

        [] ->
            0


signedArea_ : Point -> List Point -> Float -> Float
signedArea_ first points acc =
    case points of
        ( p1x, p1y ) :: ( p2x, p2y ) :: rest ->
            signedArea_ first (( p2x, p2y ) :: rest) (p1x * p2y - p2x * p1y + acc)

        [ ( p1x, p1y ) ] ->
            let
                ( p2x, p2y ) =
                    first
            in
            p1x * p2y - p2x * p1y + acc

        [] ->
            acc

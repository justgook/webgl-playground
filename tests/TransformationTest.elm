module TransformationTest exposing (..)

import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (float, int)
import Mat3 as Mat3
import Math.Vector2 as Vec2
import Math.Vector4 as Vec4
import Playground.Core.Transformation as Transformation
import Test exposing (..)


suite : Test
suite =
    describe "Mat3 VS Transformation"
        [ test "init" <|
            \_ ->
                validate Transformation.identity Mat3.identity
        , fuzz float "makeRotate" <|
            \a ->
                validate (Transformation.makeRotate a) (Mat3.makeRotate a)
        , fuzz2 float float "makeScale" <|
            \sx sy ->
                validate (Transformation.makeScale sx sy) (Mat3.makeScale sx sy)
        , fuzz2 float float "makeTranslate" <|
            \tx ty ->
                validate (Transformation.makeTranslate tx ty) (Mat3.makeTranslate tx ty)
        , fuzz2 float float "create" <|
            \tx ty ->
                validate
                    (Transformation.apply Transformation.identity (Transformation.makeTranslate tx ty))
                    (Mat3.mul Mat3.identity (Mat3.makeTranslate tx ty))
        , fuzz5 float float float float int "transform" <|
            \tx ty sx sy a ->
                validate (Transformation.transform tx ty sx sy (degrees (toFloat a))) (oldWay tx ty sx sy (degrees (toFloat a)))
        , fuzz5 float float float float int "scale" <|
            \tx ty sx sy a ->
                validate (Transformation.transform tx ty sx sy (degrees (toFloat a)) |> Transformation.scale (tx * ty) (sx * sy))
                    (oldWay tx ty sx sy (degrees (toFloat a)) |> Mat3.mul (Mat3.makeScale (tx * ty) (sx * sy)))
        ]


epsilon : Float
epsilon =
    2 ^ -52


validate transformation mat3 =
    let
        ( uT1, uP1 ) =
            Transformation.toGL transformation
                |> Tuple.mapBoth Vec4.toRecord Vec2.toRecord

        ( uT2, uP2 ) =
            Mat3.toGL mat3
                |> Tuple.mapBoth Vec4.toRecord Vec2.toRecord
    in
    Expect.all
        [ .uP1x >> Expect.within (Absolute epsilon) uP2.x
        , .uP1y >> Expect.within (Absolute epsilon) uP2.y
        , .uT1x >> Expect.within (Absolute epsilon) uT2.x
        , .uT1y >> Expect.within (Absolute epsilon) uT2.y
        , .uT1z >> Expect.within (Absolute epsilon) uT2.z
        , .uT1w >> Expect.within (Absolute epsilon) uT2.w
        ]
        { uP1x = uP1.x
        , uP1y = uP1.y
        , uT1x = uT1.x
        , uT1y = uT1.y
        , uT1z = uT1.z
        , uT1w = uT1.w
        }


oldWay tx ty sx sy a =
    let
        t =
            Mat3.makeTranslate tx ty

        r =
            Mat3.makeRotate a

        s_ =
            Mat3.makeScale sx sy
    in
    Mat3.mul t (Mat3.mul r s_)


fuzz5 : Fuzz.Fuzzer a -> Fuzz.Fuzzer b -> Fuzz.Fuzzer c -> Fuzz.Fuzzer d -> Fuzz.Fuzzer e -> String -> (a -> b -> c -> d -> e -> Expectation) -> Test
fuzz5 f1 f2 f3 f4 f5 s fn =
    fuzz (Fuzz.map5 fn f1 f2 f3 f4 f5) s identity

module WebGL.Shape2d exposing
    ( toEntities
    , Shape2d(..), Form(..)
    , Render, Opacity, ScaleRotateSkew, Translate
    )

{-|


# Render

@docs toEntities


# Shape

@docs Shape2d, Form


# Types

@docs Render, Opacity, ScaleRotateSkew, Translate

-}

import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Math.Vector4 exposing (Vec4)
import Set exposing (Set)
import WebGL exposing (Entity)
import WebGL.Shape2d.Transformation as Trans exposing (Transformation)
import WebGL.Texture exposing (Texture)


{-| Converts shapes to WebGL entities and `Set` of missing texture filenames

    import WebGL
    import WebGL.Shape2d

    main =
        WebGL.Shape2d.toEntities Dict.empty 100 100 [ rectangle red 30 30 ]
            |> Tuple.first
            |> Webgl.toHtml [ width 100, height 100 ]

-}
toEntities : Dict String Texture -> { b | width : Float, height : Float } -> List Shape2d -> ( List Entity, Set.Set String )
toEntities textures screen shapes =
    List.foldr (renderShape screen textures Trans.identity 1) ( [], Set.empty ) shapes


{-| Main building block

    rectangle : Vec3 -> Float -> Float -> Shape2d
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

-}
type Shape2d
    = Shape2d
        { x : Float
        , y : Float
        , a : Float
        , sx : Float
        , sy : Float
        , o : Float
        , form : Form
        }


{-| -}
type Form
    = Form Float Float Render
    | Textured String (Texture -> Shape2d)
    | Group (List Shape2d)


{-| Render is part of `Shape2d` that converts `Shape2d` into `Entity`

    rect : Vec3 -> Render
    rect color uP uT opacity =
        WebGL.entity
            Shader.vertNone
            Shader.fragFill
            Shader.mesh
            { color = setAlpha color opacity
            , uP = uP
            , uT = uT
            }

-}
type alias Render =
    Translate
    -> ScaleRotateSkew
    -> Opacity
    -> WebGL.Entity


{-| Vec2 representing part of transform matrix

    | 1 0 x |
    | 0 1 y |
    | 0 0 1 |

-}
type alias Translate =
    Vec2


{-| Vec4 representing part of transform matrix

    | x y 0 |
    | z w 0 |
    | 0 0 1 |

-}
type alias ScaleRotateSkew =
    Vec4


{-| -}
type alias Opacity =
    Float



--- INTERNAL STUFF


renderShape : { a | width : Float, height : Float } -> Dict String Texture -> Transformation -> Float -> Shape2d -> ( List Entity, Set String ) -> ( List Entity, Set String )
renderShape screen textures parent parentOpacity (Shape2d { x, y, a, sx, sy, o, form }) (( entities, missing ) as acc) =
    let
        createTrans tx ty sx_ sy_ a_ =
            Trans.transform tx ty sx_ sy_ a_
                |> Trans.apply parent

        opacity =
            o * parentOpacity
    in
    case form of
        Form width height fn ->
            let
                ( t1, t2 ) =
                    createTrans (x * 2) (y * 2) (width * sx) (height * sy) a
                        |> Trans.scale (1 / screen.width) (1 / screen.height)
                        |> Trans.toGL
            in
            ( fn t2 t1 opacity :: entities, missing )

        Textured src fn ->
            case Dict.get src textures of
                Just texture ->
                    renderShape screen textures (createTrans (x * 2) (y * 2) sx sy a) opacity (fn texture) acc

                Nothing ->
                    if Set.member src missing then
                        acc

                    else
                        ( entities, Set.insert src missing )

        Group shapes ->
            shapes
                |> List.foldr (renderShape screen textures (createTrans (x * 2) (y * 2) sx sy a) opacity) acc

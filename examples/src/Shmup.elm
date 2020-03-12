module Shmup exposing (Memory, init, main, update, view)

import Playground exposing (..)
import Random


main : Program () (Playground Memory) Msg
main =
    game view update Init


init : Memory
init =
    Init


view : Computer -> Memory -> List Shape
view computer phase =
    case phase of
        Play state ->
            [ state.score
                |> String.fromInt
                |> words white
                |> moveY (computer.screen.top - 25)
                |> scale 3
            , ship
                |> move state.player.x state.player.y
                |> applyIf (state.invulnerability > 0)
                    (fade (zigzag 0 1 0.25 computer.time)
                        >> moveY (0.5 * toFloat state.invulnerability)
                    )
                |> applyIf (state.invulnerability > 60) (fade 0)
            ]
                |> (++)
                    (List.indexedMap
                        (\i icon -> icon |> move (computer.screen.right - 35 * toFloat i - 25) (computer.screen.top - 25))
                        (List.repeat state.lifeLeft lifeIcon)
                    )
                |> andFold state.explosions (\{ current, x, y } -> (::) (current |> move x y |> scale 2))
                |> andFold state.mobs (\mob -> (::) (meteor_big |> rotate mob.a |> scale (mob.r * 2 / 96) |> move mob.x mob.y))
                |> andFold state.bullets (\bullet -> (::) (bullet.shape computer.time |> move bullet.x bullet.y))
                |> andFold [ "" ] (\_ -> fillBackground computer)
                |> (\v ->
                        if state.shake > 0 then
                            v
                                |> group
                                |> move (sin (zigzag 0 3 0.125 computer.time) * state.shake) (cos (zigzag 0 5 0.125 computer.time) * state.shake)
                                |> List.singleton

                        else
                            v
                   )

        Intro state ->
            [ group
                [ [ words black "SHMUP" |> move 1 -1, words white "SHMUP" ]
                    |> group
                    |> scale 3
                    |> moveY (wave 15 25 3 computer.time)
                , words white "> Press SPACE <"
                    |> moveY -20
                , "Highscore "
                    ++ String.fromInt state.score
                    |> words white
                    |> moveY -40
                ]
                |> scale 3
            ]
                |> andFold state.mobs (\mob -> (::) (meteor_big |> rotate mob.a |> scale (mob.r * 2 / 96) |> move mob.x mob.y))
                |> andFold state.explosions (\{ current, x, y } -> (::) (current |> move x y |> scale 2))
                |> andFold [ "" ] (\_ -> fillBackground computer)

        Init ->
            []


update : Computer -> Memory -> Memory
update computer phase =
    case phase of
        Play state ->
            if state.shake > 12 && state.shake < 15 then
                Play { state | shake = state.shake - 1 }

            else
                state
                    |> moveShip computer
                    |> updateMob computer
                    |> updateExplosions computer
                    |> updateBullet computer
                    |> updateShoot computer
                    |> collideBulletEnemy computer
                    |> (\s ->
                            if s.shake > 0 then
                                { s | shake = s.shake - 1 }

                            else
                                s
                       )
                    |> (\s ->
                            if s.invulnerability <= 0 then
                                collidePlayerEnemy computer s

                            else
                                Play { s | invulnerability = s.invulnerability - 1 }
                       )

        Intro state ->
            if state.timeout > 0 then
                { state | timeout = state.timeout - 1 }
                    |> updateMob computer
                    |> updateExplosions computer
                    |> Intro

            else if computer.keyboard.space then
                startWave computer { bottomShoot | seed = state.seed }
                    |> Play

            else
                state
                    |> updateMob computer
                    |> Intro

        Init ->
            let
                ( mobs, seed ) =
                    breedMobs computer bottomShoot.seed
            in
            Intro
                { mobs = mobs
                , seed = seed
                , score = 0
                , timeout = 0
                , explosions = []
                }


startWave computer data =
    let
        player =
            bottomShoot.player

        ( mobs, seed ) =
            breedMobs computer data.seed
    in
    { data
        | mobs = mobs
        , seed = seed
        , score = 0
        , player = { player | y = computer.screen.bottom + player.r + 20 }
    }


breedMobs computer seed =
    Random.step (Random.list 8 (generatorMob computer)) seed


updateExplosions computer state =
    { state
        | explosions =
            List.foldr
                (\e acc ->
                    case e.next of
                        shape :: next ->
                            { e | current = shape, next = next } :: acc

                        [] ->
                            acc
                )
                []
                state.explosions
    }


updateShoot computer state =
    if computer.keyboard.space then
        List.foldl
            (\weapon ( acc, model ) ->
                if computer.keyboard.space && weapon.counter <= 0 then
                    ( { weapon | counter = weapon.interval } :: acc, shoot weapon model )

                else if weapon.counter > 0 then
                    ( { weapon | counter = weapon.counter - 1 } :: acc, model )

                else
                    ( acc, model )
            )
            ( [], state )
            state.weapon
            |> (\( a, b ) -> { b | weapon = a })

    else
        state


collidePlayerEnemy computer state =
    let
        isColliding_ a b acc =
            if distanceSquared a b < (a.r * a.r + b.r * b.r) then
                ( Just a
                , Just b
                , { acc
                    | explosions = { explosion | x = a.x, y = a.y } :: acc.explosions
                    , invulnerability = 80
                    , lifeLeft = acc.lifeLeft - 1
                  }
                )

            else
                ( Just a, Just b, acc )

        ( _, _, newState ) =
            foldFilter isColliding_ state [ state.player ] state.mobs
    in
    if newState.lifeLeft > 0 then
        Play newState

    else
        Intro
            { mobs = newState.mobs
            , seed = newState.seed
            , score = newState.score
            , timeout = 100
            , explosions = newState.explosions
            }


collideBulletEnemy computer state =
    let
        isColliding_ a b acc =
            if distanceSquared a b < (a.r * a.r + b.r * b.r) then
                let
                    ( nMob, nSeed ) =
                        Random.step (generatorMob computer) acc.seed
                in
                ( Nothing
                , Just nMob
                , { acc
                    | score = acc.score + round b.r
                    , explosions = { explosion | x = b.x, y = b.y } :: acc.explosions
                    , seed = nSeed
                    , shake =
                        if acc.shake < 3 then
                            15

                        else
                            acc.shake
                  }
                )

            else
                ( Just a, Just b, acc )

        ( bullets, mobs, newState ) =
            foldFilter isColliding_ state state.bullets state.mobs

        ( mobs_, seed_ ) =
            if newState.score // 500 > state.score // 500 then
                let
                    ( nMob, nSeed ) =
                        Random.step (generatorMob computer) newState.seed
                in
                ( nMob :: mobs, nSeed )

            else
                ( mobs, newState.seed )
    in
    { newState | bullets = bullets, mobs = mobs_, seed = seed_ }


generatorMob : Computer -> Random.Generator Object
generatorMob computer =
    Random.map Object (normal 0 computer.screen.right)
        |> randomAndMap (Random.constant computer.screen.top)
        |> randomAndMap (normal 40 10)
        |> randomAndMap (Random.constant 0)
        |> randomAndMap (normal 0 3)
        |> randomAndMap (normal -6 1.5)


shoot weapon state =
    let
        bullet =
            { x = state.player.x + weapon.bullet.x
            , y = state.player.y + weapon.bullet.y
            , r = weapon.bullet.r
            , a = weapon.bullet.a
            , speedX = weapon.bullet.speedX
            , speedY = weapon.bullet.speedY
            , shape = weapon.bullet.shape
            }
    in
    { state | bullets = bullet :: state.bullets }


moveShip computer state =
    let
        player =
            state.player
    in
    { state
        | player =
            { player
                | x =
                    player.x
                        + toX computer.keyboard
                        * player.speedX
                        |> clamp computer.screen.left computer.screen.right
                        |> clamp computer.screen.left computer.screen.right
            }
    }


updateMob computer state =
    state.mobs
        |> List.foldr
            (\mob ( mobs, seed ) ->
                let
                    y =
                        mob.y + mob.speedY

                    x =
                        mob.x + mob.speedX
                in
                if y < computer.screen.bottom || x < computer.screen.left || x > computer.screen.right then
                    let
                        ( nMob, nSeed ) =
                            Random.step (generatorMob computer) seed
                    in
                    ( nMob :: mobs, nSeed )

                else
                    ( { mob | y = y, x = x, a = spin (3 / -mob.speedX) computer.time } :: mobs, seed )
            )
            ( [], state.seed )
        |> (\( mobs, seed ) -> { state | mobs = mobs, seed = seed })


updateBullet computer state =
    state.bullets
        |> List.foldl
            (\mob bullets ->
                let
                    y =
                        mob.y + mob.speedY

                    x =
                        mob.x + mob.speedX
                in
                if y > computer.screen.top then
                    bullets

                else
                    { mob | y = y, x = x } :: bullets
            )
            []
        |> (\bullets -> { state | bullets = bullets })


type alias Object =
    { x : Float
    , y : Float
    , r : Float
    , a : Float
    , speedX : Float
    , speedY : Float
    }


type Memory
    = Init
    | Intro
        { mobs : List Object
        , seed : Random.Seed
        , score : Int
        , timeout : Int
        , explosions : List Animation
        }
    | Play
        { player : Object
        , invulnerability : Int
        , score : Int
        , lifeLeft : Int
        , seed : Random.Seed
        , mobs : List Object
        , bullets : List Bullet
        , explosions : List Animation
        , weapon :
            List
                { counter : Int
                , interval : Int
                , bullet : Bullet
                }
        , shake : Float
        }


bottomShoot =
    { player =
        { x = 0
        , y = 0
        , r = 30
        , a = 0
        , speedX = 20
        , speedY = 0
        }
    , invulnerability = 60
    , lifeLeft = 3
    , score = 0
    , seed = Random.initialSeed 42
    , mobs = []
    , bullets = []
    , explosions = []
    , weapon =
        [ { counter = 5
          , interval = 10
          , bullet =
                { x = -30
                , y = 0
                , r = 10
                , a = 0
                , speedX = 0
                , speedY = 12
                , shape = \_ -> laserBlue
                }
          }
        , { counter = 0
          , interval = 10
          , bullet =
                { x = 30
                , y = 0
                , r = 10
                , a = 0
                , speedX = 0
                , speedY = 12
                , shape = \_ -> laserGreen
                }
          }
        ]
    , shake = 0
    }



--addWeapon w=


type alias Bullet =
    { x : Float
    , y : Float
    , r : Float
    , a : Float
    , speedX : Float
    , speedY : Float
    , shape : Time -> Shape
    }


andFold l fn acc =
    List.foldr fn acc l


foldFilter fn acc l1 l2 =
    foldFilter1 fn ( [], l2, acc ) l1


foldFilter1 fn ( acc1, acc2, acc3 ) l =
    case l of
        [] ->
            ( acc1, acc2, acc3 )

        x :: rest ->
            case foldFilter2 fn ( x, [], acc3 ) acc2 of
                ( Just a, ll2, acc3_ ) ->
                    foldFilter1 fn ( a :: acc1, ll2, acc3_ ) rest

                ( Nothing, ll2, acc3_ ) ->
                    foldFilter1 fn ( acc1, ll2, acc3_ ) rest


foldFilter2 fn ( item, acc, acc3 ) l =
    case l of
        [] ->
            ( Just item, List.reverse acc, acc3 )

        x :: rest ->
            case fn item x acc3 of
                ( Just a, Just b, acc3_ ) ->
                    foldFilter2 fn ( a, b :: acc, acc3_ ) rest

                ( Just a, Nothing, acc3_ ) ->
                    foldFilter2 fn ( a, acc, acc3_ ) rest

                ( Nothing, Just b, acc3_ ) ->
                    ( Nothing, List.reverse (b :: acc) ++ rest, acc3_ )

                ( Nothing, Nothing, acc3_ ) ->
                    ( Nothing, List.reverse acc ++ rest, acc3_ )


fillBackground { screen } acc =
    List.range (floor (screen.bottom / 256)) (ceiling (screen.top / 256))
        |> List.foldl
            (\b acc2 ->
                List.range (floor (screen.left / 256)) (ceiling (screen.right / 256))
                    |> List.foldl (\a acc_ -> (background |> move (toFloat a * 256) (toFloat b * 256)) :: acc_) acc2
            )
            acc


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world



--VEC2 Math


distanceSquared a b =
    lengthSquared (sub a b)


lengthSquared { x, y } =
    x * x + y * y


sub a b =
    { x = a.x - b.x, y = a.y - b.y }


ship =
    image 98 75 "data:image/png;base64,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"


meteor_big =
    image 96 96 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGIAAABgCAYAAADmbacFAAAHVElEQVR42u1dXW4cRRD2DTgCUt4QDyB7DSL8mF0nRCYmDg6RZScKgRAIgWCQEBJCIkiAIEqEURIF8eQj+AgcgSPkCByh2W/jWrdGPdvTVdXT3TOzUj048e6Oq3q+r76q6p6lpZ6/dsera3uT5bvbp194fml4tffaeePFl/bWV/anzj+6emZkbNt9++UPBw9FemGlw8Fw/JXJ8n9V518/94q5sfHq/GcEavCaruMPp/a06vhrZ1fN7Quvm28vj82PV98xP1/fmBn+Df+PYA3BYLzOrJx6bmc82tqdjA72JqN/XY6/tXnafHNpzfywd3bu+Kr9dO3c/M5AMAbOCCBYl+NhTRzvCwY+G0EevO0g2CvrK/+4HH/z/Gtmf/st8/3uepDj64KBu2gIRiWzcRGspuNdhjvpJBjLRwPBHhvg4s7FN813O5PZio3hfE8wDntAsHk43mXIrOiawEmdIdi6zAa5PKWUKR3vC0aRgo8yGxfB2o63c/lcDRlYMcFYRLB2Ll+C412WreCzCXaR40Nz+SEYIbAzDYArpeya411mq+/kGoMynZi5fK6WjfrGLUllhD4FILtggJBxAYChvgYiC8FHOqCU7Oe3G+fN/U/fM3/evmge39k2f399efZz0cFApkTKN0en//LRhvn9k03zx60t8+jLbfPX/gczx7sMv6PxneDIufqeokWrsITyQ66rPcTwviLVN/V5U6aoWPW+1Z46GKijRS3UUXki5V0ASNEIQDUYCHARgo9EHL5M27nA9QefXZg5BKsdkFP3u1p3QtUAbUUEA1kBvgClaQ1cP/h8qxbX4WxXMPC+GEHQDgY01nEm9VRdY1A9ibvisdJDnIJg4H325zz56lLUQGgFI5rgA/lI1DQgh+sYyvkRlNhB8N2R3GCgFaDDD5PRAT4QmQHnoqS4jmCE3lE5BAOiV1XwUZGP0z27d3OzVQfmFgw19U1FPq6alsBSLsHAYtIKBlvwESxxi3yx0s22TVqfEqtvKvJx1HTJsBQ9GOPVteAiH1dNlw5LLoPwlAQDzbRgwScp8iEX71oQtOpTweqbRmE47dDYKrgrwfAKPiryge05X/Twi/c7HQhpMBoLPkmRr8uwpFkSgeAj8vb2HjhqOldYgtNipNP4XEkWBcFcGwiKFEdN5whLCACtXKhlVH+1ioj4eyVVWmi1hUU+DI11BZbqetS/fvwuq80qTWmxwL0bKKn3wFHTOcKSfTfE6PohmFFgiXoPnJGZHGHJN7EhqQBw+WHePKqb/JAU+XKEJd/dQPDE/XxwjQSWanevUpGPo6ZzhKWm80tc4uaUyWm/BcSct/fAKfLlCEvVdqvmteNuk0yUe2GJo6ZzzZaaNHdw7ZxAcJS1LeJqYYmKfBw1nXNtyTWIYA82cGGJ0zBqCEvPeg+ckRlpLt52PwF3AYhW8nmc0gbBUm1ziHoPnJEZSdaRIhiSu0Cipm1Yqq24UpGPMzIDZdmXIp9ETZ/A0oLTDCRFvhJgSds4atoLS9R74KjpkmApZbW1ESxRkY+jpvsISxw13RSW2EW+PsISR017Ycku8nHUdNujkDloEi4swc/eAWPuyAw4oitDZLHUtAVLh95JPsm+uK5PbEjVNBa5d0sXFfmkG1D6MLXBUdOAey8s2b1p6QZFXGDXIYqjpoE0jSbBCZrA6tJDq7o266qhphvBUrXYp3G+RpchKlRNEywt7EtXlTWlsBiSHSBKR00TLNWOyyxqCmkQd5v73XJW03NYCt3mSxVYdOik5C2t9ZeupoNhqa7cISVvary0sSU3RzXNgqWY5B1jxLEENc2GpVjk3YUiYaiaFsNSLPIufVw/VE3PRZzW+U2a5J1K8Emrwxw1TbCk+gwKLfJOkUkhCFjNEjgMVdNAD++4TGryjnHWUpPxGYnIDFXT871xMY6V0yTvtsgaGVo1c2tDTdMJA9EejaNF3m01klyQEto3CVXTUWEpBnnHHjhYNIofwlOhappgqa0DFlXIO2aV1ifAmmRSHDVNsNTaycga5B2zSusj2CaZVKiaPoGlFp9JZJO3pNcdQ1c0zfsBO8jiYDQP23QvRXJYqpI3BYN7slkMbSFxpsRahyVt8gZMhBYDASvAecAHVjTIX7qiJZYElup63pD1XPK2c3wEBU4GxBB0kJM1jgKN+aSV5M8folNsOBvku2BABO+4TFvkTfNRuRzgngCW8nhooBZ5l2a0cT3qweypy+YlPOKm0RRfqeRdilnnaeT5LNO+kHeWsNQ38iZYUulLD+QthyXRuMxA3vy7ACd+wgC74nGZgbzdIzBwMHQBupAwOBtGwwBVyx6WciJvexWTg1GSwLXQBsMQw98CQ6YUrR1aGnmTg4HXcDA+m1YxVUObGq7PcvJd2Owp9OPVtWLgR5u8sQuzChNIFaWr+NkT5qdOXl/Znz1xPuSg9C69bPImB9urONTB6BQer+AjWsXk4OJgIxV5L1zB0zunChMIYq9XcRSYmmIvOdhexckf1J3w9T9ls1Vv3J0UrQAAAC10RVh0U29mdHdhcmUAYnkuYmxvb2RkeS5jcnlwdG8uaW1hZ2UuUE5HMjRFbmNvZGVyqAZ/7gAAAABJRU5ErkJggg=="


laserBlue =
    image 13 37 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAAlCAYAAACZFGMnAAACQUlEQVR42oXVv2sTYRjA8Rf8B5wEBycHEYltmpyKCE5OnRx0kE4u4iAOijo5OSgqOiioFCyiIlpsxTZ3F5SCDhZSzFCwllqitNIfscnd+16SXt67PL5v4bGP0YcGPoG7+76548lxJwT55Dx1Pu+rkbwfTmxSI05BHRX/+zhFWXKKkebkPHn7rwXm1x46nmxuyQ0HNhb0euGpfFEGaHA+bk3VtEZ2mx7vcWtHRM6Ppsy1r1pnS1Fdpx3odqncDLBx/GhIHPCiRTS5qtdbSQe62f1/Ol+9EuargpTuAId2Iu8Fs6ged4BDOzu5aVRd7wCHduZM8jNaaqXAoZ1wfDWJFhopcGhnLi/4gCpRChza2TO9R3MqBQ7tzJ8rXTQTpsChnRmEeoOmgwQ4tLMjH0blegIc2pnbSD1HpbUEOLQT9gZEn34lwKGdGYR6hD5WE+DQzi66jyZWNHBoZwYh76B3yxo4tDMjj24gb0kDh3Z25NfQ2E8NHNoJx1NX0eiiBg7t7G10BQ0vtIFDO/OAjC6gFz/awKGdHfk59PR7Gzi0E1lXnkFDlTZwaCeyvjyNBufbwKGdcNxoAD34FgOHdqLPUyfRvbkYOLQzz/JGv1OQx62bX5ord2dj6Gb3Y2OJg4XwUM4L+63L5cazW19j6Gb3Y9M3FuTEXq+60zxhjqGL5caT6zMxILtNj9t+43WTMavt224r+97Wezbfaidebtvvyj2OLw+zRtd22e6fV+ju18s7MuO1THY8yCO73fu4sp12vwEgZVqjmZ+rQQAAAC10RVh0U29mdHdhcmUAYnkuYmxvb2RkeS5jcnlwdG8uaW1hZ2UuUE5HMjRFbmNvZGVyqAZ/7gAAAABJRU5ErkJggg=="


laserGreen =
    image 13 37 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAAlCAYAAACZFGMnAAACR0lEQVR42oXVz2sTQRTA8QH/AU+CB08eRCS2dncTEcGTp5486EF68qCmJrsBoXry5KVgEcEeWgwUURFarUVRA/VHms1m1WjBKm0qJdW09kdo0xhNWlN5zlNefUYfDXwOs/sdMptMMkqxl+1ZjuObg0469GyDHkc965D63yvmWa9imWBdYqfNrr8mOJ7Vo29UNxNJW22/l5Qyjsd8a4UkCvHaZPl1neCY37dTTQeV4wezTsZaRFfftZfWf9ShUTzXsUJNLGP1Kf22MyS35K+urdegEV6nxvaC/cpJB/OkWq+AhHf4MedI5XsJJLxTep1jpLxWBAnvcNIbsrw6BxLe4SSfFGsFkPBO7wRzhMxX8yDhHe6GJ2T22weQ8A6X94gUvo6DhHf6ewoNkenKGEh4h8sbIFNfRkHCO9x7t8hkOQsS3incgGRixQcJ7/TyQr3kfckFCe9wUjd5u/wcJLxTdsa8TEaXhkHCO5zUSbLFxyDhHU66SF4uPgAJ7/CZLpDMwj2Q8E7ZvnGeuPMDIOGd3nvGWZKcuw0S3ulnMqLk6ecbIOGdinrmKTI82wcS3ulJoRMkMXMNJLzD31MbeVjoAQnv9DO1HCP3P3WDhHcq+qK5Vf+XHUF3pi4tDH28Ao3wOjVIhd3Q/mjaaEXxiXM37053QSO8Tk041WKokyO7t59xzcOkd7zjen++EwiO+X3sfx034VTAwNNuM5HUnqY/p9pRtSXs7t2l13tA0p60dmD3zxF6OrFzWyQZCETcfeYGPY4NNm/l3U9zlssVW/PVxwAAAC10RVh0U29mdHdhcmUAYnkuYmxvb2RkeS5jcnlwdG8uaW1hZ2UuUE5HMjRFbmNvZGVyqAZ/7gAAAABJRU5ErkJggg=="


powerUpBlue =
    image 34 33 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACIAAAAhCAYAAAC803lsAAAB50lEQVR42mNgQAPijRv9RZs29Ys3b9wPwxItW/6Ti5HNEWveXC9Wu1afAR8AKRBr2nieEktJcZxk9Vp5TFeUr+IXa9r0HqZQecaZ/zorH/zXW/sEjM12fyUbw8wAmQcyF2YHyD6M0AEKrgdJyvYd+G+05Q1FFhPCIPNB9kAcs/E8SpSABCXbd/w32f6Bpo6AYZA9IPtA9oo2bowHOwSUMEEC6vOv0MURMAyyDxpF66HRAkmgtI4SbFEESytgh8ASDz0dAcMwuwePQ4C5duiGSMaZ7//xgTPv/tDHIY1XfuB1CMihdHHIrLu/qBIaNHUIKaFBsUM2P/1NldCg2CEgC6kRGjRxyKdf//6XXPhBX4eALMUFnn77C85VdHEIyDJcAOTI6OPf6FfEg3yNLYpICQ2q1jXIhRsoNw1YpQcrU0DR5bR/AGtfWPSQki5o4hBQ4uy98XNg2yP+h7/9P/Dy98A3jEAlKTnpArkBjeIQWJuV0v4LOf0dlC4FsPE6HySgOucCXR0Csg/aeJ4P7+vC+jX0asmD7IH1a0D2Izre0I42qAdG604WyHxYTw9kL5YOOKTvC3Kp1rI7VHcQyDyQubCQwNr3hXXEKR2CIGU0ANyFwAdAcQbqBiKPDlADg0MAmDBR0gQSAAA0paVDNbQPtAAAAC10RVh0U29mdHdhcmUAYnkuYmxvb2RkeS5jcnlwdG8uaW1hZ2UuUE5HMjRFbmNvZGVyqAZ/7gAAAABJRU5ErkJggg=="


powerUpGreen =
    image 34 33 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACIAAAAhCAYAAAC803lsAAAB5klEQVR42mNgQAMh3br+od0G/aE9xvthOLzP+D+5GNmckF6D+sAObX0GfACkILTH6DwllpLiOK8WTXkMR7iUK/GH9hq+hylMWGLyP2Ob6f+sXWZgXHjSnGwMMwNkHshcuGOA9mGEDjAk1oMko2ca/889SJnFhDDIfJA9kJAxOo8SJSDBiInG//OP0tYRMAyyB2QfyN7gHoN4SGgAEyZIIGW1KV0cAcMg+6Chsh4WLeAESusowRZFsLQCdggs8dDTETAMs3vQOASUa4duiEy5lvkfH7j98Sx9HLLsbhNeh4AcSheH7HgymyqhQVOHkBIaFDvk5KstVAkNih0CspAaoUETh3z7/en/3Jul9HUIyFJc4O2PZ+BcRReHgCzDBUCO7L4cQ78iHuRrbFFESmhQta5BLtxAuWnAKj1YmQKKrsozzgPnEFj0kJIuaOIQUOJc/6BvYNsjTecD/l96d2DgG0agkpScdIHcgEZxCKzNSmn/hZz+DkqXIqzbaD5IIGm5CV0dArIPZC/IfnhfF9avoVdLHmQPrF8Dsh+ppwfpaIN6YLTuZIHMR/T0jPdjdsChfV+QS9M3mVLdQSDzQObCQgJr3xfeEadwCIKU0QBwFwIfAI+PALuByKMDVLEcaB4oYaKkCSQAAA2XIKv50YBWAAAALXRFWHRTb2Z0d2FyZQBieS5ibG9vZGR5LmNyeXB0by5pbWFnZS5QTkcyNEVuY29kZXKoBn/uAAAAAElFTkSuQmCC"


background =
    image 256 256 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAQAAAAEACAIAAADTED8xAAAAGXRFWHRTb2Z0d2FyZQBBZG9iZSBJbWFnZVJlYWR5ccllPAAAAyFpVFh0WE1MOmNvbS5hZG9iZS54bXAAAAAAADw/eHBhY2tldCBiZWdpbj0i77u/IiBpZD0iVzVNME1wQ2VoaUh6cmVTek5UY3prYzlkIj8+IDx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iIHg6eG1wdGs9IkFkb2JlIFhNUCBDb3JlIDUuNS1jMDIxIDc5LjE1NDkxMSwgMjAxMy8xMC8yOS0xMTo0NzoxNiAgICAgICAgIj4gPHJkZjpSREYgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj4gPHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9IiIgeG1sbnM6eG1wPSJodHRwOi8vbnMuYWRvYmUuY29tL3hhcC8xLjAvIiB4bWxuczp4bXBNTT0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wL21tLyIgeG1sbnM6c3RSZWY9Imh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC9zVHlwZS9SZXNvdXJjZVJlZiMiIHhtcDpDcmVhdG9yVG9vbD0iQWRvYmUgUGhvdG9zaG9wIENDIChXaW5kb3dzKSIgeG1wTU06SW5zdGFuY2VJRD0ieG1wLmlpZDo2MjE5OUU4OTg4RDAxMUUzOEEyRkNDM0VCNzYzOTA1NCIgeG1wTU06RG9jdW1lbnRJRD0ieG1wLmRpZDo2MjE5OUU4QTg4RDAxMUUzOEEyRkNDM0VCNzYzOTA1NCI+IDx4bXBNTTpEZXJpdmVkRnJvbSBzdFJlZjppbnN0YW5jZUlEPSJ4bXAuaWlkOjYyMTk5RTg3ODhEMDExRTM4QTJGQ0MzRUI3NjM5MDU0IiBzdFJlZjpkb2N1bWVudElEPSJ4bXAuZGlkOjYyMTk5RTg4ODhEMDExRTM4QTJGQ0MzRUI3NjM5MDU0Ii8+IDwvcmRmOkRlc2NyaXB0aW9uPiA8L3JkZjpSREY+IDwveDp4bXBtZXRhPiA8P3hwYWNrZXQgZW5kPSJyIj8+yeMMlwAAB7VJREFUeNrs3f+S0zYC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"


explosion06 =
    image 96 96 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAMAAADVRocKAAAABGdBTUEAALGOfPtRkwAAAwBQTFRFAAAAYy8dd00ir2sm0YAj/5UZ//Qj////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAeNRfPAAAAQB0Uk5T////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////AFP3ByUAAAKGSURBVGiB7ZnbloMgDEWN///PTpeCCclJCEhn5sE8tMolm9wQ2+34smwv4J8D6COmaSGAyBAsEixiKQCMeQQwLloOGB2TCTJPB4qeATLyJMjn/Fg5vh4BxHThPccQDOjHNj0FAmjfBwhUCbDXAUyY4IgFUJWmDQyZBBAAmLt9ZxtH0xQbwLd06b8I1HenAzCN15fQ/yGcmTBcyXVCtd9071WugeNbhfZHY4XQv5OQEYAPO84KYf3nx8O9yORnq/8kxAJcFPLYPfUqWA8GOKqLAk5RAEDeSu+md8rU8D4FoM2hqCViH9EiFzXrLoDS1jkqSYCfcOwYavO/FEcKwCsSTRagJTRcAjg/gEVQde6hwYA7w7OAlAE3QJbQwZ6RLCoRbfXjjasDsCIX3C6f49QuzgK4RqGpssN654wJXNzWjNjNzJwEMdn0CF+/CPgwoKmfuHCknvsqWNxW+oX+YMMmBpA8nwWmb9zfDHKtEMNEWHXKYkCrxii2ADPLs+BAaecATH0DpRZQFJJZhq+hVnVHf+cdLag8cxyYAHRqewLAzh0GlHmdZzKnx/UpD7k59foco12kU/V+juX1a0IQA1LSB5QdfwbQf2kjwoe9MIscgKn6HIDMXMdHKpSlhzgGgQXqpHBrzgLsycdkkTGfjGU6GfUzIcoimC66wcmpIUB8ZAYtktBOV1kEDeg+po+yCaChidM10k+qx69ED0Bqhb01MCGMgVaZ3UcFQJuy5pffrwMOkT4dF9kRaQLuaF6h1PcS9uibPhgRU4d/fQcj0m+ZA4DmGPYA4Dp9GaArXwYkjo/L/8PRuAUACu7+7F+omc1iCICpU9j/BJiTF/ACXsALeAEv4FcAP71+VufWkVIFAAAAAElFTkSuQmCC"


explosion05 =
    image 96 96 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAMAAADVRocKAAAABGdBTUEAALGOfPtRkwAAAwBQTFRFAAAAYy8dd00ir2sm0YAj/5UZ//Qj////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAeNRfPAAAAQB0Uk5T////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////AFP3ByUAAATlSURBVGiB7ZnbcuQqDEVHAv//J3vaIG1tYcBOzknVPISHVLpttHQXdv85f3j9+QX8AuRaPwcQXz8EENFrbQjP9B3gI78UECay3hi4BWittVwQkZksCQu/BRApDaAfgE4ILn9PeAYU8xFrayw1F+p3AZ/912pi1ZeEa7Rf3ZqwB6g6QCGs0z7Gqf5nQCc0+d1bnw/Xx0v+YQTVsk3kXRbBKR9APY5G6K5pvFqU7lghlgBzSg/xpfFhJrRVioVfizttQVgAPhuSyKax+aiY86/6MP1Vlm6aAqTLJ0LpMagZEFFfV/QNID26Tf7hWkNgBIEJlsZTwsyC6+6jAzqjq4wga68DJ3gez+ttArjUKS7fCQxwhxSLSW9WXwGoZQ0BTOH2PxqFuvfw3XlD3AFeqG01+Uj4Vl5FAmApG1a9scCamBGs3xVFsjqtVTdq7X0WOSAyH9WG8vLaOJxQ1IbTA0AAoLykyNqnYpRq1hXqrG9joK2oClqcJSLlTeRVKagMvROmgDDBvYSGHTblYvZgPwFEMAoHAOKC9pT8Zwl1r4U5QBQiumo9wtE9uJJbeDSq/MlFVsrIcKR7PaI/sXx0Ds/XNaBfCpHRzGAAGqDnMRWbJ9vWgksZ35g0dPmHx6GmTsidewcw8dU7nOJEQQY0ADWqq+KW/SK7qHUDqGnKe11UauA17CkxP+XBgpYKQygFZxbvfmaENfPDu8kLC1h+NNEou6Z8l2l9dg44U89mADxrrogQdP1tRLThHN0cvWPesgMgkYkMUJo/Hv7I0hijq5EAwNUrSX71IBAgWkWpJJ+U3wLYAOjmndK+qIefjfo3B7QgAzJjBjDvplZs8g/nttT0gRCAJmlqgWTAgRB4K+55lBtRDLj1ySsDahgA6zVNBxxJi42kCPCslyYATfqSAqhM6AbQPEC1WLS3AAimhqS9eSd50W47kgbCqg7GSW85UnEsRM/0KaTWwq977ECwB+BYwozqp0IXXA5kKlncL28HTpznoKb7yPzbAfykk8Me9bwCyABASFXUj1ueqcjVtGE70cQSlTYPIe1jhxqQJero0DUgnosTgDkFBNxkzcmnzwagRtDYHZ6mTBoAId8I8yyy1wM6aKw2hAYCTsCm0CvA6af+7JnP39Y9hkKhk0pY0E3bA/wcrv7E5EWN8+1ooBmFA41uhr6ZIP6u4BKFniF0/OGkir7iwVkCzgBgeGl0vUE+xkTN03QHSC+C/MHez9fJN3GTDU/uv8JDjWMwRhouR7y9xynfBNGo5gWArEDC8D+UuEX4cd8DEYCdi05/2zhUsqiWEZD7u3vyEeAFnROSXqp95AsPunEa7QANQkVG8gPgR5SUsBYwHUfO7CEwi46wDvLxXJZq+gEgeJ+sqVyROCE+iBkgryzIyzZCdDaBMHcDlg/iY1h7UGVY5EYP8XIeMEDYE4Pj5wQA1hMtE+6OGCSfE0LIl0cAZUq2x+WTHrH80oMFJ0uauybuovSlaw+A8yYwnr5IxIy8PvySdGy+yXjSw2+RfQzWIrA3uynfcJ6rp8xHApSLN9V792wArsx8f3oVvpO+BMSGqX5biV8EpJ+ysuv7Zd70HnDe9M1yBwMS/R1gDl255ruAYcNDNL/mItuS/nkCzNf3XHQv6v8DwEF+v+lLv4h/3UH/wE/uv4BfwD8A+AsOxlyTeMJMMAAAAABJRU5ErkJggg=="


explosion04 =
    image 96 96 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAMAAADVRocKAAAABGdBTUEAALGOfPtRkwAAAwBQTFRFAAAAYy8dd00ir2sm0YAj/5UZ//Qj////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAeNRfPAAAAQB0Uk5T////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////AFP3ByUAAAU4SURBVGiB7ZnrcuMgDIULCPv9nzhrJB1JYHDY7PTPTpjpJcY9H7qC3Z/XL4+fL+AL+AI2AIVHzr8EuLSJiBHjVM7C/hdAEf1KE6Xs7I8BrMCjMmMyV6/xiHgGqAIDqPPSpX9du+aOgxEfAS79epxnkyBxRha/8w9ibpv/GHCJXn9/Adoi22opm9slNuA/EB4AZoADqK27ckBI/HPI9OcAFbA48LVG4J88KYAVYQ3IvEgQNJtK88r1mRQkcDZpUYlLgMSxVl0iiYsUWTW9eAh+RVgAtEpNRbzlvI5gSfYXAA4iAywC8ov7hKzQxF8NsQsomiSkAChVu9rUCIHR265rmwDNcpeFR8g11YHaRjgycxMmgFyC6QJwC9Q1Fh4Alj66Azg9tUCRO0ieWlHG1boELG3fJpk0BxzaIGSt1hOMoPq8jNBx9wClhAI2bxhADGrZyoDLCN0yuKXfN74ZoNTTG0RbMXqOAwx5nrX6tjHZW+cAiLXk44+nAqTY2PmHUa23ck8a3HQDtBLWetI+RsEA/hLfh6viRsbWkTADdF2gdj21Wo8OFnDZYWvYAEjLP+4ANeu0T6HV+iIGwgyAPJHM8yJQI4I93sqrJ8EzIAvA6tbCF0bQR1rJIrR1dwX9c9N3yQA46jiOCDjC/COg6fvfS16Dd/hEN7C5+cc14NJ3+5t+bqN0LjpGAo1JV5aAnGOSg8BVcYpskKIRgAa8BrC+HoSsgDLXWbuIYLo+6QmmrcE3n7WLmn/Ow/QP7FuVW86h+0KpkFbR1n68G/GVeZqKg0KZmh/i0YXbWQkAPkiCUB4A6uzYdJA2wWMix/ciYzhS7DxteotW0QyI/SUmZGgCevyVIof7YVHRp6E5gOox9IRhIGnd1W6JRv6ub4DLyqrlPuY5flFf4yDDyB5Q2cTXG8CtkKTQZMHWlSFowYaRr9czYOIZhE76CJIAc6JfNwFz1xeLbvWurEu3RzWClUtAmetrQ9Km1AcJR7yMsLdvixikoae5AsWm1+WYTHpq6YF2nkUpzUzAmU0ioCtwQAUAp325e1oHKaWJCZZ/xPvCkGGh+8mT5zMgp9veGLtnuc3yVmZJ26oarShNAGLCsHW8A9j5FJ1EHnKXgD7ORNGECOv1+QBpBM7oBeCa0Lox/xCsIKIbguLuZIXX9KeAVwOkeKbodxHyeoUBLm6J1WKdVoA2dOuoeA4Iz2GlN6GdVbrVi5fyYEDcMgVgVVPsiCTOuh3Bgj72D7p5qDtVgIBXWdlrpyWf2UbUEaLbWgp1+v25yGxoyqVYddrWjufPzogu80p5ArzUAshne6DFOyJ5zQILahfig482ownD2ZQJCc/AWLQeJQDoTUBzkrMNlfQIkFzSp2BuZMVecRV30b3suNh2AGKEPYjbZmCZq0HQE9iYUW9d1EbWY468ztSQWwg9KhOAHGffWSAA0neZfgxSi8xXZQTIEnYAxfqQqNu7F62GlmX5ZoLp7wHQHxRA2B8FIJa5fCyDHQAULXPQT4t5KA8m4Ci/AUjZA/kMiK/c9P6c8lMlCyCZiikiNQsI2lHiboHN4B2AN09tRt6CzGf2ctkIDr/vBitAbBE5djlI5zRMrPSn7+ySIbK0V9jTvony0NhtLWOI1wBRwG9wGevLHAg5yE/05681AUgR0C0zADD03lFr8WI22ZDuV8Z1+u7nn2f663fXfr/FoUhsg5zr64+7zs7/0XyxnV2Bs1z/NiB1+sHEbsz+eOs/gQuNDfndfzUuRd6obwNUafPejwAfjy/gC/gC/gfAH9oEDV7SNcjaAAAAAElFTkSuQmCC"


explosion03 =
    image 96 96 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGAAAABgCAMAAADVRocKAAAABGdBTUEAALGOfPtRkwAAAwBQTFRFAAAAYy8dd00ir2sm0YAj/5UZ//Qj////AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAeNRfPAAAAQB0Uk5T////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////AFP3ByUAAALlSURBVGiB7ZjbjqshCIXnQuj7P/HeLSoCgmLbSeZCM5Me8nd9sMTjz79fbj8XcAEXcAEXcAF/EFCo/RqgcPs+ALgdIraApxbgqwFoyHcAL/mq/yK8/pARXwCQPDyo1RwIhA3xMYDceXBDzuQkiSWgRz8Qz38YXZIhLAAq/EaoeVRYLokFAHAGdHn6kCLEAEd/xJ/PIQJQ8c8AckmmsCUEgFICfQXIpOADSpiAAeCW4AKe+lpKipoMdoQA4AY/sd4F1ATSgDXBAZQNoA80njTOAXUI+OPgMSYlBNzPezOAFwA3+mEP6e/npBCwcIc7YABiQgRY2U/vyJtEHXkAiADNOKxjTC0NeUBdJJcJ4GgAu044Aqg+lpW68MgFaIfEJxQ5GEBESAHQvJUe9Sn1BKAHQZd6mCq1gIDgAqaZVEBag2mHcQiYCLozyKyqT1/GhKAPxt5Bl48isHxD5AGAoy0AKEckQhLQ9ougCM7InnoFIQ2ow7NXuiii7wFqBr0rrV/dM1vOLsFbcFr4Kn5UA25koQAewQcgC4jgZcByxuBns4BReyaNqarMwDgHTGkgr3XYHRKzE3gED2DDFPpc+XNiNO6OAP1FjgoH8HgHIIqwhs2SqsAYzJuwNED7whlJ0SopknkD0GYc7QqIQxoou9IAUIAxoykA7VnQAuYJz5kqQHsEYxtGbhMQ275lfH0A0ARoglxXYMniUWdNCCY71ZGCp/VUpujvj7YZ8DBwAuZ3/bkkoPQfYDO9b0O72aq3xT2Mty4HK5r4eT3qcaAyAVFP4f4uOOEo69kk6xyM5w4BNQfT0Zootu/cw96y7x8CqZBmPes5CHi0MwoAxdUTwYJp4eYxOCcLQjsCjNu0qjbre0qLu4qhwEmIY2uRjOUtZHjb0jpCGaTkVYvkF/dFsw2xfiy/upAqHuJUfn1nVwxjSGaU9wB5V52P+AjgEs70t3fXH8qnbt8/UM8BPmsXcAEXcAF/AfAfIBbtTUiE0+IAAAAASUVORK5CYII="


lifeIcon =
    image 32 26 "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAaCAYAAADWm14/AAACcElEQVR42r2Wz2sTQRTH8x/4J/gPCN4EQRAvHrz0UPQiouilB6UHtRehRVRQECyUFgRpQGpBEeupFyFSJD2Ixouw1aRbYyWiSIxijCE/xv088obtpiY7a5KBLwwz732/b96+N7OpVMLRbrdXAnwHrVbreb1e358a1QgEZ0xkEMRIxMvl8h5OjWihUBDoaDQah0d2+kqlYjzPEzDvZCE9stMXi0UbgO/7Ngu1Wm3v0AJoNptvEKlWqyI8PXVDwJw1BjbDSn0agT++Z/JPl0z2xbo5dui4gPlWNmOav34M51MEhJNyukBg+9qEnPju3KINgDlrX5Zmw10xOai0n1bS0vQZ8+7BgohdOHvZBsCcNfa+Ld60QeD7X+JcLlp0X+eumM2TB0Qo9zpnxRWssYfNz8yKBIBv4guKalZxCP3xfeb9/FURuX9vuSsA1tjDBtvfb1/aIJw7g3bTiocIQjl97pWIPFp+YmZvLewAa9KWgQ22H04dlILVzoDT6Z7XioeIAPLXz9u+7wds8fk4cdR2BpxO7Ybjp4vjQgS89bXYAWCrfnDEbs9oxSuJy+mjWQAUcN/OCDbG1AgHdQYbmVXnAPAJc0Tac+yf7aYVryhcOuEsrsA3zLVre3Yqfitc8TtOv/o4cQD4Rvm0PdGUztCiC3rVVrxi89yRxOIKOMKcaKBli1JTz5MaTZlcPrenEovrpRT9pPp8o53SwuB9/5x9ZsoP57uwnd9wFsdnNy400NJhM9BvlEql2OLYxhmSAVqCW4qfStCZz4RwxyWIsDi+Ea50WKerHeNcUPz59MLAnuJeQfQbAxcPv5L8eveC6/v/F7hzp+GkEvouAAAALXRFWHRTb2Z0d2FyZQBieS5ibG9vZGR5LmNyeXB0by5pbWFnZS5QTkcyNEVuY29kZXKoBn/uAAAAAElFTkSuQmCC"


type alias Animation =
    { current : Shape
    , next : List Shape
    , x : Float
    , y : Float
    }


explosion : Animation
explosion =
    { x = 0
    , y = 0
    , current = circle white 8
    , next =
        [ circle white 8
        , circle white 16
        , circle white 24
        , explosion03
        , explosion04
        , explosion05
        , explosion06
        ]
            |> List.concatMap (List.repeat 4)
    }



--Random.Extra


{-| Create a generator of floats that is normally distributed with
given mean and standard deviation.
-}
normal : Float -> Float -> Random.Generator Float
normal mean stdDev =
    Random.map (\u -> u * stdDev + mean) standardNormal


{-| A generator that follows a standard normal distribution (as opposed to
a uniform distribution)
-}
standardNormal : Random.Generator Float
standardNormal =
    Random.map2
        (\u theta -> sqrt (-2 * logBase e (1 - max 0 u)) * cos theta)
        (Random.float 0 1)
        (Random.float 0 (2 * pi))


randomAndMap : Random.Generator a -> Random.Generator (a -> b) -> Random.Generator b
randomAndMap =
    Random.map2 (|>)

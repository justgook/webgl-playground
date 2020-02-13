module Extra.Jump.Collision exposing (intersection, lineToMovingPoint, simulate)

import AltMath.Vector2 as Vec2 exposing (vec2)



{- http://twistedoakstudios.com/blog/Post554_minkowski-sums-and-differences -}
{- <https://cp-algorithms.com/geometry/circle-line-intersection.html>
   Line to circle
   CAPSULE!!! - https://stackoverflow.com/questions/43615547/collision-detection-for-2d-capsule-or-swept-sphere
   https://github.com/shakiba/planck.js

   Wolfenstein 3D Textures
   https://lodev.org/cgtutor/raycasting.html
-}


simulate config player static =
    let
        acc =
            player.acc
                |> Vec2.add (Vec2.mul config.friction player.v)
                |> Vec2.add config.gravity

        forceApplied =
            { player
                | v = player.v |> Vec2.add acc
                , contact = zero
            }

        newPlayer =
            --TODO add broad phase
            iterate lineToMovingPoint 5 static forceApplied
    in
    { newPlayer | p = Vec2.add newPlayer.p newPlayer.v }


iterate fn iterations walls acc =
    iterate_ fn iterations 0 walls acc


iterate_ fn iterations attempt walls acc =
    List.foldl fn acc walls
        |> (\({ contact, v, p } as acc2) ->
                let
                    adding =
                        acc2.contact
                            |> applyIf (acc2.contact.x /= 0 && acc2.contact.y /= 0) Vec2.normalize
                            |> Vec2.negate
                            |> Vec2.scale pushOut
                in
                if attempt < iterations && acc /= acc2 then
                    iterate_ fn iterations (attempt + 1) walls { acc2 | v = v |> Vec2.add adding }

                else if attempt >= iterations then
                    -- TODO add some validation to not reach that point at all
                    { acc2 | v = zero }

                else
                    { acc2 | v = v |> Vec2.add adding }
           )


zero =
    Vec2.vec2 0 0


lineToMovingPoint wall player =
    if isLeft wall.p1 wall.p2 player.p then
        intersectionVec2 player.p (Vec2.add player.p player.v) wall.p1 wall.p2
            |> Maybe.map
                (\( t, _ ) ->
                    if t < 1 then
                        let
                            zeroedWall =
                                Vec2.sub wall.p2 wall.p1

                            normal =
                                leftNormal zeroedWall

                            restV =
                                Vec2.scale (1 - t) player.v
                        in
                        { player
                            | v =
                                player.v
                                    |> Vec2.scale t
                                    |> Vec2.add (Vec2.scalarProjection restV zeroedWall)
                            , contact = Vec2.add normal player.contact
                        }

                    else
                        player
                )
            |> Maybe.withDefault player

    else
        player


intersection : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Maybe ( Float, Float )
intersection x1 y1 x2 y2 x3 y3 x4 y4 =
    let
        den =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
    in
    if den == 0 then
        Nothing

    else
        let
            t =
                ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / den

            u =
                -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / den
        in
        if (t >= 0) && (t <= 1) && (u >= 0) && (u <= 1) then
            Just ( t, u )

        else
            Nothing


intersectionVec2 p1 p2 p3 p4 =
    intersection p1.x p1.y p2.x p2.y p3.x p3.y p4.x p4.y


pushOut =
    2 ^ -30


leftNormal vec =
    let
        { x, y } =
            vec
                |> Vec2.normalize
    in
    vec2 -y x


isLeft a b c =
    (b.x - a.x) * (c.y - a.y) < (b.y - a.y) * (c.x - a.x)


applyIf : Bool -> (a -> a) -> a -> a
applyIf bool f world =
    if bool then
        f world

    else
        world



---BROAD PHASE https://gamedev.stackexchange.com/questions/20103/finding-which-tiles-are-intersected-by-a-line-without-looping-through-all-of-th
{-
   void rayCast()
   {
       if (!isComponentComplete())
           return;

       mTiles.clear();
       mTiles.fill(QColor::fromRgb(255, 222, 173), mSizeInTiles.width() * mSizeInTiles.height());

       const QPoint startTile = startTilePos();
       const QPoint endTile = endTilePos();
       // http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html
       int x0 = startTile.x();
       int y0 = startTile.y();
       int x1 = endTile.x();
       int y1 = endTile.y();

       int dx = abs(x1 - x0);
       int dy = abs(y1 - y0);
       int x = x0;
       int y = y0;
       int n = 1 + dx + dy;
       int x_inc = (x1 > x0) ? 1 : -1;
       int y_inc = (y1 > y0) ? 1 : -1;
       int error = dx - dy;
       dx *= 2;
       dy *= 2;

       for (; n > 0; --n)
       {
           visit(x, y);

           if (error > 0)
           {
               x += x_inc;
               error -= dy;
           }
           else if (error < 0)
           {
               y += y_inc;
               error += dx;
           }
           else if (error == 0) {
               // Ensure that perfectly diagonal lines don't take up more tiles than necessary.
               // http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html?showComment=1281448902099#c3785285092830049685
               x += x_inc;
               y += y_inc;
               error -= dy;
               error += dx;
               --n;
           }
       }

       update();
   }
-}

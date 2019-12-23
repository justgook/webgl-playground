module Extra.Jump.Collision exposing (intersection)


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



{- http://twistedoakstudios.com/blog/Post554_minkowski-sums-and-differences -}
{- <https://cp-algorithms.com/geometry/circle-line-intersection.html>
   Line to circle
-}
{- <https://stackoverflow.com/questions/37224912/circle-line-segment-collision>
   The function returns an array of up to two point on the line segment. If no points found returns an empty array.

       function inteceptCircleLineSeg(circle, line){
           var a, b, c, d, u1, u2, ret, retP1, retP2, v1, v2;
           v1 = {};
           v2 = {};
           v1.x = line.p2.x - line.p1.x;
           v1.y = line.p2.y - line.p1.y;
           v2.x = line.p1.x - circle.center.x;
           v2.y = line.p1.y - circle.center.y;
           b = (v1.x * v2.x + v1.y * v2.y);
           c = 2 * (v1.x * v1.x + v1.y * v1.y);
           b *= -2;
           d = Math.sqrt(b * b - 2 * c * (v2.x * v2.x + v2.y * v2.y - circle.radius * circle.radius));
           if(isNaN(d)){ // no intercept
               return [];
           }
           u1 = (b - d) / c;  // these represent the unit distance of point one and two on the line
           u2 = (b + d) / c;
           retP1 = {};   // return points
           retP2 = {}
           ret = []; // return array
           if(u1 <= 1 && u1 >= 0){  // add point if on the line segment
               retP1.x = line.p1.x + v1.x * u1;
               retP1.y = line.p1.y + v1.y * u1;
               ret[0] = retP1;
           }
           if(u2 <= 1 && u2 >= 0){  // second add point if on the line segment
               retP2.x = line.p1.x + v1.x * u2;
               retP2.y = line.p1.y + v1.y * u2;
               ret[ret.length] = retP2;
           }
           return ret;
       }


   # Lift Line

   Move line along its normal

       function liftLine(line,dist){
           var v1,l
           v1 = {};
           v1.x = line.p2.x - line.p1.x; // convert line to vector
           v1.y = line.p2.y - line.p1.y;
           l = Math.sqrt(v1.x * v1.x + v1.y * v1.y); // get length;
           v1.x /= l;  // Assuming you never pass zero length lines
           v1.y /= l;
           v1.x *= dist; // set the length
           v1.y *= dist;
           // move the line along its normal the required distance
           line.p1.x -= v1.y;
           line.p1.y += v1.x;
           line.p2.x -= v1.y;
           line.p2.y += v1.x;

           return line; // if needed
       }

   #Distance circle (or point) to a line segment

   Returns the closest distance to the line segment. It is just the circle center that I am using. So you can replace circle with a point

       function circleDistFromLineSeg(circle,line){
           var v1, v2, v3, u;
           v1 = {};
           v2 = {};
           v3 = {};
           v1.x = line.p2.x - line.p1.x;
           v1.y = line.p2.y - line.p1.y;
           v2.x = circle.center.x - line.p1.x;
           v2.y = circle.center.y - line.p1.y;
           u = (v2.x * v1.x + v2.y * v1.y) / (v1.y * v1.y + v1.x * v1.x); // unit dist of point on line
           if(u >= 0 && u <= 1){
               v3.x = (v1.x * u + line.p1.x) - circle.center.x;
               v3.y = (v1.y * u + line.p1.y) - circle.center.y;
               v3.x *= v3.x;
               v3.y *= v3.y;
               return Math.sqrt(v3.y + v3.x); // return distance from line
           }
           // get distance from end points
           v3.x = circle.center.x - line.p2.x;
           v3.y = circle.center.y - line.p2.y;
           v3.x *= v3.x;  // square vectors
           v3.y *= v3.y;
           v2.x *= v2.x;
           v2.y *= v2.y;
           return Math.min(Math.sqrt(v2.y + v2.x), Math.sqrt(v3.y + v3.x)); // return smaller of two distances as the result
       }

-}

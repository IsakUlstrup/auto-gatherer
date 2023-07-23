module Engine.Point exposing
    ( Point
    , add
    , distance
    , distanceFloat
    , neighbors
    , ring
    , toAxial
    , toString
    , valid
    )

-- import AStar.Generalised as Astar

import Set exposing (Set)


{-| A 3d point
-}
type alias Point =
    ( Int, Int, Int )


{-| Check if a point is valid
A valid point is one where x + y + z == 0
-}
valid : Point -> Bool
valid ( x, y, z ) =
    x + y + z == 0


{-| Convert cube point to axial point
Note: returns (0, 0) if point is invalid
-}
toAxial : Point -> ( Int, Int )
toAxial ( x, y, z ) =
    if valid ( x, y, z ) then
        ( x, z )

    else
        ( 0, 0 )


{-| Add two points together
-}
add : Point -> Point -> Point
add ( x1, y1, z1 ) ( x2, y2, z2 ) =
    ( x1 + x2, y1 + y2, z1 + z2 )


{-| Get direction given hex side
-}
direction : Int -> Point
direction dir =
    case dir of
        0 ->
            ( 1, -1, 0 )

        1 ->
            ( 1, 0, -1 )

        2 ->
            ( 0, 1, -1 )

        3 ->
            ( -1, 1, 0 )

        4 ->
            ( -1, 0, 1 )

        _ ->
            ( 0, -1, 1 )


{-| Convert Point to string with format "(x,y,z)"
-}
toString : Point -> String
toString ( x, y, z ) =
    [ "("
    , String.fromInt x
    , ","
    , String.fromInt y
    , ","
    , String.fromInt z
    , ")"
    ]
        |> String.concat


{-| Get the neighbor at direction dir of a given point
-}
neighbor : Point -> Int -> Point
neighbor point dir =
    add point (direction dir)


{-| Get all six neighbors
-}
neighbors : Point -> Set Point
neighbors p =
    [ neighbor p 0
    , neighbor p 1
    , neighbor p 2
    , neighbor p 3
    , neighbor p 4
    , neighbor p 5
    ]
        |> Set.fromList


{-| get distance between two points
-}
distance : Point -> Point -> Int
distance p1 p2 =
    distanceFloat p1 p2 |> round


distanceFloat : Point -> Point -> Float
distanceFloat ( x1, y1, z1 ) ( x2, y2, z2 ) =
    toFloat (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) / 2



-- movesFrom : (Point -> Bool) -> Point -> Set Point
-- movesFrom walkable point =
--     neighbors point |> Set.filter walkable
-- pathfind : (Point -> Bool) -> Point -> Point -> Maybe (List Point)
-- pathfind walkable from to =
--     Astar.findPath distanceFloat (movesFrom walkable) from to


{-| Create a new point from float values, round to nearest valid point
-}
fromFloat : ( Float, Float, Float ) -> Point
fromFloat ( x, y, z ) =
    let
        -- rounded point
        ( rx, ry, rz ) =
            ( round x, round y, round z )

        -- diierence between input point and rounded point
        ( dx, dy, dz ) =
            ( abs (toFloat rx - x), abs (toFloat ry - y), abs (toFloat rz - z) )

        -- final adjusted point
        ( fx, fy, fz ) =
            if dx > dy && dx > dz then
                ( -ry - rz, ry, rz )

            else if dy > dz then
                ( rx, -rx - rz, rz )

            else
                ( rx, ry, -rx - ry )
    in
    ( fx, fy, fz )


{-| Scale point
-}
scale : Float -> Point -> Point
scale i ( x1, y1, z1 ) =
    ( toFloat x1 * i, toFloat y1 * i, toFloat z1 * i ) |> fromFloat


{-| Returns a ring around given point with given radius
This is more of a hex for now. Will fix later
-}
ring : Point -> Int -> List Point
ring center radius =
    let
        getDirection s =
            case s of
                0 ->
                    4

                1 ->
                    5

                2 ->
                    0

                3 ->
                    1

                4 ->
                    2

                _ ->
                    3

        start s =
            add center (scale (toFloat radius) (direction (getDirection s)))

        side s =
            List.map (\i -> add (start s) (scale (toFloat i) (direction s))) (List.range 0 radius)
    in
    List.concatMap side (List.range 0 6)

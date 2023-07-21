module Engine.Point exposing
    ( Point
    , add
    , distance
    , distanceFloat
    , neighbors
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

module Engine.Grid exposing
    ( Chunk
    , Point
    , WorldMap
    , chunkPosition
    , chunkSize
    , empty
    , fill
    , insertChunk
    , insertTile
    , toVector2
    )

import Dict exposing (Dict)
import Engine.Vector2 as Vector2 exposing (Vector2)


chunkSize : Int
chunkSize =
    8


{-| A Point is a 2d tile coordinate
-}
type alias Point =
    ( Int, Int )


toVector2 : Point -> Vector2
toVector2 ( x, y ) =
    Vector2.new (toFloat x) (toFloat y)


{-| A Chunk is a section of a world map
-}
type alias Chunk a =
    Dict Point a


fill : a -> Chunk a
fill tile =
    List.range 0 (chunkSize - 1)
        |> List.concatMap (\y -> List.range 0 (chunkSize - 1) |> List.map (\x -> ( ( x, y ), tile )))
        |> Dict.fromList


{-| A World map is a dict of chunks
-}
type alias WorldMap a =
    Dict Point (Chunk a)


empty : WorldMap a
empty =
    Dict.empty


{-| Determine which chunk given point belongs in
-}
chunkPosition : Point -> Point
chunkPosition ( x, y ) =
    ( x // chunkSize, y // chunkSize )


{-| Given a point and a chunk coordinate, return local coordinate within chunk
-}
toLocal : Point -> Point -> Point
toLocal ( px, py ) ( cx, cy ) =
    ( px - (cx * chunkSize), py - (cy * chunkSize) )


insertTile : Point -> a -> WorldMap a -> WorldMap a
insertTile position tile worldMap =
    case Dict.get (chunkPosition position) worldMap of
        Just _ ->
            Dict.update (chunkPosition position)
                (\c ->
                    case c of
                        Just chunk ->
                            Just <| Dict.insert (toLocal position (chunkPosition position)) tile chunk

                        Nothing ->
                            c
                )
                worldMap

        Nothing ->
            Dict.insert (chunkPosition position) (Dict.singleton (toLocal position (chunkPosition position)) tile) worldMap


insertChunk : Point -> Chunk a -> WorldMap a -> WorldMap a
insertChunk position chunk worldMap =
    Dict.insert position chunk worldMap

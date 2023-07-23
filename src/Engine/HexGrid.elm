module Engine.HexGrid exposing (HexGrid, empty, fromList, get, insertTile, toList)

import Dict exposing (Dict)
import Engine.Point as Point exposing (Point)


type HexGrid tile
    = HexGrid (Dict Point tile)


empty : HexGrid tile
empty =
    HexGrid Dict.empty


{-| Insert tile, replace on collision
-}
insertTile : Point -> tile -> HexGrid tile -> HexGrid tile
insertTile position tile (HexGrid grid) =
    if Point.valid position then
        HexGrid <| Dict.insert position tile grid

    else
        HexGrid grid


fromList : List ( Point, tile ) -> HexGrid tile
fromList tiles =
    HexGrid <| Dict.fromList tiles


toList : HexGrid tile -> List ( Point, tile )
toList (HexGrid grid) =
    grid
        |> Dict.toList


get : Point -> HexGrid tile -> Maybe tile
get position (HexGrid grid) =
    if Point.valid position then
        Dict.get position grid

    else
        Nothing

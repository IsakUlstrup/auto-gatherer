module Content.Grids exposing (..)

import Engine.Grid as Grid exposing (WorldMap)


type Tile
    = Water
    | Ground
    | Wall


test2dGrid : Grid.WorldMap ()
test2dGrid =
    Grid.empty
        |> Grid.insertTile ( 0, 0 ) ()
        |> Grid.insertTile ( 1, 0 ) ()
        |> Grid.insertTile ( 1, 1 ) ()
        |> Grid.insertTile ( 1, 2 ) ()
        |> Grid.insertTile ( 2, 0 ) ()
        |> Grid.insertTile ( 3, 0 ) ()
        |> Grid.insertTile ( 4, 0 ) ()
        |> Grid.insertTile ( 5, 0 ) ()
        |> Grid.insertTile ( 7, 0 ) ()
        |> Grid.insertTile ( 9, 0 ) ()
        |> Grid.insertTile ( 10, 0 ) ()
        |> Grid.insertTile ( 14, 0 ) ()
        |> Grid.insertTile ( -1, 0 ) ()
        |> Grid.insertTile ( -2, 0 ) ()
        |> Grid.insertTile ( -3, 0 ) ()
        |> Grid.insertTile ( -4, 0 ) ()


test2dGrid2 : Grid.WorldMap ()
test2dGrid2 =
    Grid.empty
        |> Grid.insertChunk ( 0, 0 ) (Grid.fill ())
        |> Grid.insertChunk ( -1, -1 ) (Grid.fill ())
        |> Grid.insertChunk ( 1, 1 ) (Grid.fill ())
        |> Grid.insertChunk ( 1, 0 ) (Grid.fill ())
        |> Grid.insertChunk ( 0, 1 ) (Grid.fill ())
        |> Grid.insertChunk ( -1, 0 ) (Grid.fill ())
        |> Grid.insertChunk ( 0, -1 ) (Grid.fill ())
        |> Grid.insertChunk ( 1, -1 ) (Grid.fill ())
        |> Grid.insertChunk ( -1, 1 ) (Grid.fill ())


testMap : WorldMap Tile
testMap =
    Grid.empty
        |> Grid.insertTile ( 0, 0 ) Ground
        |> Grid.insertTile ( 1, 0 ) Ground
        |> Grid.insertTile ( 2, 0 ) Ground
        |> Grid.insertTile ( -1, 0 ) Ground
        |> Grid.insertTile ( -2, 0 ) Ground
        |> Grid.insertTile ( -2, 1 ) Ground
        |> Grid.insertTile ( -2, 2 ) Ground
        |> Grid.insertTile ( -2, -1 ) Water
        |> Grid.insertTile ( -2, -2 ) Ground
        |> Grid.insertTile ( -1, -2 ) Ground
        |> Grid.insertTile ( -1, -1 ) Ground
        |> Grid.insertTile ( 2, -1 ) Ground
        |> Grid.insertTile ( 1, -1 ) Ground
        |> Grid.insertTile ( 0, -1 ) Ground
        |> Grid.insertTile ( 0, -2 ) Ground
        |> Grid.insertTile ( 0, -3 ) Ground
        |> Grid.insertTile ( 1, -3 ) Ground
        |> Grid.insertTile ( 1, -2 ) Ground
        |> Grid.insertTile ( -1, -4 ) Wall
        |> Grid.insertTile ( 0, -4 ) Wall
        |> Grid.insertTile ( 1, -4 ) Wall
        |> Grid.insertTile ( 2, -4 ) Wall
        |> Grid.insertTile ( 2, -3 ) Wall
        |> Grid.insertTile ( 2, -2 ) Wall

module Content.Grids exposing (..)

import Engine.HexGrid as Grid exposing (HexGrid)
import Engine.Point as Point


testGrid1 : Grid.HexGrid ()
testGrid1 =
    Grid.empty
        |> Grid.insertTile ( 0, 0, 0 ) ()
        |> Grid.insertTile ( -1, 0, 1 ) ()
        |> Grid.insertTile ( 1, 1, -2 ) ()
        |> Grid.insertTile ( 1, 1, -3 ) ()
        |> Grid.insertTile ( 1, 2, -4 ) ()
        |> Grid.insertTile ( 2, 2, -4 ) ()
        |> Grid.insertTile ( 3, 2, -5 ) ()
        |> Grid.insertTile ( 0, 2, -2 ) ()
        |> Grid.insertTile ( -1, 2, -1 ) ()
        |> Grid.insertTile ( -2, 2, 0 ) ()
        |> Grid.insertTile ( -2, 0, 2 ) ()
        |> Grid.insertTile ( -3, 1, 2 ) ()
        |> Grid.insertTile ( -4, 2, 2 ) ()
        |> Grid.insertTile ( 3, -2, -1 ) ()
        |> Grid.insertTile ( 3, 1, -4 ) ()
        |> Grid.insertTile ( -2, -2, 4 ) ()
        |> Grid.insertTile ( 0, 2, -2 ) ()
        |> Grid.insertTile ( 0, 3, -3 ) ()
        |> Grid.insertTile ( 0, 4, -4 ) ()
        |> Grid.insertTile ( 0, 5, -5 ) ()
        |> Grid.insertTile ( 2, 3, -5 ) ()


circle : Int -> HexGrid ()
circle radius =
    List.range 0 radius
        |> List.concatMap (Point.ring ( 0, 0, 0 ))
        |> List.map (\p -> ( p, () ))
        |> Grid.fromList

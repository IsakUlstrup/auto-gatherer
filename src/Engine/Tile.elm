module Engine.Tile exposing (..)

import Engine.Vector2 as Vector2 exposing (Vector2)


tileSize : Int
tileSize =
    50


type alias Tile a =
    { position : Vector2
    , size : Float
    , state : a
    }


new : Float -> Float -> a -> Tile a
new x y state =
    Tile (Vector2.new x y) (toFloat tileSize) state

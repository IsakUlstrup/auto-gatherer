module Engine.Tile exposing (..)

import Engine.Vector2 exposing (Vector2)


type alias Tile a =
    { position : Vector2
    , size : Float
    , state : a
    }

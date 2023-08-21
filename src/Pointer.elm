module Pointer exposing (Pointer)

import Engine.Vector2 exposing (Vector2)


type alias Pointer =
    { position : Vector2
    , pressed : Bool
    }

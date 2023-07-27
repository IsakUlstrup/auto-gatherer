module GameState exposing (Particle(..), TileData(..))

import Engine.Vector2 exposing (Vector2)


type Particle
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition
    | MoveToClosest
    | Idle
    | Avoid


type TileData
    = Water
    | Ground
    | Wall

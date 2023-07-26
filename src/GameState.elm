module GameState exposing (Particle(..), Tile(..))

import Engine.Vector2 exposing (Vector2)


type Particle
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition
    | MoveToClosest
    | Idle
    | Avoid


type Tile
    = Water
    | Ground
    | Wall

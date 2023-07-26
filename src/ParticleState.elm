module ParticleState exposing (ParticleState(..), Tile(..))

import Engine.Vector2 exposing (Vector2)


type ParticleState
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

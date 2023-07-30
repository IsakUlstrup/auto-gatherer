module GameState exposing (Particle(..))

import Engine.Vector2 exposing (Vector2)


type Particle
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition Float
    | MoveToClosest
    | Idle
    | Avoid
    | FollowId Int

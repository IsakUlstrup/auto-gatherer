module Content.ParticleState exposing (ParticleState(..))

import Engine.Vector2 exposing (Vector2)


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition Float
    | MoveToClosest
    | Idle
    | Avoid
    | FollowId Int

module PhysicsInteraction exposing
    ( isColliding
    , resolveCollision
    )

import Physics exposing (Physics)
import Vector2


type alias HasPhysics a =
    { a | physics : Physics }


{-| Detect collisions, run action on collision
-}
isColliding : (HasPhysics b -> HasPhysics b) -> List (HasPhysics a) -> HasPhysics b -> HasPhysics b
isColliding action targets entity =
    let
        collision : Bool
        collision =
            Physics.isCollidingList
                (List.map .physics targets)
                entity.physics
    in
    if collision then
        action entity

    else
        entity


{-| Detect and react to collisions
-}
resolveCollision : List (HasPhysics a) -> HasPhysics b -> HasPhysics b
resolveCollision targets entity =
    let
        resolve res e =
            e
                |> (\a -> { a | physics = Physics.resolveCollision res a.physics })
                |> (\a -> { a | physics = Physics.applyForce (Vector2.direction res.position entity.physics.position |> Vector2.scale 0.5) a.physics })
    in
    targets
        |> List.map .physics
        |> List.filter (Physics.isColliding entity.physics)
        |> List.foldl resolve entity

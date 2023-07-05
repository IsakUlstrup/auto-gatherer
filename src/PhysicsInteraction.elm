module PhysicsInteraction exposing
    ( HasPhysics
    , isColliding
    , resolveCollision
    )

import Engine.Physics exposing (Physics)
import Engine.Vector2


type alias HasPhysics a =
    { a | physics : Physics }


{-| Detect collisions, run action on collision
-}
isColliding : (HasPhysics b -> HasPhysics b) -> List (HasPhysics a) -> HasPhysics b -> HasPhysics b
isColliding action targets entity =
    let
        collision : Bool
        collision =
            Engine.Physics.isCollidingList
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
        resolve : Physics -> HasPhysics b -> HasPhysics b
        resolve res e =
            e
                |> (\a -> { a | physics = Engine.Physics.resolveCollision res a.physics })
                |> (\a -> { a | physics = Engine.Physics.applyForce (Engine.Vector2.direction res.position entity.physics.position |> Engine.Vector2.scale 0.5) a.physics })
    in
    targets
        |> List.map .physics
        |> List.filter (Engine.Physics.isColliding entity.physics)
        |> List.foldl resolve entity

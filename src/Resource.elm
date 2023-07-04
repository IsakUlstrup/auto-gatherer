module Resource exposing (Resource, isColliding, newResource, tickState)

import Physics exposing (Physics)


type alias Resource =
    { physics : Physics
    , hitCooldown : Float
    }


newResource : Float -> Float -> Resource
newResource x y =
    Resource (Physics.initPhysics x y 25) 0


isColliding : List { a | physics : Physics.Physics } -> Resource -> Resource
isColliding animals resource =
    let
        collision =
            animals
                |> List.map .physics
                |> List.filter (Physics.isColliding resource.physics)
                |> List.isEmpty
                |> not
    in
    if collision then
        { resource | hitCooldown = 200 }

    else
        resource


tickState : Float -> Resource -> Resource
tickState dt resource =
    { resource | hitCooldown = max 0 (resource.hitCooldown - dt) }

module Resource exposing (Resource, hitCooldown, newResource, tickState)

import Physics exposing (Physics)


type alias Resource =
    { physics : Physics
    , hitCooldown : Float
    }


newResource : Float -> Float -> Resource
newResource x y =
    Resource (Physics.initPhysics x y 25) 0


hitCooldown : Resource -> Resource
hitCooldown resource =
    { resource | hitCooldown = 200 }


tickState : Float -> Resource -> Resource
tickState dt resource =
    { resource | hitCooldown = max 0 (resource.hitCooldown - dt) }

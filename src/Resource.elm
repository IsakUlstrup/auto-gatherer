module Resource exposing (..)

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Resource =
    PhysicsObject
        { hitCount : Int
        , home : Vector2
        , hitCooldown : Float
        }


new : Float -> Float -> Resource
new x y =
    PhysicsObject.new x y 30 100 { hitCount = 0, home = Vector2.new x y, hitCooldown = 0 }


incrementHits : Resource -> Resource
incrementHits resource =
    { resource | state = { hitCount = resource.state.hitCount + 1, home = resource.state.home, hitCooldown = resource.state.hitCooldown } }


hit : Resource -> Resource
hit resource =
    { resource | state = { hitCount = resource.state.hitCount, home = resource.state.home, hitCooldown = 100 } }


isHit : Resource -> Bool
isHit resource =
    resource.state.hitCooldown > 0


update : Float -> Resource -> Resource
update dt resource =
    { resource | state = { hitCount = resource.state.hitCount, home = resource.state.home, hitCooldown = max 0 resource.state.hitCooldown - dt } }

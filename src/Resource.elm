module Resource exposing (..)

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)


type HealthState
    = Health Int
    | Recharging Float


type alias Resource =
    PhysicsObject
        { hitCount : Int
        , home : Vector2
        , hitCooldown : Float
        , health : HealthState
        }


new : Float -> Float -> Resource
new x y =
    PhysicsObject.new x y 30 100 { hitCount = 0, home = Vector2.new x y, hitCooldown = 0, health = Health 10 }


incrementHits : Resource -> Resource
incrementHits resource =
    PhysicsObject.updateState (\s -> { s | hitCount = resource.state.hitCount + 1 }) resource


hit : Resource -> Resource
hit resource =
    case resource.state.health of
        Health hp ->
            if hp - 1 <= 0 then
                resource
                    |> PhysicsObject.setcollisionState False
                    |> PhysicsObject.updateState (\s -> { s | health = Recharging 8000 })

            else
                resource
                    |> PhysicsObject.updateState (\s -> { s | health = Health <| max 0 (hp - 1) })

        Recharging _ ->
            resource |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })


recharge : Float -> Resource -> Resource
recharge amount resource =
    case resource.state.health of
        Health _ ->
            resource

        Recharging r ->
            if r - amount <= 0 then
                resource |> PhysicsObject.updateState (\s -> { s | health = Health 10 }) |> PhysicsObject.setcollisionState True

            else
                resource |> PhysicsObject.updateState (\s -> { s | health = Recharging <| max 0 (r - amount) })


isHit : Resource -> Bool
isHit resource =
    resource.state.hitCooldown > 0


isRecharging : Resource -> Bool
isRecharging resource =
    case resource.state.health of
        Health _ ->
            False

        Recharging _ ->
            True


update : Float -> Resource -> Resource
update dt resource =
    resource
        |> recharge dt
        |> PhysicsObject.updateState (\s -> { s | hitCooldown = max 0 resource.state.hitCooldown - dt })

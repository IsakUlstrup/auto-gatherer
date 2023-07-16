module Resource exposing
    ( HealthState
    , Resource
    , getHealth
    , hit
    , isHit
    , isRecharging
    , new
    , update
    )

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)


type HealthState
    = Health Int
    | Recharging Float


type alias Resource =
    PhysicsObject
        { home : Vector2
        , hitCooldown : Float
        , health : HealthState
        }


new : Float -> Float -> Float -> Resource
new x y size =
    PhysicsObject.new x y size (size * 10) { home = Vector2.new x y, hitCooldown = 0, health = Health 10 }


hit : Resource -> Resource
hit resource =
    case resource.state.health of
        Health hp ->
            if hp - 1 <= 0 then
                resource
                    |> PhysicsObject.setcollisionState False
                    |> PhysicsObject.updateState (\s -> { s | health = Recharging 8000 })
                    |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })

            else
                resource
                    |> PhysicsObject.updateState (\s -> { s | health = Health <| max 0 (hp - 1) })
                    |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })

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


getHealth : Resource -> Maybe Int
getHealth resource =
    case resource.state.health of
        Health hp ->
            Just hp

        Recharging _ ->
            Nothing


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

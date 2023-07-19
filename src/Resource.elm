module Resource exposing
    ( HealthState
    , Resource
    , getHealth
    , healthZero
    , hit
    , isHit
    , isRecharging
    , new
    , setRecharging
    , update
    )

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)


type HealthState
    = Health ( Int, Int )
    | Recharging Float Int


type alias Resource =
    PhysicsObject
        { home : Vector2
        , hitCooldown : Float
        , health : HealthState
        }


new : Float -> Float -> Float -> Resource
new x y size =
    PhysicsObject.new x y size (size * 10) { home = Vector2.new x y, hitCooldown = 0, health = Health ( 10, 10 ) }


hit : Resource -> Resource
hit resource =
    case resource.state.health of
        Health ( hp, maxHp ) ->
            if hp == 0 then
                resource
                    |> PhysicsObject.setcollisionState False
                    |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })

            else
                resource
                    |> PhysicsObject.updateState (\s -> { s | health = Health <| ( max 0 (hp - 1), maxHp ) })
                    |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })

        Recharging _ _ ->
            resource |> PhysicsObject.updateState (\s -> { s | hitCooldown = 100 })


recharge : Float -> Resource -> Resource
recharge amount resource =
    case resource.state.health of
        Health _ ->
            resource

        Recharging r maxHp ->
            if r == 0 then
                resource |> PhysicsObject.updateState (\s -> { s | health = Health ( maxHp, maxHp ) }) |> PhysicsObject.setcollisionState True

            else
                resource |> PhysicsObject.updateState (\s -> { s | health = Recharging (max 0 (r - amount)) maxHp })


isHit : Resource -> Bool
isHit resource =
    resource.state.hitCooldown > 0


getHealth : Resource -> Maybe ( Int, Int )
getHealth resource =
    case resource.state.health of
        Health ( hp, maxHp ) ->
            Just ( hp, maxHp )

        Recharging _ _ ->
            Nothing


isRecharging : Resource -> Bool
isRecharging resource =
    case resource.state.health of
        Health _ ->
            False

        Recharging _ _ ->
            True


healthZero : Resource -> Bool
healthZero resource =
    case resource.state.health of
        Health ( 0, _ ) ->
            True

        _ ->
            False


setRecharging : Resource -> Resource
setRecharging resource =
    case resource.state.health of
        Health ( 0, maxHp ) ->
            resource
                |> PhysicsObject.setcollisionState False
                |> PhysicsObject.updateState (\s -> { s | health = Recharging 8000 maxHp })

        _ ->
            resource


update : Float -> Resource -> Resource
update dt resource =
    resource
        |> recharge dt
        |> PhysicsObject.updateState (\s -> { s | hitCooldown = max 0 resource.state.hitCooldown - dt })

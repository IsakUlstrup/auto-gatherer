module Resource exposing
    ( Resource
    , ResourceState
    , collideables
    , handleHit
    , isExhausted
    , isHit
    , move
    , movementAi
    , new
    , tickState
    )

import Engine.Physics exposing (Physics)
import Engine.Vector2 exposing (Vector2)


type ResourceState
    = Ready Int
    | Exhausted Float


type alias Resource =
    { physics : Physics
    , hitCooldown : Float
    , state : ResourceState
    , startPosition : Vector2
    }


new : Float -> Float -> Float -> Resource
new x y radius =
    Resource (Engine.Physics.initPhysics x y radius) 0 (Ready 10) (Engine.Vector2.new x y)


hitCooldown : Resource -> Resource
hitCooldown resource =
    { resource | hitCooldown = 100 }


isHit : Resource -> Bool
isHit resource =
    resource.hitCooldown > 0


isExhausted : Resource -> Bool
isExhausted resource =
    case resource.state of
        Ready 0 ->
            True

        Ready _ ->
            False

        Exhausted _ ->
            True


hit : Resource -> Resource
hit resource =
    case resource.state of
        Ready hp ->
            if hp - 1 <= 0 then
                { resource | state = Exhausted 10000 }

            else
                { resource | state = Ready (hp - 1) }

        Exhausted _ ->
            resource


handleHit : Resource -> Resource
handleHit resource =
    if isExhausted resource then
        resource

    else
        resource |> hitCooldown |> hit


tickState : Float -> Resource -> Resource
tickState dt resource =
    case resource.state of
        Ready _ ->
            { resource | hitCooldown = max 0 (resource.hitCooldown - dt) }

        Exhausted cd ->
            if cd - dt <= 0 then
                { resource | hitCooldown = max 0 (resource.hitCooldown - dt), state = Ready 10 }

            else
                { resource | hitCooldown = max 0 (resource.hitCooldown - dt), state = Exhausted (cd - dt) }


collideables : List Resource -> List Resource
collideables resources =
    List.filter (isExhausted >> not) resources


movementAi : Resource -> Resource
movementAi resource =
    let
        force =
            if Engine.Vector2.distance resource.physics.position resource.startPosition < 5 then
                Engine.Vector2.zero

            else
                Engine.Vector2.direction resource.physics.position resource.startPosition |> Engine.Vector2.scale 0.05
    in
    { resource | physics = Engine.Physics.applyForce force resource.physics }


move : Float -> Resource -> Resource
move dt resource =
    { resource
        | physics =
            resource.physics
                |> Engine.Physics.move dt
                |> Engine.Physics.applyFriction 0.8
    }

module Resource exposing
    ( Resource
    , ResourceState
    , collideables
    , handleHit
    , isExhausted
    , isHit
    , new
    , tickState
    )

import Engine.Physics exposing (Physics)


type ResourceState
    = Ready Int
    | Exhausted Float


type alias Resource =
    { physics : Physics
    , hitCooldown : Float
    , state : ResourceState
    }


new : Float -> Float -> Float -> Resource
new x y radius =
    Resource (Engine.Physics.initPhysics x y radius) 0 (Ready 10)


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

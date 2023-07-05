module Engine.Physics exposing
    ( Physics
    , applyForce
    , applyFriction
    , initPhysics
    , isColliding
    , isCollidingList
    , move
    , resolveCollision
    , stopIfSlow
    )

import Engine.Vector2 exposing (Vector2)


type alias Physics =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , radius : Float
    }



---- BUILDER ----


initPhysics : Float -> Float -> Float -> Physics
initPhysics x y radius =
    Physics (Engine.Vector2.new x y)
        (Engine.Vector2.new 0 0)
        (Engine.Vector2.new 0 0)
        radius



---- UPDATES ----


resetAcceleration : Physics -> Physics
resetAcceleration physics =
    { physics | acceleration = Engine.Vector2.zero }


move : Float -> Physics -> Physics
move dt phys =
    { phys
        | velocity = Engine.Vector2.add phys.velocity phys.acceleration
        , position = Engine.Vector2.add phys.position (Engine.Vector2.scale dt phys.velocity)
    }
        |> resetAcceleration


setPosition : Vector2 -> Physics -> Physics
setPosition pos physics =
    { physics | position = pos }


applyFriction : Float -> Physics -> Physics
applyFriction friction physics =
    { physics | velocity = Engine.Vector2.scale friction physics.velocity }


{-| Set velocity to zero if it's magnitude is below a given limit
-}
stopIfSlow : Float -> Physics -> Physics
stopIfSlow limit physics =
    if Engine.Vector2.magnitude physics.velocity < limit then
        { physics | velocity = Engine.Vector2.zero }

    else
        physics


stop : Physics -> Physics
stop physics =
    { physics | velocity = Engine.Vector2.zero }


applyForce : Vector2 -> Physics -> Physics
applyForce force physics =
    { physics
        | acceleration =
            Engine.Vector2.add physics.acceleration force
    }



---- COLLISION ----


isColliding : Physics -> Physics -> Bool
isColliding physics target =
    let
        dist : Vector2
        dist =
            Engine.Vector2.subtract physics.position target.position

        sumRadii : Float
        sumRadii =
            physics.radius + target.radius
    in
    dist.x ^ 2 + dist.y ^ 2 <= sumRadii ^ 2


isCollidingList : List Physics -> Physics -> Bool
isCollidingList targets physics =
    targets
        |> List.filter (isColliding physics)
        |> List.isEmpty
        |> not


resolveCollision : Physics -> Physics -> Physics
resolveCollision target physics =
    let
        pos : Vector2
        pos =
            Engine.Vector2.add
                (Engine.Vector2.singleton (physics.radius + target.radius)
                    |> Engine.Vector2.multiply (Engine.Vector2.direction target.position physics.position)
                )
                target.position
    in
    setPosition pos physics |> stop

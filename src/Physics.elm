module Physics exposing
    ( Physics
    , applyForce
    , applyFriction
    , initPhysics
    , isColliding
    , isCollidingList
    , move
    , resolveCollision
    , reverseVelocity
    , stopIfSlow
    )

import Vector2 exposing (Vector2)


type alias Physics =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , radius : Float
    }



---- BUILDER ----


initPhysics : Float -> Float -> Float -> Physics
initPhysics x y radius =
    Physics (Vector2.new x y)
        (Vector2.new 0 0)
        (Vector2.new 0 0)
        radius



---- UPDATES ----


resetAcceleration : Physics -> Physics
resetAcceleration physics =
    { physics | acceleration = Vector2.zero }


move : Float -> Physics -> Physics
move dt phys =
    { phys
        | velocity = Vector2.add phys.velocity phys.acceleration
        , position = Vector2.add phys.position (Vector2.scale dt phys.velocity)
    }
        |> resetAcceleration


setPosition : Vector2 -> Physics -> Physics
setPosition pos physics =
    { physics | position = pos }


applyFriction : Float -> Physics -> Physics
applyFriction friction physics =
    { physics | velocity = Vector2.scale friction physics.velocity }


{-| Set velocity to zero if it's magnitude is below a given limit
-}
stopIfSlow : Float -> Physics -> Physics
stopIfSlow limit physics =
    if Vector2.magnitude physics.velocity < limit then
        { physics | velocity = Vector2.zero }

    else
        physics


applyForce : Vector2 -> Physics -> Physics
applyForce force physics =
    { physics
        | acceleration =
            Vector2.add physics.acceleration force
    }


reverseVelocity : Physics -> Physics
reverseVelocity physics =
    { physics | velocity = Vector2.negate physics.velocity }



---- COLLISION ----


isColliding : Physics -> Physics -> Bool
isColliding physics target =
    let
        dist : Vector2
        dist =
            Vector2.subtract physics.position target.position

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
            Vector2.add
                (Vector2.singleton (physics.radius + target.radius)
                    |> Vector2.multiply (Vector2.direction target.position physics.position)
                )
                target.position
    in
    setPosition pos physics

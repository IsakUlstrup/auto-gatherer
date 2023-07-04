module Physics exposing
    ( Physics
    , applyForce
    , applyForces
    , applyFriction
    , initPhysics
    , isColliding
    , isCollidingVector
    , move
    , movePosition
    , resolveCollision
    , reverseVelocity
    , setPosition
    , stopIfSlow
    )

import Vector2 as Vector2 exposing (Vector2)


type alias Physics =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , radius : Float
    , mass : Float
    }



---- BUILDER ----


initPhysics : Float -> Float -> Float -> Float -> Physics
initPhysics x y radius mass =
    Physics (Vector2.new x y)
        (Vector2.new 0 0)
        (Vector2.new 0 0)
        radius
        mass



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


movePosition : Vector2 -> Physics -> Physics
movePosition vector physics =
    { physics | position = Vector2.add physics.position vector }


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


applyForces : List Vector2 -> Physics -> Physics
applyForces forces physics =
    { physics
        | acceleration =
            List.foldl Vector2.add Vector2.zero forces
                |> Vector2.divide physics.mass
                |> Vector2.add physics.acceleration
    }


applyForce : Vector2 -> Physics -> Physics
applyForce force physics =
    { physics
        | acceleration =
            force
                |> Vector2.divide physics.mass
                |> Vector2.add physics.acceleration
    }


reverseVelocity : Physics -> Physics
reverseVelocity physics =
    { physics | velocity = Vector2.negate physics.velocity }



-- applyImpulses : List Vector2 -> Physics -> Physics
-- applyImpulses impulses phys =
--     { phys | velocity = List.foldl Vector2.add phys.velocity impulses }
---- COLLISION ----


isColliding : Physics -> { a | position : Vector2, radius : Float } -> Bool
isColliding physics target =
    let
        dist =
            Vector2.subtract physics.position target.position

        sumRadii =
            physics.radius + target.radius
    in
    dist.x ^ 2 + dist.y ^ 2 <= sumRadii ^ 2


isCollidingVector : { a | position : Vector2, radius : Float } -> Physics -> Bool
isCollidingVector vector target =
    let
        dist =
            Vector2.subtract vector.position target.position

        sumRadii =
            vector.radius + target.radius
    in
    dist.x ^ 2 + dist.y ^ 2 <= sumRadii ^ 2


resolveCollision : { a | radius : Float, position : Vector2 } -> Physics -> Physics
resolveCollision target physics =
    let
        pos =
            Vector2.add
                (Vector2.singleton (physics.radius + target.radius)
                    |> Vector2.multiply (Vector2.direction target.position physics.position)
                )
                target.position
    in
    setPosition pos physics

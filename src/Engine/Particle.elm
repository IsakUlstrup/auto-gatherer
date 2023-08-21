module Engine.Particle exposing
    ( Particle
    , applyForce
    , applyFriciton
    , applyImpulse
    , distance
    , move
    , moveAwayRange
    , moveDirection
    , moveToNearest
    , moveToPosition
    , new
    , setVelocity
    , stopIfSlow
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Particle a =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , impulse : Vector2
    , mass : Float
    , radius : Float
    , elasticity : Float
    , state : a
    }


{-| Particle constructor
-}
new : Vector2 -> Float -> Float -> Float -> a -> Particle a
new position size mass elasticity state =
    Particle
        position
        Vector2.zero
        Vector2.zero
        Vector2.zero
        (clamp 1 562949953421311 mass)
        (clamp 1 562949953421311 size)
        (clamp 0 1 elasticity)
        state



-- FORCES


{-| Apply force to object
-}
applyForce : Vector2 -> Particle a -> Particle a
applyForce force particle =
    { particle
        | acceleration =
            force
                |> Vector2.divide particle.mass
                |> Vector2.add particle.acceleration
    }


{-| Apply impulse to object
-}
applyImpulse : Vector2 -> Particle a -> Particle a
applyImpulse impulse particle =
    { particle | impulse = Vector2.add particle.impulse impulse }


{-| Janky friction for now
-}
applyFriciton : Float -> Particle a -> Particle a
applyFriciton friction particle =
    let
        adjustedFriction : Float
        adjustedFriction =
            1 - clamp 0 1 friction
    in
    { particle | velocity = Vector2.scale adjustedFriction particle.velocity }



-- MOVEMENT


{-| Set velocity to zero if velocity magnitude is below limit
-}
stopIfSlow : Float -> Particle a -> Particle a
stopIfSlow limit particle =
    if Vector2.magnitude particle.velocity < limit then
        { particle | velocity = Vector2.zero }

    else
        particle


{-| Move physics particle
-}
move : Float -> Particle a -> Particle a
move dt particle =
    let
        velocity : Particle a -> Particle a
        velocity p =
            { p
                | velocity =
                    p.velocity
                        |> Vector2.add (Vector2.scale dt p.acceleration)
                        |> Vector2.add (Vector2.scale (1 / dt) p.impulse)
            }

        impulse : Particle a -> Particle a
        impulse p =
            { p | position = Vector2.add p.position p.impulse }

        position : Particle a -> Particle a
        position p =
            { p | position = Vector2.add p.position (Vector2.scale dt p.velocity) }
    in
    particle
        |> velocity
        |> impulse
        |> position
        |> resetAcceleration
        |> resetImpulse


{-| Reset particle acceleration
-}
resetAcceleration : Particle a -> Particle a
resetAcceleration particle =
    { particle | acceleration = Vector2.zero }


{-| Reset particle impulse
-}
resetImpulse : Particle a -> Particle a
resetImpulse particle =
    { particle | impulse = Vector2.zero }


setVelocity : Vector2 -> Particle a -> Particle a
setVelocity vel particle =
    { particle | velocity = vel }


distance : Particle a -> Particle b -> Float
distance p1 p2 =
    Vector2.distance p1.position p2.position - (p1.radius + p2.radius)



-- AI


{-| Apply force towards nearest target

If none are present, do nothing

-}
moveToNearest : Float -> Float -> List (Particle b) -> Particle a -> Particle a
moveToNearest speed maxDistance targets particle =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                -- |> List.filter (isNotEqual particle)
                |> List.filter (\t -> distance t particle > maxDistance)
                |> List.map .position
                |> List.sortBy (Vector2.distance particle.position)
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction particle.position target |> Vector2.scale speed
    in
    case nearest of
        Just target ->
            applyForce (force target) particle

        Nothing ->
            particle


moveDirection : Float -> Vector2 -> Particle a -> Particle a
moveDirection speed direction particle =
    let
        force : Vector2
        force =
            direction |> Vector2.normalize |> Vector2.scale speed
    in
    applyForce force particle


{-| Apply force away from nearest target in range

If none are present, do nothing

-}
moveAwayRange : Float -> Float -> List (Particle b) -> Particle a -> Vector2
moveAwayRange speed range targets particle =
    let
        inRange : List Vector2
        inRange =
            targets
                |> List.filter (\t -> distance t particle < range)
                |> List.map .position
    in
    List.foldl (\t v -> Vector2.direction t particle.position |> Vector2.add v) Vector2.zero inRange
        |> Vector2.scale speed


{-| Apply force towards target position if particle is more than limitDistance units away
-}
moveToPosition : Float -> Float -> Vector2 -> Particle a -> Vector2
moveToPosition speed limitDistance position particle =
    if Vector2.distance particle.position position < limitDistance then
        Vector2.zero

    else
        Vector2.direction particle.position position |> Vector2.scale speed

module Engine.Particle exposing
    ( Particle
    , applyForce
    , applyFriciton
    , collisionAction
    , move
    , moveAwayRange
    , moveToNearest
    , moveToPosition
    , new
    , resolveCollisions
    , stopIfSlow
    , updateState
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Particle a =
    { id : Int
    , position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , radius : Float
    , mass : Float
    , state : a
    }


{-| Physics object constructor

Mass and size will be clamped between 1 and 562949953421311

-}
new : Float -> Float -> Float -> Float -> Int -> a -> Particle a
new x y size mass id state =
    Particle
        id
        (Vector2.new x y)
        Vector2.zero
        Vector2.zero
        (clamp 1 562949953421311 size)
        (clamp 1 562949953421311 mass)
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
        stop particle

    else
        particle


{-| Move physics particle
-}
move : Float -> Particle a -> Particle a
move dt particle =
    particle
        |> (\obj -> { obj | velocity = Vector2.add obj.velocity (Vector2.scale dt obj.acceleration) })
        |> (\obj -> { obj | position = Vector2.add obj.position (Vector2.scale dt obj.velocity) })
        |> resetAcceleration


{-| Reset particle acceleration
-}
resetAcceleration : Particle a -> Particle a
resetAcceleration particle =
    { particle | acceleration = Vector2.zero }


setPosition : Vector2 -> Particle a -> Particle a
setPosition pos particle =
    { particle | position = pos }


setVelocity : Vector2 -> Particle a -> Particle a
setVelocity vel particle =
    { particle | velocity = vel }


stop : Particle a -> Particle a
stop particle =
    { particle | velocity = Vector2.zero }



-- ---- COLLISION ----


{-| Check if two particles are colliding

A collision occurs when the distance between to particles with collisions enabled is less than their combine radii

-}
isColliding : Particle a -> Particle b -> Bool
isColliding particle target =
    if particle.id /= target.id then
        let
            dist : Vector2
            dist =
                Vector2.subtract particle.position target.position
        in
        (dist |> Vector2.multiply dist |> Vector2.sum) <= (particle.radius + target.radius) ^ 2

    else
        False


{-| if particle is colliding with target, run function f on particle and return
-}
collisionAction : (Particle b -> Particle a -> Particle a) -> List (Particle b) -> Particle a -> Particle a
collisionAction f targets particle =
    let
        collisions : List (Particle b)
        collisions =
            List.filter (isColliding particle) targets

        helper : Particle b -> Particle a -> Particle a
        helper target obj =
            f target obj
    in
    List.foldl helper particle collisions


{-| calculate how much each particle should move based on the diference in mass

        When resolving collision between a light and a heavy particle, the light one moves more

-}
overlapModifier : Particle b -> Particle a -> Float
overlapModifier target particle =
    (((target.mass - particle.mass) / (target.mass + particle.mass)) + 1) * 0.5


{-| Resolve collision between two particles
-}
resolveCollision : Particle b -> Particle a -> Particle a
resolveCollision target particle =
    let
        dist : Float
        dist =
            Vector2.distance particle.position target.position

        overlap : Float
        overlap =
            (dist - particle.radius - target.radius) * overlapModifier target particle

        normal : Vector2
        normal =
            Vector2.direction particle.position target.position

        pos : Vector2
        pos =
            Vector2.add particle.position (Vector2.direction particle.position target.position |> Vector2.scale overlap)

        k : Vector2
        k =
            Vector2.subtract particle.velocity target.velocity

        p : Float
        p =
            2 * (normal.x * k.x + normal.y * k.y) / (particle.mass + target.mass)

        v : Vector2
        v =
            Vector2.new (particle.velocity.x - p * target.mass * normal.x)
                (particle.velocity.y - p * target.mass * normal.y)
    in
    particle
        |> setPosition pos
        |> setVelocity v



-- |> (\o -> { o | velocity = Vector2.new newVx newVy })
-- |> stop
-- |> applyForce
--     (Vector2.direction target.position particle.position
--         -- |> Vector2.add (Vector2.add target.velocity particle.velocity)
--         -- |> Vector2.scale ((target.mass - particle.mass) / totalMass)
--         |> Vector2.scale 2
--     )


{-| Detect and react to collisions
-}
resolveCollisions : List (Particle b) -> Particle a -> Particle a
resolveCollisions targets particle =
    let
        resolve : Particle b -> Particle a -> Particle a
        resolve res e =
            resolveCollision res e
    in
    targets
        |> List.filter (isColliding particle)
        |> List.foldl resolve particle



-- AI


{-| Apply force towards nearest target

If none are present, do nothing

-}
moveToNearest : List (Particle b) -> Float -> Particle a -> Particle a
moveToNearest targets speed particle =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (\t -> t.id /= particle.id)
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


{-| Apply force away from nearest target in range

If none are present, do nothing

-}
moveAwayRange : Float -> List (Particle b) -> Float -> Particle a -> Particle a
moveAwayRange range targets speed particle =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (\t -> t.id /= particle.id)
                |> List.map .position
                |> List.sortBy (Vector2.distance particle.position)
                |> List.filter (\t -> Vector2.distance t particle.position < range)
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction particle.position target |> Vector2.scale speed |> Vector2.scale -1
    in
    case nearest of
        Just target ->
            applyForce (force target) particle

        Nothing ->
            particle


{-| Apply force towards target position if particle is more than limitDistance units away
-}
moveToPosition : Float -> Vector2 -> Float -> Particle a -> Particle a
moveToPosition limitDistance position speed particle =
    let
        force : Vector2
        force =
            if Vector2.distance particle.position position < limitDistance then
                Vector2.zero

            else
                Vector2.direction particle.position position |> Vector2.scale speed
    in
    applyForce force particle



-- STATE


updateState : (a -> a) -> Particle a -> Particle a
updateState f particle =
    { particle | state = f particle.state }
module Engine.Particle exposing
    ( Particle
    , applyForce
    , applyFriciton
    , collisionAction
    , distance
    , isColliding
    , move
    , moveAwayRange
    , moveDirection
    , moveToId
    , moveToNearest
    , moveToPosition
    , new
    , resolveCollisions
    , stopIfSlow
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Particle a =
    { id : Int
    , position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , impulse : Vector2
    , mass : Float
    , radius : Float
    , speed : Float
    , state : a
    }


{-| Particle constructor
-}
new : Float -> Float -> Float -> Float -> Int -> a -> Particle a
new x y size speed id state =
    Particle
        id
        (Vector2.new x y)
        Vector2.zero
        Vector2.zero
        Vector2.zero
        (clamp 1 562949953421311 size)
        (clamp 1 562949953421311 size)
        speed
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
            { p | velocity = Vector2.add p.velocity (Vector2.scale dt p.acceleration) |> Vector2.add (Vector2.scale 0.05 p.impulse) }

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


isNotEqual : Particle a -> Particle b -> Bool
isNotEqual p1 p2 =
    p1.id /= p2.id



---- COLLISION ----


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
    let
        ratio x y =
            (((x - y) / (x + y)) + 1) * 0.5
    in
    ratio target.mass particle.mass


{-| Resolve collision between two particles
-}
resolveDynamicCollision : Particle b -> Particle a -> Particle a
resolveDynamicCollision target particle =
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

        impulse : Vector2
        impulse =
            Vector2.direction particle.position target.position |> Vector2.scale overlap

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
        |> applyImpulse impulse
        |> setVelocity v


{-| Detect and react to collisions
-}
resolveCollisions : List (Particle b) -> Particle a -> Particle a
resolveCollisions targets particle =
    let
        resolve : Particle b -> Particle a -> Particle a
        resolve res e =
            resolveDynamicCollision res e
    in
    targets
        |> List.filter (isColliding particle)
        |> List.foldl resolve particle



-- AI


{-| Apply force towards nearest target

If none are present, do nothing

-}
moveToNearest : Float -> List (Particle b) -> Particle a -> Particle a
moveToNearest maxDistance targets particle =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (isNotEqual particle)
                |> List.filter (\t -> distance t particle > maxDistance)
                |> List.map .position
                |> List.sortBy (Vector2.distance particle.position)
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction particle.position target |> Vector2.scale particle.speed
    in
    case nearest of
        Just target ->
            applyForce (force target) particle

        Nothing ->
            particle


{-| Move towards particle with a given id
-}
moveToId : Float -> Int -> List (Particle b) -> Particle a -> Particle a
moveToId maxDistance id targets particle =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (isNotEqual particle)
                |> List.filter (\t -> distance t particle > maxDistance && t.id == id)
                |> List.map .position
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction particle.position target |> Vector2.scale particle.speed
    in
    case nearest of
        Just target ->
            applyForce (force target) particle

        Nothing ->
            particle


moveDirection : Vector2 -> Particle a -> Particle a
moveDirection direction particle =
    let
        force : Vector2
        force =
            direction |> Vector2.normalize |> Vector2.scale particle.speed
    in
    applyForce force particle


{-| Apply force away from nearest target in range

If none are present, do nothing

-}
moveAwayRange : Float -> List (Particle b) -> Particle a -> Particle a
moveAwayRange range targets particle =
    let
        inRange : List Vector2
        inRange =
            targets
                |> List.filter (\t -> t.id /= particle.id)
                |> List.filter (\t -> distance t particle < range)
                |> List.map .position

        force : Vector2
        force =
            List.foldl (\t v -> Vector2.direction t particle.position |> Vector2.add v) Vector2.zero inRange
                |> Vector2.scale particle.speed
    in
    applyForce force particle


{-| Apply force towards target position if particle is more than limitDistance units away
-}
moveToPosition : Float -> Vector2 -> Particle a -> Particle a
moveToPosition limitDistance position particle =
    let
        force : Vector2
        force =
            if Vector2.distance particle.position position - particle.radius < limitDistance then
                Vector2.zero

            else
                Vector2.direction particle.position position |> Vector2.scale particle.speed
    in
    applyForce force particle

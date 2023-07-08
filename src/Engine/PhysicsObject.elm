module Engine.PhysicsObject exposing
    ( PhysicsObject
    , applyForce
    , applyFriciton
    , collisionAction
    , disableCollision
    , move
    , moveToNearest
    , moveToPosition
    , new
    , resolveCollisions
    , stopIfSlow
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias PhysicsObject a =
    { position : Vector2
    , velocity : Vector2
    , acceleration : Vector2
    , radius : Float
    , mass : Float
    , enableCollisions : Bool
    , state : a
    }


{-| Physics object constructor
-}
new : Float -> Float -> Float -> Float -> a -> PhysicsObject a
new x y radius mass state =
    PhysicsObject (Vector2.new x y)
        (Vector2.new 0 0)
        (Vector2.new 0 0)
        radius
        mass
        True
        state



-- FORCES


{-| Apply force to object
-}
applyForce : Vector2 -> PhysicsObject a -> PhysicsObject a
applyForce force object =
    { object
        | acceleration =
            force
                |> Vector2.divide object.mass
                |> Vector2.add object.acceleration
    }


{-| Janky friction for now
-}
applyFriciton : Float -> PhysicsObject a -> PhysicsObject a
applyFriciton friction object =
    let
        adjustedFriction =
            clamp 0 1 friction |> (-) 1
    in
    { object | velocity = Vector2.scale adjustedFriction object.velocity }



-- MOVEMENT


{-| Set velocity to zero if velocity magnitude is below limit
-}
stopIfSlow : Float -> PhysicsObject a -> PhysicsObject a
stopIfSlow limit object =
    if Vector2.magnitude object.velocity < limit then
        stop object

    else
        object


{-| Move physics object
-}
move : Float -> PhysicsObject a -> PhysicsObject a
move dt object =
    object
        |> (\obj -> { obj | velocity = Vector2.add obj.velocity (Vector2.scale dt obj.acceleration) })
        |> (\obj -> { obj | position = Vector2.add obj.position (Vector2.scale dt obj.velocity) })
        |> resetAcceleration


{-| Reset object acceleration
-}
resetAcceleration : PhysicsObject a -> PhysicsObject a
resetAcceleration object =
    { object | acceleration = Vector2.zero }


setPosition : Vector2 -> PhysicsObject a -> PhysicsObject a
setPosition pos object =
    { object | position = pos }


stop : PhysicsObject a -> PhysicsObject a
stop object =
    { object | velocity = Vector2.zero }



-- ---- COLLISION ----


disableCollision : PhysicsObject a -> PhysicsObject a
disableCollision object =
    { object | enableCollisions = False }


{-| Check if two objects are colliding

A collision occurs when the distance between to objects with collisions enabled is less than their combine radii

-}
isColliding : PhysicsObject a -> PhysicsObject b -> Bool
isColliding object target =
    let
        dist : Vector2
        dist =
            Vector2.subtract object.position target.position
    in
    if not target.enableCollisions || not object.enableCollisions then
        False

    else
        (dist |> Vector2.multiply dist |> Vector2.sum) <= (object.radius + target.radius) ^ 2


{-| if object is colliding with target, run function f on object and return
-}
collisionAction : (PhysicsObject a -> PhysicsObject a) -> List (PhysicsObject b) -> PhysicsObject a -> PhysicsObject a
collisionAction f targets object =
    let
        collisions =
            List.filter (isColliding object) targets
    in
    List.foldl (\_ b -> f b) object collisions


{-| Resolve collision between two objects
-}
resolveCollision : PhysicsObject b -> PhysicsObject a -> PhysicsObject a
resolveCollision target object =
    let
        -- totalMass =
        --     target.mass + object.mass
        -- newVx =
        --     (object.mass - target.mass)
        --         / totalMass
        --         * object.velocity.x
        --         + (2 * target.mass)
        --         / totalMass
        --         * target.velocity.x
        -- newVy =
        --     (object.mass - target.mass)
        --         / totalMass
        --         * object.velocity.y
        --         + (2 * target.mass)
        --         / totalMass
        --         * target.velocity.y
        pos : Vector2
        pos =
            Vector2.add
                (Vector2.singleton (object.radius + target.radius)
                    |> Vector2.multiply (Vector2.direction target.position object.position)
                )
                target.position
    in
    object
        |> setPosition pos
        -- |> (\o -> { o | velocity = Vector2.new newVx newVy })
        |> stop
        |> applyForce
            (Vector2.direction target.position object.position
                -- |> Vector2.add (Vector2.add target.velocity object.velocity)
                -- |> Vector2.scale ((target.mass - object.mass) / totalMass)
                |> Vector2.scale 5
            )


{-| Detect and react to collisions
-}
resolveCollisions : List (PhysicsObject b) -> PhysicsObject a -> PhysicsObject a
resolveCollisions targets object =
    let
        resolve res e =
            resolveCollision res e
    in
    targets
        |> List.filter (isColliding object)
        |> List.foldl resolve object



-- AI


{-| Apply force towards nearest target

If none are present, move home

-}
moveToNearest : List (PhysicsObject b) -> Float -> PhysicsObject a -> PhysicsObject a
moveToNearest targets speed object =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.map .position
                |> List.sortBy (Vector2.distance object.position)
                |> List.head

        force target =
            Vector2.direction object.position target |> Vector2.scale speed
    in
    case nearest of
        Just target ->
            applyForce (force target) object

        Nothing ->
            object


{-| Apply force towards target position if object is more than 5 units away
-}
moveToPosition : (a -> Vector2) -> (PhysicsObject a -> Float) -> PhysicsObject a -> PhysicsObject a
moveToPosition position speed object =
    let
        force =
            if Vector2.distance object.position (position object.state) < 3 then
                Vector2.zero

            else
                Vector2.direction object.position (position object.state) |> Vector2.scale (speed object)
    in
    applyForce force object

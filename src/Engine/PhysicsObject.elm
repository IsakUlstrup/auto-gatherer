module Engine.PhysicsObject exposing
    ( PhysicsObject
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


type alias PhysicsObject a =
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
new : Float -> Float -> Float -> Float -> Int -> a -> PhysicsObject a
new x y size mass id state =
    PhysicsObject
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
        adjustedFriction : Float
        adjustedFriction =
            1 - clamp 0 1 friction
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


setVelocity : Vector2 -> PhysicsObject a -> PhysicsObject a
setVelocity vel object =
    { object | velocity = vel }


stop : PhysicsObject a -> PhysicsObject a
stop object =
    { object | velocity = Vector2.zero }



-- ---- COLLISION ----


{-| Check if two objects are colliding

A collision occurs when the distance between to objects with collisions enabled is less than their combine radii

-}
isColliding : PhysicsObject a -> PhysicsObject b -> Bool
isColliding object target =
    if object.id /= target.id then
        let
            dist : Vector2
            dist =
                Vector2.subtract object.position target.position
        in
        (dist |> Vector2.multiply dist |> Vector2.sum) <= (object.radius + target.radius) ^ 2

    else
        False


{-| if object is colliding with target, run function f on object and return
-}
collisionAction : (PhysicsObject b -> PhysicsObject a -> PhysicsObject a) -> List (PhysicsObject b) -> PhysicsObject a -> PhysicsObject a
collisionAction f targets object =
    let
        collisions : List (PhysicsObject b)
        collisions =
            List.filter (isColliding object) targets

        helper : PhysicsObject b -> PhysicsObject a -> PhysicsObject a
        helper target obj =
            f target obj
    in
    List.foldl helper object collisions


{-| calculate how much each object should move based on the diference in mass

        When resolving collision between a light and a heavy object, the light one moves more

-}
overlapModifier : PhysicsObject b -> PhysicsObject a -> Float
overlapModifier target object =
    (((target.mass - object.mass) / (target.mass + object.mass)) + 1) * 0.5


{-| Resolve collision between two objects
-}
resolveCollision : PhysicsObject b -> PhysicsObject a -> PhysicsObject a
resolveCollision target object =
    let
        dist : Float
        dist =
            Vector2.distance object.position target.position

        overlap : Float
        overlap =
            (dist - object.radius - target.radius) * overlapModifier target object

        normal : Vector2
        normal =
            Vector2.direction object.position target.position

        pos : Vector2
        pos =
            Vector2.add object.position (Vector2.direction object.position target.position |> Vector2.scale overlap)

        k : Vector2
        k =
            Vector2.subtract object.velocity target.velocity

        p : Float
        p =
            2 * (normal.x * k.x + normal.y * k.y) / (object.mass + target.mass)

        v : Vector2
        v =
            Vector2.new (object.velocity.x - p * target.mass * normal.x)
                (object.velocity.y - p * target.mass * normal.y)
    in
    object
        |> setPosition pos
        |> setVelocity v



-- |> (\o -> { o | velocity = Vector2.new newVx newVy })
-- |> stop
-- |> applyForce
--     (Vector2.direction target.position object.position
--         -- |> Vector2.add (Vector2.add target.velocity object.velocity)
--         -- |> Vector2.scale ((target.mass - object.mass) / totalMass)
--         |> Vector2.scale 2
--     )


{-| Detect and react to collisions
-}
resolveCollisions : List (PhysicsObject b) -> PhysicsObject a -> PhysicsObject a
resolveCollisions targets object =
    let
        resolve : PhysicsObject b -> PhysicsObject a -> PhysicsObject a
        resolve res e =
            resolveCollision res e
    in
    targets
        |> List.filter (isColliding object)
        |> List.foldl resolve object



-- AI


{-| Apply force towards nearest target

If none are present, do nothing

-}
moveToNearest : List (PhysicsObject b) -> Float -> PhysicsObject a -> PhysicsObject a
moveToNearest targets speed object =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (\t -> t.id /= object.id)
                |> List.map .position
                |> List.sortBy (Vector2.distance object.position)
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction object.position target |> Vector2.scale speed
    in
    case nearest of
        Just target ->
            applyForce (force target) object

        Nothing ->
            object


{-| Apply force away from nearest target in range

If none are present, do nothing

-}
moveAwayRange : Float -> List (PhysicsObject b) -> Float -> PhysicsObject a -> PhysicsObject a
moveAwayRange range targets speed object =
    let
        nearest : Maybe Vector2
        nearest =
            targets
                |> List.filter (\t -> t.id /= object.id)
                |> List.map .position
                |> List.sortBy (Vector2.distance object.position)
                |> List.filter (\t -> Vector2.distance t object.position < range)
                |> List.head

        force : Vector2 -> Vector2
        force target =
            Vector2.direction object.position target |> Vector2.scale speed |> Vector2.scale -1
    in
    case nearest of
        Just target ->
            applyForce (force target) object

        Nothing ->
            object


{-| Apply force towards target position if object is more than limitDistance units away
-}
moveToPosition : Float -> Vector2 -> Float -> PhysicsObject a -> PhysicsObject a
moveToPosition limitDistance position speed object =
    let
        force : Vector2
        force =
            if Vector2.distance object.position position < limitDistance then
                Vector2.zero

            else
                Vector2.direction object.position position |> Vector2.scale speed
    in
    applyForce force object



-- STATE


updateState : (a -> a) -> PhysicsObject a -> PhysicsObject a
updateState f object =
    { object | state = f object.state }

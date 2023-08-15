module Engine.Particle exposing
    ( Kinematics
    , Particle
    , PhysicsType(..)
    , applyForce
    , applyFriciton
    , collisionAction
    , distance
    , getImpulse
    , getSpeed
    , getVelocity
    , move
    , moveAwayRange
    , moveDirection
    , moveToId
    , moveToNearest
    , moveToPosition
    , new
    , newKinematics
    , resolveCollisions
    , stopIfSlow
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Kinematics =
    { velocity : Vector2
    , acceleration : Vector2
    , impulse : Vector2
    , mass : Float
    , speed : Float
    }


newKinematics : Float -> Float -> Kinematics
newKinematics mass speed =
    Kinematics Vector2.zero Vector2.zero Vector2.zero mass speed


updateKinematics : (Kinematics -> Kinematics) -> Particle a -> Particle a
updateKinematics f particle =
    case particle.physicsType of
        Fixed ->
            particle

        Static k ->
            { particle | physicsType = Static <| f k }

        Dynamic k ->
            { particle | physicsType = Dynamic <| f k }


getSpeed : Particle a -> Float
getSpeed particle =
    case particle.physicsType of
        Fixed ->
            0

        Static k ->
            k.speed

        Dynamic k ->
            k.speed


getVelocity : Particle a -> Vector2
getVelocity particle =
    case particle.physicsType of
        Fixed ->
            Vector2.zero

        Static k ->
            k.velocity

        Dynamic k ->
            k.velocity


getImpulse : Particle a -> Vector2
getImpulse particle =
    case particle.physicsType of
        Fixed ->
            Vector2.zero

        Static k ->
            k.impulse

        Dynamic k ->
            k.impulse


getMass : Particle a -> Float
getMass particle =
    case particle.physicsType of
        Fixed ->
            0

        Static k ->
            k.mass

        Dynamic k ->
            k.mass


{-| Fixed: A particle that can't move and can't collide with other particles. Other particles can collide with a fixed one however

Static: A particle with normal movement that resolves collisions in a static way. It only moves position, no forces are involved

Dynamic: A particle with normal movement and dynamic collision handling

-}
type PhysicsType
    = Fixed
    | Static Kinematics
    | Dynamic Kinematics


type alias Particle a =
    { id : Int
    , position : Vector2
    , radius : Float
    , physicsType : PhysicsType
    , state : a
    }


{-| Particle constructor
-}
new : Float -> Float -> Float -> PhysicsType -> Int -> a -> Particle a
new x y size physics id state =
    Particle
        id
        (Vector2.new x y)
        (clamp 1 562949953421311 size)
        physics
        state



-- FORCES


{-| Apply force to object
-}
applyForce : Vector2 -> Particle a -> Particle a
applyForce force particle =
    let
        forceHelper : Kinematics -> Kinematics
        forceHelper k =
            { k
                | acceleration =
                    force
                        |> Vector2.divide k.mass
                        |> Vector2.add k.acceleration
            }
    in
    updateKinematics forceHelper particle


{-| Apply impulse to object
-}
applyImpulse : Vector2 -> Particle a -> Particle a
applyImpulse impulse particle =
    let
        impulseHelper : Kinematics -> Kinematics
        impulseHelper k =
            { k | impulse = Vector2.add k.impulse impulse }
    in
    updateKinematics impulseHelper particle


{-| Janky friction for now
-}
applyFriciton : Float -> Particle a -> Particle a
applyFriciton friction particle =
    let
        adjustedFriction : Float
        adjustedFriction =
            1 - clamp 0 1 friction

        frictionHelper k =
            { k | velocity = Vector2.scale adjustedFriction k.velocity }
    in
    updateKinematics frictionHelper particle



-- MOVEMENT


{-| Set velocity to zero if velocity magnitude is below limit
-}
stopIfSlow : Float -> Particle a -> Particle a
stopIfSlow limit particle =
    let
        helper k =
            if Vector2.magnitude k.velocity < limit then
                { k | velocity = Vector2.zero }

            else
                k
    in
    updateKinematics helper particle


{-| Move physics particle
-}
move : Float -> Particle a -> Particle a
move dt particle =
    let
        velocity : Kinematics -> Kinematics
        velocity k =
            { k | velocity = Vector2.add k.velocity (Vector2.scale dt k.acceleration) }

        impulse : Particle a -> Particle a
        impulse p =
            case p.physicsType of
                Fixed ->
                    p

                Static k ->
                    { p | position = Vector2.add p.position k.impulse }

                Dynamic k ->
                    { p | position = Vector2.add p.position k.impulse }

        position : Particle a -> Particle a
        position p =
            case p.physicsType of
                Fixed ->
                    p

                Static k ->
                    { p | position = Vector2.add p.position (Vector2.scale dt k.velocity) }

                Dynamic k ->
                    { p | position = Vector2.add p.position (Vector2.scale dt k.velocity) }
    in
    particle
        |> updateKinematics velocity
        |> impulse
        |> position
        |> resetAcceleration
        |> resetImpulse


{-| Reset particle acceleration
-}
resetAcceleration : Particle a -> Particle a
resetAcceleration particle =
    let
        reset k =
            { k | acceleration = Vector2.zero }
    in
    updateKinematics reset particle


{-| Reset particle impulse
-}
resetImpulse : Particle a -> Particle a
resetImpulse particle =
    let
        reset k =
            { k | impulse = Vector2.zero }
    in
    updateKinematics reset particle


setVelocity : Vector2 -> Particle a -> Particle a
setVelocity vel particle =
    updateKinematics (\k -> { k | velocity = vel }) particle


distance : Particle a -> Particle b -> Float
distance p1 p2 =
    Vector2.distance p1.position p2.position - (p1.radius + p2.radius)


isNotEqual : Particle a -> Particle b -> Bool
isNotEqual p1 p2 =
    p1.id /= p2.id



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
    let
        ratio x y =
            (((x - y) / (x + y)) + 1) * 0.5
    in
    case ( target.physicsType, particle.physicsType ) of
        ( _, Fixed ) ->
            0

        ( Fixed, Static _ ) ->
            1

        ( Fixed, Dynamic _ ) ->
            1

        ( Static k1, Static k2 ) ->
            ratio k1.mass k2.mass

        ( Dynamic k1, Static k2 ) ->
            ratio k1.mass k2.mass

        ( Static k1, Dynamic k2 ) ->
            ratio k1.mass k2.mass

        ( Dynamic k1, Dynamic k2 ) ->
            ratio k1.mass k2.mass


{-| Resolve collision without forces
-}
resolveStaticCollision : Particle b -> Particle a -> Particle a
resolveStaticCollision target particle =
    let
        dist : Float
        dist =
            Vector2.distance particle.position target.position

        overlap : Float
        overlap =
            (dist - particle.radius - target.radius) * overlapModifier target particle

        impulse : Vector2
        impulse =
            Vector2.direction particle.position target.position |> Vector2.scale overlap
    in
    particle
        |> applyImpulse impulse


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
            Vector2.subtract (getVelocity particle) (getVelocity target)

        p : Float
        p =
            2 * (normal.x * k.x + normal.y * k.y) / (getMass particle + getMass target)

        v : Vector2
        v =
            Vector2.new ((getVelocity particle).x - p * getMass target * normal.x)
                ((getVelocity particle).y - p * getMass target * normal.y)
    in
    particle
        |> applyImpulse impulse
        |> setVelocity v


resolveCollision : Particle b -> Particle a -> Particle a
resolveCollision target particle =
    case ( target.physicsType, particle.physicsType ) of
        ( _, Fixed ) ->
            particle

        ( Fixed, Static _ ) ->
            resolveStaticCollision target particle

        ( Fixed, Dynamic _ ) ->
            resolveStaticCollision target particle

        ( Static _, Static _ ) ->
            resolveStaticCollision target particle

        ( Dynamic _, Static _ ) ->
            resolveDynamicCollision target particle

        ( Static _, Dynamic _ ) ->
            resolveStaticCollision target particle

        ( Dynamic _, Dynamic _ ) ->
            resolveDynamicCollision target particle


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
            Vector2.direction particle.position target |> Vector2.scale (getSpeed particle)
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
            Vector2.direction particle.position target |> Vector2.scale (getSpeed particle)
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
            direction |> Vector2.normalize |> Vector2.scale (getSpeed particle)
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
                |> Vector2.scale (getSpeed particle)
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
                Vector2.direction particle.position position |> Vector2.scale (getSpeed particle)
    in
    applyForce force particle

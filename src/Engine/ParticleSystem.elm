module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addParticle
    , addParticles
    , collisions
    , filterParticles
    , getParticles
    , getPlayer
    , new
    , updateParticles
    , updateParticlesWithTargets
    , updatePlayer
    )

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Random


type ParticleSystem a
    = ParticleSystem
        { particles : List ( Int, Particle a )
        , player : ( Int, Particle a )
        , idCounter : Int
        , seed : Random.Seed
        }


new : Particle a -> ParticleSystem a
new player =
    ParticleSystem
        { particles = []
        , player = ( 0, player )
        , idCounter = 1
        , seed = Random.initialSeed 2
        }


addParticle : Particle a -> ParticleSystem a -> ParticleSystem a
addParticle particle (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = ( world.idCounter, particle ) :: world.particles
            , idCounter = world.idCounter + 1
        }


addParticles : List (Particle a) -> ParticleSystem a -> ParticleSystem a
addParticles particles system =
    let
        addHelper p =
            addParticle p
    in
    List.foldl addHelper system particles


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = List.map (Tuple.mapSecond f) world.particles
            , player = Tuple.mapSecond f world.player
        }


updatePlayer : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updatePlayer f (ParticleSystem world) =
    ParticleSystem { world | player = Tuple.mapSecond f world.player }


filterParticles : (Particle a -> Bool) -> ParticleSystem a -> ParticleSystem a
filterParticles pred (ParticleSystem system) =
    ParticleSystem { system | particles = List.filter (\p -> pred (Tuple.second p)) system.particles }



-- updateParticlesWithSeed : (Random.Seed -> Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
-- updateParticlesWithSeed f (ParticleSystem world) =
--     let
--         ( randomInt, worldSeed ) =
--             Random.step (Random.int -10000 10000) world.seed
--         helper ( id, p ) =
--             ( id, f (Random.initialSeed (randomInt + id)) p )
--     in
--     ParticleSystem
--         { world
--             | particles = List.map helper world.particles
--             , player = helper world.player
--             , seed = worldSeed
--         }


updateParticlesWithTargets : (List (Particle a) -> Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticlesWithTargets f (ParticleSystem system) =
    let
        targets pid =
            List.filter (\( id, _ ) -> id /= pid) (system.player :: system.particles)
                |> List.map Tuple.second
    in
    ParticleSystem
        { system
            | particles = List.map (\( id, p ) -> ( id, f (targets id) p )) system.particles
            , player = (\( id, p ) -> ( id, f (targets id) p )) system.player
        }


getParticles : ParticleSystem a -> List (Particle a)
getParticles (ParticleSystem world) =
    (world.player
        :: world.particles
    )
        |> List.map Tuple.second


getPlayer : ParticleSystem a -> Particle a
getPlayer (ParticleSystem world) =
    Tuple.second world.player



---- COLLISION ----


{-| Check if two particles are colliding

A collision occurs when the distance between to particles with collisions enabled is less than their combine radii

-}
isColliding : ( Int, Particle a ) -> ( Int, Particle b ) -> Bool
isColliding ( pid, particle ) ( tid, target ) =
    if pid /= tid then
        let
            dist : Vector2
            dist =
                Vector2.subtract particle.position target.position
        in
        (dist |> Vector2.multiply dist |> Vector2.sum) <= (particle.radius + target.radius) ^ 2

    else
        False



-- {-| if particle is colliding with target, run function f on particle and return
-- -}
-- collisionAction : (Particle b -> Particle a -> Particle a) -> List (Particle b) -> Particle a -> Particle a
-- collisionAction f targets particle =
--     let
--         collisions : List (Particle b)
--         collisions =
--             List.filter (isColliding particle) targets
--         helper : Particle b -> Particle a -> Particle a
--         helper target obj =
--             f target obj
--     in
--     List.foldl helper particle collisions


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
        elasticity : Float
        elasticity =
            min target.elasticity particle.elasticity

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
            (2 * (normal.x * k.x + normal.y * k.y) / (particle.mass + target.mass))
                * elasticity

        v : Vector2
        v =
            Vector2.new (particle.velocity.x - p * target.mass * normal.x)
                (particle.velocity.y - p * target.mass * normal.y)
    in
    particle
        |> Particle.applyImpulse impulse
        |> Particle.setVelocity v


{-| Detect and react to collisions
-}
resolveCollisions : List ( Int, Particle b ) -> ( Int, Particle a ) -> ( Int, Particle a )
resolveCollisions targets particle =
    let
        resolve : ( Int, Particle b ) -> ( Int, Particle a ) -> ( Int, Particle a )
        resolve ( _, res ) ( pid, p ) =
            ( pid, resolveDynamicCollision res p )
    in
    targets
        |> List.filter (isColliding particle)
        |> List.foldl resolve particle


collisions : ParticleSystem a -> ParticleSystem a
collisions (ParticleSystem system) =
    ParticleSystem
        { system
            | particles = List.map (resolveCollisions (system.player :: system.particles)) system.particles
            , player = resolveCollisions (system.player :: system.particles) system.player
        }

module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addDynamicParticle
    , addParticles
    , filterParticles
    , getParticles
    , getPlayer
    , new
    , updateParticles
    , updateParticlesWithSeed
    , updatePlayer
    )

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 exposing (Vector2)
import Random


type ParticleSystem a
    = ParticleSystem
        { particles : List ( Int, Particle a )
        , player : ( Int, Particle a )
        , idCounter : Int
        , seed : Random.Seed
        }


new : a -> Float -> ParticleSystem a
new playerState playerSpeed =
    ParticleSystem
        { particles = []
        , player = ( 0, Particle.new 0 0 30 playerSpeed 0 playerState )
        , idCounter = 1
        , seed = Random.initialSeed 2
        }


addDynamicParticle : Float -> Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addDynamicParticle x y size speed state (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = ( world.idCounter, Particle.new x y size speed world.idCounter state ) :: world.particles
            , idCounter = world.idCounter + 1
        }


addParticles : List ( Vector2, a ) -> ParticleSystem a -> ParticleSystem a
addParticles particles system =
    let
        addHelper ( pos, state ) s =
            addDynamicParticle pos.x pos.y 20 0.1 state s
    in
    List.foldl addHelper system particles


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f (ParticleSystem world) =
    ParticleSystem { world | particles = List.map (Tuple.mapSecond f) world.particles, player = Tuple.mapSecond f world.player }


updatePlayer : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updatePlayer f (ParticleSystem world) =
    ParticleSystem { world | player = Tuple.mapSecond f world.player }


filterParticles : (Particle a -> Bool) -> ParticleSystem a -> ParticleSystem a
filterParticles pred (ParticleSystem system) =
    ParticleSystem { system | particles = List.filter (\p -> pred (Tuple.second p)) system.particles }


updateParticlesWithSeed : (Random.Seed -> Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticlesWithSeed f (ParticleSystem world) =
    let
        ( randomInt, worldSeed ) =
            Random.step (Random.int -10000 10000) world.seed

        helper ( id, p ) =
            ( id, f (Random.initialSeed (randomInt + id)) p )
    in
    ParticleSystem
        { world
            | particles = List.map helper world.particles
            , player = helper world.player
            , seed = worldSeed
        }


getParticles : ParticleSystem a -> List (Particle a)
getParticles (ParticleSystem world) =
    world.player
        :: world.particles
        |> List.map Tuple.second


getPlayer : ParticleSystem a -> Particle a
getPlayer (ParticleSystem world) =
    Tuple.second world.player

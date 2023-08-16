module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addDynamicParticle
    , addFixedParticle
    , addStaticParticle
    , getParticles
    , getPlayer
    , new
    , updateParticles
    , updateParticlesWithSeed
    , updatePlayer
    )

import Engine.Particle as Particle exposing (Particle, PhysicsType(..))
import Random


type ParticleSystem a
    = ParticleSystem
        { particles : List (Particle a)
        , player : Particle a
        , idCounter : Int
        , seed : Random.Seed
        }


new : a -> Float -> ParticleSystem a
new playerState playerSpeed =
    ParticleSystem
        { particles = []
        , player = Particle.new 0 0 30 (Dynamic <| Particle.newKinematics 400 playerSpeed) 0 playerState
        , idCounter = 1
        , seed = Random.initialSeed 2
        }


addDynamicParticle : Float -> Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addDynamicParticle x y size speed state (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = Particle.new x y size (Dynamic <| Particle.newKinematics (size * 10) speed) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addStaticParticle : Float -> Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addStaticParticle x y size speed state (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = Particle.new x y size (Static <| Particle.newKinematics (size * 10) speed) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addFixedParticle : Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addFixedParticle x y size state (ParticleSystem world) =
    ParticleSystem
        { world
            | particles = Particle.new x y size Fixed world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f (ParticleSystem world) =
    ParticleSystem { world | particles = List.map f world.particles, player = f world.player }


updateParticlesWithSeed : (Random.Seed -> Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticlesWithSeed f (ParticleSystem world) =
    let
        ( randomInt, worldSeed ) =
            Random.step (Random.int -10000 10000) world.seed

        combinedSeed : Particle a -> Random.Seed
        combinedSeed p =
            Random.initialSeed (randomInt + p.id)
    in
    ParticleSystem
        { world
            | particles = List.map (\p -> f (combinedSeed p) p) world.particles
            , player = f (combinedSeed world.player) world.player
            , seed = worldSeed
        }


updatePlayer : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updatePlayer f (ParticleSystem world) =
    ParticleSystem { world | player = f world.player }


getParticles : ParticleSystem a -> List (Particle a)
getParticles (ParticleSystem world) =
    world.player :: world.particles


getPlayer : ParticleSystem a -> Particle a
getPlayer (ParticleSystem world) =
    world.player

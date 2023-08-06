module Engine.World exposing
    ( World
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


type World a
    = World
        { particles : List (Particle a)
        , player : Particle a
        , idCounter : Int
        , seed : Random.Seed
        }


new : a -> Float -> World a
new playerState playerSpeed =
    World
        { particles = []
        , player = Particle.new 0 0 30 (Dynamic <| Particle.newKinematics 400 playerSpeed) 0 playerState
        , idCounter = 1
        , seed = Random.initialSeed 2
        }


addDynamicParticle : Float -> Float -> Float -> Float -> a -> World a -> World a
addDynamicParticle x y size speed state (World world) =
    World
        { world
            | particles = Particle.new x y size (Dynamic <| Particle.newKinematics (size * 10) speed) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addStaticParticle : Float -> Float -> Float -> Float -> a -> World a -> World a
addStaticParticle x y size speed state (World world) =
    World
        { world
            | particles = Particle.new x y size (Static <| Particle.newKinematics (size * 10) speed) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addFixedParticle : Float -> Float -> Float -> a -> World a -> World a
addFixedParticle x y size state (World world) =
    World
        { world
            | particles = Particle.new x y size Fixed world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> World a -> World a
updateParticles f (World world) =
    World { world | particles = List.map f world.particles, player = f world.player }


updateParticlesWithSeed : (Random.Seed -> Particle a -> Particle a) -> World a -> World a
updateParticlesWithSeed f (World world) =
    let
        ( randomInt, worldSeed ) =
            Random.step (Random.int -10000 10000) world.seed

        combinedSeed : Particle a -> Random.Seed
        combinedSeed p =
            Random.initialSeed (randomInt + p.id)
    in
    World
        { world
            | particles = List.map (\p -> f (combinedSeed p) p) world.particles
            , player = f (combinedSeed world.player) world.player
            , seed = worldSeed
        }


updatePlayer : (Particle a -> Particle a) -> World a -> World a
updatePlayer f (World world) =
    World { world | player = f world.player }


getParticles : World a -> List (Particle a)
getParticles (World world) =
    world.player :: world.particles


getPlayer : World a -> Particle a
getPlayer (World world) =
    world.player

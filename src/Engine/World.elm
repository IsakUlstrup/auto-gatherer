module Engine.World exposing
    ( World
    , addDynamicParticle
    , addFixedParticle
    , addStaticParticle
    , generate
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
        ( seed, worldSeed ) =
            Random.step Random.independentSeed world.seed
    in
    World
        { world
            | particles = List.map (f seed) world.particles
            , player = f seed world.player
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


generate : Random.Generator b -> World a -> ( b, World a )
generate gen (World world) =
    let
        ( val, seed ) =
            Random.step gen world.seed
    in
    ( val, World { world | seed = seed } )

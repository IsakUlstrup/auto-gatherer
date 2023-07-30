module Engine.World exposing
    ( World
    , addParticle
    , addStaticParticle
    , getParticles
    , getPlayer
    , new
    , updateParticles
    )

import Engine.Particle as Particle exposing (Particle)


type World a
    = World
        { particles : List (Particle a)
        , player : Particle a
        , idCounter : Int
        }


new : a -> Float -> World a
new playerState playerSpeed =
    World
        { particles = []
        , player = Particle.new 0 0 30 400 playerSpeed 0 playerState
        , idCounter = 1
        }


addParticle : Float -> Float -> Float -> Float -> a -> World a -> World a
addParticle x y size speed state (World world) =
    World
        { world
            | particles = Particle.new x y size (size * 10) speed world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addStaticParticle : Float -> Float -> Float -> Float -> a -> World a -> World a
addStaticParticle x y size speed state (World world) =
    World
        { world
            | particles = Particle.newStatic x y size (size * 10) speed world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> World a -> World a
updateParticles f (World world) =
    World { world | particles = List.map f world.particles, player = f world.player }


getParticles : World a -> List (Particle a)
getParticles (World world) =
    world.player :: world.particles


getPlayer : World a -> Particle a
getPlayer (World world) =
    world.player

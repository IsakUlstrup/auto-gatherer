module Engine.World exposing
    ( World
    , addParticle
    , addStaticParticle
    , getMap
    , getParticles
    , getPlayer
    , new
    , updateParticles
    )

import Engine.Particle as Particle exposing (Particle, Tile)


type World a b
    = World
        { particles : List (Particle a)
        , player : Particle a
        , map : List (Tile b)
        , idCounter : Int
        }


new : a -> List (Tile b) -> World a b
new playerState map =
    World
        { particles = []
        , player = Particle.new 0 0 30 400 0 playerState
        , map = map
        , idCounter = 1
        }


addParticle : Float -> Float -> Float -> a -> World a b -> World a b
addParticle x y size state (World world) =
    World
        { world
            | particles = Particle.new x y size (size * 10) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


addStaticParticle : Float -> Float -> Float -> a -> World a b -> World a b
addStaticParticle x y size state (World world) =
    World
        { world
            | particles = Particle.newStatic x y size (size * 10) world.idCounter state :: world.particles
            , idCounter = world.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> World a b -> World a b
updateParticles f (World world) =
    World { world | particles = List.map f world.particles, player = f world.player }


getParticles : World a b -> List (Particle a)
getParticles (World world) =
    world.player :: world.particles


getPlayer : World a b -> Particle a
getPlayer (World world) =
    world.player


getMap : World a b -> List (Tile b)
getMap (World world) =
    world.map

module Engine.World exposing
    ( World
    , addParticle
    , getMap
    , getParticles
    , getPlayer
    , new
    , updateParticles
    )

import Engine.Grid exposing (WorldMap)
import Engine.Particle as Particle exposing (Particle)


type World a b
    = World
        { particles : List (Particle a)
        , player : Particle a
        , map : WorldMap b
        , idCounter : Int
        }


new : a -> WorldMap b -> World a b
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


updateParticles : (Particle a -> Particle a) -> World a b -> World a b
updateParticles f (World world) =
    World { world | particles = List.map f world.particles, player = f world.player }


getParticles : World a b -> List (Particle a)
getParticles (World world) =
    world.player :: world.particles


getPlayer : World a b -> Particle a
getPlayer (World world) =
    world.player


getMap : World a b -> WorldMap b
getMap (World world) =
    world.map

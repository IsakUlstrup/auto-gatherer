module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addParticle
    , getMap
    , getParticles
    , getPlayer
    , new
    , updateParticles
    )

import Engine.Grid exposing (WorldMap)
import Engine.Particle as Particle exposing (Particle)


type ParticleSystem a b
    = ParticleSystem
        { particles : List (Particle a)
        , player : Particle a
        , map : WorldMap b
        , idCounter : Int
        }


new : a -> WorldMap b -> ParticleSystem a b
new playerState map =
    ParticleSystem
        { particles = []
        , player = Particle.new 0 0 30 400 0 playerState
        , map = map
        , idCounter = 1
        }


addParticle : Float -> Float -> Float -> a -> ParticleSystem a b -> ParticleSystem a b
addParticle x y size state (ParticleSystem system) =
    ParticleSystem
        { system
            | particles = Particle.new x y size (size * 10) system.idCounter state :: system.particles
            , idCounter = system.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> ParticleSystem a b -> ParticleSystem a b
updateParticles f (ParticleSystem system) =
    ParticleSystem { system | particles = List.map f system.particles, player = f system.player }


getParticles : ParticleSystem a b -> List (Particle a)
getParticles (ParticleSystem system) =
    system.player :: system.particles


getPlayer : ParticleSystem a b -> Particle a
getPlayer (ParticleSystem system) =
    system.player


getMap : ParticleSystem a b -> WorldMap b
getMap (ParticleSystem system) =
    system.map

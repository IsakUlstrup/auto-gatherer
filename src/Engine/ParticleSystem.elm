module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addParticle
    , empty
    , getParticles
    , getPlayer
    , updateParticles
    )

import Engine.Particle as Particle exposing (Particle)


type ParticleSystem a
    = ParticleSystem
        { particles : List (Particle a)
        , player : Particle a
        , idCounter : Int
        }


empty : a -> ParticleSystem a
empty playerState =
    ParticleSystem { particles = [], player = Particle.new 0 0 30 1000 0 playerState, idCounter = 1 }


addParticle : Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addParticle x y size state (ParticleSystem system) =
    ParticleSystem
        { system
            | particles = Particle.new x y size (size * 10) system.idCounter state :: system.particles
            , idCounter = system.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f (ParticleSystem system) =
    ParticleSystem { system | particles = List.map f system.particles, player = f system.player }


getParticles : ParticleSystem a -> List (Particle a)
getParticles (ParticleSystem system) =
    system.player :: system.particles


getPlayer : ParticleSystem a -> Particle a
getPlayer (ParticleSystem system) =
    system.player

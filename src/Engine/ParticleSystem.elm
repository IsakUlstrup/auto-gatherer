module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addParticle
    , empty
    , getParticles
    , updateParticles
    )

import Engine.Particle as Particle exposing (Particle)


type ParticleSystem a
    = ParticleSystem
        { particles : List (Particle a)
        , idCounter : Int
        }


empty : ParticleSystem a
empty =
    ParticleSystem { particles = [], idCounter = 0 }


addParticle : Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addParticle x y size state (ParticleSystem system) =
    ParticleSystem
        { system
            | particles = Particle.new x y size (size * 10) system.idCounter state :: system.particles
            , idCounter = system.idCounter + 1
        }


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f (ParticleSystem system) =
    ParticleSystem { system | particles = List.map f system.particles }


getParticles : ParticleSystem a -> List (Particle a)
getParticles (ParticleSystem system) =
    system.particles

module Engine.ParticleSystem exposing
    ( ParticleSystem
    , addParticle
    , empty
    , updateParticles
    )

import Engine.Particle as Particle exposing (Particle)


type alias ParticleSystem a =
    { particles : List (Particle a)
    , idCounter : Int
    }


empty : ParticleSystem a
empty =
    ParticleSystem [] 0


addParticle : Float -> Float -> Float -> a -> ParticleSystem a -> ParticleSystem a
addParticle x y size state system =
    { system
        | particles = Particle.new x y size (size * 10) system.idCounter state :: system.particles
        , idCounter = system.idCounter + 1
    }


updateParticles : (Particle a -> Particle a) -> ParticleSystem a -> ParticleSystem a
updateParticles f system =
    { system | particles = List.map f system.particles }

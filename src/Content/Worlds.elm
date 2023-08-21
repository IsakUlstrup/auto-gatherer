module Content.Worlds exposing (testWorld1)

import Content.Particles as Particles
import Engine.ParticleSystem as World exposing (ParticleSystem)
import GameParticle exposing (GameParticle)


testWorld1 : ParticleSystem GameParticle
testWorld1 =
    World.new (Particles.followPointer 0 0)
        |> World.addParticle (Particles.wall 200 100)
        |> World.addParticle (Particles.wall 310 100)
        |> World.addParticle (Particles.wall 250 190)
        |> World.addParticle (Particles.followPointer -100 50)
        |> World.addParticle (Particles.followPointer -100 150)
        |> World.addParticle (Particles.followPointer -130 40)

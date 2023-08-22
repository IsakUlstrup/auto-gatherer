module Content.Worlds exposing (testWorld1)

import Content.Particles as Particles
import Engine.ParticleSystem as World exposing (ParticleSystem)
import GameParticle exposing (Component)


testWorld1 : ParticleSystem Component
testWorld1 =
    World.new (Particles.followPointer 0 0)
        |> World.addParticle (Particles.wall 200 100)
        |> World.addParticle (Particles.wall 310 100)
        |> World.addParticle (Particles.wall 250 190)
        |> World.addParticle (Particles.wall 405 195)
        |> World.addParticle (Particles.idle -55 195)
        |> World.addParticle (Particles.dying -55 125)
        |> World.addParticle (Particles.dying -95 125)

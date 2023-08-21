module Content.Worlds exposing (testWorld1)

import Content.Particles as Particles
import Engine.ParticleSystem as World exposing (ParticleSystem)
import ParticleState exposing (GameParticle)


testWorld1 : ParticleSystem GameParticle
testWorld1 =
    World.new Particles.player
        |> World.addParticle (Particles.wall 200 100)
        |> World.addParticle (Particles.wall 300 100)
        |> World.addParticle (Particles.wall 250 190)

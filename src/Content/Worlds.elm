module Content.Worlds exposing (testWorld1)

import Content.Particles as Particles
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.Vector2 as Vector2
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
        |> World.addParticles (Particles.line (Vector2.new -200 -200) (Vector2.new 1 -0.5) 10 (\p -> Particles.wall p.x p.y))
        |> World.addParticles (Particles.line (Vector2.new -200 -99) (Vector2.new 0 1) 10 (\p -> Particles.wall p.x p.y))

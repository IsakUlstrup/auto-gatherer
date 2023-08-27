module Content.Worlds exposing (testWorld1)

import Component exposing (Component)
import Content.Particles as Particles
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.Vector2 as Vector2


testWorld1 : ParticleSystem Component
testWorld1 =
    World.new (Particles.shootAtCursor 0 0)
        |> World.addParticle (Particles.wall 200 100)
        |> World.addParticle (Particles.wall 310 100)
        |> World.addParticle (Particles.wall 250 190)
        |> World.addParticle (Particles.wall 405 195)
        |> World.addParticle (Particles.idle -55 195)
        |> World.addParticle (Particles.idle 55 145)
        |> World.addParticle (Particles.idle -55 145)
        |> World.addParticle (Particles.idle 75 45)
        |> World.addParticle (Particles.idle 155 45)
        |> World.addParticle (Particles.idle -155 -45)
        |> World.addParticle (Particles.idle -175 -145)
        |> World.addParticle (Particles.idle -145 -125)
        |> World.addParticle (Particles.idle -245 -105)
        |> World.addParticle (Particles.idle 245 -95)
        |> World.addParticles (Particles.ring Vector2.zero 40 (\p -> Particles.wall p.x p.y))

module Content.Worlds exposing (testWorld1)

import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.Vector2 as Vector2
import ParticleState exposing (ParticleState(..))


wallParticle : Float -> Float -> Particle ParticleState
wallParticle x y =
    Particle.new (Vector2.new x y) 50 10000 Idle


testWorld1 : ParticleSystem ParticleState
testWorld1 =
    World.new (Particle.new Vector2.zero 20 50 Idle)
        |> World.addParticle (wallParticle 200 100)
        |> World.addParticle (wallParticle 300 100)

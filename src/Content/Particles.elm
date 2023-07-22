module Content.Particles exposing (..)

import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Vector2 as Vector2
import ParticleState exposing (ParticleState(..))


particleSystem1 : ParticleSystem ParticleState
particleSystem1 =
    ParticleSystem.empty (MoveToPosition <| Vector2.zero)
        |> ParticleSystem.addParticle 200 20 40 MoveToCenter
        |> ParticleSystem.addParticle -300 200 70 Idle
        -- |> ParticleSystem.addParticle 0 0 30 (MoveToPosition <| Vector2.new 200 -175)
        |> ParticleSystem.addParticle -97 20 20 MoveToClosest
        |> ParticleSystem.addParticle -100 20 20 MoveToCenter
        |> ParticleSystem.addParticle -101 20 20 MoveToCenter
        |> ParticleSystem.addParticle -102 20 20 MoveToCenter
        |> ParticleSystem.addParticle -103 20 20 MoveToCenter
        |> ParticleSystem.addParticle -104 20 20 MoveToCenter
        |> ParticleSystem.addParticle -150 20 20 MoveToClosest
        |> ParticleSystem.addParticle -150 50 20 MoveToClosest
        |> ParticleSystem.addParticle 150 20 20 MoveToClosest
        |> ParticleSystem.addParticle 0 0 70 Idle
        -- |> ParticleSystem.addParticle -100 -100 30 (MoveToPosition <| Vector2.new 50 -75)
        -- |> ParticleSystem.addParticle 100 100 30 (MoveToPosition <| Vector2.new 150 -75)
        |> ParticleSystem.addParticle 140 100 10 FollowMoveToPosition
        |> ParticleSystem.addParticle 100 -107 8 FollowMoveToPosition
        |> ParticleSystem.addParticle -107 12 9 FollowMoveToPosition
        |> ParticleSystem.addParticle -240 -107 7 FollowMoveToPosition
        |> ParticleSystem.addParticle -340 -107 23 Avoid
        |> ParticleSystem.addParticle 240 -17 18 Avoid

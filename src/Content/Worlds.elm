module Content.Worlds exposing (testWorld1)

import Engine.Vector2 as Vector2
import Engine.World as World exposing (World)
import GameState exposing (Particle(..))


testWorld1 : World Particle
testWorld1 =
    World.new (MoveToPosition <| Vector2.zero)
        |> World.addStaticParticle 200 20 15 0.1 (FollowId 0)
        |> World.addStaticParticle 220 30 15 0.1 (FollowId 1)
        |> World.addStaticParticle 220 35 15 0.1 (FollowId 2)
        |> World.addStaticParticle 190 35 15 0.1 (FollowId 3)
        |> World.addStaticParticle 190 15 15 0.1 (FollowId 4)
        |> World.addParticle -97 20 20 0.08 MoveToClosest
        |> World.addParticle 200 20 40 0.08 MoveToCenter
        |> World.addParticle -300 200 55 0 Idle
        -- |> World.addParticle 0 0 30 (MoveToPosition <| Vector2.new 200 -175)
        |> World.addParticle -100 20 20 0.1 MoveToCenter
        |> World.addParticle -101 20 20 0.12 MoveToCenter
        |> World.addParticle -102 20 20 0.13 MoveToCenter
        |> World.addParticle -103 20 20 0.14 MoveToCenter
        |> World.addParticle -104 20 20 0.15 MoveToCenter
        -- |> World.addParticle -150 20 20 MoveToClosest
        -- |> World.addParticle -150 50 20 MoveToClosest
        |> World.addParticle 150 20 20 0.08 MoveToClosest
        |> World.addParticle 0 0 50 0 Idle
        -- |> World.addParticle -100 -100 30 (MoveToPosition <| Vector2.new 50 -75)
        -- |> World.addParticle 100 100 30 (MoveToPosition <| Vector2.new 150 -75)
        -- |> World.addStaticParticle 180 100 15 FollowMoveToPosition
        -- |> World.addStaticParticle 140 -107 14 FollowMoveToPosition
        -- |> World.addStaticParticle -107 12 13 FollowMoveToPosition
        -- |> World.addStaticParticle -240 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -241 -97 12 0.08 (FollowMoveToPosition 100)
        |> World.addStaticParticle -252 -117 12 0.08 (FollowMoveToPosition 100)
        |> World.addStaticParticle -263 -137 12 0.07 (FollowMoveToPosition 100)
        |> World.addStaticParticle -274 -87 12 0.06 (FollowMoveToPosition 100)
        |> World.addStaticParticle -285 -117 12 0.05 (FollowMoveToPosition 100)
        |> World.addStaticParticle -296 -77 12 0.07 (FollowMoveToPosition 100)
        |> World.addStaticParticle -207 -107 12 0.05 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -248 -107 12 FollowMoveToPosition
        |> World.addParticle -340 -107 23 0.06 Avoid
        |> World.addParticle 240 -17 18 0.05 Avoid

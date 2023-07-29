module Content.Worlds exposing (..)

import Engine.Vector2 as Vector2
import Engine.World as World exposing (World)
import GameState exposing (Particle(..))


testWorld1 : World Particle
testWorld1 =
    World.new (MoveToPosition <| Vector2.zero)
        |> World.addParticle 200 20 40 MoveToCenter
        |> World.addParticle -300 200 55 Idle
        -- |> World.addParticle 0 0 30 (MoveToPosition <| Vector2.new 200 -175)
        |> World.addParticle -97 20 20 MoveToClosest
        |> World.addParticle -100 20 20 MoveToCenter
        |> World.addParticle -101 20 20 MoveToCenter
        |> World.addParticle -102 20 20 MoveToCenter
        |> World.addParticle -103 20 20 MoveToCenter
        |> World.addParticle -104 20 20 MoveToCenter
        |> World.addParticle -150 20 20 MoveToClosest
        |> World.addParticle -150 50 20 MoveToClosest
        |> World.addParticle 150 20 20 MoveToClosest
        |> World.addParticle 0 0 50 Idle
        -- |> World.addParticle -100 -100 30 (MoveToPosition <| Vector2.new 50 -75)
        -- |> World.addParticle 100 100 30 (MoveToPosition <| Vector2.new 150 -75)
        |> World.addStaticParticle 180 100 15 FollowMoveToPosition
        |> World.addStaticParticle 140 -107 14 FollowMoveToPosition
        |> World.addStaticParticle -107 12 13 FollowMoveToPosition
        |> World.addStaticParticle -240 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -241 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -242 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -243 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -244 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -245 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -246 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -247 -107 12 FollowMoveToPosition
        |> World.addStaticParticle -248 -107 12 FollowMoveToPosition
        |> World.addParticle -340 -107 23 Avoid
        |> World.addParticle 240 -17 18 Avoid

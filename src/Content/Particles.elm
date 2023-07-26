module Content.Particles exposing (..)

import Engine.Grid as Grid exposing (WorldMap)
import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Vector2 as Vector2
import ParticleState exposing (ParticleState(..), Tile(..))


particleSystem1 : ParticleSystem ParticleState Tile
particleSystem1 =
    ParticleSystem.new (MoveToPosition <| Vector2.zero) testMap
        |> ParticleSystem.addParticle 200 20 40 MoveToCenter
        |> ParticleSystem.addParticle -300 200 55 Idle
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
        |> ParticleSystem.addParticle 0 0 50 Idle
        -- |> ParticleSystem.addParticle -100 -100 30 (MoveToPosition <| Vector2.new 50 -75)
        -- |> ParticleSystem.addParticle 100 100 30 (MoveToPosition <| Vector2.new 150 -75)
        -- |> ParticleSystem.addParticle 140 100 5 FollowMoveToPosition
        -- |> ParticleSystem.addParticle 100 -107 4 FollowMoveToPosition
        -- |> ParticleSystem.addParticle -107 12 3 FollowMoveToPosition
        -- |> ParticleSystem.addParticle -240 -107 2 FollowMoveToPosition
        |> ParticleSystem.addParticle -340 -107 23 Avoid
        |> ParticleSystem.addParticle 240 -17 18 Avoid


testMap : WorldMap Tile
testMap =
    Grid.empty
        |> Grid.insertTile ( 0, 0 ) Ground
        |> Grid.insertTile ( 1, 0 ) Ground
        |> Grid.insertTile ( 2, 0 ) Ground
        |> Grid.insertTile ( -1, 0 ) Ground
        |> Grid.insertTile ( -2, 0 ) Ground
        |> Grid.insertTile ( -2, 1 ) Ground
        |> Grid.insertTile ( -2, 2 ) Ground
        |> Grid.insertTile ( -2, -1 ) Water
        |> Grid.insertTile ( -3, -1 ) Water
        |> Grid.insertTile ( -3, -2 ) Water
        |> Grid.insertTile ( -3, 0 ) Water
        |> Grid.insertTile ( -2, -2 ) Ground
        |> Grid.insertTile ( -1, -2 ) Ground
        |> Grid.insertTile ( -1, -1 ) Ground
        |> Grid.insertTile ( 2, -1 ) Ground
        |> Grid.insertTile ( 1, -1 ) Ground
        |> Grid.insertTile ( 0, -1 ) Ground
        |> Grid.insertTile ( 0, -2 ) Ground
        |> Grid.insertTile ( 0, -3 ) Ground
        |> Grid.insertTile ( 1, -3 ) Ground
        |> Grid.insertTile ( 1, -2 ) Ground
        |> Grid.insertTile ( -1, -3 ) Ground
        |> Grid.insertTile ( -1, -4 ) Wall
        |> Grid.insertTile ( 0, -4 ) Wall
        |> Grid.insertTile ( 1, -4 ) Wall
        |> Grid.insertTile ( 2, -4 ) Wall
        |> Grid.insertTile ( 2, -3 ) Wall
        |> Grid.insertTile ( 2, -2 ) Wall
        |> Grid.insertTile ( -2, -4 ) Wall
        |> Grid.insertTile ( -2, -3 ) Ground

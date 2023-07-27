module Content.Worlds exposing (..)

import Engine.Particle as Particle exposing (Tile)
import Engine.Vector2 as Vector2
import Engine.World as World exposing (World)
import GameState exposing (Particle(..), TileData(..))


testWorld1 : World Particle TileData
testWorld1 =
    World.new (MoveToPosition <| Vector2.zero) testMap
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
        |> World.addStaticParticle 140 100 15 FollowMoveToPosition
        |> World.addStaticParticle 100 -107 14 FollowMoveToPosition
        |> World.addStaticParticle -107 12 13 FollowMoveToPosition
        |> World.addStaticParticle -240 -107 12 FollowMoveToPosition
        |> World.addParticle -340 -107 23 Avoid
        |> World.addParticle 240 -17 18 Avoid


testMap : List (Tile TileData)
testMap =
    [ Particle.newTile 0 0 Ground
    , Particle.newTile 1 0 Ground
    , Particle.newTile 2 0 Ground
    , Particle.newTile -1 0 Ground
    , Particle.newTile -2 0 Ground
    , Particle.newTile -2 1 Ground
    , Particle.newTile -2 2 Ground
    , Particle.newTile -2 -1 Water
    , Particle.newTile -3 -1 Water
    , Particle.newTile -3 -2 Water
    , Particle.newTile -3 0 Water
    , Particle.newTile -2 -2 Ground
    , Particle.newTile -1 -2 Ground
    , Particle.newTile -1 -1 Ground
    , Particle.newTile 2 -1 Ground
    , Particle.newTile 1 -1 Ground
    , Particle.newTile 0 -1 Ground
    , Particle.newTile 0 -2 Ground
    , Particle.newTile 0 -3 Ground
    , Particle.newTile 1 -3 Ground
    , Particle.newTile 1 -2 Ground
    , Particle.newTile -1 -3 Ground
    , Particle.newTile -1 -4 Wall
    , Particle.newColliderTile 0 -4 Wall
    , Particle.newColliderTile 1 -4 Wall
    , Particle.newColliderTile 2 -4 Wall
    , Particle.newColliderTile 2 -3 Wall
    , Particle.newColliderTile 2 -2 Wall
    , Particle.newColliderTile -2 -4 Wall
    , Particle.newTile -2 -3 Ground
    , Particle.newTile -1 1 Ground
    , Particle.newTile -1 2 Ground
    , Particle.newTile -1 3 Ground
    , Particle.newTile -1 4 Ground
    , Particle.newTile 0 1 Ground
    , Particle.newTile 0 2 Ground
    , Particle.newTile 0 3 Ground
    , Particle.newTile 0 4 Ground
    , Particle.newTile 1 1 Ground
    , Particle.newTile 1 2 Ground
    , Particle.newTile 1 3 Ground
    , Particle.newTile 1 4 Ground
    , Particle.newTile 2 1 Ground
    , Particle.newTile 2 2 Ground
    , Particle.newTile 2 3 Ground
    , Particle.newTile 2 4 Ground
    , Particle.newTile 3 -4 Ground
    , Particle.newTile 3 -3 Ground
    , Particle.newTile 3 -2 Ground
    , Particle.newTile 3 -1 Ground
    , Particle.newTile 3 0 Ground
    , Particle.newTile 3 1 Ground
    , Particle.newTile 3 2 Ground
    , Particle.newTile 3 3 Ground
    , Particle.newTile 3 4 Ground
    , Particle.newTile 4 -4 Ground
    , Particle.newTile 4 -3 Ground
    , Particle.newTile 4 -2 Ground
    , Particle.newTile 4 -1 Ground
    , Particle.newTile 4 0 Ground
    , Particle.newTile 4 1 Ground
    , Particle.newTile 4 2 Ground
    , Particle.newTile 4 3 Ground
    , Particle.newTile 4 4 Ground
    , Particle.newTile -3 1 Water
    , Particle.newTile -3 2 Water
    , Particle.newTile -3 3 Water
    , Particle.newTile -2 3 Water
    , Particle.newTile -3 4 Water
    , Particle.newTile -2 4 Water
    , Particle.newTile -3 -3 Water
    , Particle.newTile -3 -4 Water
    ]

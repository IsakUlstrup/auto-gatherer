module Content.Worlds exposing (testWorld1)

import Content.ParticleState exposing (ParticleState(..))
import Engine.Vector2 as Vector2
import Engine.World as World exposing (World)


{-| A tree is a static particle that slowly returns to target position if pushed
-}
addTree : Float -> Float -> Float -> Float -> World ParticleState -> World ParticleState
addTree x y tx ty =
    World.addStaticParticle x y 20 0.05 (MoveToPosition <| Vector2.new tx ty)


{-| A wall is an immoveable particle with no movement ai
-}
addWall : Float -> Float -> Float -> World ParticleState -> World ParticleState
addWall x y r =
    World.addFixedParticle x y r Idle


testWorld1 : World ParticleState
testWorld1 =
    World.new (MoveToPosition <| Vector2.zero) 0.15
        |> World.addStaticParticle 200 20 15 0.1 (FollowId 0)
        |> World.addStaticParticle 220 30 15 0.1 (FollowId 1)
        |> World.addStaticParticle 220 35 15 0.1 (FollowId 2)
        |> World.addStaticParticle 190 35 15 0.1 (FollowId 3)
        |> World.addStaticParticle 190 15 15 0.1 (FollowId 4)
        -- |> World.addDynamicParticle -97 20 15 0.08 MoveToClosest
        -- |> World.addDynamicParticle 200 20 40 0.08 MoveToCenter
        |> World.addDynamicParticle -300 200 55 0 Idle
        -- |> World.addDynamicParticle -100 20 20 0.1 MoveToCenter
        -- |> World.addDynamicParticle -101 20 20 0.12 MoveToCenter
        -- |> World.addDynamicParticle -102 20 20 0.13 MoveToCenter
        -- |> World.addDynamicParticle -103 20 20 0.14 MoveToCenter
        -- |> World.addDynamicParticle -104 20 20 0.15 MoveToCenter
        -- |> World.addDynamicParticle -150 20 20 MoveToClosest
        -- |> World.addDynamicParticle -150 50 20 MoveToClosest
        -- |> World.addDynamicParticle 150 20 12 0.08 MoveToClosest
        |> World.addDynamicParticle 0 80 50 0 Idle
        |> addTree 200 0 500 -73
        |> World.addStaticParticle -100 -100 20 0.05 (MoveToPosition <| Vector2.new 460 -80)
        |> World.addStaticParticle 100 100 20 0.05 (MoveToPosition <| Vector2.new 450 -75)
        |> World.addStaticParticle 300 -100 20 0.05 (MoveToPosition <| Vector2.new 440 -65)
        |> World.addStaticParticle 350 -120 20 0.05 (MoveToPosition <| Vector2.new 455 -55)
        |> World.addStaticParticle 350 120 20 0.05 (MoveToPosition <| Vector2.new 440 -35)
        |> World.addStaticParticle 180 100 15 0.1 (FollowMoveToPosition 150)
        |> World.addStaticParticle 140 -107 14 0.1 (FollowMoveToPosition 150)
        |> World.addStaticParticle -107 12 13 0.1 (FollowMoveToPosition 150)
        |> World.addStaticParticle -240 -107 12 0.09 (FollowMoveToPosition 150)
        |> World.addStaticParticle -241 -97 12 0.08 (FollowMoveToPosition 100)
        |> World.addStaticParticle -252 -117 12 0.08 (FollowMoveToPosition 100)
        |> World.addStaticParticle -263 -137 12 0.07 (FollowMoveToPosition 100)
        |> World.addStaticParticle -274 -87 12 0.06 (FollowMoveToPosition 100)
        |> World.addStaticParticle -285 -117 12 0.05 (FollowMoveToPosition 100)
        |> World.addStaticParticle -296 -77 12 0.07 (FollowMoveToPosition 100)
        |> World.addStaticParticle -207 -107 12 0.05 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -248 -107 12 FollowMoveToPosition
        |> World.addDynamicParticle -340 -107 23 0.06 Avoid
        |> World.addDynamicParticle 240 -17 18 0.05 Avoid
        |> World.addFixedParticle 23 -400 108 Idle
        |> World.addFixedParticle 170 -420 88 Idle
        |> World.addFixedParticle 250 -390 103 Idle
        |> World.addFixedParticle 350 -450 133 Idle
        |> World.addStaticParticle 341 -97 12 0.08 Idle
        |> World.addStaticParticle 352 -117 12 0.08 Idle
        |> World.addStaticParticle 363 -137 12 0.07 Idle
        |> World.addStaticParticle 374 -87 12 0.06 Idle
        |> World.addStaticParticle 385 -117 12 0.05 Idle
        |> World.addStaticParticle 396 -77 12 0.07 Idle
        |> World.addStaticParticle 307 -107 12 0.05 Idle
        |> addWall 527 -207 102
        |> addWall 587 -307 82
        |> addWall 627 -407 72
        |> addWall -527 -207 92
        |> addWall -527 20 92
        |> addWall -727 -197 92
        |> addWall -727 10 92
        |> World.addDynamicParticle 0 200 50 0 (Meander ( 1000, 1000 ))

module Content.Worlds exposing (testWorld1)

import Content.ParticleState exposing (ParticleState(..))
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.Vector2 as Vector2


{-| A wall is an immoveable particle with no movement ai
-}
addWall : Float -> Float -> Float -> ParticleSystem ParticleState -> ParticleSystem ParticleState
addWall x y r =
    World.addFixedParticle x y r Idle


addMoveToPosition : Float -> Float -> ParticleSystem ParticleState -> ParticleSystem ParticleState
addMoveToPosition x y =
    World.addStaticParticle x y 20 0.05 (MoveToPosition <| Vector2.new x y)


testWorld1 : ParticleSystem ParticleState
testWorld1 =
    World.new Idle 0.2
        |> World.addDynamicParticle 0 80 50 0 Idle
        |> World.addStaticParticle 200 20 15 0.1 (FollowId 1)
        |> World.addStaticParticle 220 30 15 0.1 (FollowId 2)
        |> World.addStaticParticle 220 35 15 0.1 (FollowId 3)
        |> World.addStaticParticle 190 35 15 0.1 (FollowId 4)
        |> World.addStaticParticle 190 15 15 0.1 (FollowId 5)
        |> World.addDynamicParticle -97 20 15 0.08 MoveToClosest
        -- |> World.addDynamicParticle 200 20 40 0.08 MoveToCenter
        |> World.addDynamicParticle -300 200 55 0 Idle
        |> World.addDynamicParticle -100 20 20 0.1 MoveToCenter
        |> World.addDynamicParticle -101 20 20 0.12 MoveToCenter
        |> World.addDynamicParticle -102 20 20 0.13 MoveToCenter
        |> World.addDynamicParticle -103 20 20 0.14 MoveToCenter
        |> World.addDynamicParticle -104 20 20 0.15 MoveToCenter
        -- |> World.addDynamicParticle -150 20 20 MoveToClosest
        -- |> World.addDynamicParticle -150 50 20 MoveToClosest
        -- |> World.addDynamicParticle 150 20 12 0.08 MoveToClosest
        |> addMoveToPosition 310 -100
        |> addMoveToPosition 300 -120
        |> addMoveToPosition 300 -100
        |> addMoveToPosition 350 -120
        |> addMoveToPosition 350 -120
        |> addMoveToPosition 340 -114
        |> addMoveToPosition 330 -100
        |> addMoveToPosition 328 -124
        |> addMoveToPosition 298 -99
        |> addMoveToPosition 288 -103
        |> addMoveToPosition 278 -113
        |> addMoveToPosition 268 -93
        -- |> World.addStaticParticle 180 100 15 0.1 (FollowMoveToPosition 150)
        -- |> World.addStaticParticle 140 -107 14 0.1 (FollowMoveToPosition 150)
        -- |> World.addStaticParticle -107 12 13 0.1 (FollowMoveToPosition 150)
        -- |> World.addStaticParticle -240 -107 12 0.09 (FollowMoveToPosition 150)
        -- |> World.addStaticParticle -241 -97 12 0.08 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -252 -117 12 0.08 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -263 -137 12 0.07 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -274 -87 12 0.06 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -285 -117 12 0.05 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -296 -77 12 0.07 (FollowMoveToPosition 100)
        -- |> World.addStaticParticle -207 -107 12 0.05 (FollowMoveToPosition 100)
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
        |> World.addFixedParticle 23 -400 58 Idle
        |> addWall 527 -207 102
        |> addWall 587 -307 82
        |> addWall 627 -407 72
        |> addWall -527 -207 92
        |> addWall -527 20 92
        |> addWall -727 -197 92
        |> addWall -727 10 92
        |> World.addDynamicParticle 0 200 20 0.1 Meander
        |> World.addDynamicParticle 100 200 20 0.1 Meander
        |> World.addDynamicParticle 200 200 20 0.1 Meander
        |> World.addDynamicParticle 300 200 20 0.1 Meander
        |> World.addDynamicParticle 400 200 20 0.1 Meander
        |> World.addDynamicParticle -400 -400 10 0.1 DestroyOnHit
        |> World.addDynamicParticle -350 -430 10 0.1 DestroyOnHit
        |> World.addDynamicParticle -420 -420 10 0.1 DestroyOnHit
        |> World.addDynamicParticle -440 -390 10 0.1 DestroyOnHit
        |> World.addDynamicParticle -440 -460 10 0.1 DestroyOnHit
        |> World.addDynamicParticle -460 -450 10 0.1 DestroyOnHit

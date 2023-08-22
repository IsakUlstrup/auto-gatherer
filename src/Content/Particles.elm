module Content.Particles exposing (dying, followPointer, idle, wall)

import Color
import Engine.Particle as Particle
import Engine.Vector2 as Vector2
import GameParticle exposing (Component(..), GameParticle)


normalizedColor : Float -> Color.Color
normalizedColor hue =
    Color.new hue 35 60


followPointer : Float -> Float -> GameParticle
followPointer x y =
    Particle.new (Vector2.new x y) 20 50 1 [ Color <| normalizedColor 140, FollowPointer 0.02 ]


idle : Float -> Float -> GameParticle
idle x y =
    Particle.new (Vector2.new x y) 25 50 1 [ Color <| normalizedColor 300 ]


dying : Float -> Float -> GameParticle
dying x y =
    Particle.new (Vector2.new x y) 25 30 0 [ Color <| normalizedColor 350, Die 1000 1000 ]
        |> Particle.applyForce (Vector2.new 1 0)


wall : Float -> Float -> GameParticle
wall x y =
    Particle.new (Vector2.new x y)
        50
        100000
        0
        [ MoveToPosition (Vector2.new x y) 0.5
        , Color <| normalizedColor 200
        ]

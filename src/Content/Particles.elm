module Content.Particles exposing (dying, followPointer, idle, wall)

import Color
import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2
import GameParticle exposing (Component(..), GameParticle)


normalizedColor : Float -> Color.Color
normalizedColor hue =
    Color.new hue 35 60


followPointer : Float -> Float -> Particle GameParticle
followPointer x y =
    Particle.new (Vector2.new x y) 20 50 1 [ Color <| normalizedColor 140, FollowPointer 0.02 ]


idle : Float -> Float -> Particle GameParticle
idle x y =
    Particle.new (Vector2.new x y) 25 50 1 [ Color <| normalizedColor 300 ]


dying : Float -> Float -> Particle GameParticle
dying x y =
    Particle.new (Vector2.new x y) 25 30 1 [ Color <| normalizedColor 350, Die 1000 ]


wall : Float -> Float -> Particle GameParticle
wall x y =
    Particle.new (Vector2.new x y)
        50
        100000
        0
        [ MoveToPosition (Vector2.new x y) 0.5
        , Color <| normalizedColor 200
        ]

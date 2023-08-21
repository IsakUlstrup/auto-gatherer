module Content.Particles exposing (followPointer, wall)

import Color
import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2
import GameParticle exposing (Component(..), GameParticle)


followPointer : Float -> Float -> Particle GameParticle
followPointer x y =
    Particle.new (Vector2.new x y) 20 50 [ Color <| Color.new 100 50 50, FollowPointer ]


wall : Float -> Float -> Particle GameParticle
wall x y =
    Particle.new (Vector2.new x y)
        50
        1000
        [ MoveToPosition <| Vector2.new x y
        , Color <| Color.new 200 50 50
        ]

module Content.Particles exposing
    ( dying
    , followPointer
    , idle
    , line
    , ring
    , shootAtCursor
    , wall
    )

import Color
import Component exposing (Component(..))
import Engine.Particle as Particle exposing (Particle)
import Engine.Progress as Progress
import Engine.Vector2 as Vector2 exposing (Vector2)


normalizedColor : Float -> Color.Color
normalizedColor hue =
    Color.new hue 35 60


followPointer : Float -> Float -> Particle Component
followPointer x y =
    Particle.new (Vector2.new x y) 20 50 1 [ Color <| normalizedColor 140, FollowPointer 0.02 ]


shootAtCursor : Float -> Float -> Particle Component
shootAtCursor x y =
    Particle.new (Vector2.new x y) 20 50 1 [ Color <| normalizedColor 105, FireParticleAtCursor (Progress.new 1000) (dying 0 0) ]


idle : Float -> Float -> Particle Component
idle x y =
    Particle.new (Vector2.new x y) 25 50 1 [ Color <| normalizedColor 300 ]


dying : Float -> Float -> Particle Component
dying x y =
    Particle.new (Vector2.new x y) 10 5 0 [ Color <| Color.new 350 100 100, Die (Progress.new 1000) ]


wall : Float -> Float -> Particle Component
wall x y =
    Particle.new (Vector2.new x y)
        50
        100000
        0
        [ MoveToPosition (Vector2.new x y) 0.5
        , Color <| normalizedColor 200
        ]


{-| Create a line of particles that go from start point in provided direction.

Length is determined by amount of particles and particle radius

-}
line : Vector2 -> Vector2 -> Int -> (Vector2 -> Particle Component) -> List (Particle Component)
line start direction amount particle =
    List.range 0 amount
        |> List.map (\index -> particle (Vector2.add start (Vector2.scale (toFloat index * ((particle Vector2.zero).radius * 2)) (Vector2.normalize direction))))


ring : Vector2 -> Int -> (Vector2 -> Particle Component) -> List (Particle Component)
ring center amount particle =
    let
        particleRadius =
            particle Vector2.zero |> .radius

        pos i =
            let
                angle =
                    toFloat i * pi / (toFloat amount / 2)
            in
            center
                |> Vector2.add (Vector2.scale (particleRadius * (toFloat amount * 0.32)) (Vector2.new (sin angle) (cos angle)))
                |> particle
    in
    List.range 1 amount
        |> List.map pos

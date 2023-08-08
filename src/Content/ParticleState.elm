module Content.ParticleState exposing (ParticleState(..), particleForce)

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Random


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition Float
    | MoveToClosest
    | Idle
    | Avoid
    | FollowId Int
    | Meander
    | MoveAwayAngle Bool Float


particleForce : List (Particle ParticleState) -> Random.Seed -> Particle ParticleState -> Particle ParticleState
particleForce particles seed particle =
    case particle.state of
        MoveToCenter ->
            Particle.moveToPosition 50 Vector2.zero particle

        MoveToPosition p ->
            Particle.moveToPosition 50 p particle

        FollowMoveToPosition range ->
            let
                followTarget : Particle.Particle ParticleState -> Bool
                followTarget t =
                    case t.state of
                        MoveToPosition _ ->
                            True

                        _ ->
                            False

                isInRange p =
                    Particle.distance p particle < range
            in
            Particle.moveToNearest 50 (particles |> List.filter followTarget |> List.filter isInRange) particle

        MoveToClosest ->
            Particle.moveToNearest 50 particles particle

        Idle ->
            particle

        Avoid ->
            Particle.moveAwayRange 100 particles particle

        FollowId id ->
            Particle.moveToId 5 id particles particle

        Meander ->
            Particle.applyForce (Random.step Vector2.random seed |> Tuple.first |> Vector2.scale 0.1) particle

        MoveAwayAngle moveFlag angle ->
            if moveFlag then
                Particle.moveAwayAngle angle particle

            else
                particle

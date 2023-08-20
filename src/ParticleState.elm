module ParticleState exposing (ParticleState(..), particleForce, stateUpdate, toString)

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition Float
    | MoveToClosest
    | Idle
    | Avoid
    | Meander
    | DestroyOnHit
    | Summon Float Float
    | DieCooldown Float


particleForce : List (Particle ParticleState) -> Particle ParticleState -> Particle ParticleState
particleForce particles particle =
    case particle.state of
        MoveToCenter ->
            Particle.moveToPosition 0.1 50 Vector2.zero particle

        MoveToPosition p ->
            Particle.moveToPosition 0.1 2 p particle

        FollowMoveToPosition range ->
            let
                followTarget : Particle.Particle ParticleState -> Bool
                followTarget t =
                    case t.state of
                        MoveToPosition _ ->
                            True

                        _ ->
                            False

                isInRange : Particle ParticleState -> Bool
                isInRange p =
                    Particle.distance p particle < range
            in
            Particle.moveToNearest 0.1 50 (particles |> List.filter followTarget |> List.filter isInRange) particle

        MoveToClosest ->
            Particle.moveToNearest 0.1 50 particles particle

        Idle ->
            particle

        Avoid ->
            Particle.moveAwayRange 0.1 100 particles particle

        Meander ->
            Particle.applyForce Vector2.zero particle

        DestroyOnHit ->
            particle

        Summon _ _ ->
            particle

        DieCooldown _ ->
            particle


stateUpdate : Float -> Particle ParticleState -> Particle ParticleState
stateUpdate dt particle =
    case particle.state of
        Summon cd maxCd ->
            if cd <= 0 then
                { particle | state = Summon maxCd maxCd }

            else
                { particle | state = Summon (max 0 (cd - dt)) maxCd }

        DieCooldown cd ->
            { particle | state = DieCooldown <| max 0 (cd - dt) }

        _ ->
            particle


toString : ParticleState -> String
toString particle =
    case particle of
        MoveToCenter ->
            "move-center"

        MoveToPosition _ ->
            "move-to"

        FollowMoveToPosition _ ->
            "follow-move-to"

        MoveToClosest ->
            "move-closest"

        Idle ->
            "idle"

        Avoid ->
            "avoid"

        Meander ->
            "meander"

        DestroyOnHit ->
            "destroy-on-hit"

        Summon _ _ ->
            "summon"

        DieCooldown _ ->
            "die-cooldown"

module Content.ParticleState exposing (ParticleState(..), particleForce, updateState)

import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition Float
    | MoveToClosest
    | Idle
    | Avoid
    | FollowId Int
    | Meander ( Float, Float )


particleForce : Vector2 -> List (Particle ParticleState) -> Particle ParticleState -> Particle ParticleState
particleForce randomVector particles particle =
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

        Meander ( cd, _ ) ->
            let
                setCooldown p =
                    case p.state of
                        Meander ( _, maxCd ) ->
                            { p | state = Meander <| ( maxCd, maxCd ) }

                        _ ->
                            p
            in
            if cd <= 0 then
                Particle.applyForce randomVector particle
                    |> setCooldown

            else
                particle


updateState : Float -> Particle ParticleState -> Particle ParticleState
updateState dt particle =
    case particle.state of
        Meander ( cd, maxCd ) ->
            { particle | state = Meander <| ( max 0 (cd - dt), maxCd ) }

        _ ->
            particle

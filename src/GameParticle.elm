module GameParticle exposing
    ( Component(..)
    , GameParticle
    , addComponent
    , componentToString
    , componentTypeToString
    , keepParticle
    , particleForce
    , stateUpdate
    )

import Color exposing (Color)
import Engine.Particle as Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Pointer exposing (Pointer)


type alias GameParticle =
    List Component


type Component
    = MoveToPosition Vector2 Float
    | FollowPointer Float
    | Avoid Float
    | Color Color
    | Hit Float
    | Die Float Float


addComponent : Component -> Particle GameParticle -> Particle GameParticle
addComponent component particle =
    { particle | state = component :: particle.state }


componentForce : Pointer -> List (Particle GameParticle) -> Particle GameParticle -> Component -> Vector2
componentForce pointer particles particle component =
    case component of
        MoveToPosition position forceMulti ->
            Particle.moveToPosition forceMulti 5 position particle

        FollowPointer forceMulti ->
            if pointer.pressed then
                Vector2.direction particle.position pointer.position
                    |> Vector2.scale forceMulti

            else
                Vector2.zero

        Avoid forceMulti ->
            Particle.moveAwayRange forceMulti 100 particles particle

        Color _ ->
            Vector2.zero

        Hit _ ->
            Vector2.zero

        Die _ _ ->
            Vector2.zero


particleForce : Pointer -> List (Particle GameParticle) -> Particle GameParticle -> Particle GameParticle
particleForce pointer particles particle =
    let
        sumForces =
            List.foldl (\comp force -> Vector2.add force (componentForce pointer particles particle comp)) Vector2.zero particle.state
    in
    Particle.applyForce sumForces particle


componentToString : Component -> String
componentToString component =
    case component of
        MoveToPosition pos forceMulti ->
            "MoveToPosition " ++ Vector2.toString pos ++ " " ++ String.fromFloat forceMulti

        FollowPointer forceMulti ->
            "FollowPointer " ++ String.fromFloat forceMulti

        Avoid forceMulti ->
            "Avoid " ++ String.fromFloat forceMulti

        Color color ->
            "Color " ++ Color.toString color

        Hit duration ->
            "Hit " ++ String.fromFloat duration

        Die duration maxDuration ->
            "Die " ++ String.fromFloat duration ++ " " ++ String.fromFloat maxDuration


componentTypeToString : Component -> String
componentTypeToString component =
    case component of
        MoveToPosition _ _ ->
            "move-to-position"

        FollowPointer _ ->
            "follow-pointer"

        Avoid _ ->
            "avoid"

        Color _ ->
            "color"

        Hit _ ->
            "hit"

        Die _ _ ->
            "die"



-- case particle.state of
--     MoveToCenter ->
--         Particle.moveToPosition 0.1 50 Vector2.zero particle
--     MoveToPosition p ->
--         Particle.moveToPosition 0.1 2 p particle
--     FollowMoveToPosition range ->
--         let
--             followTarget : Particle.Particle Component -> Bool
--             followTarget t =
--                 case t.state of
--                     MoveToPosition _ ->
--                         True
--                     _ ->
--                         False
--             isInRange : Particle Component -> Bool
--             isInRange p =
--                 Particle.distance p particle < range
--         in
--         Particle.moveToNearest 0.1 50 (particles |> List.filter followTarget |> List.filter isInRange) particle
--     MoveToClosest ->
--         Particle.moveToNearest 0.1 50 particles particle
--     Idle ->
--         particle
--     Avoid ->
--         Particle.moveAwayRange 0.1 100 particles particle
--     Meander ->
--         Particle.applyForce Vector2.zero particle
--     DestroyOnHit ->
--         particle
--     Summon _ _ ->
--         particle
--     DieCooldown _ ->
--         particle
-- MoveToPosition Vector2
--     | FollowPointer
--     | Avoid
--     | Color Color
--     | Hit Float


stateUpdate : Float -> Particle GameParticle -> Particle GameParticle
stateUpdate dt particle =
    let
        updateComponent c =
            case c of
                MoveToPosition _ _ ->
                    c

                FollowPointer _ ->
                    c

                Avoid _ ->
                    c

                Color _ ->
                    c

                Hit duration ->
                    Hit (max 0 (duration - dt))

                Die duration maxDuration ->
                    Die (max 0 (duration - dt)) maxDuration

        filterComponent c =
            case c of
                MoveToPosition _ _ ->
                    True

                FollowPointer _ ->
                    True

                Avoid _ ->
                    True

                Color _ ->
                    True

                Hit duration ->
                    duration > 0

                Die _ _ ->
                    True
    in
    { particle | state = particle.state |> List.map updateComponent |> List.filter filterComponent }


{-| Check if particle should stay alive or be removed
-}
keepParticle : Particle GameParticle -> Bool
keepParticle particle =
    let
        keep c =
            case c of
                MoveToPosition _ _ ->
                    True

                FollowPointer _ ->
                    True

                Avoid _ ->
                    True

                Color _ ->
                    True

                Hit _ ->
                    True

                Die duration _ ->
                    duration > 0
    in
    particle.state |> List.map keep |> List.all ((==) True)



-- case particle.state of
--     Summon cd maxCd ->
--         if cd <= 0 then
--             { particle | state = Summon maxCd maxCd }
--         else
--             { particle | state = Summon (max 0 (cd - dt)) maxCd }
--     DieCooldown cd ->
--         { particle | state = DieCooldown <| max 0 (cd - dt) }
--     _ ->
--         particle

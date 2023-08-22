module GameParticle exposing
    ( Component(..)
    , GameParticle
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
    Particle Component


type Component
    = MoveToPosition Vector2 Float
    | FollowPointer Float
    | Avoid Float
    | Color Color
    | Hit Float
    | Die Float Float


componentForce : Pointer -> List GameParticle -> GameParticle -> Component -> Vector2
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


particleForce : Pointer -> List GameParticle -> GameParticle -> GameParticle
particleForce pointer particles particle =
    let
        sumForces =
            List.foldl (\comp force -> Vector2.add force (componentForce pointer particles particle comp)) Vector2.zero particle.components
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


stateUpdate : Float -> GameParticle -> GameParticle
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
    { particle | components = particle.components |> List.map updateComponent |> List.filter filterComponent }


{-| Check if particle should stay alive or be removed
-}
keepParticle : GameParticle -> Bool
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
    particle.components |> List.map keep |> List.all ((==) True)



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

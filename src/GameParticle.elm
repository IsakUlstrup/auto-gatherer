module GameParticle exposing
    ( Component(..)
    , GameParticle
    , componentForce
    , componentToString
    , componentTypeToString
    , keepParticle
    )

import Color exposing (Color)
import Engine.Particle as Particle exposing (Particle)
import Engine.Progress as Progress exposing (Progress)
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
    | Die Progress
    | FireParticleAtCursor Progress GameParticle


componentForce : Pointer -> List GameParticle -> GameParticle -> Component -> Vector2
componentForce pointer particles parent component =
    case component of
        MoveToPosition position forceMulti ->
            Particle.moveToPosition forceMulti 5 position parent

        FollowPointer forceMulti ->
            if pointer.pressed then
                Vector2.direction parent.position pointer.position
                    |> Vector2.scale forceMulti

            else
                Vector2.zero

        Avoid forceMulti ->
            Particle.moveAwayRange forceMulti 100 particles parent

        Color _ ->
            Vector2.zero

        Hit _ ->
            Vector2.zero

        Die _ ->
            Vector2.zero

        FireParticleAtCursor progress p ->
            if Progress.isDone progress && pointer.pressed then
                Vector2.direction parent.position pointer.position |> Vector2.scale -(p.mass * 0.01)

            else
                Vector2.zero


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

        Die progress ->
            "Die " ++ Progress.toString progress

        FireParticleAtCursor progress _ ->
            "FireParticleAtCursor " ++ Progress.toString progress


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

        Die _ ->
            "die"

        FireParticleAtCursor _ _ ->
            "fire-particle-at-cursor"


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

                Die progress ->
                    Progress.isNotDone progress

                FireParticleAtCursor _ _ ->
                    True
    in
    particle.components |> List.map keep |> List.all ((==) True)

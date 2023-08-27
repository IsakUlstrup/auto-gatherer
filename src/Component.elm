module Component exposing
    ( Component(..)
    , componentToString
    , componentTypeToString
    , tickComponent
    )

import Color exposing (Color)
import Engine.Particle exposing (Particle)
import Engine.Progress as Progress exposing (Progress)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Pointer exposing (Pointer)


type Component
    = MoveToPosition Vector2 Float
    | FollowPointer Float
    | Avoid Float
    | Color Color
    | Hit Float
    | Die Progress
    | FireParticleAtCursor Progress (Particle Component)


tickComponent : Pointer -> Float -> Component -> Component
tickComponent pointer dt component =
    case component of
        Hit duration ->
            Hit (max 0 (duration - dt))

        Die progress ->
            Die <| Progress.tick dt progress

        FireParticleAtCursor progress p ->
            if Progress.isDone progress && pointer.pressed then
                FireParticleAtCursor (Progress.reset progress) p

            else
                FireParticleAtCursor (Progress.tick dt progress) p

        _ ->
            component


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

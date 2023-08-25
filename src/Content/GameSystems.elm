module Content.GameSystems exposing (collisionInteraction, cull, forces, movement, spawn, state)

import Engine.Particle as Particle
import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Progress as Progress
import Engine.Vector2 as Vector2
import GameParticle exposing (Component(..), GameParticle)
import Pointer exposing (Pointer)



-- SYSTEM


forces : Pointer -> ParticleSystem Component -> ParticleSystem Component
forces pointer world =
    let
        worldPointer =
            { pointer | position = pointer.position |> Vector2.add (ParticleSystem.getPlayer world |> .position) }
    in
    ParticleSystem.updateParticlesWithTargets (\targets -> Particle.applyComponentForce (GameParticle.componentForce worldPointer targets)) world


movement : Float -> ParticleSystem Component -> ParticleSystem Component
movement dt system =
    ParticleSystem.updateParticles (Particle.move dt >> Particle.applyFriciton 0.015 >> Particle.stopIfSlow 0.0001) system


collisionInteraction : ParticleSystem Component -> ParticleSystem Component
collisionInteraction system =
    ParticleSystem.collisionAction (\_ p -> Particle.addComponent (Hit 100) p) system


stateUpdate : Pointer -> Float -> GameParticle -> GameParticle
stateUpdate pointer dt particle =
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

                Die progress ->
                    Die <| Progress.tick dt progress

                FireParticleAtCursor progress p ->
                    if Progress.isDone progress && pointer.pressed then
                        FireParticleAtCursor (Progress.reset progress) p

                    else
                        FireParticleAtCursor (Progress.tick dt progress) p

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

                Die _ ->
                    True

                FireParticleAtCursor _ _ ->
                    True
    in
    { particle | components = particle.components |> List.map updateComponent |> List.filter filterComponent }


state : Pointer -> Float -> ParticleSystem Component -> ParticleSystem Component
state pointer dt system =
    ParticleSystem.updateParticles (stateUpdate pointer dt) system


spawn : Pointer -> ParticleSystem Component -> ParticleSystem Component
spawn pointer system =
    let
        worldPointer =
            { pointer | position = pointer.position |> Vector2.add (ParticleSystem.getPlayer system |> .position) }

        readySpawn : GameParticle -> Component -> Maybe GameParticle
        readySpawn p c =
            case c of
                FireParticleAtCursor progress particle ->
                    if Progress.isDone progress && pointer.pressed then
                        Just
                            ({ particle
                                | position =
                                    Vector2.add p.position
                                        (Vector2.direction p.position worldPointer.position
                                            |> Vector2.scale (p.radius + particle.radius)
                                        )
                             }
                                |> (\part -> Particle.applyForce (Vector2.direction p.position part.position |> Vector2.scale (particle.mass * 0.01)) part)
                            )

                    else
                        Nothing

                _ ->
                    Nothing

        readySummoner p =
            List.filterMap (readySpawn p) p.components

        ready : List GameParticle
        ready =
            ParticleSystem.getParticles system
                |> List.concatMap readySummoner
    in
    ParticleSystem.addParticles ready system


cull : ParticleSystem Component -> ParticleSystem Component
cull system =
    ParticleSystem.filterParticles GameParticle.keepParticle system

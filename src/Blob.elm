module Blob exposing
    ( Blob
    , ai
    , incrementHits
    , isResting
    , new
    , reduceEnergy
    , resetEnergy
    , update
    )

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 exposing (Vector2)


type EnergyState
    = Resting Float
    | Energy Int


type alias Blob =
    PhysicsObject
        { hitCount : Int
        , trail : List Vector2
        , energy : EnergyState
        }


new : Float -> Float -> Float -> Float -> Blob
new x y radius mass =
    PhysicsObject.new x y radius mass { hitCount = 0, trail = [], energy = Energy 10 }


incrementHits : Blob -> Blob
incrementHits blob =
    PhysicsObject.updateState (\s -> { s | hitCount = blob.state.hitCount + 1 }) blob


reduceEnergy : Blob -> Blob
reduceEnergy blob =
    case blob.state.energy of
        Energy e ->
            if e - 1 <= 0 then
                blob
                    |> PhysicsObject.setcollisionState False
                    |> PhysicsObject.updateState (\s -> { s | energy = Resting 5000 })

            else
                blob
                    |> PhysicsObject.updateState (\s -> { s | energy = Energy <| max 0 (e - 1) })

        Resting _ ->
            blob


tickRest : Float -> Blob -> Blob
tickRest dt blob =
    case blob.state.energy of
        Resting e ->
            if e - dt <= 0 then
                resetEnergy blob

            else
                PhysicsObject.updateState (\s -> { s | energy = Resting (max 0 (e - dt)) }) blob

        Energy _ ->
            blob


resetEnergy : Blob -> Blob
resetEnergy blob =
    blob
        |> PhysicsObject.setcollisionState True
        |> PhysicsObject.updateState (\s -> { s | energy = Energy 10 })


isResting : Blob -> Bool
isResting blob =
    case blob.state.energy of
        Resting _ ->
            True

        Energy _ ->
            False


update : Float -> Blob -> Blob
update dt blob =
    blob
        |> PhysicsObject.updateState (\s -> { s | trail = blob.position :: blob.state.trail |> List.take 20 })
        |> tickRest dt


ai : PhysicsObject a -> List (PhysicsObject b) -> Float -> Blob -> Blob
ai home resources speed blob =
    if (isResting >> not) blob && (List.isEmpty >> not) resources then
        PhysicsObject.moveToNearest resources speed blob

    else
        PhysicsObject.moveToPosition (always home.position) (always speed) blob

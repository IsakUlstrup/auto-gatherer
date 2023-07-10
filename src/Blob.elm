module Blob exposing (Blob, addTrail, ai, incrementHits, new, reduceEnergy, resetEnergy)

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 exposing (Vector2)


type alias Blob =
    PhysicsObject
        { hitCount : Int
        , trail : List Vector2
        , energy : Int
        }


new : Float -> Float -> Float -> Float -> Blob
new x y radius mass =
    PhysicsObject.new x y radius mass { hitCount = 0, trail = [], energy = 10 }


incrementHits : Blob -> Blob
incrementHits blob =
    PhysicsObject.updateState (\s -> { s | hitCount = blob.state.hitCount + 1 }) blob


reduceEnergy : Blob -> Blob
reduceEnergy blob =
    if blob.state.energy - 1 <= 0 then
        blob
            |> PhysicsObject.setcollisionState False
            |> PhysicsObject.updateState (\s -> { s | energy = 0 })

    else
        PhysicsObject.updateState (\s -> { s | energy = max 0 (s.energy - 1) }) blob


resetEnergy : Blob -> Blob
resetEnergy blob =
    blob
        |> PhysicsObject.setcollisionState True
        |> PhysicsObject.updateState (\s -> { s | energy = 10 })


addTrail : Blob -> Blob
addTrail blob =
    PhysicsObject.updateState (\s -> { s | trail = blob.position :: blob.state.trail |> List.take 20 }) blob


ai : List (PhysicsObject b) -> Float -> Blob -> Blob
ai resources speed blob =
    if blob.state.energy > 0 then
        PhysicsObject.moveToNearest resources speed blob

    else
        blob

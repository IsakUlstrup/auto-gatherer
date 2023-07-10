module Blob exposing (Blob, addTrail, incrementHits, new)

import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 exposing (Vector2)


type alias Blob =
    PhysicsObject
        { hitCount : Int
        , trail : List Vector2
        }


new : Float -> Float -> Float -> Float -> Blob
new x y radius mass =
    PhysicsObject.new x y radius mass { hitCount = 0, trail = [] }


incrementHits : Blob -> Blob
incrementHits blob =
    PhysicsObject.updateState (\s -> { s | hitCount = blob.state.hitCount + 1 }) blob


addTrail : Blob -> Blob
addTrail blob =
    PhysicsObject.updateState (\s -> { s | trail = blob.position :: blob.state.trail |> List.take 20 }) blob

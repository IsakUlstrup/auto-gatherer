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
    { blob
        | state =
            { hitCount = blob.state.hitCount + 1
            , trail = blob.state.trail
            }
    }


addTrail : Blob -> Blob
addTrail blob =
    { blob
        | state =
            { hitCount = blob.state.hitCount
            , trail = blob.position :: blob.state.trail |> List.take 20
            }
    }

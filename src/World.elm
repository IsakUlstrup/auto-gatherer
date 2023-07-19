module World exposing (World, collisionInteraction, collisionResolution, movement, stateUpdate)

import Blob exposing (Blob)
import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Resource exposing (Resource)


type alias World =
    { blobs : List Blob
    , resources : List Resource
    , player : PhysicsObject Vector2
    }


movement : Float -> World -> World
movement dt world =
    let
        f : PhysicsObject a -> PhysicsObject a
        f =
            PhysicsObject.applyFriciton 0.05
                >> PhysicsObject.move dt
                >> PhysicsObject.stopIfSlow 0.0001
    in
    { world
        | blobs = List.map f world.blobs
        , resources = List.map f world.resources
        , player = f world.player
    }


collisionInteraction : World -> World
collisionInteraction world =
    { world
        | blobs =
            List.map
                (PhysicsObject.collisionAction
                    (\target object ->
                        object
                            |> Blob.reduceEnergy
                            |> PhysicsObject.applyForce (Vector2.direction target.position object.position)
                    )
                    world.resources
                )
                world.blobs
        , resources =
            List.map
                (PhysicsObject.collisionAction
                    (\_ object ->
                        Resource.hit object
                    )
                    world.blobs
                )
                world.resources
        , player =
            PhysicsObject.collisionAction
                (\target object ->
                    object
                        |> PhysicsObject.applyForce (Vector2.direction target.position object.position)
                )
                world.resources
                world.player
    }


collisionResolution : World -> World
collisionResolution world =
    { world
        | blobs = List.map (PhysicsObject.resolveCollisions world.resources) world.blobs
        , resources = List.map (PhysicsObject.resolveCollisions world.blobs) world.resources
        , player = PhysicsObject.resolveCollisions world.resources world.player
    }


stateUpdate : Float -> World -> World
stateUpdate dt world =
    { world
        | blobs = List.map (Blob.update dt) world.blobs
        , resources = List.map (Resource.update dt) world.resources
    }

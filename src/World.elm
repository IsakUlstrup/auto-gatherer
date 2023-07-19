module World exposing
    ( World
    , addBlob
    , addResource
    , collisionInteraction
    , collisionResolution
    , fixedUpdate
    , forces
    , movement
    , stateUpdate
    , updateBlobs
    , updatePlayer
    , updateResources
    )

import Blob exposing (Blob)
import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Resource exposing (Resource)


type alias World =
    { blobs : List Blob
    , resources : List Resource
    , player : PhysicsObject Vector2
    , physicsStepTime : Float
    , physicsStepAccumulator : Float
    }


addResource : Resource -> World -> World
addResource resource world =
    { world | resources = resource :: world.resources }


updateResources : (Resource -> Resource) -> World -> World
updateResources f world =
    { world | resources = List.map f world.resources }


addBlob : Blob -> World -> World
addBlob blob world =
    { world | blobs = blob :: world.blobs }


updateBlobs : (Blob -> Blob) -> World -> World
updateBlobs f world =
    { world | blobs = List.map f world.blobs }


updatePlayer : (PhysicsObject Vector2 -> PhysicsObject Vector2) -> World -> World
updatePlayer f world =
    { world | player = f world.player }


forces : World -> World
forces world =
    let
        movementForce : Float
        movementForce =
            0.05
    in
    { world
        | blobs =
            List.map
                (Blob.ai world.player.position (world.resources |> List.filter .enableCollisions) movementForce)
                world.blobs
        , resources =
            List.map
                (PhysicsObject.moveToPosition 5 .home (always movementForce))
                world.resources
        , player = PhysicsObject.moveToPosition 20 identity (always movementForce) world.player
    }


movement : World -> World
movement world =
    let
        f : PhysicsObject a -> PhysicsObject a
        f =
            PhysicsObject.applyFriciton 0.05
                >> PhysicsObject.move world.physicsStepTime
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


stateUpdate : World -> World
stateUpdate world =
    { world
        | blobs = List.map (Blob.update world.physicsStepTime) world.blobs
        , resources = List.map (Resource.update world.physicsStepTime) world.resources
    }


fixedUpdate : (World -> World) -> Float -> World -> World
fixedUpdate f dt world =
    if dt >= world.physicsStepTime then
        { world | physicsStepAccumulator = dt - world.physicsStepTime }
            |> f
            |> fixedUpdate f (dt - world.physicsStepTime)

    else
        { world | physicsStepAccumulator = dt }

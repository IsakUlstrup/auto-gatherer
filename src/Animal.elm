module Animal exposing (Animal, animalCollision, moveAnimal, moveToNearest, newAnimal, restAnimal)

import Physics exposing (Physics)
import Vector2 exposing (Vector2)


type alias Animal =
    { physics : Physics
    , stamina : Int
    }


newAnimal : Float -> Float -> Float -> Animal
newAnimal x y mass =
    Animal (Physics.initPhysics x y 20 mass) 10


moveAnimal : Float -> Animal -> Animal
moveAnimal dt animal =
    { animal
        | physics =
            animal.physics
                |> Physics.move dt
                |> Physics.applyFriction 0.93
                |> Physics.stopIfSlow 0.001
    }


applyForceToAnimal : Vector2 -> Animal -> Animal
applyForceToAnimal force animal =
    { animal | physics = Physics.applyForce force animal.physics }


restAnimal : Animal -> Animal
restAnimal animal =
    { animal | stamina = 10 }


reverseAnimal : Animal -> Animal
reverseAnimal animal =
    { animal | physics = Physics.reverseVelocity animal.physics }


moveToNearest : List { a | position : Vector2 } -> Animal -> Animal
moveToNearest resources animal =
    let
        nearest =
            resources
                |> List.map .position
                |> List.sortBy (Vector2.distance animal.physics.position)
                |> List.head
    in
    if animal.stamina > 0 then
        case nearest of
            Just r ->
                applyForceToAnimal (Vector2.direction animal.physics.position r) animal

            Nothing ->
                animal

    else
        animal


animalCollision : List { a | position : Vector2, radius : Float } -> Animal -> Animal
animalCollision resources animal =
    let
        collision =
            resources
                |> List.filter (Physics.isColliding animal.physics)
                |> List.head
    in
    case collision of
        Just r ->
            { animal | stamina = max 0 (animal.stamina - 1) }
                |> (\a -> { a | physics = Physics.resolveCollision r a.physics })
                |> reverseAnimal
                |> applyForceToAnimal (Vector2.direction r.position animal.physics.position |> Vector2.scale (animal.physics.mass * 0.4))

        Nothing ->
            animal

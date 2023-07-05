module Animal exposing
    ( Animal
    , AnimalState
    , isExhausted
    , moveAnimal
    , moveToNearest
    , newAnimal
    , removeStamina
    , restAnimal
    , tickState
    )

import Physics exposing (Physics)
import Vector2 exposing (Vector2)


type AnimalState
    = Ready Int
    | Exhausted Float


type alias Animal =
    { physics : Physics
    , state : AnimalState
    , moveSpeed : Float
    }


newAnimal : Float -> Float -> Float -> Animal
newAnimal x y speed =
    Animal (Physics.initPhysics x y 20) (Ready 10) speed


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
    { animal | state = Ready 10 }


isExhausted : Animal -> Bool
isExhausted animal =
    case animal.state of
        Ready 0 ->
            True

        Ready _ ->
            False

        Exhausted _ ->
            True


removeStamina : Int -> Animal -> Animal
removeStamina amount animal =
    case animal.state of
        Ready s ->
            if s - amount <= 0 then
                { animal | state = Exhausted 5000 }

            else
                { animal | state = Ready <| max 0 (s - amount) }

        _ ->
            animal


tickState : Float -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Ready _ ->
            animal

        Exhausted cd ->
            if cd <= 0 then
                { animal | state = Ready 10 }

            else
                { animal | state = Exhausted <| max 0 (cd - dt) }


moveToNearest : List { a | physics : Physics.Physics } -> Animal -> Animal
moveToNearest resources animal =
    if not <| isExhausted animal then
        let
            nearest : Maybe Vector2
            nearest =
                resources
                    |> List.map (.physics >> .position)
                    |> List.sortBy (Vector2.distance animal.physics.position)
                    |> List.head
        in
        case nearest of
            Just r ->
                applyForceToAnimal (Vector2.direction animal.physics.position r |> Vector2.scale animal.moveSpeed) animal

            Nothing ->
                animal

    else
        animal

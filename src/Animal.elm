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

import Engine.Physics exposing (Physics)
import Engine.Vector2 exposing (Vector2)


type AnimalState
    = Ready Int
    | Exhausted Float


type alias Animal =
    { physics : Physics
    , state : AnimalState
    , moveSpeed : Float
    , trail : List Vector2
    }


newAnimal : Float -> Float -> Float -> Animal
newAnimal x y speed =
    Animal (Engine.Physics.initPhysics x y 20) (Ready 10) speed []


moveAnimal : Float -> Animal -> Animal
moveAnimal dt animal =
    { animal
        | physics =
            animal.physics
                |> Engine.Physics.move dt
                |> Engine.Physics.applyFriction 0.93
                |> Engine.Physics.stopIfSlow 0.001
    }


applyForceToAnimal : Vector2 -> Animal -> Animal
applyForceToAnimal force animal =
    { animal | physics = Engine.Physics.applyForce force animal.physics }


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
    { animal | trail = animal.physics.position :: animal.trail |> List.take 20 }
        |> (\a ->
                case a.state of
                    Ready _ ->
                        a

                    Exhausted cd ->
                        if cd <= 0 then
                            { a | state = Ready 10 }

                        else
                            { a | state = Exhausted <| max 0 (cd - dt) }
           )


derivedMovementSpeed : Animal -> Float
derivedMovementSpeed animal =
    case animal.state of
        Ready _ ->
            animal.moveSpeed

        Exhausted _ ->
            animal.moveSpeed * 0.5


moveToNearest : List { a | physics : Physics } -> Animal -> Animal
moveToNearest resources animal =
    if not <| isExhausted animal then
        let
            nearest : Maybe Vector2
            nearest =
                resources
                    |> List.map (.physics >> .position)
                    |> List.sortBy (Engine.Vector2.distance animal.physics.position)
                    |> List.head
        in
        case nearest of
            Just r ->
                applyForceToAnimal (Engine.Vector2.direction animal.physics.position r |> Engine.Vector2.scale (derivedMovementSpeed animal)) animal

            Nothing ->
                applyForceToAnimal (Engine.Vector2.direction animal.physics.position Engine.Vector2.zero |> Engine.Vector2.scale (derivedMovementSpeed animal)) animal

    else if Engine.Vector2.distance animal.physics.position Engine.Vector2.zero > 20 then
        applyForceToAnimal (Engine.Vector2.direction animal.physics.position Engine.Vector2.zero |> Engine.Vector2.scale (derivedMovementSpeed animal)) animal

    else
        animal

module Animal exposing
    ( Animal
    , AnimalState
    , exhaustedSort
    , isExhausted
    , movement
    , movementAi
    , new
    , removeStamina
    , rest
    , update
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


{-| Animal constructor
-}
new : Float -> Float -> Float -> Animal
new x y speed =
    Animal (Engine.Physics.initPhysics x y 20) (Ready 10) speed []


{-| move animal based on physics
-}
movement : Float -> Float -> Animal -> Animal
movement friction dt animal =
    { animal
        | physics =
            animal.physics
                |> Engine.Physics.move dt
                |> Engine.Physics.applyFriction friction
    }


{-| Apply force to animal
-}
applyForce : Vector2 -> Animal -> Animal
applyForce force animal =
    { animal | physics = Engine.Physics.applyForce force animal.physics }


{-| Fill animal stamina
-}
rest : Animal -> Animal
rest animal =
    { animal | state = Ready 10 }


{-| Is animal exhausted
-}
isExhausted : Animal -> Bool
isExhausted animal =
    case animal.state of
        Ready 0 ->
            True

        Ready _ ->
            False

        Exhausted _ ->
            True


{-| Sort animals by exhaustion, ready ones first
-}
exhaustedSort : Animal -> Animal -> Order
exhaustedSort a1 a2 =
    case ( a1.state, a2.state ) of
        ( Ready _, Ready _ ) ->
            EQ

        ( Exhausted _, Ready _ ) ->
            LT

        ( Ready _, Exhausted _ ) ->
            GT

        ( Exhausted _, Exhausted _ ) ->
            EQ


{-| Remove animal stamina, exhaust if stamina is empty
-}
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


{-| Add current position to trail
-}
addToTrail : Animal -> Animal
addToTrail animal =
    { animal | trail = animal.physics.position :: animal.trail |> List.take 20 }


{-| Tick state, reset stamina if exhaustion is done
-}
tickState : Float -> Animal -> Animal
tickState dt animal =
    case animal.state of
        Ready _ ->
            animal

        Exhausted cd ->
            if cd <= 0 then
                rest animal

            else
                { animal | state = Exhausted <| max 0 (cd - dt) }


{-| Main update function for animal
-}
update : Float -> Animal -> Animal
update dt animal =
    animal
        |> tickState dt
        |> addToTrail


{-| Derive animal move speed, move slower if exhausted
-}
derivedMovementSpeed : Animal -> Float
derivedMovementSpeed animal =
    case animal.state of
        Ready _ ->
            animal.moveSpeed

        Exhausted _ ->
            animal.moveSpeed * 0.5


{-| Apply force towards home
-}
moveHome : Vector2 -> Animal -> Animal
moveHome home animal =
    applyForce (Engine.Vector2.direction animal.physics.position home |> Engine.Vector2.scale (derivedMovementSpeed animal)) animal


{-| Apply force towards nearest resource

If none are present, move home

-}
moveToNearest : Vector2 -> List { a | physics : Physics } -> Animal -> Animal
moveToNearest home resources animal =
    let
        nearest : Maybe Vector2
        nearest =
            resources
                |> List.map (.physics >> .position)
                |> List.sortBy (Engine.Vector2.distance animal.physics.position)
                |> List.head

        force r =
            Engine.Vector2.direction animal.physics.position r
                |> Engine.Vector2.scale (derivedMovementSpeed animal)
    in
    case nearest of
        Just r ->
            applyForce (force r) animal

        Nothing ->
            moveHome home animal


{-| Main movement AI

Move towards closest resource if animal has stamina, otherwise move home

-}
movementAi : Vector2 -> List { a | physics : Physics } -> Animal -> Animal
movementAi home resources animal =
    if not <| isExhausted animal then
        moveToNearest home resources animal

    else
        moveHome home animal

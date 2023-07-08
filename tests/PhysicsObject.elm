module PhysicsObject exposing (momentum)

import Engine.PhysicsObject as PhysicsObject
import Engine.Vector2 as Vector2
import Expect
import Test exposing (Test, describe, test)


momentum : Test
momentum =
    describe "Conservation of momentum"
        [ test "Attempt at conservation of momentum test" <|
            \_ ->
                (PhysicsObject.new 10 0 30 10 () |> PhysicsObject.applyForce (Vector2.new -10 0))
                    |> PhysicsObject.resolveCollisions [ PhysicsObject.new -10 0 30 10 () |> PhysicsObject.applyForce (Vector2.new 10 0) ]
                    |> (\o -> o.mass * Vector2.magnitude o.velocity)
                    |> Expect.equal 10
        ]

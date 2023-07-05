module Physics exposing (..)

import Engine.Physics as Physics
import Expect
import Test exposing (Test, describe, test)


collision : Test
collision =
    describe "Physics collision"
        [ test "Two colliding objects should no longer be colliding after collision is handled" <|
            \_ ->
                Physics.resolveCollision (Physics.initPhysics 0 0 30) (Physics.initPhysics 1 0 30)
                    |> Physics.isColliding (Physics.initPhysics 0 0 30)
                    |> Expect.equal True
        ]

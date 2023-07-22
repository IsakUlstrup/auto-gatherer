module Physics exposing (..)

import Engine.Particle exposing (Particle)
import Expect
import Fuzz exposing (float)
import Test exposing (Test, describe, fuzz2)


overlapModifier : Particle b -> Particle a -> Float
overlapModifier target object =
    (((target.mass - object.mass) / (target.mass + object.mass)) + 1) * 0.5


testObject : Float -> Particle ()
testObject mass =
    Engine.Particle.new 0 0 0 mass 0 ()


collision : Test
collision =
    describe "Collision"
        [ fuzz2 float float "The sum of two overlap modifiers between two objects should always be > 1" <|
            \m1 m2 ->
                ( overlapModifier (testObject m1) (testObject m2), overlapModifier (testObject m2) (testObject m1) )
                    |> (\( o1, o2 ) -> o1 + o2)
                    |> Expect.equal 1
        ]

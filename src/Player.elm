module Player exposing (Player, move, moveAi, new, setTarget)

import Engine.Physics as Physics exposing (Physics)
import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Player =
    { physics : Physics
    , target : Vector2
    , movementSpeed : Float
    }


new : Player
new =
    Player (Physics.initPhysics 0 0 20) Vector2.zero 0.02


setTarget : Vector2 -> Player -> Player
setTarget position player =
    { player | target = position }


moveAi : Player -> Player
moveAi player =
    let
        force =
            if Vector2.distance player.physics.position player.target < 5 then
                Vector2.zero

            else
                Vector2.direction player.physics.position player.target
    in
    player
        |> (\p -> { p | physics = Physics.applyForce (force |> Vector2.scale player.movementSpeed) p.physics })


move : Float -> Player -> Player
move dt player =
    { player
        | physics =
            player.physics
                |> Physics.move dt
                |> Physics.applyFriction 0.9
    }

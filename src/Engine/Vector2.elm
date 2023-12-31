module Engine.Vector2 exposing
    ( Vector2
    , add
    , direction
    , distance
    , divide
    , dot
    , magnitude
    , multiply
    , new
    , normalize
    , random
    , scale
    , singleton
    , subtract
    , sum
    , tangent
    , toString
    , zero
    )

import Random


type alias Vector2 =
    { x : Float
    , y : Float
    }


new : Float -> Float -> Vector2
new x y =
    Vector2 x y


singleton : Float -> Vector2
singleton n =
    Vector2 n n


{-| A vector where x and y are 0
-}
zero : Vector2
zero =
    Vector2 0 0


{-| Get the sum of vector components
-}
sum : Vector2 -> Float
sum vector =
    vector.x + vector.y


multiply : Vector2 -> Vector2 -> Vector2
multiply v1 v2 =
    { v1
        | x = v1.x * v2.x
        , y = v1.y * v2.y
    }


add : Vector2 -> Vector2 -> Vector2
add v1 v2 =
    { v1
        | x = v1.x + v2.x
        , y = v1.y + v2.y
    }


subtract : Vector2 -> Vector2 -> Vector2
subtract v1 v2 =
    { v1
        | x = v1.x - v2.x
        , y = v1.y - v2.y
    }


{-| Multiply the components of a vector by a number
-}
scale : Float -> Vector2 -> Vector2
scale amount vector =
    { vector
        | x = vector.x * amount
        , y = vector.y * amount
    }


divide : Float -> Vector2 -> Vector2
divide amount vector =
    if amount /= 0 then
        { vector
            | x = vector.x / amount
            , y = vector.y / amount
        }

    else
        new 0 0


magnitude : Vector2 -> Float
magnitude vector =
    sqrt (vector.x ^ 2 + vector.y ^ 2)


{-| Normalize a vector, a normalized vector is one where length/manitude is 1
-}
normalize : Vector2 -> Vector2
normalize vector =
    divide (magnitude vector) vector


{-| Get distance between two vectors
-}
distance : Vector2 -> Vector2 -> Float
distance v1 v2 =
    sqrt (((v1.x - v2.x) ^ 2) + ((v1.y - v2.y) ^ 2))


{-| Returns a normalized vector pointing from origin to target
-}
direction : Vector2 -> Vector2 -> Vector2
direction origin target =
    Vector2 (target.x - origin.x) (target.y - origin.y) |> normalize


tangent : Vector2 -> Vector2
tangent vector =
    Vector2 -vector.y vector.x


dot : Vector2 -> Vector2 -> Float
dot v1 v2 =
    (v2.x * v1.x) + (v2.y * v1.y)


toString : Vector2 -> String
toString vector =
    String.fromFloat vector.x ++ "," ++ String.fromFloat vector.y


random : Random.Generator Vector2
random =
    Random.map2
        (\x y -> new x y |> normalize)
        (Random.float -1 1)
        (Random.float -1 1)

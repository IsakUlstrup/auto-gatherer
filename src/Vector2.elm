module Vector2 exposing
    ( Vector2
    , add
    , direction
    , direction2
    , distance
    , divide
    , dot
    , limitMagnitude
    , magnitude
    , multiply
    , negate
    , negateScaleX
    , negateScaleY
    , negateX
    , negateY
    , new
    , normalize
    , scale
    , setMagnitude
    , setX
    , setY
    , singleton
    , subtract
    , zero
    )


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


setX : Float -> Vector2 -> Vector2
setX x vector =
    { vector | x = x }


setY : Float -> Vector2 -> Vector2
setY y vector =
    { vector | y = y }


negate : Vector2 -> Vector2
negate vector =
    scale -1 vector


negateX : Vector2 -> Vector2
negateX vector =
    { vector
        | x = vector.x * -1
    }


negateY : Vector2 -> Vector2
negateY vector =
    { vector
        | y = vector.y * -1
    }


negateScaleX : Float -> Vector2 -> Vector2
negateScaleX s vector =
    { vector
        | x = vector.x * -1 * s
    }


negateScaleY : Float -> Vector2 -> Vector2
negateScaleY s vector =
    { vector
        | y = vector.y * s * -1
    }


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


dot : Vector2 -> Vector2 -> Float
dot v1 v2 =
    (v1.x * v2.x) + (v1.y * v2.y)


{-| Set vector magnitude
-}
setMagnitude : Float -> Vector2 -> Vector2
setMagnitude mag vector =
    normalize vector |> scale mag


{-| Limit vector magnitude
-}
limitMagnitude : Float -> Vector2 -> Vector2
limitMagnitude limit vector =
    if magnitude vector > limit then
        setMagnitude limit vector

    else
        vector


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


{-| Returns a non-normalized vector pointing from origin to target
-}
direction2 : Vector2 -> Vector2 -> Vector2
direction2 origin target =
    Vector2 (target.x - origin.x) (target.y - origin.y)

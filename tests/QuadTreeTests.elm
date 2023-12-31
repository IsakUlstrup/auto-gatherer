module QuadTreeTests exposing (boundary, quadTree)

import Engine.Particle as Particle
import Engine.QuadTree as QuadTree exposing (QuadTree(..))
import Engine.Vector2 as Vector2 exposing (Vector2)
import Expect
import Fuzz exposing (float)
import Test exposing (Test, describe, fuzz, test)


testTree : QuadTree ()
testTree =
    QuadTree.new 0 0 100


isEmpty : QuadTree () -> Bool
isEmpty tree =
    case tree of
        Node _ ps ->
            List.isEmpty ps

        Leaf _ _ _ _ ->
            False


isSubdivided : QuadTree () -> Bool
isSubdivided tree =
    case tree of
        Node _ _ ->
            False

        Leaf _ _ _ _ ->
            True


pointIsIn : Vector2 -> QuadTree a -> Bool
pointIsIn point tree =
    case tree of
        Node b _ ->
            QuadTree.isIn .position b (Particle.new point 0 0 0 [])

        Leaf nw ne se sw ->
            pointIsIn point nw || pointIsIn point ne || pointIsIn point se || pointIsIn point sw


boundary : Test
boundary =
    describe "Boundary tests"
        [ test "Test if point is within boundary, should be true" <|
            \_ ->
                testTree
                    |> pointIsIn (Vector2.new 50 50)
                    |> Expect.equal True
        , test "Test if point is within boundary, should be false" <|
            \_ ->
                testTree
                    |> pointIsIn (Vector2.new 300 100)
                    |> Expect.equal False
        , fuzz float "Test if point with random x position is within boundary" <|
            \xpos ->
                testTree
                    |> pointIsIn (Vector2.new xpos 50)
                    |> Expect.equal (xpos < 100 && xpos > -100)
        ]


quadTree : Test
quadTree =
    describe "QuadTree tests"
        [ test "insert point outside boundary, should return unchanged tree" <|
            \_ ->
                testTree
                    |> QuadTree.insert (Particle.new (Vector2.new 300 0) 10 100 1 [])
                    |> isEmpty
                    |> Expect.equal True
        , test "insert point inside boundary, should return tree with point added" <|
            \_ ->
                testTree
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> isEmpty
                    |> Expect.equal False
        , test "insert five points inside boundary, should return subdivided tree with points added" <|
            \_ ->
                testTree
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> QuadTree.insert (Particle.new (Vector2.new 30 0) 10 100 1 [])
                    |> isSubdivided
                    |> Expect.equal True
        ]

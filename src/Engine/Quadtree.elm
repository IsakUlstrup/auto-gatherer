module Engine.Quadtree exposing (Boundary, QuadTree(..), insert, new, newBoundary, pointIsInBoundary)

import Engine.Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Boundary =
    { center : Vector2, size : Float }


newBoundary : Float -> Float -> Float -> Boundary
newBoundary x y size =
    Boundary (Vector2.new x y) size


type QuadTree a
    = Node Boundary (List (Particle a))
    | Leaf Boundary (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)


new : Float -> Float -> Float -> QuadTree a
new x y size =
    Node (Boundary (Vector2.new x y) size) []


pointIsInBoundary : Vector2 -> QuadTree a -> Bool
pointIsInBoundary { x, y } tree =
    let
        isIn b =
            (x
                < (b.center.x + b.size)
                && x
                > (b.center.x - b.size)
            )
                && (y
                        < (b.center.y + b.size)
                        && y
                        > (b.center.y - b.size)
                   )
    in
    case tree of
        Node boundary _ ->
            isIn boundary

        Leaf boundary _ _ _ _ ->
            isIn boundary


insert : Particle a -> QuadTree a -> QuadTree a
insert particle tree =
    let
        insertHelper p t =
            case t of
                Node b ps ->
                    Node b (p :: ps)

                Leaf _ _ _ _ _ ->
                    t
    in
    if pointIsInBoundary particle.position tree then
        -- do stuff
        insertHelper particle tree

    else
        tree

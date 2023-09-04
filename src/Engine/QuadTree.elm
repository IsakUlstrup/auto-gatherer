module Engine.QuadTree exposing
    ( Boundary
    , QuadTree(..)
    , fromList
    , indexedMap
    , insert
    , isIn
    , new
    , subdivide
    )

import Engine.Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Boundary =
    { center : Vector2
    , size : Float
    }


type QuadTree a
    = Node Boundary (List (Particle a))
    | Leaf (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)


new : Float -> Float -> Float -> QuadTree a
new x y size =
    Node (Boundary (Vector2.new x y) size) []


isIn : Boundary -> Particle a -> Bool
isIn boundary particle =
    particle.position.x
        >= (boundary.center.x - boundary.size)
        && particle.position.x
        <= (boundary.center.x + boundary.size)
        && particle.position.y
        >= (boundary.center.y - boundary.size)
        && particle.position.y
        <= (boundary.center.y + boundary.size)


nw : Boundary -> List (Particle a) -> QuadTree a
nw boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn newBoundary) xs)


ne : Boundary -> List (Particle a) -> QuadTree a
ne boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn newBoundary) xs)


se : Boundary -> List (Particle a) -> QuadTree a
se boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn newBoundary) xs)


sw : Boundary -> List (Particle a) -> QuadTree a
sw boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn newBoundary) xs)


{-| Subdivide node, is tree is a leaf do nothing
-}
subdivide : QuadTree a -> QuadTree a
subdivide tree =
    case tree of
        Node b xs ->
            if List.length xs >= 4 then
                Leaf (nw b xs) (ne b xs) (se b xs) (sw b xs)

            else
                tree

        Leaf _ _ _ _ ->
            tree


insert : Particle a -> QuadTree a -> QuadTree a
insert particle tree =
    case tree of
        Node b ps ->
            if isIn b particle then
                subdivide (Node b (particle :: ps))

            else
                tree

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (insert particle nw1)
                (insert particle ne1)
                (insert particle sw1)
                (insert particle se1)


fromList : List (Particle a) -> QuadTree a
fromList particles =
    List.foldl insert (new 0 0 1000) particles


indexedMap : (Int -> Boundary -> List (Particle a) -> b) -> QuadTree a -> List b
indexedMap f tree =
    let
        helper i accum t =
            case t of
                Node b ps ->
                    f i b ps :: accum

                Leaf nw1 ne1 se1 sw1 ->
                    helper (i + 1) accum nw1
                        ++ helper (i + 2) accum ne1
                        ++ helper (i + 3) accum se1
                        ++ helper (i + 4) accum sw1
                        ++ accum
    in
    helper 0 [] tree

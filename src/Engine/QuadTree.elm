module Engine.QuadTree exposing
    ( Boundary
    , QuadTree(..)
    , filter
    , fromList
    , insert
    , isInBoundary
    , map
    , new
    , query
    , subdivide
    , toList
    )

import Engine.Particle exposing (Particle)
import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Boundary =
    { center : Vector2
    , size : Float
    }


type QuadTree a
    = Node Boundary (List ( Int, Particle a ))
    | Leaf (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)


new : Float -> Float -> Float -> QuadTree a
new x y size =
    Node (Boundary (Vector2.new x y) size) []


isInBoundary : Boundary -> Particle a -> Bool
isInBoundary boundary particle =
    particle.position.x
        >= (boundary.center.x - boundary.size)
        && particle.position.x
        <= (boundary.center.x + boundary.size)
        && particle.position.y
        >= (boundary.center.y - boundary.size)
        && particle.position.y
        <= (boundary.center.y + boundary.size)


northWest : Boundary -> List ( Int, Particle a ) -> QuadTree a
northWest boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (Tuple.second >> isInBoundary newBoundary) xs)


northEast : Boundary -> List ( Int, Particle a ) -> QuadTree a
northEast boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (Tuple.second >> isInBoundary newBoundary) xs)


southEast : Boundary -> List ( Int, Particle a ) -> QuadTree a
southEast boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (Tuple.second >> isInBoundary newBoundary) xs)


southWest : Boundary -> List ( Int, Particle a ) -> QuadTree a
southWest boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (Tuple.second >> isInBoundary newBoundary) xs)


{-| Subdivide node, is tree is a leaf do nothing
-}
subdivide : QuadTree a -> QuadTree a
subdivide tree =
    case tree of
        Node b xs ->
            if List.length xs >= 4 then
                Leaf (northWest b xs) (northEast b xs) (southEast b xs) (southWest b xs)

            else
                tree

        Leaf _ _ _ _ ->
            tree


insert : ( Int, Particle a ) -> QuadTree a -> QuadTree a
insert particle tree =
    case tree of
        Node b ps ->
            if isInBoundary b (Tuple.second particle) then
                subdivide (Node b (particle :: ps))

            else
                tree

        Leaf nw ne se sw ->
            Leaf (insert particle nw)
                (insert particle ne)
                (insert particle sw)
                (insert particle se)


fromList : List ( Int, Particle a ) -> QuadTree a
fromList particles =
    List.foldl insert (new 0 0 1000) particles


toList : QuadTree a -> List ( Int, Particle a )
toList tree =
    let
        helper accum t =
            case t of
                Node _ xs ->
                    xs ++ accum

                Leaf nw1 ne1 se1 sw1 ->
                    helper accum nw1 ++ helper accum ne1 ++ helper accum se1 ++ helper accum sw1
    in
    helper [] tree


query : Boundary -> QuadTree a -> List ( Int, Particle a )
query boundary tree =
    let
        helper accum t =
            case t of
                Node _ ps ->
                    List.filter (Tuple.second >> isInBoundary boundary) ps ++ accum

                Leaf nw ne se sw ->
                    helper accum nw
                        ++ helper accum ne
                        ++ helper accum se
                        ++ helper accum sw
    in
    helper [] tree


filter : (( Int, Particle a ) -> Bool) -> QuadTree a -> QuadTree a
filter pred tree =
    case tree of
        Node b xs ->
            Node b (List.filter pred xs)

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (filter pred nw1) (filter pred ne1) (filter pred se1) (filter pred sw1)


map : (( Int, Particle a ) -> ( Int, Particle b )) -> QuadTree a -> QuadTree b
map f tree =
    case tree of
        Node b xs ->
            Node b (List.map f xs)

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (map f nw1) (map f ne1) (map f se1) (map f sw1)

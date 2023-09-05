module Engine.QuadTree exposing
    ( Boundary
    , QuadTree(..)
    , filter
    , fromList
    , indexedMap
    , insert
    , isIn
    , map
    , new
    , subdivide
    , toList
    )

import Engine.Vector2 as Vector2 exposing (Vector2)


type alias Boundary =
    { center : Vector2
    , size : Float
    }


type QuadTree a
    = Node Boundary (List a)
    | Leaf (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a)


new : Float -> Float -> Float -> QuadTree a
new x y size =
    Node (Boundary (Vector2.new x y) size) []


isIn : (a -> Vector2) -> Boundary -> a -> Bool
isIn getPos boundary particle =
    (getPos particle).x
        >= (boundary.center.x - boundary.size)
        && (getPos particle).x
        <= (boundary.center.x + boundary.size)
        && (getPos particle).y
        >= (boundary.center.y - boundary.size)
        && (getPos particle).y
        <= (boundary.center.y + boundary.size)


nw : (a -> Vector2) -> Boundary -> List a -> QuadTree a
nw getPos boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn getPos newBoundary) xs)


ne : (a -> Vector2) -> Boundary -> List a -> QuadTree a
ne getPos boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y - (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn getPos newBoundary) xs)


se : (a -> Vector2) -> Boundary -> List a -> QuadTree a
se getPos boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x + (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn getPos newBoundary) xs)


sw : (a -> Vector2) -> Boundary -> List a -> QuadTree a
sw getPos boundary xs =
    let
        newBoundary =
            Boundary
                (Vector2
                    (boundary.center.x - (boundary.size / 2))
                    (boundary.center.y + (boundary.size / 2))
                )
                (boundary.size / 2)
    in
    Node newBoundary (List.filter (isIn getPos newBoundary) xs)


{-| Subdivide node, is tree is a leaf do nothing
-}
subdivide : (a -> Vector2) -> QuadTree a -> QuadTree a
subdivide getPos tree =
    case tree of
        Node b xs ->
            if List.length xs >= 4 then
                Leaf (nw getPos b xs) (ne getPos b xs) (se getPos b xs) (sw getPos b xs)

            else
                tree

        Leaf _ _ _ _ ->
            tree


insert : (a -> Vector2) -> a -> QuadTree a -> QuadTree a
insert getPos particle tree =
    case tree of
        Node b ps ->
            if isIn getPos b particle then
                subdivide getPos (Node b (particle :: ps))

            else
                tree

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (insert getPos particle nw1)
                (insert getPos particle ne1)
                (insert getPos particle sw1)
                (insert getPos particle se1)


fromList : (a -> Vector2) -> List a -> QuadTree a
fromList getPos particles =
    List.foldl (insert getPos) (new 0 0 1000) particles


toList : QuadTree a -> List a
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


filter : (a -> Bool) -> QuadTree a -> QuadTree a
filter pred tree =
    case tree of
        Node b xs ->
            Node b (List.filter pred xs)

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (filter pred nw1) (filter pred ne1) (filter pred se1) (filter pred sw1)


map : (a -> b) -> QuadTree a -> QuadTree b
map f tree =
    case tree of
        Node b xs ->
            Node b (List.map f xs)

        Leaf nw1 ne1 se1 sw1 ->
            Leaf (map f nw1) (map f ne1) (map f se1) (map f sw1)


indexedMap : (Int -> Boundary -> List a -> b) -> QuadTree a -> List b
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

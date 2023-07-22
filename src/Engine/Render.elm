module Engine.Render exposing
    ( HexCorners
    , RenderConfig
    , cornerListToPoints
    , cornersToPoints
    , generateHexCorners
    , initRenderConfig
    , pointAdd
    , pointToPixel
    , viewMap
    , withHexFocus
    , withPosition
    , withRenderDistance
    , withZoom
    )

import Engine.HexGrid exposing (HexGrid)
import Engine.Point as Point exposing (Point)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Keyed



---- CONFIG BUILDER ----


{-| Holds renderer config values
-}
type alias RenderConfig =
    { position : Vector2
    , zoom : Float
    , renderDistance : Float
    }


{-| Default render config
-}
initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig Vector2.zero 1 100


withRenderDistance : Float -> RenderConfig -> RenderConfig
withRenderDistance distance config =
    { config | renderDistance = distance }


withPosition : Vector2 -> RenderConfig -> RenderConfig
withPosition position config =
    { config | position = position }


{-| move camera to focus on point
-}
withHexFocus : Point -> RenderConfig -> RenderConfig
withHexFocus point config =
    { config | position = pointToPixel point }


{-| Set camera zoom
-}
withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }



-- HELPERS


{-| Hex size constant

If you want to change hex size, use WithZoom instead

-}
hexSize : Float
hexSize =
    40


{-| Get the center of a given point in screen coordinates
-}
pointToPixel : Point -> Vector2
pointToPixel point =
    let
        ( q, r ) =
            Point.toAxial point
    in
    Vector2
        (hexSize * (3 / 2 * toFloat q))
        (hexSize * (sqrt 3 / 2 * toFloat q + sqrt 3 * toFloat r))


{-| Get point y position in pixels
-}
yPixelPosition : Point -> Float
yPixelPosition position =
    pointToPixel position |> .y


{-| Convert a list of floats to a Svg points attribute
-}
cornerListToPoints : List ( Float, Float ) -> Attribute msg
cornerListToPoints points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    List.map tupleToString points |> List.intersperse " " |> String.concat |> Svg.Attributes.points


{-| Transform Hex Corners to Svg points attribute
-}
cornersToPoints : HexCorners -> Attribute msg
cornersToPoints points =
    let
        tupleToString : ( Float, Float ) -> String
        tupleToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y
    in
    [ tupleToString points.p0
    , tupleToString points.p1
    , tupleToString points.p2
    , tupleToString points.p3
    , tupleToString points.p4
    , tupleToString points.p5
    ]
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.points


{-| Represents the points of a hexagon, staring with east and moving counter clockwise
-}
type alias HexCorners =
    { p0 : ( Float, Float )
    , p1 : ( Float, Float )
    , p2 : ( Float, Float )
    , p3 : ( Float, Float )
    , p4 : ( Float, Float )
    , p5 : ( Float, Float )
    }


{-| Add two tuples of floats together
-}
pointAdd : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
pointAdd ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Calculate hex corners in screen coordinates
-}
generateHexCorners : HexCorners
generateHexCorners =
    let
        corner : Float -> ( Float, Float )
        corner cornerNumber =
            ( hexSize * cos (degrees <| 60 * cornerNumber)
            , hexSize * sin (degrees <| 60 * cornerNumber)
            )
    in
    HexCorners
        (corner 0)
        (corner 1)
        (corner 2)
        (corner 3)
        (corner 4)
        (corner 5)



-- RENDER


{-| CSS transform translate attribute based on position
-}
translatePoint : Point -> Attribute msg
translatePoint position =
    let
        pos =
            pointToPixel position
    in
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromInt (round pos.x)
            ++ "px, "
            ++ String.fromInt (round pos.y)
            ++ "px);"


{-| Create a wrapper with correct position and render a tile with provied function
-}
renderTile : (( Point, a ) -> Svg msg) -> ( Point, a ) -> Svg msg
renderTile renderFunc ( point, t ) =
    Svg.g
        [ Svg.Attributes.class "tile"
        , translatePoint point
        ]
        [ renderFunc ( point, t ) ]


{-| Keyed and lazy tile render
-}
viewKeyedTile : (( Point, a ) -> Svg msg) -> ( Point, a ) -> ( String, Svg msg )
viewKeyedTile renderFunc entity =
    ( Point.toString (Tuple.first entity)
    , renderTile renderFunc entity
    )


viewMap : (( Point, tileData ) -> Svg msg) -> RenderConfig -> HexGrid tileData -> Svg msg
viewMap renderFunc config grid =
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "map"
        , Svg.Attributes.filter "url(#goo-filter)"
        ]
        (grid
            |> Engine.HexGrid.toList
            |> List.filter (\( p, _ ) -> Vector2.distance config.position (pointToPixel p) < config.renderDistance)
            |> List.sortBy (Tuple.first >> yPixelPosition)
            |> List.map (viewKeyedTile renderFunc)
        )



-- setMapKey : ( Point, Svg msg ) -> ( String, Svg msg )
-- setMapKey ( position, svg ) =
--     ( Point.toString position, svg )
-- mapIsActive : List Point -> Point -> a -> Bool
-- mapIsActive activeMaps position _ =
--     List.member position activeMaps
-- viewMaps : (( Point, tileData ) -> Svg msg) -> List Point -> Dict Point (HexGrid tileData) -> Svg msg
-- viewMaps renderFunc activeMaps grids =
--     Svg.Keyed.node "g"
--         [ Svg.Attributes.class "maps" ]
--         (grids
--             |> Dict.filter (mapIsActive activeMaps)
--             |> Dict.map (viewMap renderFunc)
--             |> Dict.toList
--             |> List.map setMapKey
--         )

module Engine.Render exposing
    ( RenderConfig
    , initRenderConfig
    , rect2d
    , tileSize
    , view2DGrid
    , withPosition
    , withRenderDistance
    , withZoom
    )

import Dict
import Engine.Grid as Grid
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


{-| Set camera zoom
-}
withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }



-- HELPERS
-- RENDER
-- 2d grid


tileSize : Int
tileSize =
    50


rect2d : List (Svg.Attribute msg) -> Svg msg
rect2d attrs =
    Svg.rect
        (attrs
            ++ [ Svg.Attributes.x <| String.fromInt -(tileSize // 2)
               , Svg.Attributes.y <| String.fromInt -(tileSize // 2)
               , Svg.Attributes.width <| String.fromInt tileSize
               , Svg.Attributes.height <| String.fromInt tileSize
               ]
        )
        []


{-| CSS transform translate attribute based on position
-}
translatePoint2D : Grid.Point -> Attribute msg
translatePoint2D ( x, y ) =
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromInt (x * tileSize)
            ++ "px, "
            ++ String.fromInt (y * tileSize)
            ++ "px);"


{-| Create a wrapper with correct position and render a tile with provied function
-}
renderTile2D : (( Grid.Point, a ) -> Svg msg) -> ( Grid.Point, a ) -> Svg msg
renderTile2D renderFunc ( point, t ) =
    Svg.g
        [ Svg.Attributes.class "tile-container"
        , translatePoint2D point
        ]
        [ renderFunc ( point, t ) ]


pointToString2D : Grid.Point -> String
pointToString2D ( x, y ) =
    String.fromInt x ++ "," ++ String.fromInt y


{-| Keyed and lazy tile render
-}
viewKeyedTile2D : (( Grid.Point, a ) -> Svg msg) -> ( Grid.Point, a ) -> ( String, Svg msg )
viewKeyedTile2D renderFunc tile =
    ( pointToString2D (Tuple.first tile)
    , renderTile2D renderFunc tile
    )


{-| Convert local chunk corrdinates to screen coordinates
-}
screenPos : Grid.Point -> ( Grid.Point, a ) -> ( Grid.Point, a )
screenPos ( cx, cy ) ( ( x, y ), t ) =
    ( ( x + (cx * Grid.chunkSize)
      , y + (cy * Grid.chunkSize)
      )
    , t
    )


view2DChunk : (( Grid.Point, a ) -> Svg msg) -> ( Grid.Point, Grid.Chunk a ) -> ( String, Svg msg )
view2DChunk renderFunc ( chunkPos, chunk ) =
    ( pointToString2D chunkPos
    , Svg.Keyed.node "g"
        [ Svg.Attributes.class "chunk"
        ]
        (chunk
            |> Dict.toList
            |> List.map (screenPos chunkPos)
            -- |> List.filter (\( p, _ ) -> Vector2.distance config.position (Vector2.new (Tuple.first p |> toFloat) (Tuple.second p |> toFloat)) < config.renderDistance)
            -- |> List.sortBy (Tuple.first >> yPixelPosition)
            |> List.map (viewKeyedTile2D renderFunc)
        )
    )


view2DGrid : (( Grid.Point, a ) -> Svg msg) -> Grid.WorldMap a -> Svg msg
view2DGrid renderFunc grid =
    Svg.Keyed.node "g"
        [ Svg.Attributes.class "map" ]
        (grid
            |> Dict.toList
            -- |> List.filter (\( p, _ ) -> Vector2.distance config.position (Grid.toVector2 p |> Vector2.scale (tileSize * Grid.chunkSize |> toFloat)) < config.renderDistance)
            -- |> List.sortBy (Tuple.first >> yPixelPosition)
            |> List.map (view2DChunk renderFunc)
        )

module Engine.SvgRenderer exposing
    ( RenderConfig
    , initRenderConfig
    , screenToWorldCoords
    , transformAttr
    , viewQuadTreeSvg
    , viewSvg
    , withDebug
    , withHeight
    , withRenderDistance
    , withWidth
    , withZoom
    )

import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.QuadTree as QuadTree exposing (Boundary)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Keyed



---- CONFIG BUILDER ----


{-| Holds renderer config values
-}
type alias RenderConfig =
    { zoom : Float
    , renderDistance : Float
    , debug : Bool
    , screenWidth : Int
    , screenHeight : Int
    }


{-| Default render config
-}
initRenderConfig : RenderConfig
initRenderConfig =
    RenderConfig 1 100 False 1000 1000


withRenderDistance : Float -> RenderConfig -> RenderConfig
withRenderDistance distance config =
    { config | renderDistance = distance }


withDebug : Bool -> RenderConfig -> RenderConfig
withDebug flag config =
    { config | debug = flag }


{-| Set camera zoom
-}
withZoom : Float -> RenderConfig -> RenderConfig
withZoom zoom config =
    { config | zoom = zoom }


{-| Set window width
-}
withWidth : Int -> RenderConfig -> RenderConfig
withWidth width config =
    { config | screenWidth = width }


{-| Set window height
-}
withHeight : Int -> RenderConfig -> RenderConfig
withHeight height config =
    { config | screenHeight = height }



-- VIEW


transformAttr : Vector2 -> Svg.Attribute msg
transformAttr position =
    Svg.Attributes.transform <|
        "translate("
            ++ String.fromInt (round position.x)
            ++ ", "
            ++ String.fromInt (round position.y)
            ++ ")"


screenToWorldCoords : Float -> Float -> Float -> Float -> Vector2
screenToWorldCoords x y screenWidth screenHeight =
    let
        ratio =
            if screenWidth > screenHeight then
                1000 / screenWidth

            else
                1000 / screenHeight
    in
    Vector2.new
        (x - (screenWidth / 2))
        (y - (screenHeight / 2))
        |> Vector2.scale ratio


viewKeyedParticle : (Particle a -> Svg msg) -> ( Int, Particle a ) -> ( String, Svg msg )
viewKeyedParticle f ( id, particle ) =
    ( String.fromInt id, f particle )


viewSvg : List (Svg.Attribute msg) -> List (Svg msg) -> (Particle a -> Svg msg) -> ParticleSystem a -> RenderConfig -> Svg msg
viewSvg attrs children viewParticle particles config =
    Svg.svg
        ([ Svg.Attributes.class "game"
         , Svg.Attributes.viewBox "-500 -500 1000 1000"
         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
         ]
            ++ attrs
        )
        (Svg.Keyed.node "g"
            [ Svg.Attributes.class "camera"
            , transformAttr <| Vector2.scale -1 <| (.position <| World.getPlayer particles)
            ]
            (particles
                |> World.filterParticles (\o -> Particle.distance (World.getPlayer particles) o < config.renderDistance)
                |> World.mapParticles (viewKeyedParticle viewParticle)
            )
            :: children
        )



-- QUAD TREE


boundaryTransform : Boundary -> Svg.Attribute msg
boundaryTransform boundary =
    Svg.Attributes.transform <|
        "translate("
            ++ String.fromFloat (boundary.center.x - boundary.size)
            ++ " , "
            ++ String.fromFloat (boundary.center.y - boundary.size)
            ++ ")"


viewParticle2 : Particle a -> Svg msg
viewParticle2 particle =
    let
        transform =
            Svg.Attributes.transform <|
                "translate("
                    ++ String.fromFloat particle.position.x
                    ++ " , "
                    ++ String.fromFloat particle.position.y
                    ++ ")"
    in
    Svg.circle
        [ transform
        , Svg.Attributes.class "particle"
        , Svg.Attributes.r <| String.fromFloat particle.radius
        ]
        []


viewKeyedBoundary : Int -> Boundary -> List (Particle a) -> ( String, Svg msg )
viewKeyedBoundary index boundary particles =
    ( String.fromInt index
    , Svg.g
        [ Svg.Attributes.style <| "fill: hsl(" ++ String.fromInt (index * 70) ++ ", 100%, 50%)"
        ]
        [ Svg.g [] (List.map viewParticle2 particles)
        , Svg.rect
            [ Svg.Attributes.class "leaf"
            , Svg.Attributes.class <| String.fromInt index
            , boundaryTransform boundary
            , Svg.Attributes.width <| String.fromFloat (boundary.size * 2)
            , Svg.Attributes.height <| String.fromFloat (boundary.size * 2)
            , Svg.Attributes.style <| "fill-opacity: 0"
            , Svg.Attributes.stroke <| "hsl(" ++ String.fromInt (index * 70) ++ ", 100%, 50%)"
            ]
            []
        ]
    )


viewQuadTreeSvg : List (Svg.Attribute msg) -> List (Svg msg) -> (Particle a -> Svg msg) -> ParticleSystem a -> RenderConfig -> Svg msg
viewQuadTreeSvg attrs children _ particles _ =
    Svg.svg
        ([ Svg.Attributes.class "game"
         , Svg.Attributes.viewBox "-500 -500 1000 1000"
         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
         ]
            ++ attrs
        )
        (Svg.Keyed.node "g"
            [ Svg.Attributes.class "camera"
            , transformAttr <| Vector2.scale -1 <| (.position <| World.getPlayer particles)
            ]
            (particles
                |> World.getParticles
                |> QuadTree.fromList
                |> QuadTree.indexedMap viewKeyedBoundary
            )
            :: children
        )

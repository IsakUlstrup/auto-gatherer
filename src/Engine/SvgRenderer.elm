module Engine.SvgRenderer exposing
    ( RenderConfig
    , initRenderConfig
    , screenToWorldCoords
    , transformAttr
    , viewSvg
    , withDebug
    , withHeight
    , withRenderDistance
    , withWidth
    , withZoom
    )

import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes



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


viewSvg : List (Svg.Attribute msg) -> List (Svg msg) -> (Particle a -> Svg msg) -> ParticleSystem a -> RenderConfig -> Svg msg
viewSvg attrs children viewParticle particles config =
    Svg.svg
        ([ Svg.Attributes.class "game"
         , Svg.Attributes.viewBox "-500 -500 1000 1000"
         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
         ]
            ++ attrs
        )
        (Svg.g
            [ Svg.Attributes.class "camera"
            , transformAttr <| Vector2.scale -1 <| (.position <| World.getPlayer particles)
            ]
            [ Svg.g []
                (World.getParticles particles
                    |> List.filter (\o -> Particle.distance (World.getPlayer particles) o < config.renderDistance)
                    |> List.map viewParticle
                )
            ]
            :: children
        )

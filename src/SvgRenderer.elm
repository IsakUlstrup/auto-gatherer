module SvgRenderer exposing
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

import Content.ParticleState exposing (ParticleState(..))
import Engine.Particle as Particle exposing (PhysicsType(..))
import Engine.Vector2 as Vector2 exposing (Vector2)
import Engine.World as World
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
    Svg.Attributes.style <|
        "translate("
            ++ String.fromInt (round position.x)
            ++ ", "
            ++ String.fromInt (round position.y)
            ++ ")"


viewParticle : Bool -> Particle.Particle ParticleState -> Svg msg
viewParticle showVectors particle =
    let
        typeString : String
        typeString =
            case particle.state of
                MoveToCenter ->
                    "move-center"

                MoveToPosition _ ->
                    "move-to"

                FollowMoveToPosition _ ->
                    "follow-move-to"

                MoveToClosest ->
                    "move-closest"

                Idle ->
                    "idle"

                Avoid ->
                    "avoid"

                FollowId _ ->
                    "follow-id"

                Meander ->
                    "meander"

                MoveAwayAngle _ _ ->
                    "move-away-angle"

        physicsTypeString : String
        physicsTypeString =
            case particle.physicsType of
                Fixed ->
                    "fixed"

                Static _ ->
                    "static"

                Dynamic _ ->
                    "dynamic"
    in
    Svg.g
        [ transformAttr particle.position
        , Svg.Attributes.class "particle"
        , Svg.Attributes.class typeString
        , Svg.Attributes.class physicsTypeString
        ]
        (Svg.circle
            [ Svg.Attributes.r <| String.fromInt (round particle.radius)
            , Svg.Attributes.class "body"
            ]
            []
            :: (if showVectors then
                    [ Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 <| String.fromInt (round ((Particle.getVelocity particle).x * 300))
                        , Svg.Attributes.y2 <| String.fromInt (round ((Particle.getVelocity particle).y * 300))
                        , Svg.Attributes.class "velocity"
                        ]
                        []
                    , Svg.line
                        [ Svg.Attributes.x1 "0"
                        , Svg.Attributes.y1 "0"
                        , Svg.Attributes.x2 <| String.fromInt (round ((Particle.getImpulse particle).x * 300))
                        , Svg.Attributes.y2 <| String.fromInt (round ((Particle.getImpulse particle).y * 300))
                        , Svg.Attributes.class "impulse"
                        ]
                        []
                    ]

                else
                    []
               )
        )


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


viewSvg : List (Svg.Attribute msg) -> List (Svg msg) -> World.World ParticleState -> RenderConfig -> Svg msg
viewSvg attrs children particles config =
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
                    |> List.map (viewParticle config.debug)
                )
            ]
            :: children
        )

module SvgRenderer exposing
    ( RenderConfig
    , initRenderConfig
    , viewSvg
    , withDebug
    , withHeight
    , withRenderDistance
    , withWidth
    , withZoom
    )

import Content.ParticleState exposing (ParticleState(..))
import Engine.Particle as Particle exposing (PhysicsType(..))
import Engine.Vector2 exposing (Vector2)
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
    , windowWidth : Int
    , windowHeight : Int
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
    { config | windowWidth = width }


{-| Set window height
-}
withHeight : Int -> RenderConfig -> RenderConfig
withHeight height config =
    { config | windowHeight = height }



-- VIEW


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromInt (round position.x)
        ++ ", "
        ++ String.fromInt (round position.y)
        ++ ")"


cameraTransform : Vector2 -> Svg.Attribute msg
cameraTransform position =
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromInt (round -position.x)
            ++ "px, "
            ++ String.fromInt (round -position.y)
            ++ "px)"


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
        [ Svg.Attributes.transform <| transformString particle.position
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



-- rect2d : Int -> List (Svg.Attribute msg) -> Svg msg
-- rect2d size attrs =
--     Svg.rect
--         (attrs
--             ++ [ Svg.Attributes.x <| String.fromInt -(size // 2)
--                , Svg.Attributes.y <| String.fromInt -(size // 2)
--                , Svg.Attributes.width <| String.fromInt size
--                , Svg.Attributes.height <| String.fromInt size
--                ]
--         )
--         []
-- viewTile2D : (Vector2 -> msg) -> Float -> Vector2 -> Svg msg
-- viewTile2D setMoveTarget size position =
--     let
--         isOdd : Int -> Bool
--         isOdd n =
--             modBy 2 n == 1
--         fillSting : String
--         fillSting =
--             if isOdd <| round (position.x / size) + round (position.y / size) then
--                 "hsl(" ++ (String.fromFloat <| Vector2.distance position Vector2.zero / 20) ++ " 80% 90%)"
--             else
--                 "transparent"
--     in
--     rect2d (round size)
--         [ Svg.Events.onClick <| setMoveTarget position
--         , Svg.Attributes.class "tile"
--         , Svg.Attributes.fill fillSting
--         , Svg.Attributes.transform <| transformString position
--         , Svg.Attributes.rx "3"
--         ]
-- viewMap : (Vector2 -> msg) -> RenderConfig -> Svg msg
-- viewMap setMoveTarget config =
--     let
--         row : Int -> List Vector2
--         row y =
--             List.range -30 30
--                 |> List.map (\x -> Vector2.new (toFloat x) (toFloat y))
--         map : List Vector2
--         map =
--             List.range -30 30
--                 |> List.reverse
--                 |> List.concatMap row
--                 |> List.map (Vector2.scale 50)
--                 |> List.filter (\t -> Vector2.distance t config.position < config.renderDistance)
--     in
--     Svg.g [] (List.map (viewTile2D setMoveTarget 50) map)


viewPlayerTarget : Particle.Particle ParticleState -> Svg msg
viewPlayerTarget player =
    case player.state of
        MoveToPosition p ->
            Svg.circle
                [ Svg.Attributes.transform <| transformString p
                , Svg.Attributes.r "20"
                , Svg.Attributes.class "player-target"
                ]
                []

        _ ->
            Svg.circle [] []


viewSvg : List (Svg.Attribute msg) -> World.World ParticleState -> RenderConfig -> Svg msg
viewSvg attrs particles config =
    Svg.svg
        ([ Svg.Attributes.class "game"
         , Svg.Attributes.viewBox "-500 -500 1000 1000"
         , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
         ]
            ++ attrs
        )
        [ Svg.g
            [ Svg.Attributes.class "camera"
            , cameraTransform <| (.position <| World.getPlayer particles)
            ]
            [ Svg.g []
                (viewPlayerTarget (World.getPlayer particles)
                    :: (World.getParticles particles
                            |> List.filter (\o -> Particle.distance (World.getPlayer particles) o < config.renderDistance)
                            |> List.map (viewParticle config.debug)
                       )
                )
            ]
        ]

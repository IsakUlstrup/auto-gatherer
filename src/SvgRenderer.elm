module SvgRenderer exposing
    ( RenderConfig
    , initRenderConfig
    , transformString
    , viewNavSlices
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
import Json.Decode as Decode
import Svg exposing (Attribute, Svg)
import Svg.Attributes
import Svg.Events



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


viewNavSlice : List (Attribute msg) -> Float -> Float -> Svg msg
viewNavSlice attrs startAngle size =
    let
        radius : Float
        radius =
            600

        point : Float -> Float -> Vector2
        point a r =
            Vector2.new (cos (a * (pi / 180)) * r) (sin (a * (pi / 180)) * r)

        pointPair : Vector2 -> String
        pointPair p =
            String.fromInt (round p.x) ++ "," ++ String.fromInt (round p.y)

        points : String
        points =
            [ point (radians startAngle) 0
            , point (radians startAngle) radius
            , point (radians (startAngle + size)) radius
            , point (radians (startAngle + size)) 0
            ]
                |> List.map pointPair
                |> List.intersperse " "
                |> String.concat
    in
    Svg.polygon
        ([ Svg.Attributes.class "nav-slice"
         , Svg.Attributes.points points
         ]
            ++ attrs
        )
        []


viewNavSlices : (Bool -> msg) -> (Float -> msg) -> Int -> Svg msg
viewNavSlices toggleMoveEvent hoverEvent sliceCount =
    let
        sliceSize : Float
        sliceSize =
            360 / toFloat sliceCount

        slice : Float -> Svg msg
        slice i =
            viewNavSlice
                [ Svg.Events.onMouseOver <| hoverEvent (i * sliceSize)
                , Svg.Events.onMouseDown <| toggleMoveEvent True
                , Svg.Events.onMouseUp <| toggleMoveEvent False
                , Svg.Events.on "touchstart" (Decode.succeed <| toggleMoveEvent True)
                , Svg.Events.on "touchend" (Decode.succeed <| toggleMoveEvent False)
                , Svg.Events.on "ontouchmove" (Decode.succeed <| hoverEvent (i * sliceSize))
                ]
                (i * sliceSize)
                sliceSize
    in
    Svg.g []
        (List.range 1 sliceCount |> List.map toFloat |> List.map slice)


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
            , cameraTransform <| (.position <| World.getPlayer particles)
            ]
            [ Svg.g []
                (World.getParticles particles
                    |> List.filter (\o -> Particle.distance (World.getPlayer particles) o < config.renderDistance)
                    |> List.map (viewParticle config.debug)
                )
            ]
            :: children
        )

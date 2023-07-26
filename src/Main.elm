module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Particles
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.Grid as Grid
import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Render as Render exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Attributes
import ParticleState exposing (ParticleState(..), Tile(..))
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy



-- SYSTEM


forces : ParticleSystem ParticleState Tile -> ParticleSystem ParticleState Tile
forces system =
    let
        moveSpeed =
            0.05

        forceHelper o =
            case o.state of
                MoveToCenter ->
                    Particle.moveToPosition 50 Vector2.zero moveSpeed o

                MoveToPosition p ->
                    Particle.moveToPosition 50 p moveSpeed o

                FollowMoveToPosition ->
                    let
                        followTarget t =
                            case t.state of
                                MoveToPosition _ ->
                                    True

                                _ ->
                                    False
                    in
                    Particle.moveToNearest (system |> ParticleSystem.getParticles |> List.filter followTarget) moveSpeed o

                MoveToClosest ->
                    Particle.moveToNearest (system |> ParticleSystem.getParticles) moveSpeed o

                Idle ->
                    o

                Avoid ->
                    Particle.moveAwayRange 100 (system |> ParticleSystem.getParticles) moveSpeed o
    in
    ParticleSystem.updateParticles forceHelper system


movement : Float -> ParticleSystem ParticleState Tile -> ParticleSystem ParticleState Tile
movement dt system =
    ParticleSystem.updateParticles (Particle.move dt >> Particle.applyFriciton 0.02 >> Particle.stopIfSlow 0.0001) system


resolveCollisions : ParticleSystem ParticleState Tile -> ParticleSystem ParticleState Tile
resolveCollisions system =
    ParticleSystem.updateParticles (Particle.resolveCollisions (system |> ParticleSystem.getParticles)) system



-- CONSOLE


initConsole : Console Msg
initConsole =
    Engine.Console.new
        |> Engine.Console.addMessage "Set render debug mode"
            (Engine.Console.constructor1
                SetRenderDebug
                (Engine.Console.argBool "Debug enabled")
            )
        |> Engine.Console.addMessage "Set render distance"
            (Engine.Console.constructor1
                SetDrawDistance
                (Engine.Console.argFloat "Distance")
            )



-- MODEL


type alias Model =
    { particles : ParticleSystem ParticleState Tile
    , renderConfig : RenderConfig
    , console : Console Msg
    , stepTime : Float
    , timeAccum : Float
    , renderDebug : Bool
    , deltaHistory : List Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Particles.particleSystem1
        (Render.initRenderConfig |> Render.withRenderDistance 1000)
        initConsole
        20
        0
        False
        []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | ConsoleMsg (ConsoleMsg Msg)
    | SetRenderDebug Bool
    | SetDrawDistance Float
    | SetMoveTarget Vector2


focusCamera : Model -> Model
focusCamera model =
    let
        playerPos =
            model.particles |> ParticleSystem.getPlayer |> .position

        cameraDist =
            Vector2.distance playerPos model.renderConfig.position
    in
    if cameraDist > 50 then
        { model | renderConfig = Render.withPosition playerPos model.renderConfig }

    else
        model


fixedUpdate : (Model -> Model) -> Float -> Model -> Model
fixedUpdate f dt world =
    if dt >= world.stepTime then
        { world | timeAccum = dt - world.stepTime }
            |> f
            |> fixedUpdate f (dt - world.stepTime)

    else
        { world | timeAccum = dt }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model | deltaHistory = dt :: model.deltaHistory |> List.take 20 }
                |> focusCamera
                |> fixedUpdate
                    (\m ->
                        { m | particles = m.particles |> forces >> movement model.stepTime >> resolveCollisions }
                    )
                    (model.timeAccum + dt)

        ConsoleMsg cmsg ->
            let
                ( newConsole, mmsg ) =
                    Engine.Console.update cmsg model.console
            in
            case mmsg of
                Just m ->
                    { model | console = newConsole } |> update m

                Nothing ->
                    { model | console = newConsole }

        SetRenderDebug flag ->
            { model | renderDebug = flag }

        SetDrawDistance dist ->
            { model | renderConfig = Render.withRenderDistance dist model.renderConfig }

        SetMoveTarget target ->
            let
                helper p =
                    case p.state of
                        MoveToPosition _ ->
                            { p | state = MoveToPosition target }

                        _ ->
                            p
            in
            { model | particles = ParticleSystem.updateParticles helper model.particles }



-- VIEW


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromInt (round position.x)
        ++ ", "
        ++ String.fromInt (round position.y)
        ++ ")"


viewParticle : Bool -> Particle ParticleState -> Svg msg
viewParticle showVectors particle =
    let
        typeString =
            case particle.state of
                MoveToCenter ->
                    "move-center"

                MoveToPosition _ ->
                    "move-to"

                FollowMoveToPosition ->
                    "follow-move-to"

                MoveToClosest ->
                    "move-closest"

                Idle ->
                    "idle"

                Avoid ->
                    "avoid"
    in
    Svg.g
        [ Svg.Attributes.transform <| transformString particle.position
        , Svg.Attributes.class "particle"
        , Svg.Attributes.class typeString
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
                        , Svg.Attributes.x2 <| String.fromInt (round (particle.velocity.x * 300))
                        , Svg.Attributes.y2 <| String.fromInt (round (particle.velocity.y * 300))
                        , Svg.Attributes.class "velocity"
                        ]
                        []
                    ]

                else
                    []
               )
        )


viewTile2D : ( Grid.Point, Tile ) -> Svg Msg
viewTile2D ( p, t ) =
    let
        isOdd n =
            modBy 2 n == 1

        evenOddClass =
            if Tuple.first p + Tuple.second p |> isOdd then
                "odd"

            else
                "even"

        tileType =
            case t of
                Water ->
                    "water"

                Ground ->
                    "ground"

                Wall ->
                    "wall"
    in
    Svg.g
        [ Svg.Attributes.class "tile"
        , Svg.Attributes.class evenOddClass
        , Svg.Attributes.class tileType
        ]
        [ Render.rect2d
            [ Svg.Events.onClick <| SetMoveTarget (Grid.toVector2 p |> Vector2.scale (toFloat Render.tileSize))
            ]
        , Svg.text_ [ Svg.Attributes.class "coordinates" ] [ Svg.text <| (Tuple.first p |> String.fromInt) ++ ", " ++ (Tuple.second p |> String.fromInt) ]
        ]


transformStyle : Vector2 -> Svg.Attribute msg
transformStyle position =
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromInt (round -position.x)
            ++ "px, "
            ++ String.fromInt (round -position.y)
            ++ "px)"


fpsString : List Float -> String
fpsString dts =
    let
        averageDelta =
            List.sum dts / toFloat (List.length dts)

        averageFps =
            1000 / averageDelta
    in
    averageFps
        |> String.fromFloat
        |> String.split "."
        |> List.head
        |> Maybe.withDefault "-"


view : Model -> Html Msg
view model =
    main_ []
        [ Html.div [ Html.Attributes.class "fps-display" ] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]

        -- , Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g
                [ Svg.Attributes.class "camera"
                , transformStyle <| (.position <| ParticleSystem.getPlayer model.particles)
                ]
                [ Svg.Lazy.lazy (Render.view2DGrid viewTile2D) (ParticleSystem.getMap model.particles)
                , Svg.g []
                    (ParticleSystem.getParticles model.particles
                        |> List.filter (\o -> Vector2.distance (.position <| ParticleSystem.getPlayer model.particles) o.position < model.renderConfig.renderDistance)
                        |> List.map (viewParticle model.renderDebug)
                    )
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 10000 >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

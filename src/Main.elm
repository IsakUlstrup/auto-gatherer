module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Grids
import Content.Particles
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.HexGrid exposing (HexGrid)
import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Point exposing (Point)
import Engine.Render as Render exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import ParticleState exposing (ParticleState(..))
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy



-- SYSTEM


forces : ParticleSystem ParticleState -> ParticleSystem ParticleState
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


movement : Float -> ParticleSystem ParticleState -> ParticleSystem ParticleState
movement dt system =
    ParticleSystem.updateParticles (Particle.move dt >> Particle.applyFriciton 0.02 >> Particle.stopIfSlow 0.0001) system


resolveCollisions : ParticleSystem ParticleState -> ParticleSystem ParticleState
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
    { particles : ParticleSystem ParticleState
    , map : HexGrid ()
    , renderConfig : RenderConfig
    , console : Console Msg
    , stepTime : Float
    , timeAccum : Float
    , renderDebug : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Particles.particleSystem1
        (Content.Grids.circle 50)
        (Render.initRenderConfig |> Render.withRenderDistance 400)
        initConsole
        20
        0
        False
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
    { model | renderConfig = Render.withPosition (model.particles |> ParticleSystem.getPlayer |> .position) model.renderConfig }


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
            model
                |> fixedUpdate
                    (\m ->
                        { m | particles = m.particles |> forces >> movement model.stepTime >> resolveCollisions }
                            |> focusCamera
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


viewTile : ( Point, () ) -> Svg Msg
viewTile ( p, _ ) =
    Svg.polygon
        [ Svg.Attributes.points "40,0 20,35 -20,35 -40,0 -20,-35 20,-35"
        , Svg.Attributes.class "tile"
        , Svg.Events.onClick <| SetMoveTarget (Render.pointToPixel p)
        ]
        []


gooFilter : Svg msg
gooFilter =
    -- <filter id="drop-shadow">
    --     <feGaussianBlur in="SourceGraphic" stdDeviation="7" result="shadow" />
    --     <feOffset in="shadow" dx="3" dy="4" result="shadow" />
    --     <feColorMatrix in="shadow" type="matrix" values="0 0 0 0 0  0 0 0 0 0  0 0 0 0 0  0 0 0 0.6 0" result="shadow" />
    --     <feBlend in="SourceGraphic" in2="shadow" />
    -- </filter>
    Svg.filter [ Svg.Attributes.id "goo-filter" ]
        [ Svg.feGaussianBlur
            [ Svg.Attributes.in_ "SourceGraphic"
            , Svg.Attributes.stdDeviation "5"
            , Svg.Attributes.result "blur"
            ]
            []
        , Svg.feColorMatrix
            [ Svg.Attributes.in_ "blur"
            , Svg.Attributes.type_ "matrix"
            , Svg.Attributes.values "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 19 -9"
            , Svg.Attributes.result "goo"
            ]
            []

        -- , Svg.feComposite
        --     [ Svg.Attributes.in_ "SourceGraphic"
        --     , Svg.Attributes.in2 "goo"
        --     , Svg.Attributes.operator "atop"
        --     ]
        --     []
        ]


transformStyle : Vector2 -> Svg.Attribute msg
transformStyle position =
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromFloat -position.x
            ++ "px, "
            ++ String.fromFloat -position.y
            ++ "px) scale(0.7)"


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g
                [ Svg.Attributes.class "camera"
                , transformStyle <| model.renderConfig.position
                ]
                [ Svg.defs [] [ gooFilter ]
                , Svg.Lazy.lazy2 (Render.viewMap viewTile) model.renderConfig model.map
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

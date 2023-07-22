module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.HexGrid as Grid exposing (HexGrid)
import Engine.Particle as Particle exposing (Particle)
import Engine.ParticleSystem as ParticleSystem exposing (ParticleSystem)
import Engine.Point exposing (Point)
import Engine.Render exposing (RenderConfig)
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
        (ParticleSystem.empty
            |> ParticleSystem.addParticle 200 20 40 MoveToCenter
            |> ParticleSystem.addParticle -300 200 70 Idle
            |> ParticleSystem.addParticle 0 0 30 (MoveToPosition <| Vector2.new 200 -175)
            |> ParticleSystem.addParticle -97 20 20 MoveToClosest
            |> ParticleSystem.addParticle -100 20 20 MoveToCenter
            |> ParticleSystem.addParticle -101 20 20 MoveToCenter
            |> ParticleSystem.addParticle -102 20 20 MoveToCenter
            |> ParticleSystem.addParticle -103 20 20 MoveToCenter
            |> ParticleSystem.addParticle -104 20 20 MoveToCenter
            |> ParticleSystem.addParticle -150 20 20 MoveToClosest
            |> ParticleSystem.addParticle -150 50 20 MoveToClosest
            |> ParticleSystem.addParticle 150 20 20 MoveToClosest
            |> ParticleSystem.addParticle 0 0 70 Idle
            |> ParticleSystem.addParticle -100 -100 30 (MoveToPosition <| Vector2.new 50 -75)
            |> ParticleSystem.addParticle 100 100 30 (MoveToPosition <| Vector2.new 150 -75)
            |> ParticleSystem.addParticle 140 100 10 FollowMoveToPosition
            |> ParticleSystem.addParticle 100 -107 8 FollowMoveToPosition
            |> ParticleSystem.addParticle -107 12 9 FollowMoveToPosition
            |> ParticleSystem.addParticle -240 -107 7 FollowMoveToPosition
            |> ParticleSystem.addParticle -340 -107 23 Avoid
            |> ParticleSystem.addParticle 240 -17 18 Avoid
        )
        (Grid.empty
            |> Grid.insertTile ( 0, 0, 0 ) ()
            |> Grid.insertTile ( -1, 0, 1 ) ()
            |> Grid.insertTile ( 1, 1, -2 ) ()
            |> Grid.insertTile ( 1, 1, -3 ) ()
            |> Grid.insertTile ( 1, 2, -4 ) ()
            |> Grid.insertTile ( 2, 2, -4 ) ()
            |> Grid.insertTile ( 3, 2, -5 ) ()
            |> Grid.insertTile ( 0, 2, -2 ) ()
            |> Grid.insertTile ( -1, 2, -1 ) ()
            |> Grid.insertTile ( -2, 2, 0 ) ()
            |> Grid.insertTile ( -2, 0, 2 ) ()
            |> Grid.insertTile ( -3, 1, 2 ) ()
            |> Grid.insertTile ( -4, 2, 2 ) ()
            |> Grid.insertTile ( 3, -2, -1 ) ()
            |> Grid.insertTile ( 3, 1, -4 ) ()
            |> Grid.insertTile ( -2, -2, 4 ) ()
            |> Grid.insertTile ( 0, 2, -2 ) ()
            |> Grid.insertTile ( 0, 3, -3 ) ()
            |> Grid.insertTile ( 0, 4, -4 ) ()
            |> Grid.insertTile ( 0, 5, -5 ) ()
            |> Grid.insertTile ( 2, 3, -5 ) ()
        )
        (Engine.Render.initRenderConfig |> Engine.Render.withRenderDistance 1000)
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
            fixedUpdate (\m -> { m | particles = m.particles |> forces >> movement model.stepTime >> resolveCollisions }) (model.timeAccum + dt) model

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
            { model | renderConfig = Engine.Render.withRenderDistance dist model.renderConfig }

        SetMoveTarget target ->
            let
                helper p =
                    case p.state of
                        MoveToPosition _ ->
                            { p | state = MoveToPosition target }

                        _ ->
                            p
            in
            { model
                | particles = ParticleSystem.updateParticles helper model.particles
                , renderConfig = Engine.Render.withPosition target model.renderConfig
            }



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
        [ Engine.Render.generateHexCorners |> Engine.Render.cornersToPoints
        , Svg.Attributes.class "tile"
        , Svg.Events.onClick <| SetMoveTarget (Engine.Render.pointToPixel p)
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


view : Model -> Html Msg
view model =
    main_ []
        [ -- Html.map ConsoleMsg (Html.Lazy.lazy Engine.Console.viewConsole model.console)
          Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g
                [ Svg.Attributes.class "camera"
                , Svg.Attributes.style <| "transform: translate(" ++ String.fromFloat -model.renderConfig.position.x ++ "px, " ++ String.fromFloat -model.renderConfig.position.y ++ "px)"
                ]
                [ Svg.defs [] [ gooFilter ]
                , Svg.Lazy.lazy (Engine.Render.viewMap viewTile) model.map
                , Svg.g [] (ParticleSystem.getParticles model.particles |> List.filter (\o -> Vector2.distance Vector2.zero o.position < model.renderConfig.renderDistance) |> List.map (viewParticle model.renderDebug))
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

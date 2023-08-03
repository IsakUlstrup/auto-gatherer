module Main exposing (Model, Msg, main)

-- import Engine.Render as Render exposing (RenderConfig)

import Browser
import Browser.Events
import Content.ParticleState exposing (ParticleState(..))
import Content.Worlds
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.Particle as Particle exposing (PhysicsType(..))
import Engine.Vector2 as Vector2 exposing (Vector2)
import Engine.World as World exposing (World)
import Html exposing (Html, main_)
import Html.Attributes
import SvgRenderer exposing (RenderConfig)
import WebGlRenderer



-- SYSTEM


forces : World ParticleState -> World ParticleState
forces system =
    let
        forceHelper : Particle.Particle ParticleState -> Particle.Particle ParticleState
        forceHelper o =
            case o.state of
                MoveToCenter ->
                    Particle.moveToPosition 50 Vector2.zero o

                MoveToPosition p ->
                    Particle.moveToPosition 50 p o

                FollowMoveToPosition range ->
                    let
                        followTarget : Particle.Particle ParticleState -> Bool
                        followTarget t =
                            case t.state of
                                MoveToPosition _ ->
                                    True

                                _ ->
                                    False

                        isInRange p =
                            Particle.distance p o < range
                    in
                    Particle.moveToNearest 50 (system |> World.getParticles |> List.filter followTarget |> List.filter isInRange) o

                MoveToClosest ->
                    Particle.moveToNearest 50 (system |> World.getParticles) o

                Idle ->
                    o

                Avoid ->
                    Particle.moveAwayRange 100 (system |> World.getParticles) o

                FollowId id ->
                    Particle.moveToId 5 id (system |> World.getParticles) o
    in
    World.updateParticles forceHelper system


movement : Float -> World ParticleState -> World ParticleState
movement dt system =
    World.updateParticles (Particle.move dt >> Particle.applyFriciton 0.05 >> Particle.stopIfSlow 0.0001) system


resolveCollisions : World ParticleState -> World ParticleState
resolveCollisions system =
    system
        |> World.updateParticles (Particle.resolveCollisions (system |> World.getParticles))



-- CONSOLE


initConsole : Console Msg
initConsole =
    Engine.Console.new
        |> Engine.Console.addMessage "Set render debug mode"
            (Engine.Console.constructor1
                SetRenderDebug
                (Engine.Console.argBool "Debug enabled")
            )
        |> Engine.Console.addMessage "Enable experimental renderer"
            (Engine.Console.constructor1
                SetExperimentalRender
                (Engine.Console.argBool "Experimental")
            )
        |> Engine.Console.addMessage "Set render distance"
            (Engine.Console.constructor1
                SetDrawDistance
                (Engine.Console.argFloat "Distance")
            )



-- MODEL


type alias Model =
    { particles : World ParticleState
    , renderConfig : RenderConfig
    , console : Console Msg
    , stepTime : Float
    , timeAccum : Float
    , renderDebug : Bool
    , deltaHistory : List Float
    , experimentalRender : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.testWorld1
        (SvgRenderer.initRenderConfig |> SvgRenderer.withRenderDistance 600)
        initConsole
        20
        0
        False
        []
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
    | SetExperimentalRender Bool


focusCamera : Model -> Model
focusCamera model =
    let
        playerPos : Vector2
        playerPos =
            model.particles |> World.getPlayer |> .position

        cameraDist : Float
        cameraDist =
            Vector2.distance playerPos model.renderConfig.position
    in
    if cameraDist > 100 then
        { model | renderConfig = SvgRenderer.withPosition playerPos model.renderConfig }

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
            { model | renderConfig = SvgRenderer.withRenderDistance dist model.renderConfig }

        SetMoveTarget target ->
            let
                helper : Particle.Particle ParticleState -> Particle.Particle ParticleState
                helper p =
                    case p.state of
                        MoveToPosition _ ->
                            { p | state = MoveToPosition target }

                        _ ->
                            p
            in
            { model | particles = World.updatePlayer helper model.particles }

        SetExperimentalRender flag ->
            { model | experimentalRender = flag }



-- VIEW


fpsString : List Float -> String
fpsString dts =
    let
        averageDelta : Float
        averageDelta =
            List.sum dts / toFloat (List.length dts)

        averageFps : Float
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
        , Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , if model.experimentalRender then
            WebGlRenderer.viewWebGl 1000 1000 2 (World.getParticles model.particles)

          else
            SvgRenderer.viewSvg SetMoveTarget model.particles model.renderConfig
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

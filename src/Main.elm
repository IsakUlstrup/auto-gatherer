module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.ParticleState exposing (ParticleState(..), particleForce)
import Content.Worlds
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.Particle as Particle
import Engine.Vector2 as Vector2 exposing (Vector2)
import Engine.World as World exposing (World)
import Html exposing (Html, main_)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Svg.Events exposing (on)
import SvgRenderer exposing (RenderConfig)



-- SYSTEM


forces : World ParticleState -> World ParticleState
forces world =
    World.updateParticlesWithSeed (particleForce (World.getParticles world)) world


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
    , deltaHistory : List Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.testWorld1
        (SvgRenderer.initRenderConfig |> SvgRenderer.withRenderDistance 600)
        initConsole
        20
        0
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
    | WindowResize Int Int
    | GameClick Int Int


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


fixedUpdate : Float -> Model -> Model
fixedUpdate dt model =
    if dt >= model.stepTime then
        { model
            | timeAccum = dt - model.stepTime
            , particles =
                model.particles
                    |> forces
                    |> movement model.stepTime
                    |> resolveCollisions
        }
            |> fixedUpdate (dt - model.stepTime)

    else
        { model | timeAccum = dt }


addDtHistory : Float -> Model -> Model
addDtHistory dt model =
    { model | deltaHistory = dt :: model.deltaHistory |> List.take 20 }


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            model
                |> addDtHistory dt
                |> focusCamera
                |> fixedUpdate (model.timeAccum + dt)

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
            { model | renderConfig = SvgRenderer.withDebug flag model.renderConfig }

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

        WindowResize w h ->
            { model
                | renderConfig =
                    model.renderConfig
                        |> SvgRenderer.withWidth w
                        |> SvgRenderer.withHeight h
            }

        GameClick x y ->
            let
                _ =
                    Debug.log "click" ( toFloat x / toFloat model.renderConfig.windowWidth, toFloat y / toFloat model.renderConfig.windowHeight )
            in
            model



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


clickDecoder : Decoder Msg
clickDecoder =
    Decode.map2 GameClick
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)


view : Model -> Html Msg
view model =
    main_ []
        [ Html.div [ Html.Attributes.class "fps-display" ] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]
        , Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , SvgRenderer.viewSvg [ on "click" clickDecoder ] model.particles model.renderConfig
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (min 10000 >> Tick)
        , Browser.Events.onResize WindowResize
        ]



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

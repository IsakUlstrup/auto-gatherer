module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.ParticleState exposing (ParticleState(..), particleForce)
import Content.Worlds
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.Particle as Particle
import Engine.Vector2 as Vector2
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


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model
        Content.Worlds.testWorld1
        (SvgRenderer.initRenderConfig
            |> SvgRenderer.withRenderDistance 600
            |> SvgRenderer.withWidth flags.width
            |> SvgRenderer.withHeight flags.height
        )
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
    | WindowResize Int Int
    | GameClick Int Int


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

        WindowResize w h ->
            { model
                | renderConfig =
                    model.renderConfig
                        |> SvgRenderer.withWidth w
                        |> SvgRenderer.withHeight h
            }

        GameClick x y ->
            let
                helper : Particle.Particle ParticleState -> Particle.Particle ParticleState
                helper p =
                    case p.state of
                        MoveToPosition _ ->
                            { p | state = MoveToPosition force }

                        _ ->
                            p

                force =
                    Vector2.new (toFloat x - (toFloat model.renderConfig.windowWidth * 0.5))
                        (toFloat y - (toFloat model.renderConfig.windowHeight * 0.5))
                        |> Vector2.add (World.getPlayer model.particles).position
            in
            { model | particles = World.updatePlayer helper model.particles }



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
        [ Html.div [ Html.Attributes.class "render-stats" ]
            [ Html.div [] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]
            , Html.div [] [ Html.text <| "window size: " ++ String.fromInt model.renderConfig.windowWidth ++ "x" ++ String.fromInt model.renderConfig.windowHeight ]
            ]
        , Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , SvgRenderer.viewSvg [ on "click" clickDecoder ] model.particles model.renderConfig
        ]



-- DECODERS


clickDecoder : Decoder Msg
clickDecoder =
    Decode.map2 GameClick
        (Decode.field "offsetX" Decode.int)
        (Decode.field "offsetY" Decode.int)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (min 10000 >> Tick)
        , Browser.Events.onResize WindowResize
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

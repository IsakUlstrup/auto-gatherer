module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
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
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import SvgRenderer exposing (RenderConfig)
import Task



-- SYSTEM


forces : Cursor -> World ParticleState -> World ParticleState
forces cursor world =
    let
        cursorForce =
            if cursor.pressed then
                Vector2.direction Vector2.zero cursor.position |> Vector2.scale -0.3

            else
                Vector2.zero
    in
    world
        |> World.updateParticlesWithSeed (particleForce (World.getParticles world))
        |> World.updatePlayer (Particle.applyForce cursorForce)


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
    , cursor : Cursor
    }


type alias Flags =
    ()


type alias Cursor =
    { position : Vector2
    , pressed : Bool
    }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.testWorld1
        (SvgRenderer.initRenderConfig
            |> SvgRenderer.withRenderDistance 600
        )
        initConsole
        20
        0
        []
        (Cursor Vector2.zero False)
    , gameResize
    )



-- UPDATE


type Msg
    = Tick Float
    | ConsoleMsg (ConsoleMsg Msg)
    | SetRenderDebug Bool
    | SetDrawDistance Float
    | SetCursorPosition Float Float
    | SetCursorPressed Bool
    | WindowResize
    | GetGameElement (Result Browser.Dom.Error Browser.Dom.Element)


fixedUpdate : Float -> Model -> Model
fixedUpdate dt model =
    if dt >= model.stepTime then
        { model
            | timeAccum = dt - model.stepTime
            , particles =
                model.particles
                    |> forces model.cursor
                    |> movement model.stepTime
                    |> resolveCollisions
        }
            |> fixedUpdate (dt - model.stepTime)

    else
        { model | timeAccum = dt }


addDtHistory : Float -> Model -> Model
addDtHistory dt model =
    { model | deltaHistory = dt :: model.deltaHistory |> List.take 20 }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> addDtHistory dt
                |> fixedUpdate (model.timeAccum + dt)
            , Cmd.none
            )

        ConsoleMsg cmsg ->
            let
                ( newConsole, mmsg ) =
                    Engine.Console.update cmsg model.console
            in
            case mmsg of
                Just m ->
                    { model | console = newConsole } |> update m

                Nothing ->
                    ( { model | console = newConsole }, Cmd.none )

        SetRenderDebug flag ->
            ( { model | renderConfig = SvgRenderer.withDebug flag model.renderConfig }, Cmd.none )

        SetDrawDistance dist ->
            ( { model | renderConfig = SvgRenderer.withRenderDistance dist model.renderConfig }, Cmd.none )

        SetCursorPosition x y ->
            let
                pos =
                    SvgRenderer.screenToWorldCoords
                        x
                        y
                        (toFloat model.renderConfig.screenWidth)
                        (toFloat model.renderConfig.screenHeight)
            in
            ( { model | cursor = Cursor pos model.cursor.pressed }, Cmd.none )

        SetCursorPressed pressed ->
            ( { model | cursor = Cursor model.cursor.position pressed }, Cmd.none )

        WindowResize ->
            ( model, gameResize )

        GetGameElement (Ok element) ->
            ( { model
                | renderConfig =
                    model.renderConfig
                        |> SvgRenderer.withWidth (round element.element.width)
                        |> SvgRenderer.withHeight (round element.element.height)
              }
            , Cmd.none
            )

        GetGameElement (Err _) ->
            ( model, Cmd.none )



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


screenSizeString : Int -> Int -> String
screenSizeString width height =
    "screen: " ++ String.fromInt width ++ "x" ++ String.fromInt height


viewCursor : Cursor -> Svg msg
viewCursor cursor =
    let
        pressedClass =
            if cursor.pressed then
                "pressed"

            else
                "not-pressed"
    in
    Svg.circle
        [ Svg.Attributes.transform <| SvgRenderer.transformString cursor.position
        , Svg.Attributes.r "20"
        , Svg.Attributes.class "cursor"
        , Svg.Attributes.class pressedClass
        ]
        []


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , SvgRenderer.viewSvg
            [ Svg.Attributes.id "game-view"
            , Svg.Events.on "mousemove" clickDecoder
            , Svg.Events.onMouseDown <| SetCursorPressed True
            , Svg.Events.onMouseUp <| SetCursorPressed False
            ]
            [ viewCursor model.cursor ]
            model.particles
            model.renderConfig
        , Html.div [ Html.Attributes.class "render-stats" ]
            [ Html.div [] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]
            , Html.div [] [ Html.text <| screenSizeString model.renderConfig.screenWidth model.renderConfig.screenHeight ]
            ]
        ]


clickDecoder : Decoder Msg
clickDecoder =
    Decode.map2 SetCursorPosition
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)



-- SUBSCRIPTIONS


gameResize : Cmd Msg
gameResize =
    Browser.Dom.getElement "game-view" |> Task.attempt GetGameElement


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta (min 10000 >> Tick)
        , Browser.Events.onResize (\_ _ -> WindowResize)
        ]



-- MAIN


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

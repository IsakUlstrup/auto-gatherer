module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Content.ParticleState exposing (ParticleState(..), particleForce)
import Content.Worlds
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
        cursorSpeedMultiplier =
            -- (Vector2.distance Vector2.zero cursor.position + 100) / 500
            (500 - Vector2.distance Vector2.zero cursor.position) / 500

        cursorForce =
            if cursor.pressed then
                Vector2.direction Vector2.zero cursor.position
                    |> Vector2.scale cursorSpeedMultiplier

            else
                Vector2.zero
    in
    world
        |> World.updateParticlesWithSeed (particleForce (World.getParticles world))
        |> World.updatePlayer (\p -> Particle.applyForce (Vector2.scale -(Particle.getSpeed p) cursorForce) p)


movement : Float -> World ParticleState -> World ParticleState
movement dt system =
    World.updateParticles (Particle.move dt >> Particle.applyFriciton 0.05 >> Particle.stopIfSlow 0.0001) system


resolveCollisions : World ParticleState -> World ParticleState
resolveCollisions system =
    system
        |> World.updateParticles (Particle.resolveCollisions (system |> World.getParticles))



-- MODEL


type alias Model =
    { particles : World ParticleState
    , renderConfig : RenderConfig
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
        20
        0
        []
        (Cursor Vector2.zero False)
    , gameResize
    )



-- UPDATE


type Msg
    = Tick Float
    | SetRenderDebug Bool
    | SetDrawDistance Float
    | SetCursor Float Float Bool
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

        SetRenderDebug flag ->
            ( { model | renderConfig = SvgRenderer.withDebug flag model.renderConfig }, Cmd.none )

        SetDrawDistance dist ->
            ( { model | renderConfig = SvgRenderer.withRenderDistance dist model.renderConfig }, Cmd.none )

        SetCursor x y pressed ->
            let
                pos =
                    SvgRenderer.screenToWorldCoords
                        x
                        y
                        (toFloat model.renderConfig.screenWidth)
                        (toFloat model.renderConfig.screenHeight)
            in
            ( { model | cursor = Cursor pos pressed }, Cmd.none )

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
        [ Html.div [ Html.Attributes.class "render-stats" ]
            [ Html.div [] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]
            , Html.div [] [ Html.text <| screenSizeString model.renderConfig.screenWidth model.renderConfig.screenHeight ]
            ]
        , SvgRenderer.viewSvg
            [ Svg.Attributes.id "game-view"
            , Svg.Events.on "pointermove" pointerDecoder
            , Svg.Events.on "pointerdown" pointerDecoder
            , Svg.Events.on "pointerup" pointerDecoder
            , Svg.Events.on "pointercancel" pointerDecoder
            ]
            [ viewCursor model.cursor ]
            model.particles
            model.renderConfig
        ]


pressedDecoder : Decoder Bool
pressedDecoder =
    Decode.field "buttons" Decode.float
        |> Decode.map (\p -> p > 0)


pointerDecoder : Decoder Msg
pointerDecoder =
    Decode.map3 SetCursor
        (Decode.field "offsetX" Decode.float)
        (Decode.field "offsetY" Decode.float)
        pressedDecoder



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

module Main exposing (Cursor, Flags, Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Content.ParticleState as ParticleState exposing (ParticleState(..), particleForce)
import Content.Worlds
import Engine.Particle as Particle exposing (Particle, PhysicsType(..))
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.SvgRenderer exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task



-- SYSTEM


forces : Cursor -> ParticleSystem ParticleState -> ParticleSystem ParticleState
forces cursor world =
    let
        cursorForce : Vector2
        cursorForce =
            if cursor.pressed then
                Vector2.direction Vector2.zero cursor.position
                    |> Vector2.scale ((500 - Vector2.distance Vector2.zero cursor.position) / 500)

            else
                Vector2.zero
    in
    world
        |> World.updateParticlesWithSeed (particleForce (World.getParticles world))
        |> World.updatePlayer (\p -> Particle.applyForce (Vector2.scale -(Particle.getSpeed p) cursorForce) p)


movement : Float -> ParticleSystem ParticleState -> ParticleSystem ParticleState
movement dt system =
    World.updateParticles (Particle.move dt >> Particle.applyFriciton 0.05 >> Particle.stopIfSlow 0.0001) system


state : Float -> ParticleSystem ParticleState -> ParticleSystem ParticleState
state dt system =
    World.updateParticles (ParticleState.stateUpdate dt) system


spawn : ParticleSystem ParticleState -> ParticleSystem ParticleState
spawn system =
    let
        readySummoner p =
            case p.state of
                Summon cd ->
                    if cd <= 0 then
                        True

                    else
                        False

                _ ->
                    False

        ready : List ( Vector2, ParticleState )
        ready =
            World.getParticles system
                |> List.filter readySummoner
                |> List.map (\p -> ( Vector2.add (Vector2.new (p.radius + 20) 0) p.position, Idle ))
    in
    World.addParticles ready system


collisionInteraction : ParticleSystem ParticleState -> ParticleSystem ParticleState
collisionInteraction system =
    let
        shouldKeep targets p =
            case ( List.filter (Particle.isColliding p) targets |> List.head, p.state ) of
                ( Just _, DestroyOnHit ) ->
                    False

                _ ->
                    True
    in
    World.filterParticles (shouldKeep <| World.getParticles system) system


resolveCollisions : ParticleSystem ParticleState -> ParticleSystem ParticleState
resolveCollisions system =
    World.updateParticles (Particle.resolveCollisions (system |> World.getParticles)) system



-- MODEL


type alias Model =
    { particles : ParticleSystem ParticleState
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
        (Engine.SvgRenderer.initRenderConfig
            |> Engine.SvgRenderer.withRenderDistance 600
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
    | ToggleRenderDebug
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
                    |> state model.stepTime
                    |> spawn
                    |> forces model.cursor
                    |> movement model.stepTime
                    |> collisionInteraction
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

        ToggleRenderDebug ->
            ( { model | renderConfig = Engine.SvgRenderer.withDebug (not model.renderConfig.debug) model.renderConfig }, Cmd.none )

        SetCursor x y pressed ->
            let
                pos : Vector2
                pos =
                    Engine.SvgRenderer.screenToWorldCoords
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
                        |> Engine.SvgRenderer.withWidth (round element.element.width)
                        |> Engine.SvgRenderer.withHeight (round element.element.height)
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


viewParticle : Bool -> Particle.Particle ParticleState -> Svg msg
viewParticle showVectors particle =
    Svg.g
        [ Engine.SvgRenderer.transformAttr particle.position
        , Svg.Attributes.class "particle"
        , Svg.Attributes.class <| ParticleState.toString particle.state
        , Svg.Attributes.class <| Particle.physicsTypeString particle
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


viewCursor : Cursor -> Svg msg
viewCursor cursor =
    let
        pressedClass : String
        pressedClass =
            if cursor.pressed then
                "pressed"

            else
                "not-pressed"
    in
    Svg.circle
        [ Engine.SvgRenderer.transformAttr cursor.position
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
            , Html.button [ Html.Events.onClick ToggleRenderDebug ] [ Html.text "debug" ]
            ]
        , Engine.SvgRenderer.viewSvg
            [ Svg.Attributes.id "game-view"
            , Svg.Events.on "pointermove" pointerDecoder
            , Svg.Events.on "pointerdown" pointerDecoder
            , Svg.Events.on "pointerup" pointerDecoder
            , Svg.Events.on "pointercancel" pointerDecoder
            ]
            [ viewCursor model.cursor ]
            (viewParticle model.renderConfig.debug)
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

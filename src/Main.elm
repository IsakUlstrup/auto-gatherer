module Main exposing (Flags, Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Color
import Content.Worlds
import Engine.Particle as Particle
import Engine.ParticleSystem as World exposing (ParticleSystem)
import Engine.SvgRenderer exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import GameParticle exposing (Component(..), GameParticle, particleForce)
import Html exposing (Html, main_)
import Html.Attributes
import Html.Events
import Json.Decode as Decode exposing (Decoder)
import Pointer exposing (Pointer)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Task



-- SYSTEM


forces : Pointer -> ParticleSystem Component -> ParticleSystem Component
forces pointer world =
    let
        worldPointer =
            { pointer | position = pointer.position |> Vector2.add (World.getPlayer world |> .position) }
    in
    World.updateParticlesWithTargets (particleForce worldPointer) world


movement : Float -> ParticleSystem Component -> ParticleSystem Component
movement dt system =
    World.updateParticles (Particle.move dt >> Particle.applyFriciton 0.015 >> Particle.stopIfSlow 0.0001) system


collisionInteraction : ParticleSystem Component -> ParticleSystem Component
collisionInteraction system =
    World.collisionAction (\_ p -> Particle.addComponent (Hit 100) p) system


state : Float -> ParticleSystem Component -> ParticleSystem Component
state dt system =
    World.updateParticles (GameParticle.stateUpdate dt) system



-- spawn : ParticleSystem GameParticle -> ParticleSystem GameParticle
-- spawn system =
--     let
--         readySummoner p =
--             case p.state of
--                 Summon cd _ ->
--                     cd <= 0
--                 _ ->
--                     False
--         spawnPosition p =
--             p.position
--                 |> Vector2.add (Vector2.new 35 -35)
--         ready : List (Particle ParticleState)
--         ready =
--             World.getParticles system
--                 |> List.filter readySummoner
--                 |> List.map
--                     (\p -> Particle.new (spawnPosition p) 20 10 (DieCooldown 600))
--     in
--     World.addParticles ready system


cull : ParticleSystem Component -> ParticleSystem Component
cull system =
    World.filterParticles GameParticle.keepParticle system



-- resolveCollisions : ParticleSystem ParticleState -> ParticleSystem ParticleState
-- resolveCollisions system =
--     World.collisions system
-- MODEL


type alias Model =
    { particles : ParticleSystem Component
    , renderConfig : RenderConfig
    , stepTime : Float
    , timeAccum : Float
    , deltaHistory : List Float
    , pointer : Pointer
    }


type alias Flags =
    ()


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
        (Pointer Vector2.zero False)
    , gameResize
    )



-- UPDATE


type Msg
    = Tick Float
    | ToggleRenderDebug
    | Reset
    | SetPointer Float Float Bool
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
                    |> cull
                    |> forces model.pointer
                    |> movement model.stepTime
                    |> collisionInteraction
                    |> World.collisions

            -- |> cull
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

        Reset ->
            init ()

        SetPointer x y pressed ->
            let
                pos : Vector2
                pos =
                    Engine.SvgRenderer.screenToWorldCoords
                        x
                        y
                        (toFloat model.renderConfig.screenWidth)
                        (toFloat model.renderConfig.screenHeight)
            in
            ( { model | pointer = Pointer pos pressed }, Cmd.none )

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


viewDebugComponent : Int -> Component -> Svg msg
viewDebugComponent index component =
    Svg.text_
        [ Svg.Attributes.class "component"
        , Svg.Attributes.transform <| "translate(0 " ++ String.fromInt (index * 10) ++ ")"
        ]
        [ Svg.text <| GameParticle.componentToString component ]


viewParticleDebug : GameParticle -> Svg msg
viewParticleDebug particle =
    Svg.g [ Svg.Attributes.class "debug" ]
        [ Svg.g [ Svg.Attributes.transform <| "translate(10 0)" ] (List.indexedMap viewDebugComponent particle.components)
        , Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromInt (round (particle.velocity.x * 300))
            , Svg.Attributes.y2 <| String.fromInt (round (particle.velocity.y * 300))
            , Svg.Attributes.class "velocity"
            ]
            []
        , Svg.line
            [ Svg.Attributes.x1 "0"
            , Svg.Attributes.y1 "0"
            , Svg.Attributes.x2 <| String.fromInt (round (particle.impulse.x * 300 / 20))
            , Svg.Attributes.y2 <| String.fromInt (round (particle.impulse.y * 300 / 20))
            , Svg.Attributes.class "impulse"
            ]
            []
        ]


viewParticle : Bool -> GameParticle -> Svg msg
viewParticle debug particle =
    let
        colors =
            List.filterMap
                (\c ->
                    case c of
                        Color clr ->
                            Just clr

                        _ ->
                            Nothing
                )
                particle.components

        componentClasses =
            particle.components
                |> List.map GameParticle.componentTypeToString
                |> List.map Svg.Attributes.class

        opacity =
            List.foldl
                (\c o ->
                    case c of
                        Die dur maxDur ->
                            dur / maxDur

                        _ ->
                            o
                )
                1
                particle.components
    in
    Svg.g
        ([ Engine.SvgRenderer.transformAttr particle.position
         , Svg.Attributes.class "particle"
         ]
            ++ componentClasses
        )
        (Svg.circle
            [ Svg.Attributes.r <| String.fromInt (round particle.radius)
            , Svg.Attributes.class "body"
            , Svg.Attributes.fill <| Color.toString <| Color.average colors
            , Svg.Attributes.opacity <| String.fromFloat opacity
            ]
            []
            :: (if debug then
                    [ viewParticleDebug particle ]

                else
                    []
               )
        )


viewPointer : Pointer -> Svg msg
viewPointer pointer =
    let
        pressedClass : String
        pressedClass =
            if pointer.pressed then
                "pressed"

            else
                "not-pressed"
    in
    Svg.circle
        [ Engine.SvgRenderer.transformAttr pointer.position
        , Svg.Attributes.r "20"
        , Svg.Attributes.class "pointer"
        , Svg.Attributes.class pressedClass
        ]
        []


view : Model -> Html Msg
view model =
    main_ []
        [ Html.div [ Html.Attributes.class "render-stats" ]
            [ Html.div [] [ Html.text <| "fps: " ++ fpsString model.deltaHistory ]
            , Html.div [] [ Html.text <| screenSizeString model.renderConfig.screenWidth model.renderConfig.screenHeight ]
            , Html.div [] [ Html.text <| "particle count: " ++ (World.getParticles model.particles |> List.length |> String.fromInt) ]
            , Html.button [ Html.Events.onClick ToggleRenderDebug ] [ Html.text "debug" ]
            , Html.button [ Html.Events.onClick Reset ] [ Html.text "reset" ]
            ]
        , Engine.SvgRenderer.viewSvg
            [ Svg.Attributes.id "game-view"
            , Svg.Events.on "pointermove" pointerDecoder
            , Svg.Events.on "pointerdown" pointerDecoder
            , Svg.Events.on "pointerup" pointerDecoder
            , Svg.Events.on "pointercancel" pointerDecoder
            ]
            [ viewPointer model.pointer ]
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
    Decode.map3 SetPointer
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

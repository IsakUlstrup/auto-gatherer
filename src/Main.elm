module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.HexGrid as Grid exposing (HexGrid)
import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Point exposing (Point)
import Engine.Render exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Lazy
import Svg exposing (Svg)
import Svg.Attributes



-- PARTICLE


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | MoveToClosest
    | Idle



-- SYSTEM


forces : { a | particles : List (PhysicsObject ParticleState) } -> { a | particles : List (PhysicsObject ParticleState) }
forces model =
    let
        moveSpeed =
            0.05

        forceHelper o =
            case o.state of
                MoveToCenter ->
                    PhysicsObject.moveToPosition 50 Vector2.zero moveSpeed o

                MoveToPosition p ->
                    PhysicsObject.moveToPosition 50 p moveSpeed o

                MoveToClosest ->
                    PhysicsObject.moveToNearest model.particles moveSpeed o

                Idle ->
                    o
    in
    { model | particles = List.map forceHelper model.particles }


movement : Float -> { a | particles : List (PhysicsObject ParticleState) } -> { a | particles : List (PhysicsObject ParticleState) }
movement dt model =
    { model
        | particles =
            model.particles
                |> List.map (PhysicsObject.move dt)
                |> List.map (PhysicsObject.applyFriciton 0.01)
                |> List.map (PhysicsObject.stopIfSlow 0.0001)
    }


resolveCollisions : { a | particles : List (PhysicsObject ParticleState) } -> { a | particles : List (PhysicsObject ParticleState) }
resolveCollisions model =
    { model
        | particles =
            model.particles
                |> List.map (PhysicsObject.resolveCollisions model.particles)
    }



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
    { particles : List (PhysicsObject ParticleState)
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
        [ PhysicsObject.new 200 20 40 (40 * 10) 0 MoveToCenter
            |> PhysicsObject.applyForce (Vector2.new -6 0.7)
        , PhysicsObject.new -300 200 70 (70 * 10) 1 Idle
            |> PhysicsObject.applyForce (Vector2.new 0.3 -0.5)
        , PhysicsObject.new 0 0 30 (30 * 10) 2 (MoveToPosition <| Vector2.new 200 -175)
            |> PhysicsObject.applyForce (Vector2.new -2 -3)
        , PhysicsObject.new -100 20 20 (20 * 10) 3 MoveToClosest
            |> PhysicsObject.applyForce (Vector2.new 2 -3)
        , PhysicsObject.new -100 20 20 (20 * 10) 4 MoveToCenter
        , PhysicsObject.new -101 20 20 (20 * 10) 5 MoveToCenter
        , PhysicsObject.new -102 20 20 (20 * 10) 6 MoveToCenter
        , PhysicsObject.new -103 20 20 (20 * 10) 7 MoveToCenter
        , PhysicsObject.new -104 20 20 (20 * 10) 8 MoveToCenter
        , PhysicsObject.new -105 20 20 (20 * 10) 9 MoveToCenter
        , PhysicsObject.new -106 20 20 (20 * 10) 10 MoveToCenter
        , PhysicsObject.new -107 20 20 (20 * 10) 11 MoveToCenter
        , PhysicsObject.new -108 20 20 (20 * 10) 12 MoveToCenter
        , PhysicsObject.new -150 20 20 (20 * 10) 13 MoveToClosest
        , PhysicsObject.new -150 50 20 (20 * 10) 14 MoveToClosest
        , PhysicsObject.new 150 20 20 (20 * 10) 15 MoveToClosest
        , PhysicsObject.new 0 0 70 (70 * 10) 16 Idle
        ]
        (Grid.empty
            |> Grid.insertTile ( 0, 0, 0 ) ()
            |> Grid.insertTile ( -1, 0, 1 ) ()
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
            fixedUpdate (forces >> movement model.stepTime >> resolveCollisions) (model.timeAccum + dt) model

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



-- VIEW


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromInt (round position.x)
        ++ ", "
        ++ String.fromInt (round position.y)
        ++ ")"


viewParticle : Bool -> PhysicsObject ParticleState -> Svg msg
viewParticle showVectors particle =
    let
        typeString =
            case particle.state of
                MoveToCenter ->
                    "move-center"

                MoveToPosition _ ->
                    "move-to"

                MoveToClosest ->
                    "move-closest"

                Idle ->
                    "idle"
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


viewTile : ( Point, () ) -> Svg msg
viewTile _ =
    Svg.polygon
        [ Engine.Render.generateHexCorners |> Engine.Render.cornersToPoints
        , Svg.Attributes.class "tile"
        ]
        []


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Html.Lazy.lazy Engine.Console.viewConsole model.console)
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Engine.Render.viewMap model.renderConfig viewTile model.map
            , Svg.g [] (model.particles |> List.filter (\o -> Vector2.distance Vector2.zero o.position < model.renderConfig.renderDistance) |> List.map (viewParticle model.renderDebug))
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
        }

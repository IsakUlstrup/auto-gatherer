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
import Svg.Events
import Svg.Lazy



-- PARTICLE


type ParticleState
    = MoveToCenter
    | MoveToPosition Vector2
    | FollowMoveToPosition
    | MoveToClosest
    | Idle
    | Avoid



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

                FollowMoveToPosition ->
                    let
                        followTarget t =
                            case t.state of
                                MoveToPosition _ ->
                                    True

                                _ ->
                                    False
                    in
                    PhysicsObject.moveToNearest (List.filter followTarget model.particles) moveSpeed o

                MoveToClosest ->
                    PhysicsObject.moveToNearest model.particles moveSpeed o

                Idle ->
                    o

                Avoid ->
                    PhysicsObject.moveAwayRange 100 model.particles moveSpeed o
    in
    { model | particles = List.map forceHelper model.particles }


movement : Float -> { a | particles : List (PhysicsObject ParticleState) } -> { a | particles : List (PhysicsObject ParticleState) }
movement dt model =
    { model
        | particles =
            model.particles
                |> List.map (PhysicsObject.move dt)
                |> List.map (PhysicsObject.applyFriciton 0.02)
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
        , PhysicsObject.new -100 -100 30 (30 * 10) 17 (MoveToPosition <| Vector2.new 50 -75)
        , PhysicsObject.new 100 100 30 (30 * 10) 18 (MoveToPosition <| Vector2.new 150 -75)
        , PhysicsObject.new 140 100 10 (10 * 10) 19 FollowMoveToPosition
        , PhysicsObject.new 100 -107 8 (8 * 10) 20 FollowMoveToPosition
        , PhysicsObject.new 200 -107 12 (12 * 10) 21 FollowMoveToPosition
        , PhysicsObject.new -240 -107 7 (7 * 10) 22 FollowMoveToPosition
        , PhysicsObject.new -340 -107 23 (23 * 10) 23 Avoid
        , PhysicsObject.new 240 -207 18 (18 * 10) 24 Avoid
        ]
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
                | particles = List.map helper model.particles
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


viewParticle : Bool -> PhysicsObject ParticleState -> Svg msg
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
        [ Html.map ConsoleMsg (Html.Lazy.lazy Engine.Console.viewConsole model.console)
        , Svg.svg
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
                , Svg.g [] (model.particles |> List.filter (\o -> Vector2.distance Vector2.zero o.position < model.renderConfig.renderDistance) |> List.map (viewParticle model.renderDebug))
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

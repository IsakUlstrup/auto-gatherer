module Main exposing (Model, Msg, main)

import Browser
import Browser.Events
import Content.Worlds
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.Particle as Particle exposing (PhysicsType(..))
import Engine.Render as Render exposing (RenderConfig)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Engine.World as World exposing (World)
import GameState exposing (Particle(..))
import Html exposing (Html, main_)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy



-- SYSTEM


forces : World Particle -> World Particle
forces system =
    let
        forceHelper : Particle.Particle Particle -> Particle.Particle Particle
        forceHelper o =
            case o.state of
                MoveToCenter ->
                    Particle.moveToPosition 50 Vector2.zero o

                MoveToPosition p ->
                    Particle.moveToPosition 50 p o

                FollowMoveToPosition range ->
                    let
                        followTarget : Particle.Particle Particle -> Bool
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


movement : Float -> World Particle -> World Particle
movement dt system =
    World.updateParticles (Particle.move dt >> Particle.applyFriciton 0.05 >> Particle.stopIfSlow 0.0001) system


resolveCollisions : World Particle -> World Particle
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
    { particles : World Particle
    , renderConfig : RenderConfig
    , console : Console Msg
    , stepTime : Float
    , timeAccum : Float
    , renderDebug : Bool
    , deltaHistory : List Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Content.Worlds.testWorld1
        (Render.initRenderConfig |> Render.withRenderDistance 600)
        initConsole
        20
        0
        False
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
        { model | renderConfig = Render.withPosition playerPos model.renderConfig }

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
            { model | renderConfig = Render.withRenderDistance dist model.renderConfig }

        SetMoveTarget target ->
            let
                helper : Particle.Particle Particle -> Particle.Particle Particle
                helper p =
                    case p.state of
                        MoveToPosition _ ->
                            { p | state = MoveToPosition target }

                        _ ->
                            p
            in
            { model | particles = World.updatePlayer helper model.particles }



-- VIEW


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromInt (round position.x)
        ++ ", "
        ++ String.fromInt (round position.y)
        ++ ")"


cameraTransform : Vector2 -> Svg.Attribute msg
cameraTransform position =
    Svg.Attributes.style <|
        "transform: translate("
            ++ String.fromInt (round -position.x)
            ++ "px, "
            ++ String.fromInt (round -position.y)
            ++ "px)"


viewParticle : Bool -> Particle.Particle Particle -> Svg msg
viewParticle showVectors particle =
    let
        typeString : String
        typeString =
            case particle.state of
                MoveToCenter ->
                    "move-center"

                MoveToPosition _ ->
                    "move-to"

                FollowMoveToPosition _ ->
                    "follow-move-to"

                MoveToClosest ->
                    "move-closest"

                Idle ->
                    "idle"

                Avoid ->
                    "avoid"

                FollowId _ ->
                    "follow-id"

        physicsTypeString : String
        physicsTypeString =
            case particle.physicsType of
                Fixed ->
                    "fixed"

                Static _ ->
                    "static"

                Dynamic _ ->
                    "dynamic"
    in
    Svg.g
        [ Svg.Attributes.transform <| transformString particle.position
        , Svg.Attributes.class "particle"
        , Svg.Attributes.class typeString
        , Svg.Attributes.class physicsTypeString
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
                    ]

                else
                    []
               )
        )


viewTile2D : Float -> Vector2 -> Svg Msg
viewTile2D size position =
    let
        isOdd : Int -> Bool
        isOdd n =
            modBy 2 n == 1

        fillSting : String
        fillSting =
            if isOdd <| round (position.x / size) + round (position.y / size) then
                "hsl(" ++ (String.fromFloat <| Vector2.distance position Vector2.zero / 20) ++ " 80% 90%)"

            else
                "transparent"
    in
    Render.rect2d (round size)
        [ Svg.Events.onClick <| SetMoveTarget position
        , Svg.Attributes.class "tile"
        , Svg.Attributes.fill fillSting
        , Svg.Attributes.transform <| transformString position
        , Svg.Attributes.rx "3"
        ]


viewMap : RenderConfig -> Svg Msg
viewMap config =
    let
        row : Int -> List Vector2
        row y =
            List.range -30 30
                |> List.map (\x -> Vector2.new (toFloat x) (toFloat y))

        map : List Vector2
        map =
            List.range -30 30
                |> List.reverse
                |> List.concatMap row
                |> List.map (Vector2.scale 50)
                |> List.filter (\t -> Vector2.distance t config.position < config.renderDistance)
    in
    Svg.g [] (List.map (viewTile2D 50) map)


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
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g
                [ Svg.Attributes.class "camera"
                , cameraTransform <| (.position <| World.getPlayer model.particles)
                ]
                [ Svg.Lazy.lazy viewMap model.renderConfig
                , Svg.g []
                    (World.getParticles model.particles
                        |> List.filter (\o -> Particle.distance (World.getPlayer model.particles) o < model.renderConfig.renderDistance)
                        |> List.map (viewParticle model.renderDebug)
                    )
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

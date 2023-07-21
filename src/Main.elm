module Main exposing (Model, Msg, main)

import Blob
import Browser
import Browser.Events
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.PhysicsObject as PhysicsObject
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Lazy
import Resource
import Svg
import Svg.Attributes
import Svg.Lazy
import View
import World exposing (World)



-- CONSOLE


initConsole : Console Msg
initConsole =
    Engine.Console.new
        |> Engine.Console.addMessage "Add resource"
            (Engine.Console.constructor3
                AddResource
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
                (Engine.Console.argFloat "size")
            )
        |> Engine.Console.addMessage "Apply force to blobs"
            (Engine.Console.constructor2
                BlobForce
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
            )
        |> Engine.Console.addMessage "Apply force to resources"
            (Engine.Console.constructor2
                ResourceForce
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
            )
        |> Engine.Console.addMessage "Add blob"
            (Engine.Console.constructor2
                AddBlob
                (Engine.Console.argFloat "size")
                (Engine.Console.argInt "energy")
            )
        |> Engine.Console.addMessage "Reset state"
            (Engine.Console.constructor Reset)
        |> Engine.Console.addMessage "Set resource collision enabled"
            (Engine.Console.constructor1
                SetResourceCollisionState
                (Engine.Console.argBool "Enabled")
            )
        |> Engine.Console.addMessage "Set tile size"
            (Engine.Console.constructor1
                SetTileSize
                (Engine.Console.argInt "Size")
            )
        |> Engine.Console.addMessage "Set camera zoom"
            (Engine.Console.constructor1
                SetCameraZoom
                (Engine.Console.argFloat "Zoom")
            )
        |> Engine.Console.addMessage "Rest blobs"
            (Engine.Console.constructor RestBlobs)
        |> Engine.Console.addMessage "Move player"
            (Engine.Console.constructor2
                MovePlayer
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
            )



-- MODEL


type alias Model =
    { world : World
    , console : Console Msg
    , cameraZoom : Float
    , tileSize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        (World
            [ Blob.new 0 0 30 12
            , Blob.new 0 0 20 21
            , Blob.new 0 0 10 10
            ]
            [ Resource.new 150 0 20
            , Resource.new 178 -183 30
            , Resource.new 0 -200 35
            , Resource.new -200 200 25
            ]
            []
            (PhysicsObject.new 0 0 30 100 Vector2.zero)
            20
            0
        )
        initConsole
        0.7
        60
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | AddResource Float Float Float
    | AddBlob Float Int
    | SetResourceCollisionState Bool
    | BlobForce Float Float
    | ResourceForce Float Float
    | MovePlayer Float Float
    | RestBlobs
    | Reset
    | SetCameraZoom Float
    | ConsoleMsg (ConsoleMsg Msg)
    | GameClick Vector2
    | SetTileSize Int
    | PickupItem Int


worldUpdate : World -> World
worldUpdate world =
    world
        |> World.itemSpawn
        |> World.forces
        |> World.movement
        |> World.collisionInteraction
        |> World.collisionResolution
        |> World.stateUpdate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | world = World.fixedUpdate worldUpdate (model.world.physicsStepAccumulator + dt) model.world }
            , Cmd.none
            )

        AddResource x y size ->
            ( { model | world = World.addResource (Resource.new x y size) model.world }
            , Cmd.none
            )

        AddBlob size energy ->
            ( { model | world = World.addBlob (Blob.new 0 0 size energy) model.world }
            , Cmd.none
            )

        BlobForce x y ->
            ( { model | world = World.updateBlobs (PhysicsObject.applyForce <| Vector2.new x y) model.world }
            , Cmd.none
            )

        ResourceForce x y ->
            ( { model | world = World.updateResources (PhysicsObject.applyForce <| Vector2.new x y) model.world }
            , Cmd.none
            )

        MovePlayer x y ->
            ( { model | world = World.updatePlayer (PhysicsObject.updateState (always <| Vector2.new x y)) model.world }
            , Cmd.none
            )

        Reset ->
            init ()

        RestBlobs ->
            ( { model | world = World.updateBlobs Blob.resetEnergy model.world }
            , Cmd.none
            )

        SetResourceCollisionState flag ->
            ( { model | world = World.updateResources (PhysicsObject.setcollisionState flag) model.world }
            , Cmd.none
            )

        SetCameraZoom zoom ->
            ( { model | cameraZoom = zoom }
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

        GameClick coordinate ->
            ( { model | world = World.updatePlayer (PhysicsObject.updateState (always <| coordinate)) model.world }
            , Cmd.none
            )

        SetTileSize size ->
            ( { model | tileSize = size }
            , Cmd.none
            )

        PickupItem index ->
            ( { model | world = World.pickupItem index model.world }, Cmd.none )



-- VIEW


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
                , View.cameraTransform model.cameraZoom model.world.player.position
                ]
                [ Svg.Lazy.lazy (View.viewBackground GameClick) model.tileSize
                , Svg.g [ Svg.Attributes.class "blobs" ] (List.map View.viewBlob model.world.blobs)
                , Svg.g [] (List.indexedMap (View.viewItem PickupItem) model.world.items)
                , Svg.g [ Svg.Attributes.class "resources" ] (List.map (View.viewResource model.world.player.position) model.world.resources)
                , Svg.Lazy.lazy View.viewPlayer model.world.player
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 1000 >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

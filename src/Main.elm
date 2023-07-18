module Main exposing (Model, Msg, main)

import Blob exposing (Blob)
import Browser
import Browser.Events
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Html.Lazy
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy



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
    { blobs : List Blob
    , resources : List Resource
    , console : Console Msg
    , physicsStepTime : Float
    , physicsStepAccumulator : Float
    , cameraZoom : Float
    , player : PhysicsObject Vector2
    , tileSize : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Blob.new 0 0 30 12
        , Blob.new 0 0 20 21
        , Blob.new 0 0 10 10
        ]
        [ Resource.new 150 0 20
        , Resource.new 178 -183 30
        , Resource.new 0 -200 35
        , Resource.new -200 200 25
        ]
        initConsole
        20
        0
        0.7
        (PhysicsObject.new 0 0 30 100 Vector2.zero)
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


forces : Model -> Model
forces model =
    let
        movementForce : Float
        movementForce =
            0.03
    in
    { model
        | blobs =
            List.map
                (Blob.ai model.player.position (model.resources |> List.filter .enableCollisions) movementForce)
                model.blobs
        , resources =
            List.map
                (PhysicsObject.moveToPosition 5 .home (always movementForce))
                model.resources
        , player = PhysicsObject.moveToPosition 20 identity (always movementForce) model.player
    }


movement : Float -> Model -> Model
movement dt model =
    let
        f : PhysicsObject a -> PhysicsObject a
        f =
            PhysicsObject.applyFriciton 0.05
                >> PhysicsObject.move dt
                >> PhysicsObject.stopIfSlow 0.0001
    in
    { model
        | blobs = List.map f model.blobs
        , resources = List.map f model.resources
        , player = f model.player
    }


collisionInteraction : Model -> Model
collisionInteraction model =
    { model
        | blobs =
            List.map
                (PhysicsObject.collisionAction
                    (\target object ->
                        object
                            |> Blob.reduceEnergy
                            |> PhysicsObject.applyForce (Vector2.direction target.position object.position)
                    )
                    model.resources
                )
                model.blobs
        , resources =
            List.map
                (PhysicsObject.collisionAction
                    (\_ object ->
                        object
                            |> Resource.hit
                    )
                    model.blobs
                )
                model.resources
        , player =
            PhysicsObject.collisionAction
                (\target object ->
                    object
                        |> PhysicsObject.applyForce (Vector2.direction target.position object.position)
                )
                model.resources
                model.player
    }


collisionResolution : Model -> Model
collisionResolution model =
    { model
        | blobs = List.map (PhysicsObject.resolveCollisions model.resources) model.blobs
        , resources = List.map (PhysicsObject.resolveCollisions model.blobs) model.resources
        , player = PhysicsObject.resolveCollisions model.resources model.player
    }


stateUpdate : Float -> Model -> Model
stateUpdate dt model =
    { model
        | blobs = List.map (Blob.update dt) model.blobs
        , resources = List.map (Resource.update dt) model.resources
    }


fixedUpdate : (Model -> Model) -> Float -> Model -> ( Model, Float )
fixedUpdate f dt model =
    if dt >= model.physicsStepTime then
        { model | physicsStepAccumulator = dt - model.physicsStepTime }
            |> f
            |> fixedUpdate f (dt - model.physicsStepTime)

    else
        ( f model, dt )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                ( newModel, dtRemainder ) =
                    fixedUpdate
                        (forces
                            >> movement model.physicsStepTime
                            >> collisionInteraction
                            >> collisionResolution
                            >> stateUpdate model.physicsStepTime
                        )
                        (model.physicsStepAccumulator + dt)
                        model
            in
            ( { newModel | physicsStepAccumulator = dtRemainder }, Cmd.none )

        AddResource x y size ->
            ( { model | resources = Resource.new x y size :: model.resources }, Cmd.none )

        AddBlob size energy ->
            ( { model | blobs = Blob.new 0 0 size energy :: model.blobs }, Cmd.none )

        BlobForce x y ->
            ( { model | blobs = List.map (PhysicsObject.applyForce <| Vector2.new x y) model.blobs }, Cmd.none )

        ResourceForce x y ->
            ( { model | resources = List.map (PhysicsObject.applyForce <| Vector2.new x y) model.resources }, Cmd.none )

        MovePlayer x y ->
            ( { model | player = PhysicsObject.updateState (always <| Vector2.new x y) model.player }, Cmd.none )

        Reset ->
            init ()

        RestBlobs ->
            ( { model | blobs = List.map Blob.resetEnergy model.blobs }, Cmd.none )

        SetResourceCollisionState flag ->
            ( { model | resources = List.map (PhysicsObject.setcollisionState flag) model.resources }, Cmd.none )

        SetCameraZoom zoom ->
            ( { model | cameraZoom = zoom }, Cmd.none )

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
            ( { model | player = PhysicsObject.updateState (always <| coordinate) model.player }, Cmd.none )

        SetTileSize size ->
            ( { model | tileSize = size }, Cmd.none )



-- VIEW


svgClassList : List ( String, Bool ) -> Svg.Attribute msg
svgClassList classes =
    classes
        |> List.filter Tuple.second
        |> List.map Tuple.first
        |> List.intersperse " "
        |> String.concat
        |> Svg.Attributes.class


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromInt (round position.x)
        ++ ", "
        ++ String.fromInt (round position.y)
        ++ ")"


viewObject : List (Svg.Attribute msg) -> List (Svg msg) -> Vector2 -> Svg msg
viewObject attrs children position =
    Svg.g
        ([ Svg.Attributes.transform <| transformString position
         , Svg.Attributes.class "object"
         ]
            ++ attrs
        )
        children


viewResource : Vector2 -> Resource -> Svg msg
viewResource playerPosition resource =
    let
        circumference =
            resource.radius * pi * 2

        x =
            case Resource.getHealth resource of
                Just ( hp, maxHp ) ->
                    toFloat hp / toFloat maxHp

                Nothing ->
                    0
    in
    Svg.Lazy.lazy
        (viewObject
            [ svgClassList
                [ ( "entity", True )
                , ( "resource", True )
                , ( "hit", Resource.isHit resource )
                , ( "recharging", Resource.isRecharging resource )
                , ( "healthy", (Resource.isRecharging >> not) resource )
                , ( "player-close", Vector2.distance playerPosition resource.position < 200 )
                ]
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| resource.radius
                , Svg.Attributes.class "body"
                ]
                []
            , Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| resource.radius
                , Svg.Attributes.class "progress"
                , Svg.Attributes.strokeDasharray <| (String.fromFloat <| (circumference * x)) ++ " " ++ (String.fromFloat <| circumference - (circumference * x))
                , Svg.Attributes.fill "none"
                ]
                []
            ]
        )
        resource.position


viewBlob : Blob -> Svg msg
viewBlob blob =
    let
        circumference =
            blob.radius * pi * 2

        x =
            case Blob.getEnergy blob of
                Just ( energy, maxEnergy ) ->
                    toFloat energy / toFloat maxEnergy

                Nothing ->
                    0
    in
    Svg.Lazy.lazy
        (viewObject
            [ svgClassList
                [ ( "entity", True )
                , ( "blob", True )
                , ( "resting", Blob.isResting blob )
                , ( "rested", (Blob.isResting >> not) blob )
                ]
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| blob.radius
                , Svg.Attributes.class "body"
                ]
                []
            , Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| blob.radius
                , Svg.Attributes.class "progress"
                , Svg.Attributes.strokeDasharray <| (String.fromFloat <| (circumference * x)) ++ " " ++ (String.fromFloat <| circumference - (circumference * x))
                , Svg.Attributes.fill "none"
                ]
                []
            ]
        )
        blob.position


viewPlayer : PhysicsObject Vector2 -> Svg msg
viewPlayer player =
    Svg.Lazy.lazy
        (viewObject
            [ svgClassList
                [ ( "entity", True )
                , ( "player", True )
                ]
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| player.radius
                , Svg.Attributes.class "body"
                ]
                []
            ]
        )
        player.position


cameraTransform : Float -> Vector2 -> Svg.Attribute msg
cameraTransform zoom position =
    Svg.Attributes.style <|
        "transform: scale("
            ++ String.fromFloat zoom
            ++ ") translate("
            ++ String.fromInt -(round position.x)
            ++ "px, "
            ++ String.fromInt -(round position.y)
            ++ "px) rotate(3deg)"


viewBackground : Int -> Svg Msg
viewBackground tileSize =
    let
        isOdd : Int -> Bool
        isOdd n =
            modBy 2 n == 1

        gridSize : Int
        gridSize =
            6

        viewTile : Int -> Int -> Svg Msg
        viewTile x y =
            Svg.g
                [ Svg.Attributes.style <|
                    "transform: translate("
                        ++ (String.fromInt <| x * tileSize)
                        ++ "px, "
                        ++ (String.fromInt <| y * tileSize)
                        ++ "px)"
                ]
                [ Svg.rect
                    [ Svg.Attributes.width <| String.fromInt tileSize
                    , Svg.Attributes.height <| String.fromInt tileSize
                    , Svg.Events.onClick <| GameClick <| Vector2.new (toFloat <| x * tileSize + (tileSize // 2)) (toFloat <| y * tileSize + (tileSize // 2))
                    , svgClassList
                        [ ( "tile", True )
                        , ( "odd", isOdd (x + y) )
                        ]
                    ]
                    []
                ]

        viewRow : Int -> List Int -> List (Svg Msg)
        viewRow i r =
            List.map (viewTile (i - gridSize)) r
    in
    Svg.g [ Svg.Attributes.class "background-tiles" ]
        (List.range -gridSize gridSize
            |> List.map (\_ -> List.range -gridSize gridSize)
            |> List.indexedMap viewRow
            |> List.concat
        )


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
                , cameraTransform model.cameraZoom model.player.position
                ]
                [ Svg.Lazy.lazy viewBackground model.tileSize
                , Svg.g [ Svg.Attributes.class "blobs" ] (List.map viewBlob model.blobs)
                , Svg.g [ Svg.Attributes.class "resources" ] (List.map (viewResource model.player.position) model.resources)
                , Svg.Lazy.lazy viewPlayer model.player
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

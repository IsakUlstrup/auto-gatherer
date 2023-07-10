module Main exposing (Model, Msg, main)

import Blob exposing (Blob)
import Browser
import Browser.Events
import Engine.Console exposing (Console, ConsoleMsg)
import Engine.PhysicsObject as PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes



-- CONSOLE


initConsole : Console Msg
initConsole =
    Engine.Console.new
        |> Engine.Console.addMessage "Add resource"
            (Engine.Console.constructor2
                AddResource
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
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
            (Engine.Console.constructor3
                AddBlob
                (Engine.Console.argFloat "x")
                (Engine.Console.argFloat "y")
                (Engine.Console.argFloat "radius")
            )
        |> Engine.Console.addMessage "Reset state"
            (Engine.Console.constructor Reset)
        |> Engine.Console.addMessage "Disable resource collision"
            (Engine.Console.constructor DisableResourceCollision)



-- MODEL


type alias Model =
    { blobs : List Blob
    , resources : List Resource
    , console : Console Msg
    , physicsStepTime : Float
    , physicsStepAccumulator : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Blob.new 0 0 20 75
        , Blob.new -100 -50 20 75
        , Blob.new 100 50 20 75
        , Blob.new 120 70 20 75
        , Blob.new -150 -250 20 75
        , Blob.new -150 250 20 75
        ]
        [ Resource.new 200 0
        , Resource.new 200 -200
        , Resource.new 0 -200
        , Resource.new -200 200
        ]
        initConsole
        20
        0
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | AddResource Float Float
    | AddBlob Float Float Float
    | DisableResourceCollision
    | BlobForce Float Float
    | ResourceForce Float Float
    | Reset
    | ConsoleMsg (ConsoleMsg Msg)


forces : Model -> Model
forces model =
    { model
        | blobs = List.map (PhysicsObject.moveToNearest model.resources 0.02) model.blobs
        , resources = List.map (PhysicsObject.moveToPosition .home (always 0.02)) model.resources
    }


movement : Float -> Model -> Model
movement dt model =
    let
        f =
            PhysicsObject.applyFriciton 0.05
                >> PhysicsObject.move dt
                >> PhysicsObject.stopIfSlow 0.0001
    in
    { model
        | blobs = List.map f model.blobs
        , resources = List.map f model.resources
    }


collisionInteraction : Model -> Model
collisionInteraction model =
    { model
        | blobs =
            List.map
                (PhysicsObject.collisionAction
                    (\target object ->
                        object
                            |> Blob.incrementHits
                            |> PhysicsObject.applyForce (Vector2.direction target.position object.position)
                    )
                    model.resources
                )
                model.blobs
        , resources =
            List.map
                (PhysicsObject.collisionAction
                    (\_ o ->
                        o
                            |> Resource.incrementHits
                            |> Resource.hit
                    )
                    model.blobs
                )
                model.resources
    }


collisionResolution : Model -> Model
collisionResolution model =
    { model
        | blobs = List.map (PhysicsObject.resolveCollisions model.resources) model.blobs
        , resources = List.map (PhysicsObject.resolveCollisions model.blobs) model.resources
    }


stateUpdate : Float -> Model -> Model
stateUpdate dt model =
    { model
        | blobs = List.map Blob.addTrail model.blobs
        , resources = List.map (Resource.update dt) model.resources
    }


fixedUpdate : (Model -> Model) -> Float -> Model -> Model
fixedUpdate f dt model =
    if dt >= model.physicsStepTime then
        { model | physicsStepAccumulator = dt - model.physicsStepTime }
            |> f
            |> fixedUpdate f (dt - model.physicsStepTime)

    else
        f
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                deltaSum =
                    model.physicsStepAccumulator + dt
            in
            if deltaSum >= model.physicsStepTime then
                ( model
                    |> stateUpdate dt
                    |> fixedUpdate
                        (forces
                            >> movement model.physicsStepTime
                            >> collisionInteraction
                            >> collisionResolution
                        )
                        deltaSum
                , Cmd.none
                )

            else
                ( { model | physicsStepAccumulator = deltaSum }
                    |> stateUpdate dt
                , Cmd.none
                )

        AddResource x y ->
            ( { model | resources = Resource.new x y :: model.resources }, Cmd.none )

        AddBlob x y radius ->
            ( { model | blobs = Blob.new x y radius 50 :: model.blobs }, Cmd.none )

        BlobForce x y ->
            ( { model | blobs = List.map (PhysicsObject.applyForce <| Vector2.new x y) model.blobs }, Cmd.none )

        ResourceForce x y ->
            ( { model | resources = List.map (PhysicsObject.applyForce <| Vector2.new x y) model.resources }, Cmd.none )

        Reset ->
            init ()

        DisableResourceCollision ->
            ( { model | resources = List.map PhysicsObject.disableCollision model.resources }, Cmd.none )

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
        ++ String.fromFloat position.x
        ++ ", "
        ++ String.fromFloat position.y
        ++ ")"


viewObject : List (Svg.Attribute msg) -> List (Svg msg) -> PhysicsObject state -> Svg msg
viewObject attrs children object =
    Svg.g
        ([ Svg.Attributes.transform <| transformString object.position
         , Svg.Attributes.class "object"
         ]
            ++ attrs
        )
        children


viewResource : Resource -> Svg msg
viewResource resource =
    viewObject
        [ svgClassList
            [ ( "entity", True )
            , ( "resource", True )
            , ( "hit", Resource.isHit resource )
            ]
        ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r <| String.fromFloat <| resource.radius
            ]
            []

        -- , Svg.text_ [ Svg.Attributes.class "hit-count" ] [ Svg.text <| String.fromInt resource.state.hitCount ]
        ]
        resource


viewBlob : Blob -> Svg msg
viewBlob blob =
    let
        viewTrail : Float -> Int -> Vector2 -> Svg msg
        viewTrail r i p =
            Svg.circle
                [ Svg.Attributes.cx <| String.fromFloat p.x
                , Svg.Attributes.cy <| String.fromFloat p.y
                , Svg.Attributes.r <| String.fromFloat <| r - toFloat i
                , Svg.Attributes.fillOpacity <| String.fromInt <| 100 - (i * 3)
                ]
                []
    in
    Svg.g
        [ svgClassList
            [ ( "entity", True )
            , ( "blob", True )
            ]
        ]
        [ Svg.g [ Svg.Attributes.class "trail" ] (blob.state.trail |> List.indexedMap (viewTrail blob.radius))
        , Svg.g [ Svg.Attributes.transform <| transformString blob.position ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| blob.radius
                ]
                []

            -- , Svg.text_ [ Svg.Attributes.class "hit-count" ] [ Svg.text <| String.fromInt blob.state.hitCount ]
            ]
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Engine.Console.viewConsole model.console)
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g [] (List.map viewResource model.resources)
            , Svg.g [] (List.map viewBlob model.blobs)
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
        , update = update
        , subscriptions = subscriptions
        }

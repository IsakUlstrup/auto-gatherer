module Main exposing (Model, Msg, main)

import Animal exposing (Animal)
import Browser
import Browser.Events
import Console exposing (Console)
import Engine.Physics exposing (Physics)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import PhysicsInteraction
import Player exposing (Player)
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { animals : List Animal
    , resources : List Resource
    , console : Console Msg
    , player : Player
    }


initConsole : Console Msg
initConsole =
    Console.new
        |> Console.addMessage "Add resource"
            (Console.constructor3
                AddResource
                (Console.argFloat "x")
                (Console.argFloat "y")
                (Console.argFloat "radius")
            )
        |> Console.addMessage "Set player target"
            (Console.constructor2
                SetPlayerTarget
                (Console.argFloat "x")
                (Console.argFloat "y")
            )
        |> Console.addMessage "Add animal"
            (Console.constructor3
                AddAnimal
                (Console.argFloat "x")
                (Console.argFloat "y")
                (Console.argFloat "speed")
            )
        |> Console.addMessage "Remove resources"
            (Console.constructor RemoveResources)
        |> Console.addMessage "Rest animals"
            (Console.constructor RechargeAnimals)
        |> Console.addMessage "Reset state"
            (Console.constructor Reset)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Animal.new 0 0 0.01
        , Animal.new 20 -20 0.05
        , Animal.new -20 40 0.09
        , Animal.new 30 -20 0.02
        ]
        [ Resource.new -10 -180 40
        , Resource.new 165 -10 20
        , Resource.new 160 20 30
        , Resource.new -250 70 25
        , Resource.new -10 120 32
        ]
        initConsole
        Player.new
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | AddResource Float Float Float
    | AddAnimal Float Float Float
    | SetPlayerTarget Float Float
    | RechargeAnimals
    | RemoveResources
    | Reset
    | ConsoleMsg (Console.ConsoleMsg Msg)


forces : Model -> Model
forces model =
    let
        collideableResources : List Resource
        collideableResources =
            Resource.collideables model.resources
    in
    { model
        | animals = List.map (Animal.movementAi model.player.physics.position collideableResources) model.animals
        , resources = List.map Resource.movementAi model.resources
        , player = Player.moveAi model.player
    }


movement : Float -> Model -> Model
movement dt model =
    { model
        | animals = List.map (Animal.movement 0.93 dt) model.animals
        , resources = List.map (Resource.move dt) model.resources
        , player = Player.move dt model.player
    }


collisionInteraction : Model -> Model
collisionInteraction model =
    let
        collideableResources : List Resource
        collideableResources =
            Resource.collideables model.resources
    in
    { model
        | animals = List.map (PhysicsInteraction.isColliding (Animal.removeStamina 1) collideableResources) model.animals
        , resources = List.map (PhysicsInteraction.isColliding Resource.handleHit model.animals) model.resources
    }


collisionResolution : Model -> Model
collisionResolution model =
    let
        collideableResources : List Resource
        collideableResources =
            Resource.collideables model.resources

        collideadbleAnimals : List Animal
        collideadbleAnimals =
            model.animals |> List.filter (Animal.isExhausted >> not)
    in
    { model
        | animals = List.map (PhysicsInteraction.resolveCollision collideableResources) model.animals
        , resources = List.map (PhysicsInteraction.resolveCollision collideadbleAnimals) model.resources
    }


stateUpdate : Float -> Model -> Model
stateUpdate dt model =
    { model
        | animals = List.map (Animal.update dt) model.animals
        , resources = List.map (Resource.tickState dt) model.resources
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> forces
                |> movement dt
                |> collisionInteraction
                |> collisionResolution
                |> stateUpdate dt
            , Cmd.none
            )

        AddResource x y radius ->
            ( { model | resources = Resource.new x y radius :: model.resources }, Cmd.none )

        AddAnimal x y speed ->
            ( { model | animals = Animal.new x y speed :: model.animals }, Cmd.none )

        SetPlayerTarget x y ->
            ( { model | player = Player.setTarget (Vector2.new x y) model.player }, Cmd.none )

        RechargeAnimals ->
            ( { model | animals = List.map Animal.rest model.animals }, Cmd.none )

        RemoveResources ->
            ( { model | resources = [] }, Cmd.none )

        Reset ->
            init ()

        ConsoleMsg cmsg ->
            let
                ( newConsole, mmsg ) =
                    Console.update cmsg model.console
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


viewCircle : List (Svg.Attribute msg) -> Physics -> Svg msg
viewCircle attrs physics =
    Svg.circle
        ([ Svg.Attributes.transform <| transformString physics.position
         , Svg.Attributes.cx "0"
         , Svg.Attributes.cy "0"
         , Svg.Attributes.r <| String.fromFloat <| physics.radius
         ]
            ++ attrs
        )
        []


viewAnimal : Animal -> Svg msg
viewAnimal animal =
    let
        viewTrail i p =
            Svg.circle
                [ Svg.Attributes.cx <| String.fromFloat p.x
                , Svg.Attributes.cy <| String.fromFloat p.y
                , Svg.Attributes.r <| String.fromInt <| 20 - i
                , Svg.Attributes.fillOpacity <| String.fromInt <| 100 - (i * 3)
                ]
                []
    in
    Svg.g
        [ svgClassList
            [ ( "entity", True )
            , ( "animal", True )
            , ( "exhausted", Animal.isExhausted animal )
            ]
        ]
        [ Svg.g [ Svg.Attributes.class "trail" ] (animal.trail |> List.indexedMap viewTrail)
        , Svg.circle
            [ Svg.Attributes.transform <| transformString animal.physics.position
            , Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r <| String.fromFloat <| animal.physics.radius
            ]
            []
        ]


viewResource : Resource -> Svg msg
viewResource resource =
    viewCircle
        [ svgClassList
            [ ( "entity", True )
            , ( "resource", True )
            , ( "hit", Resource.isHit resource )
            , ( "exhausted", Resource.isExhausted resource )
            ]
        ]
        resource.physics


viewPlayer : Player -> Svg msg
viewPlayer player =
    Svg.circle
        [ Svg.Attributes.transform <| transformString player.physics.position
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.r <| String.fromFloat player.physics.radius
        , Svg.Attributes.class "home"
        ]
        []


view : Model -> Html Msg
view model =
    main_ []
        [ Html.map ConsoleMsg (Console.viewConsole model.console)
        , Svg.svg
            [ Svg.Attributes.class "game"
            , Svg.Attributes.viewBox "-500 -500 1000 1000"
            , Svg.Attributes.preserveAspectRatio "xMidYMid slice"
            ]
            [ Svg.g [] (List.map viewResource model.resources)
            , Svg.g []
                (model.animals
                    |> List.sortWith Animal.exhaustedSort
                    |> List.map viewAnimal
                )
            , viewPlayer model.player
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta (min 200 >> Tick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

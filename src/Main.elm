module Main exposing (Model, Msg, main)

import Animal exposing (Animal)
import Browser
import Browser.Events
import Console exposing (Console)
import Engine.Physics exposing (Physics)
import Engine.Vector2 exposing (Vector2)
import Html exposing (Html, main_)
import PhysicsInteraction
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes



-- MODEL


type alias Model =
    { animals : List Animal
    , resources : List Resource
    , console : Console Msg
    }


initConsole : Console Msg
initConsole =
    Console.new
        |> Console.addMessage "Add resource"
            (Console.constructor2
                AddResource
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Animal.newAnimal 0 0 0.01
        , Animal.newAnimal 20 -20 0.05
        , Animal.newAnimal -20 40 0.09
        , Animal.newAnimal 30 -20 0.02
        ]
        [ Resource.newResource -10 -80
        , Resource.newResource -100 100
        , Resource.newResource -150 50
        , Resource.newResource -10 100
        ]
        initConsole
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | AddResource Float Float
    | AddAnimal Float Float Float
    | RechargeAnimals
    | RemoveResources
    | ConsoleMsg (Console.ConsoleMsg Msg)


updateAnimals : (Animal -> Animal) -> Model -> Model
updateAnimals f model =
    { model | animals = List.map f model.animals }


updateResources : (Resource -> Resource) -> Model -> Model
updateResources f model =
    { model | resources = List.map f model.resources }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                collideableResources : List Resource
                collideableResources =
                    Resource.collideables model.resources
            in
            ( model
                |> updateAnimals (Animal.movementAi collideableResources)
                |> updateAnimals (PhysicsInteraction.isColliding (Animal.removeStamina 1) collideableResources)
                |> updateResources (PhysicsInteraction.isColliding Resource.handleHit model.animals)
                |> updateAnimals (PhysicsInteraction.resolveCollision collideableResources)
                |> updateResources (Resource.tickState dt)
                |> updateAnimals (Animal.tickState dt)
                |> updateAnimals (Animal.moveAnimal dt)
            , Cmd.none
            )

        AddResource x y ->
            ( { model | resources = Resource.newResource x y :: model.resources }, Cmd.none )

        AddAnimal x y speed ->
            ( { model | animals = Animal.newAnimal x y speed :: model.animals }, Cmd.none )

        RechargeAnimals ->
            ( { model | animals = List.map Animal.restAnimal model.animals }, Cmd.none )

        RemoveResources ->
            ( { model | resources = [] }, Cmd.none )

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

module Main exposing (Model, Msg, main)

import Animal exposing (Animal)
import Browser
import Browser.Events
import Console exposing (Console)
import Html exposing (Html, main_)
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes
import Vector2 exposing (Vector2)



-- RESOURCE
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
                -- cap dt to 5 fps, to prevent crazy updates of refocus
                cappedDt =
                    min 200 dt
            in
            ( model
                |> updateAnimals (Animal.moveToNearest (List.map .physics model.resources))
                |> updateAnimals (Animal.moveAnimal cappedDt)
                |> updateAnimals (Animal.isColliding (List.map .physics model.resources))
                |> updateResources (Resource.isColliding model.animals)
                |> updateAnimals (Animal.animalCollision (List.map .physics model.resources))
                |> updateResources (Resource.tickState cappedDt)
                |> updateAnimals (Animal.tickState cappedDt)
            , Cmd.none
            )

        AddResource x y ->
            ( { model | resources = Resource.newResource x y :: model.resources }, Cmd.none )

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


viewAnimal : Animal -> Svg msg
viewAnimal animal =
    Svg.circle
        [ svgClassList
            [ ( "entity", True )
            , ( "animal", True )
            , ( "exhausted", Animal.isExhausted animal )
            ]
        , Svg.Attributes.transform <| transformString animal.physics.position
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.r <| String.fromFloat <| animal.physics.radius
        ]
        []


viewResource : Resource -> Svg msg
viewResource resource =
    Svg.circle
        [ svgClassList [ ( "entity", True ), ( "resource", True ), ( "hit", resource.hitCooldown /= 0 ) ]
        , Svg.Attributes.transform <| transformString resource.physics.position
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.r <| String.fromFloat <| resource.physics.radius
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
            , Svg.g [] (List.map viewAnimal model.animals)
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

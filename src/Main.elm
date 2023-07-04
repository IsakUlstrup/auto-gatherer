module Main exposing (Model, Msg, main)

import Animal exposing (Animal)
import Browser
import Browser.Events
import Console exposing (Console)
import Html exposing (Html, main_)
import Svg exposing (Svg)
import Svg.Attributes
import Vector2 exposing (Vector2)



-- RESOURCE


type alias Resource =
    { position : Vector2
    , radius : Float
    }


newResource : Float -> Float -> Resource
newResource x y =
    Resource (Vector2.new x y) 25



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
        |> Console.addMessage "Rest animal"
            (Console.constructor RechargeAnimal)


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        [ Animal.newAnimal 0 0 50
        , Animal.newAnimal 20 -20 30
        , Animal.newAnimal -20 40 40
        , Animal.newAnimal 30 -20 60
        ]
        [ newResource -10 -80
        , newResource -100 100
        , newResource -150 50
        , newResource -10 100
        ]
        initConsole
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Float
    | AddResource Float Float
    | RechargeAnimal
    | RemoveResources
    | ConsoleMsg (Console.ConsoleMsg Msg)


animalUpdate : Float -> List Resource -> Animal -> Animal
animalUpdate dt resources animal =
    animal
        |> Animal.moveToNearest resources
        |> Animal.animalCollision resources
        |> Animal.moveAnimal dt


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | animals =
                    model.animals
                        |> List.map (animalUpdate dt model.resources)
              }
            , Cmd.none
            )

        AddResource x y ->
            ( { model | resources = newResource x y :: model.resources }, Cmd.none )

        RechargeAnimal ->
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


transformString : Vector2 -> String
transformString position =
    "translate("
        ++ String.fromFloat position.x
        ++ ", "
        ++ String.fromFloat position.y
        ++ ")"


viewAnimal : Animal -> Svg msg
viewAnimal animal =
    Svg.g [ Svg.Attributes.transform <| transformString animal.physics.position ]
        [ Svg.circle
            [ Svg.Attributes.cx "0"
            , Svg.Attributes.cy "0"
            , Svg.Attributes.r <| String.fromFloat <| animal.physics.radius
            , Svg.Attributes.fill "cyan"
            ]
            []
        , Svg.text_
            [ Svg.Attributes.textAnchor "middle"
            , Svg.Attributes.alignmentBaseline "central"
            ]
            [ Svg.text <| String.fromInt animal.stamina ]
        ]


viewResource : Resource -> Svg msg
viewResource resource =
    Svg.circle
        [ Svg.Attributes.transform <| transformString resource.position
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.r <| String.fromFloat <| resource.radius
        , Svg.Attributes.fill "magenta"
        , Svg.Attributes.stroke "magenta"
        , Svg.Attributes.strokeWidth "3"
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

module Main exposing (Model, Msg, main)

import Animal exposing (Animal)
import Browser
import Browser.Events
import Console exposing (Console)
import Html exposing (Html, main_)
import Physics
import Svg exposing (Svg)
import Svg.Attributes
import Vector2 exposing (Vector2)



-- RESOURCE


type alias Resource =
    { position : Vector2
    , radius : Float
    , hitCooldown : Float
    }


newResource : Float -> Float -> Resource
newResource x y =
    Resource (Vector2.new x y) 25 0


isColliding : List Animal -> Resource -> Resource
isColliding animals resource =
    let
        collision =
            animals
                |> List.map .physics
                |> List.filter (Physics.isCollidingVector resource)
                |> List.isEmpty
                |> not
    in
    if collision then
        { resource | hitCooldown = 200 }

    else
        resource


tickState : Float -> Resource -> Resource
tickState dt resource =
    { resource | hitCooldown = max 0 (resource.hitCooldown - dt) }



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


animalAi : Model -> Model
animalAi model =
    { model | animals = List.map (Animal.moveToNearest model.resources) model.animals }


animalHitDetection : Model -> Model
animalHitDetection model =
    { model | animals = List.map (Animal.isColliding model.resources) model.animals }


animalCollision : Model -> Model
animalCollision model =
    { model | animals = List.map (Animal.animalCollision model.resources) model.animals }


animalMovement : Float -> Model -> Model
animalMovement dt model =
    { model | animals = List.map (Animal.moveAnimal dt) model.animals }


resourceHitDetection : Model -> Model
resourceHitDetection model =
    { model | resources = List.map (isColliding model.animals) model.resources }


resourceUpdate : Float -> Model -> Model
resourceUpdate dt model =
    { model | resources = List.map (tickState dt) model.resources }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( model
                |> animalAi
                |> animalMovement dt
                |> animalHitDetection
                |> resourceHitDetection
                |> animalCollision
                |> resourceUpdate dt
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
        [ svgClassList [ ( "entity", True ), ( "animal", True ), ( "exhausted", animal.stamina == 0 ) ]
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
        , Svg.Attributes.transform <| transformString resource.position
        , Svg.Attributes.cx "0"
        , Svg.Attributes.cy "0"
        , Svg.Attributes.r <| String.fromFloat <| resource.radius
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

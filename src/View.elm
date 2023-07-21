module View exposing (..)

import Blob exposing (Blob)
import Engine.PhysicsObject exposing (PhysicsObject)
import Engine.Vector2 as Vector2 exposing (Vector2)
import Resource exposing (Resource)
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Svg.Lazy


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


viewBackground : (Vector2 -> msg) -> Int -> Svg msg
viewBackground clickMsg tileSize =
    let
        isOdd : Int -> Bool
        isOdd n =
            modBy 2 n == 1

        gridSize : Int
        gridSize =
            6

        viewTile : Int -> Int -> Svg msg
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
                    , Svg.Events.onClick <| clickMsg <| Vector2.new (toFloat <| x * tileSize + (tileSize // 2)) (toFloat <| y * tileSize + (tileSize // 2))
                    , svgClassList
                        [ ( "tile", True )
                        , ( "odd", isOdd (x + y) )
                        ]
                    ]
                    []
                ]

        viewRow : Int -> List Int -> List (Svg msg)
        viewRow i r =
            List.map (viewTile (i - gridSize)) r
    in
    Svg.g [ Svg.Attributes.class "background-tiles" ]
        (List.range -gridSize gridSize
            |> List.map (\_ -> List.range -gridSize gridSize)
            |> List.indexedMap viewRow
            |> List.concat
        )


viewItem : (Int -> msg) -> Int -> PhysicsObject Char -> Svg msg
viewItem pickupMsg index item =
    Svg.Lazy.lazy
        (viewObject
            [ svgClassList
                [ ( "entity", True )
                , ( "item", True )
                ]
            , Svg.Events.onClick <| pickupMsg index
            ]
            [ Svg.circle
                [ Svg.Attributes.cx "0"
                , Svg.Attributes.cy "0"
                , Svg.Attributes.r <| String.fromFloat <| item.radius
                , Svg.Attributes.class "body"
                ]
                []
            , Svg.text_ [ Svg.Attributes.class "label", Svg.Attributes.y "5" ] [ Svg.text <| String.fromChar item.state ]
            ]
        )
        item.position

port module Main exposing (..)

import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import ContextMenu exposing (ContextMenu)
import Configs
import FontAwesome
import Material.Icons.Image as Material


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { contextMenu : ContextMenu Context
    , config : ContextMenu.Config
    , message : String
    }


type Context
    = Object
    | Background


type Msg
    = ContextMenuMsg (ContextMenu.Msg Context)
    | Item Int


init : ( Model, Cmd Msg )
init =
    let
        ( contextMenu, msg ) =
            ContextMenu.init

        config =
            Configs.winDesktop
    in
        { contextMenu =
            contextMenu
                |> ContextMenu.setOnDeHover True
        , config = config
        , message = ""
        }
            ! [ Cmd.map ContextMenuMsg msg ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContextMenuMsg msg ->
            let
                ( contextMenu, cmd ) =
                    ContextMenu.update msg model.contextMenu
            in
                { model | contextMenu = contextMenu } ! [ Cmd.map ContextMenuMsg cmd ]

        Item num ->
            { model | message = "Item[" ++ toString num ++ "] was clicked." } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)


view : Model -> Html Msg
view model =
    div
        []
        [ div
            [ ContextMenu.open ContextMenuMsg Background
            , style backgroundStyles
            ]
            [ div
                [ ContextMenu.open ContextMenuMsg Object
                , style objectStyles
                ]
                []
            ]
        , div [] [ text model.message ]
        , ContextMenu.view
            model.config
            ContextMenuMsg
            toItemGroups
            model.contextMenu
        ]


backgroundStyles : List ( String, String )
backgroundStyles =
    [ ( "left", "10%" )
    , ( "right", "10%" )
    , ( "top", "10%" )
    , ( "bottom", "-10%" )
    , ( "position", "absolute" )
    , ( "background-color", "#cdb" )
    ]


objectStyles : List ( String, String )
objectStyles =
    [ ( "position", "absolute" )
    , ( "top", "100px" )
    , ( "left", "150px" )
    , ( "width", "100px" )
    , ( "height", "100px" )
    , ( "background-color", "#976" )
    ]


toItemGroups : Context -> List (List ( ContextMenu.Item, Msg ))
toItemGroups context =
    case context of
        Background ->
            [ [ ( ContextMenu.item "Hey", Item 1 )
              , ( ContextMenu.item "Yo!", Item 2 )
              ]
            , [ ( ContextMenu.item "Take photos"
                    |> ContextMenu.icon FontAwesome.camera Color.green
                    |> ContextMenu.disabled True
                , Item 3
                )
              , ( ContextMenu.item "Have a break"
                    |> ContextMenu.icon FontAwesome.coffee Color.brown
                    |> ContextMenu.disabled False
                , Item 4
                )
              , ( ContextMenu.item "Pneumonoultramicroscopicsilicovolcanoconiosis", Item 5 )
              , ( ContextMenu.item "Save"
                    |> ContextMenu.shortcut "Ctrl+S"
                , Item 6
                )
              , ( ContextMenu.itemWithAnnotation "Item with annotation" "Some annotation here"
                    |> ContextMenu.icon Material.tag_faces Color.red
                    |> ContextMenu.disabled False
                , Item 7
                )
              ]
            ]

        Object ->
            [ [ ( ContextMenu.item "Pen", Item 8 )
              , ( ContextMenu.item "Pineapple", Item 9 )
              , ( ContextMenu.item "Apple", Item 10 )
              ]
            ]

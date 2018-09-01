port module Main exposing (Context(..), Model, Msg(..), backgroundStyles, init, main, objectStyles, subscriptions, toItemGroups, update, view)

import Browser
import Configs
import ContextMenu exposing (ContextMenu)
import Html exposing (..)
import Html.Attributes exposing (..)


main : Program () Model Msg
main =
    Browser.element
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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( contextMenu, msg ) =
            ContextMenu.init
    in
    ( { contextMenu = contextMenu
      , config = Configs.winChrome
      , message = ""
      }
    , Cmd.map ContextMenuMsg msg
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ContextMenuMsg msg_ ->
            let
                ( contextMenu, cmd ) =
                    ContextMenu.update msg_ model.contextMenu
            in
            ( { model | contextMenu = contextMenu }
            , Cmd.map ContextMenuMsg cmd
            )

        Item num ->
            ( { model | message = "Item[" ++ String.fromInt num ++ "] was clicked." }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)


view : Model -> Html Msg
view model =
    div
        []
        [ div
            (ContextMenu.open ContextMenuMsg Background :: backgroundStyles)
            [ div
                (ContextMenu.open ContextMenuMsg Object :: objectStyles)
                []
            ]
        , div [] [ text model.message ]
        , ContextMenu.view
            model.config
            ContextMenuMsg
            toItemGroups
            model.contextMenu
        ]


backgroundStyles : List (Attribute msg)
backgroundStyles =
    [ style "left" "10%"
    , style "right" "10%"
    , style "top" "10%"
    , style "bottom" "-10%"
    , style "position" "absolute"
    , style "background-color" "#cdb"
    ]


objectStyles : List (Attribute msg)
objectStyles =
    [ style "position" "absolute"
    , style "top" "100px"
    , style "left" "150px"
    , style "width" "100px"
    , style "height" "100px"
    , style "background-color" "#976"
    ]


toItemGroups : Context -> List (List ( ContextMenu.Item, Msg ))
toItemGroups context =
    case context of
        Background ->
            [ [ ( ContextMenu.item "Hey", Item 1 )
              , ( ContextMenu.item "Yo!", Item 2 )
              ]
            , [ ( ContextMenu.item "Take photos"
                    -- |> ContextMenu.icon FontAwesome.camera Color.green
                    |> ContextMenu.disabled True
                , Item 3
                )
              , ( ContextMenu.item "Have a break"
                    -- |> ContextMenu.icon FontAwesome.coffee Color.brown
                    |> ContextMenu.disabled False
                , Item 4
                )
              , ( ContextMenu.item "Pneumonoultramicroscopicsilicovolcanoconiosis", Item 5 )
              , ( ContextMenu.item "Save"
                    |> ContextMenu.shortcut "Ctrl+S"
                , Item 6
                )
              , ( ContextMenu.itemWithAnnotation "Item with annotation" "Some annotation here"
                    -- |> ContextMenu.icon Material.tag_faces Color.red
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

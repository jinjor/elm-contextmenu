port module Main exposing (..)

import Color exposing (..)
import Svg
import Svg.Attributes exposing (width, height, viewBox, d, fill)
import Html exposing (..)
import Html.Attributes exposing (..)
import ContextMenu exposing (ContextMenu)


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
  , message : String
  }


type Context
  = Object
  | Background


type Msg
  = ContextMenuMsg (ContextMenu.Msg Context)
  | Item Int


init : (Model, Cmd Msg)
init =
  let
    (contextMenu, msg) = ContextMenu.init
  in
    { contextMenu = contextMenu
    , message = ""
    }
      ! [ Cmd.map ContextMenuMsg msg ]


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ContextMenuMsg msg ->
      let
        (contextMenu, cmd) =
          ContextMenu.update msg model.contextMenu
      in
        { model | contextMenu = contextMenu } ! [ Cmd.map ContextMenuMsg cmd ]

    Item num ->
      { model | message = "Item " ++ toString num ++ " was clicked."} ! []



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
        ContextMenu.defaultConfig
        ContextMenuMsg
        toItemGroups
        model.contextMenu
    ]


backgroundStyles : List (String, String)
backgroundStyles =
  [ ("width", "100%")
  , ("height", "400px")
  , ("background-color", "#ddd")
  ]


objectStyles : List (String, String)
objectStyles =
  [ ("position", "absolute")
  , ("top", "100px")
  , ("left", "150px")
  , ("width", "100px")
  , ("height", "100px")
  , ("background-color", "#457")
  ]


toItemGroups : Context -> List (List (ContextMenu.Item, Msg))
toItemGroups context =
  case context of
    Background ->
      [ [ (ContextMenu.item "Item1", Item 1)
        , (ContextMenu.item "Item2", Item 2)
        ]
      , [ ( ContextMenu.item "Item3"
              |> ContextMenu.icon camera Color.green
              |> ContextMenu.disabled True
          , Item 3
          )
        , ( ContextMenu.item "Item4"
              |> ContextMenu.icon coffee Color.brown
              |> ContextMenu.disabled False
          , Item 4
          )
        , ( ContextMenu.item "Item5 Item5 Item5 Item5 Item5 Item5 Item5 Item5 Item5 Item5", Item 5)
        , ( ContextMenu.item "Item6"
              |> ContextMenu.shortcut "Ctrl+A"
          , Item 6
          )
        , ( ContextMenu.itemWithAnnotation "Item7" "Some annotation"
              |> ContextMenu.icon coffee Color.red
              |> ContextMenu.disabled False
          , Item 7
          )
        ]
      ]

    Object ->
      [ [ (ContextMenu.item "Item8", Item 8)
        , (ContextMenu.item "Item9", Item 9)
        ]
      ]


------------------------------------------------------------------------

camera : Color -> Int -> Html msg
camera = icon "M896 672q119 0 203.5 84.5t84.5 203.5-84.5 203.5-203.5 84.5-203.5-84.5-84.5-203.5 84.5-203.5 203.5-84.5zm704-416q106 0 181 75t75 181v896q0 106-75 181t-181 75h-1408q-106 0-181-75t-75-181v-896q0-106 75-181t181-75h224l51-136q19-49 69.5-84.5t103.5-35.5h512q53 0 103.5 35.5t69.5 84.5l51 136h224zm-704 1152q185 0 316.5-131.5t131.5-316.5-131.5-316.5-316.5-131.5-316.5 131.5-131.5 316.5 131.5 316.5 316.5 131.5z"


coffee : Color -> Int -> Html msg
coffee = icon "M1600 640q0-80-56-136t-136-56h-64v384h64q80 0 136-56t56-136zm-1664 768h1792q0 106-75 181t-181 75h-1280q-106 0-181-75t-75-181zm1856-768q0 159-112.5 271.5t-271.5 112.5h-64v32q0 92-66 158t-158 66h-704q-92 0-158-66t-66-158v-736q0-26 19-45t45-19h1152q159 0 271.5 112.5t112.5 271.5z"

------------------------------------------------------------------------

type alias Size    = Int
type alias Path    = String
type alias ViewBox = (Int, Int, Int, Int)

------------------------------------------------------------------------

icon : Path -> Color -> Size -> Html msg
icon path color size =
  fromPath color size size (0, 0, 1792, 1792) path

fromPath : Color -> Int -> Int -> ViewBox -> Path -> Html msg
fromPath color w h (x0, y0, x1, y1) pathStr =

  let widthStr  = toString w
      heightStr = toString h
      colorStr  = fromColor color

      viewBoxStr = toString x0
         ++ " " ++ toString y0
         ++ " " ++ toString x1
         ++ " " ++ toString y1
  in
      Svg.svg
          [ Svg.Attributes.width   widthStr
          , Svg.Attributes.height  heightStr
          , viewBox viewBoxStr ]
          [ Svg.path [ fill colorStr, d pathStr ] [] ]

------------------------------------------------------------------------

fromColor : Color -> String
fromColor color =

  let c = toRgb color
      r = toString c.red
      g = toString c.green
      b = toString c.blue
      a = toString c.alpha
  in
      "rgba(" ++ r
       ++ "," ++ g
       ++ "," ++ b
       ++ "," ++ a
       ++ ")"

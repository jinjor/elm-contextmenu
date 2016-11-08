module ContextMenu exposing
  ( Direction(..), Overflow(..), Cursor(..), Config, defaultConfig
  , ContextMenu, Msg, init, update, subscriptions
  , Item, item, itemWithAnnotation, disabled, icon, shortcut
  , view, open
  )

import Task
import Process
import Color exposing (Color)
import Json.Decode as Decode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse
import Window

import Styles as S


-- MODEL


type alias ContextMenu context =
  { openState : Maybe (Int, Int, context)
  , x : Int
  , y : Int
  , windowWidth : Int
  , windowHeight : Int
  , hoverIndex : Maybe (Int, Int)
  }



-- UPDATE


type Msg context
  = NoOp
  | RequestOpen context
  | Open context
  | Close
  | Pos Mouse.Position
  | WindowSize Window.Size
  | HoverChanged (Maybe (Int, Int))


init : (ContextMenu context, Cmd (Msg context))
init =
  { openState = Nothing
  , x = 0
  , y = 0
  , windowWidth = 500 -- dummy
  , windowHeight = 500 -- dummy
  , hoverIndex = Nothing
  } ! [ Task.perform WindowSize Window.size ]


update : Msg context -> ContextMenu context -> (ContextMenu context, Cmd (Msg context))
update msg model =
  case msg of
    NoOp ->
      model ! []

    RequestOpen context ->
      model ! [ Task.perform (\_ -> Open context) (Process.sleep 0) ]

    Open context ->
      { model | openState = Just (model.x, model.y, context) } ! []

    Close ->
      { model | openState = Nothing, hoverIndex = Nothing } ! []

    Pos { x, y } ->
      { model | x = x, y = y } ! []

    WindowSize { width, height } ->
      { model | windowWidth = width, windowHeight = height } ! []

    HoverChanged index ->
      { model | hoverIndex = index } ! []


subscriptions : ContextMenu context -> Sub (Msg context)
subscriptions model =
  Sub.batch
    [ Mouse.moves Pos
    , Mouse.downs (\_ -> Close)
    , Window.resizes WindowSize
    ]


open : (Msg context -> msg) -> context -> Attribute msg
open transform context =
  onWithOptions
    "contextmenu"
    { preventDefault = True, stopPropagation = True }
    (Decode.succeed (transform (RequestOpen context)))



-- CONFIG


type alias Config =
  { width : Int
  , direction : Direction
  , overflowX : Overflow
  , overflowY : Overflow
  , containerColor : Color
  , hoverColor : Color
  , invertText : Bool
  , cursor : Cursor
  , rounded : Bool
  }


type Direction
  = LeftBottom
  | RightBottom


type Overflow
  = Shift
  | Mirror


type Cursor
  = Arrow
  | Pointer


defaultConfig : Config
defaultConfig =
  { width = 300
  , direction = RightBottom
  , overflowX = Mirror
  , overflowY = Mirror
  , containerColor = Color.white
  , hoverColor = Color.rgb 240 240 240
  , invertText = False
  , cursor = Pointer
  , rounded = False
  }


-- ITEM


type Item
  = Item
    { height : Int
    , icon : Maybe (Color -> Int -> Html Never, Color)
    , content : Content
    , shortcut : String
    , disabled : Bool
    }


type Content
  = Text String
  | Custom (Bool -> Html Never)


item : String -> Item
item s =
  Item
    { height = defaultItemHeight
    , icon = Nothing
    , content = Text s
    , shortcut = ""
    , disabled = False
    }


custom : Int -> (Bool -> Html Never) -> Item
custom height content =
  Item
    { height = Basics.max defaultItemHeight height
    , icon = Nothing
    , content = Custom content
    , shortcut = ""
    , disabled = False
    }


itemWithAnnotation : String -> String -> Item
itemWithAnnotation s ann =
  custom (defaultItemHeight + annotationHeight - 2) (annotationView s ann)


annotationHeight : Int
annotationHeight = 12


annotationFontSize : Int
annotationFontSize = 10


annotationView : String -> String -> Bool -> Html Never
annotationView s ann disabled =
  div []
    [ div
        [ style (S.text defaultItemHeight) ]
        [ text s ]
    , div
        [ style (S.annotation annotationTextColor annotationHeight annotationFontSize disabled) ]
        [ text ann ]
    ]


disabled : Bool -> Item -> Item
disabled disabled_ (Item item) =
  Item { item | disabled = disabled_ }


shortcut : String -> Item -> Item
shortcut shortcut (Item item) =
  Item { item | shortcut = shortcut }


icon : (Color -> Int -> Html Never) -> Color -> Item -> Item
icon icon_ color (Item item) =
  Item { item | icon = Just (icon_, color) }



-- VIEW


view : Config -> (Msg context -> msg) -> (context -> List (List (Item, msg))) -> ContextMenu context -> Html msg
view config transform toItemGroups model =
  case model.openState of
    Just (x, y, context) ->
      let
        groups =
          toItemGroups context

        itemGroups =
          List.map (List.map Tuple.first) groups

        groupsView =
          List.indexedMap
            (itemGroupView config transform model.hoverIndex)
            groups
      in
        case joinGroupsWithPartition groupsView of
          Just items ->
            let
              x_ =
                calculateX
                  config.direction
                  config.overflowX
                  model.windowWidth
                  (menuWidthWithBorders config.width)
                  x

              y_ =
                calculateY
                  config.overflowY
                  model.windowHeight
                  (calculateMenuHeight itemGroups)
                  y
            in
              div
                [ style
                    ( S.container
                        config.containerColor
                        containerBorderWidth
                        containerPadding
                        config.rounded
                        config.width
                        x_
                        y_
                        fontSize
                    )
                -- , onWithOptions
                --     "mousedown"
                --     { preventDefault = False
                --     , stopPropagation = True
                --     }
                --     (Decode.succeed (transform NoOp))
                ]
                items

          Nothing ->
            Html.text ""

    Nothing ->
      Html.text ""


joinGroupsWithPartition : List (List (Html msg)) -> Maybe (List (Html msg))
joinGroupsWithPartition groups =
  List.foldr
    (\group prev ->
      case prev of
        Just items ->
          Just (group ++ ( partition :: items ))

        Nothing ->
          Just group
    ) Nothing groups


partition : Html msg
partition =
  hr [ style (S.partition partitionWidth partitionMargin) ] []


itemGroupView : Config -> (Msg context -> msg) -> Maybe (Int, Int) -> Int -> List (Item, msg) -> List (Html msg)
itemGroupView config transform hoverIndex groupIndex items =
  List.indexedMap (itemView config transform hoverIndex groupIndex) items


itemView : Config -> (Msg context -> msg) -> Maybe (Int, Int) -> Int -> Int -> (Item, msg) -> Html msg
itemView config transform hoverIndex groupIndex index (Item item, msg) =
  let
    styles =
      style <|
        S.row
          config.hoverColor
          disabledTextColor
          config.invertText
          (config.cursor == Pointer)
          item.height
          (hoverIndex == Just (groupIndex, index))
          item.disabled

    events =
      if item.disabled then
        []
      else
        [ onMouseEnter (transform <| HoverChanged (Just (groupIndex, index)))
        , onMouseLeave (transform <| HoverChanged Nothing)
        , onMouseDown msg
        ]

    icon =
      case item.icon of
        Just (icon, color) ->
          Html.map never <|
            div
              [ style (S.icon fontSize) ]
              [ icon
                (if item.disabled then disabledTextColor else color)
                fontSize
              ]

        Nothing ->
          Html.text ""

    content =
      case item.content of
        Text s ->
          div [ style (S.text item.height) ] [ text s ]

        Custom toHtml ->
          toHtml item.disabled

    shortCut =
      div [ style (S.shortcut shortcutTextColor item.height) ] [ text item.shortcut ]
  in
    div
      ( styles :: events )
      [ icon, Html.map never content, shortCut ]


disabledTextColor : Color
disabledTextColor =
  Color.rgb 200 200 200


annotationTextColor : Color
annotationTextColor =
  Color.rgb 200 200 200


shortcutTextColor : Color
shortcutTextColor =
  Color.rgb 200 200 200


containerBorderWidth : Int
containerBorderWidth = 1


containerPadding : Int
containerPadding = 4


partitionWidth : Int
partitionWidth = 1


partitionMargin : Int
partitionMargin = 6


defaultItemHeight : Int
defaultItemHeight = 20


fontSize : Int
fontSize = 13


menuWidthWithBorders : Int -> Int
menuWidthWithBorders menuWidth =
  menuWidth + containerBorderWidth * 2


calculateMenuHeight : List (List Item) -> Int
calculateMenuHeight groups =
  let
    containerBorders = containerBorderWidth * 2
    containerPaddings = containerPadding * 2
    partitions = (List.length groups - 1) * (partitionMargin * 2 + partitionWidth)
    items = List.sum (List.map (\items -> List.sum (List.map (\(Item item) -> item.height) items)) groups)
  in
    containerBorders + containerPaddings + partitions + items


calculateX : Direction -> Overflow -> Int -> Int -> Int -> Int
calculateX direction overflow windowWidth menuWidth x =
  Basics.max 0 <|
    case direction of
      LeftBottom ->
        if x - menuWidth < 0 then
          if overflow == Shift then
            0
          else
            x
        else
          x - menuWidth

      RightBottom ->
        if x + menuWidth > windowWidth then
          if overflow == Shift then
            windowWidth - menuWidth
          else
            x - menuWidth
        else
          x


calculateY : Overflow -> Int -> Int -> Int -> Int
calculateY overflow windowHeight menuHeight y =
  Basics.max 0 <|
    if y + menuHeight > windowHeight then
      if overflow == Shift then
        windowHeight - menuHeight
      else
        y - menuHeight
    else
      y

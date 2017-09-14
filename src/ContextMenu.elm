module ContextMenu exposing
  ( ContextMenu, Msg, init, update, subscriptions
  , Item, item, itemWithAnnotation, disabled, icon, shortcut
  , Direction(..), Overflow(..), Cursor(..), Config, defaultConfig
  , view, open, openIf, setOnDeHover
  )

{-| The ContextMenu component that follows the Elm Architecture.

See [How to use](http://package.elm-lang.org/packages/jinjor/elm-contextmenu/latest).

# TEA Parts

The boilerplace functions. See [The Elm Architecture](https://guide.elm-lang.org/architecture/) for more information.

@docs ContextMenu, Msg, init, update, subscriptions

# Item
@docs Item, item, itemWithAnnotation, disabled, icon, shortcut

# Config
@docs Config, Direction, Overflow, Cursor, defaultConfig

# View
@docs view, open, openIf

-}


import Task
import Process
import Color exposing (Color)
import Json.Decode as Decode

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Mouse exposing (Position)
import Window exposing (Size)

import Styles as S


-- MODEL

{-| The Model. Put whatever context you like, which is used to create menu items.
-}
type ContextMenu context =
  ContextMenu { openState : OpenState context, closeOnDeHover : Bool }


type HoverState
  = Container
  | ItemIndex (Int, Int)
  | None


getItemIndex : HoverState -> Maybe (Int, Int)
getItemIndex hover =
  case hover of
    ItemIndex index -> Just index
    _ -> Nothing


type alias OpenState context =
  Maybe (Position, Size, HoverState, context)


shouldCloseOnClick : OpenState context -> Bool -> Bool
shouldCloseOnClick openState deHover =
  case openState of
    Just (_, _, hover, _) ->
      if deHover then False else hover /= Container

    Nothing ->
      True


setHoverState : HoverState -> OpenState context -> OpenState context
setHoverState hover openState =
  openState
    |> Maybe.map
      (\(mouse, window, _, context) -> (mouse, window, hover, context))

setOnDeHover : Bool -> ContextMenu context -> ContextMenu context
setOnDeHover deHover (ContextMenu model) =
  (ContextMenu {model | closeOnDeHover = deHover})


enterItem : (Int, Int) -> OpenState context -> OpenState context
enterItem index openState =
  setHoverState (ItemIndex index) openState


leaveItem : OpenState context -> OpenState context
leaveItem openState =
  setHoverState Container openState


enterContainer : OpenState context -> OpenState context
enterContainer openState =
  setHoverState Container openState


leaveContainer : OpenState context -> OpenState context
leaveContainer openState =
  setHoverState None openState


-- UPDATE


{-| The Message.
-}
type Msg context
  = NoOp
  | RequestOpen context Position
  | Open context Position Size
  | Close
  | EnterItem (Int, Int)
  | LeaveItem
  | EnterContainer
  | LeaveContainer


{-| The init function.
-}
init : (ContextMenu context, Cmd (Msg context))
init =
  ( ContextMenu { openState = Nothing, closeOnDeHover = False }, Cmd.none )


{-| The update function.
-}
update : Msg context -> ContextMenu context -> (ContextMenu context, Cmd (Msg context))
update msg (ContextMenu model) =
  case msg of
    NoOp ->
      ( ContextMenu model, Cmd.none )

    RequestOpen context mouse ->
      ( ContextMenu model
      , Task.perform (Open context mouse) Window.size
      )

    Open context mouse window ->
      ( ContextMenu { model | openState = Just (mouse, window, None, context) }
      , Cmd.none
      )

    Close ->
      ( ContextMenu { model | openState = Nothing }, Cmd.none )

    EnterItem index ->
      ( ContextMenu { model | openState = enterItem index model.openState }
      , Cmd.none
      )

    LeaveItem ->
      ( ContextMenu { model | openState = leaveItem model.openState }
      , Cmd.none
      )

    EnterContainer ->
      ( ContextMenu { model | openState = enterContainer model.openState }
      , Cmd.none
      )

    LeaveContainer ->
      if model.closeOnDeHover then update Close (ContextMenu { model | openState = leaveContainer model.openState }) else ( ContextMenu { model | openState = leaveContainer model.openState }, Cmd.none)


{-| The Subscription.
-}
subscriptions : ContextMenu context -> Sub (Msg context)
subscriptions (ContextMenu model) =
  Sub.batch
    [ if shouldCloseOnClick model.openState model.closeOnDeHover then
        Mouse.downs (\_ -> Close)
      else
        Sub.none
    ]


-- NUMBERS AND CALCULATION


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


annotationHeight : Int
annotationHeight = 12


annotationFontSize : Int
annotationFontSize = 10


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


-- ITEM


{-| The menu item. You can construct it with pipe-friendly functions.

```
ContextMenu.item "Take photos"
  |> ContextMenu.icon FontAwesome.camera Color.green
  |> ContextMenu.disabled True
```
-}
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


{-| Creates a simple text item.
-}
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


{-| Creates an item with annotation which will displayed just below the item name.
-}
itemWithAnnotation : String -> String -> Item
itemWithAnnotation s ann =
  custom (defaultItemHeight + annotationHeight - 2) (annotationView s ann)


{-| Disables the item. True = disabled, False = enabled.
-}
disabled : Bool -> Item -> Item
disabled disabled_ (Item item) =
  Item { item | disabled = disabled_ }


{-| Displays the shortcut key at the right.
-}
shortcut : String -> Item -> Item
shortcut shortcut (Item item) =
  Item { item | shortcut = shortcut }


{-| Shows the icon.

The first function creates the icon.
This fuction is compatible with
 [elm-material-icons](http://package.elm-lang.org/packages/elm-community/elm-material-icons/latest) and
 [elm-font-awesome](http://package.elm-lang.org/packages/jystic/elm-font-awesome/latest).
-}
icon : (Color -> Int -> Html Never) -> Color -> Item -> Item
icon icon_ color (Item item) =
  Item { item | icon = Just (icon_, color) }


-- CONFIG


{-| Defines the styles of the menu. See [examples](https://github.com/jinjor/elm-contextmenu/blob/master/examples/Configs.elm).

-}
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
  , fontFamily : String
  }


{-| The default direction the menu will be shown at.
-}
type Direction
  = LeftBottom
  | RightBottom


{-| The strategies how to show the menu when it goes out of window.
-}
type Overflow
  = Shift
  | Mirror


{-| The shape of cursor during hovering on the menu.
-}
type Cursor
  = Arrow
  | Pointer


{-| The default config.

```
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
  , fontFamily = "initial"
  }
```

-}
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
  , fontFamily = "initial"
  }


-- VIEW


{-| Makes the attribute that triggers to open the menu.
This attribute is passed for each element that needs a menu.

Arguments:

1. function to transform component's message into user's message
2. the context which is used to create items

-}
open : (Msg context -> msg) -> context -> Attribute msg
open transform context =
  openIf True transform context


{-| Similar to `open` but only works under particular condition.

This is useful for debugging on browser.
-}
openIf : Bool -> (Msg context -> msg) -> context -> Attribute msg
openIf condition transform context =
  if condition then
    onWithOptions
      "contextmenu"
      { preventDefault = True, stopPropagation = True }
      ( position
          |> Decode.map (RequestOpen context)
          |> Decode.map transform
      )
  else
    on "contextmenu" (Decode.succeed (transform NoOp))


position : Decode.Decoder Position
position =
  Decode.map2 Position
    (Decode.field "clientX" Decode.int)
    (Decode.field "clientY" Decode.int)



{-| Shows the menu. This should be called at only one place.

Arguments:

1. the Config
2. function to transform component's message into user's message
3. function to create item groups
4. the Model

-}
view : Config -> (Msg context -> msg) -> (context -> List (List (Item, msg))) -> ContextMenu context -> Html msg
view config transform toItemGroups (ContextMenu model) =
  case model.openState of
    Just (mouse, window, hover, context) ->
      let
        groups =
          toItemGroups context

        itemGroups =
          List.map (List.map Tuple.first) groups

        groupsView =
          List.indexedMap
            (itemGroupView config transform (getItemIndex hover))
            groups
      in
        case joinGroupsWithPartition groupsView of
          Just items ->
            let
              x_ =
                calculateX
                  config.direction
                  config.overflowX
                  window.width
                  (menuWidthWithBorders config.width)
                  mouse.x

              y_ =
                calculateY
                  config.overflowY
                  window.height
                  (calculateMenuHeight itemGroups)
                  mouse.y
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
                        config.fontFamily
                        fontSize
                    )
                , onMouseEnter (transform EnterContainer)
                , onMouseLeave (transform LeaveContainer)
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
    hovered =
      hoverIndex == Just (groupIndex, index)

    styles =
      style <|
        S.row
          config.hoverColor
          disabledTextColor
          config.invertText
          (config.cursor == Pointer)
          item.height
          hovered
          item.disabled
          (String.trim item.shortcut /= "")

    events =
      if item.disabled then
        []
      else
        [ onMouseEnter (transform <| EnterItem (groupIndex, index))
        , onMouseLeave (transform <| LeaveItem)
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
      div [ style (S.shortcut shortcutTextColor item.height hovered) ] [ text item.shortcut ]
  in
    div
      ( styles :: events )
      [ icon, Html.map never content, shortCut ]


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

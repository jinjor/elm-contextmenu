module ContextMenu exposing
    ( ContextMenu, Msg, init, update, subscriptions
    , Item, item, itemWithAnnotation, disabled, icon, shortcut
    , Config, Direction(..), Overflow(..), Cursor(..), defaultConfig
    , view, open, openIf
    , setOnDehover
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


# Advanced

@docs setOnDehover

-}

import Browser
import Browser.Dom
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Process
import Styles as S
import Task exposing (Task)



-- TYPES


type alias Color =
    String


type alias Size =
    { width : Float
    , height : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }



-- MODEL


{-| The Model. Put whatever context you like, which is used to create menu items.
-}
type ContextMenu context
    = ContextMenu { openState : OpenState context, closeOnDehover : Bool }


type HoverState
    = Container
    | ItemIndex ( Int, Int )
    | None


getItemIndex : HoverState -> Maybe ( Int, Int )
getItemIndex hover =
    case hover of
        ItemIndex index ->
            Just index

        _ ->
            Nothing


type alias OpenState context =
    Maybe
        { mouse : Position
        , window : Size
        , hover : HoverState
        , context : context
        }


shouldCloseOnClick : Bool -> OpenState context -> Bool
shouldCloseOnClick closeOnDehover openState =
    case openState of
        Just { hover } ->
            if closeOnDehover then
                False

            else
                hover /= Container

        Nothing ->
            True


setHoverState : HoverState -> OpenState context -> OpenState context
setHoverState hover openState =
    openState
        |> Maybe.map
            (\{ mouse, window, context } ->
                { mouse = mouse
                , window = window
                , hover = hover
                , context = context
                }
            )


{-| This switches when the menu should be closed.

  - True: Closes when mouse leaves the menu (keeps opening on cliking)
  - False(default): Closes when somewhere in the window is clicked

-}
setOnDehover : Bool -> ContextMenu context -> ContextMenu context
setOnDehover closeOnDehover (ContextMenu model) =
    ContextMenu { model | closeOnDehover = closeOnDehover }


enterItem : ( Int, Int ) -> OpenState context -> OpenState context
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
    | EnterItem ( Int, Int )
    | LeaveItem
    | EnterContainer
    | LeaveContainer


{-| The init function.
-}
init : ( ContextMenu context, Cmd (Msg context) )
init =
    ( ContextMenu { openState = Nothing, closeOnDehover = False }, Cmd.none )


{-| The update function.
-}
update : Msg context -> ContextMenu context -> ( ContextMenu context, Cmd (Msg context) )
update msg (ContextMenu model) =
    case msg of
        NoOp ->
            ( ContextMenu model, Cmd.none )

        RequestOpen context mouse ->
            ( ContextMenu model
            , Task.perform (Open context mouse) windowSize
            )

        Open context mouse window ->
            ( ContextMenu
                { model
                    | openState =
                        Just
                            { mouse = mouse
                            , window = window
                            , hover = None
                            , context = context
                            }
                }
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
            if model.closeOnDehover then
                update Close (ContextMenu { model | openState = leaveContainer model.openState })

            else
                ( ContextMenu { model | openState = leaveContainer model.openState }, Cmd.none )


windowSize : Task x Size
windowSize =
    Browser.Dom.getViewport
        |> Task.map
            (\v ->
                Size v.viewport.width v.viewport.height
            )


{-| The Subscription.
-}
subscriptions : ContextMenu context -> Sub (Msg context)
subscriptions (ContextMenu model) =
    Sub.batch
        [ if shouldCloseOnClick model.closeOnDehover model.openState then
            Browser.Events.onMouseDown (Decode.succeed Close)

          else
            Sub.none
        ]



-- NUMBERS AND CALCULATION


disabledTextColor : Color
disabledTextColor =
    "rgb(200, 200, 200)"


annotationTextColor : Color
annotationTextColor =
    "rgb(200, 200, 200)"


shortcutTextColor : Color
shortcutTextColor =
    "rgb(200, 200, 200)"


containerBorderWidth : Float
containerBorderWidth =
    1


containerPadding : Float
containerPadding =
    4


partitionWidth : Float
partitionWidth =
    1


partitionMargin : Float
partitionMargin =
    6


defaultItemHeight : Float
defaultItemHeight =
    20


fontSize : Float
fontSize =
    13


annotationHeight : Float
annotationHeight =
    12


annotationFontSize : Float
annotationFontSize =
    10


menuWidthWithBorders : Float -> Float
menuWidthWithBorders menuWidth =
    menuWidth + containerBorderWidth * 2


calculateMenuHeight : List (List Item) -> Float
calculateMenuHeight groups =
    let
        containerBorders =
            containerBorderWidth * 2

        containerPaddings =
            containerPadding * 2

        partitions =
            toFloat (List.length groups - 1) * (partitionMargin * 2 + partitionWidth)

        items =
            List.sum
                (List.map
                    (\items_ ->
                        List.sum (List.map (\(Item item_) -> item_.height) items_)
                    )
                    groups
                )
    in
    containerBorders + containerPaddings + partitions + toFloat items


calculateX : Direction -> Overflow -> Float -> Float -> Float -> Float
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


calculateY : Overflow -> Float -> Float -> Float -> Float
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

    ContextMenu.item "Take photos"
        -- This library is outdated. See README.
        -- |> ContextMenu.icon FontAwesome.camera Color.green
        |> ContextMenu.disabled True

-}
type Item
    = Item
        { height : Int
        , icon : Maybe ( Color -> Int -> Html Never, Color )
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
        { height = floor defaultItemHeight
        , icon = Nothing
        , content = Text s
        , shortcut = ""
        , disabled = False
        }


custom : Int -> (Bool -> Html Never) -> Item
custom height content =
    Item
        { height = Basics.max (floor defaultItemHeight) height
        , icon = Nothing
        , content = Custom content
        , shortcut = ""
        , disabled = False
        }


{-| Creates an item with annotation which will displayed just below the item name.
-}
itemWithAnnotation : String -> String -> Item
itemWithAnnotation s ann =
    custom (floor <| defaultItemHeight + annotationHeight - 2) (annotationView s ann)


{-| Disables the item. True = disabled, False = enabled.
-}
disabled : Bool -> Item -> Item
disabled disabled_ (Item item_) =
    Item { item_ | disabled = disabled_ }


{-| Displays the shortcut key at the right.
-}
shortcut : String -> Item -> Item
shortcut shortcutName (Item item_) =
    Item { item_ | shortcut = shortcutName }


{-| Shows the icon.

The first function creates the icon.
The Color is just a String like `#fff`, `rgb(100,200,200)`, etc.

-}
icon : (Color -> Int -> Html Never) -> Color -> Item -> Item
icon icon_ color (Item item_) =
    Item { item_ | icon = Just ( icon_, color ) }



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

    defaultConfig =
        { width = 300
        , direction = RightBottom
        , overflowX = Mirror
        , overflowY = Mirror
        , containerColor = "white"
        , hoverColor = "rgb(240 240 240)"
        , invertText = False
        , cursor = Pointer
        , rounded = False
        , fontFamily = "initial"
        }

-}
defaultConfig : Config
defaultConfig =
    { width = 300
    , direction = RightBottom
    , overflowX = Mirror
    , overflowY = Mirror
    , containerColor = "white"
    , hoverColor = "rgb(240 240 240)"
    , invertText = False
    , cursor = Pointer
    , rounded = False
    , fontFamily = "initial"
    }



-- VIEW


{-| Makes the attribute that triggers to open the menu.
This attribute is passed for each element that needs a menu.

Arguments:

1.  function to transform component's message into user's message
2.  the context which is used to create items

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
        Html.Events.custom
            "contextmenu"
            (position
                |> Decode.map (RequestOpen context)
                |> Decode.map transform
                |> Decode.map
                    (\msg ->
                        { message = msg
                        , stopPropagation = True
                        , preventDefault = True
                        }
                    )
            )

    else
        on "contextmenu" (Decode.succeed (transform NoOp))


position : Decode.Decoder Position
position =
    Decode.map2 Position
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


{-| Shows the menu. This should be called at only one place.

Arguments:

1.  the Config
2.  function to transform component's message into user's message
3.  function to create item groups
4.  the Model

-}
view : Config -> (Msg context -> msg) -> (context -> List (List ( Item, msg ))) -> ContextMenu context -> Html msg
view config transform toItemGroups (ContextMenu model) =
    case model.openState of
        Just { mouse, window, hover, context } ->
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
                                (menuWidthWithBorders (toFloat config.width))
                                mouse.x

                        y_ =
                            calculateY
                                config.overflowY
                                window.height
                                (calculateMenuHeight itemGroups)
                                mouse.y
                    in
                    div
                        (S.container
                            config.containerColor
                            containerBorderWidth
                            containerPadding
                            config.rounded
                            (toFloat config.width)
                            x_
                            y_
                            config.fontFamily
                            fontSize
                            ++ [ onMouseEnter (transform EnterContainer)
                               , onMouseLeave (transform LeaveContainer)
                               ]
                        )
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
                    Just (group ++ (partition :: items))

                Nothing ->
                    Just group
        )
        Nothing
        groups


partition : Html msg
partition =
    hr (S.partition partitionWidth partitionMargin) []


itemGroupView : Config -> (Msg context -> msg) -> Maybe ( Int, Int ) -> Int -> List ( Item, msg ) -> List (Html msg)
itemGroupView config transform hoverIndex groupIndex items =
    List.indexedMap (itemView config transform hoverIndex groupIndex) items


itemView : Config -> (Msg context -> msg) -> Maybe ( Int, Int ) -> Int -> Int -> ( Item, msg ) -> Html msg
itemView config transform hoverIndex groupIndex index ( Item item_, msg ) =
    let
        hovered =
            hoverIndex == Just ( groupIndex, index )

        styles =
            S.row
                config.hoverColor
                disabledTextColor
                config.invertText
                (config.cursor == Pointer)
                (toFloat item_.height)
                hovered
                item_.disabled
                (String.trim item_.shortcut /= "")

        events =
            if item_.disabled then
                []

            else
                [ onMouseEnter (transform <| EnterItem ( groupIndex, index ))
                , onMouseLeave (transform <| LeaveItem)
                , onMouseDown msg
                ]

        icon_ =
            case item_.icon of
                Just ( icon__, color ) ->
                    Html.map never <|
                        div
                            (S.icon fontSize)
                            [ icon__
                                (if item_.disabled then
                                    disabledTextColor

                                 else
                                    color
                                )
                                (floor fontSize)
                            ]

                Nothing ->
                    Html.text ""

        content =
            case item_.content of
                Text s ->
                    div (S.text (toFloat item_.height)) [ text s ]

                Custom toHtml ->
                    toHtml item_.disabled

        shortCut =
            div
                (S.shortcut shortcutTextColor (toFloat item_.height) hovered)
                [ text item_.shortcut ]
    in
    div
        (styles ++ events)
        [ icon_
        , Html.map never content
        , shortCut
        ]


annotationView : String -> String -> Bool -> Html Never
annotationView s ann disabled_ =
    div []
        [ div
            (S.text defaultItemHeight)
            [ text s ]
        , div
            (S.annotation
                annotationTextColor
                annotationHeight
                annotationFontSize
                disabled_
            )
            [ text ann ]
        ]

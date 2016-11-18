elm-context-menu
----

Flexible context menu for Elm ([Demo](https://jinjor.github.io/elm-contextmenu/))


## How to use

This component works with [The Elm Architecture](https://guide.elm-lang.org/architecture/).

1. Model.
```elm
type alias Model =
  { contextMenu : ContextMenu Context
  , config : ContextMenu.Config
  , message : String
  }
```

2. Msg
```elm
type Msg
  = ContextMenuMsg (ContextMenu.Msg Context)
  | Item Int
```

3. Initialize
```elm
init : (Model, Cmd Msg)
init =
  let
    (contextMenu, msg) = ContextMenu.init
  in
    { contextMenu = contextMenu
    }
      ! [ Cmd.map ContextMenuMsg msg ]
```

4. Update
```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ContextMenuMsg msg ->
      let
        (contextMenu, cmd) =
          ContextMenu.update msg model.contextMenu
      in
        { model | contextMenu = contextMenu } ! [ Cmd.map ContextMenuMsg cmd ]
```

5. Subscribe
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)
```

6. View
```elm
view : Model -> Html Msg
view model =
  div
    [ ContextMenu.open ContextMenuMsg "context1" ]
    [ ContextMenu.view
        ContextMenu.defaultConfig
        ContextMenuMsg
        toItemGroups
        toItemGroups model.contextMenu
    ]

toItemGroups : String -> List (List Item)
toItemGroups context =
  if context == "context1" then
    [ [ (ContextMenu.item "Hey", Item 1)
      , (ContextMenu.item "Yo!", Item 2)
      ]
    ]
  else
    []
```


## License

BSD3

elm-contextmenu
===

Flexible context menu for Elm ([Demo](https://jinjor.github.io/elm-contextmenu/))

## Warning

On the migration from Elm 0.18 to 0.19, the legacy `Color` type has changed to just a type alias of `String` like `#aaa`, `rgb(100,100,200)`. Also, some icon libraries that uses `Color` type (i.e. `FontAwesome`, `MaterialIcons`) cannot be used anymore. So now you need to make a function typed as `String -> Int -> Html msg`. It *should* work but I haven't tested yet.

I also think the implementation can be improved using new Browser API, but I cannot spend my time to try it. The styling method can be improved too. I would really appreciate if someone do that. Don't hesitate to fork this package or make your own from scratch! ([This article](http://jinjor-labo.hatenablog.com/entry/2016/11/05/201107) may help.)


## How to use

This component works with [The Elm Architecture](https://guide.elm-lang.org/architecture/).

<span>1. Model</span>
```elm
type alias Model =
  { contextMenu : ContextMenu Context
  , config : ContextMenu.Config
  , message : String
  }
```

<span>2. Msg</span>
```elm
type Msg
  = ContextMenuMsg (ContextMenu.Msg Context)
  | Item Int
```

<span>3. Initialize</span>
```elm
init : Flags -> (Model, Cmd Msg)
init flags =
  let
    (contextMenu, msg) = ContextMenu.init
  in
    ( { contextMenu = contextMenu
      }
    , Cmd.map ContextMenuMsg msg
    )
```

<span>4. Update</span>
```elm
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ContextMenuMsg msg ->
      let
        (contextMenu, cmd) =
          ContextMenu.update msg model.contextMenu
      in
        ( { model | contextMenu = contextMenu }
        , Cmd.map ContextMenuMsg cmd
        )
```

<span>5. Subscribe</span>
```elm
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map ContextMenuMsg (ContextMenu.subscriptions model.contextMenu)
```

<span>6. View</span>
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

BSD-3-Clause

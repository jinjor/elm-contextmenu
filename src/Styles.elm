module Styles exposing (..)

import Color exposing (Color)

type alias Style =
  List (String, String)


borderColor : String
borderColor =
  "#ccc"


container : Color -> Int -> Int -> Bool -> Int -> Int -> Int -> String -> Int -> Style
container containerColor borderWidth padding rounded width left top fontFamily fontSize =
  [ ("border-style", "solid")
  , ("border-width", px borderWidth)
  , ("border-color", borderColor)
  , ("position", "fixed")
  , ("top", px top)
  , ("left", px left)
  , ("width", px width)
  , ("z-index", toString (2147483647 - 10))
  , ("background-color", fromColor containerColor)
  , ("cursor", "default")
  , ("box-shadow", "0px 3px 8px 0px rgba(0,0,0,0.3)")
  , ("padding", px padding ++ " 0")
  , ("border-radius", if rounded then px padding else "")
  , ("font-family", fontFamily)
  , ("font-size", px fontSize)
  ]


row : Color -> Color -> Bool -> Bool -> Int -> Bool -> Bool -> Bool -> Style
row hoverColor disabledTextColor invertText usePointer lineHeight hovered disabled hasShortCut =
  [ ("position", "relative")
  , ("padding", "0 18px 0 28px")
  , ("background-color", if hovered then fromColor hoverColor else "")
  , ("height", px lineHeight)
  , ("color",
      if disabled then
        fromColor disabledTextColor
      else if hovered && invertText then
        "#fff"
      else
        ""
    )
  , ("cursor", if (not disabled) && usePointer then "pointer" else "")
  , ("display", "flex")
  , ("justify-content", if hasShortCut then "space-between" else "")
  ]


text : Int -> Style
text lineHeight =
  [ ("line-height", px lineHeight)
  , ("text-overflow", "ellipsis")
  , ("overflow", "hidden")
  , ("white-space", "nowrap")
  ]


annotation : Color -> Int -> Int -> Bool -> Style
annotation color annotationHeight fontSize disabled =
  [ ("margin-top", "-2px")
  , ("line-height", px annotationHeight)
  , ("font-size", px fontSize)
  , ("color", fromColor color)
  ]


shortcut : Color -> Int -> Bool -> Style
shortcut color lineHeight hovered =
  [ ("line-height", px lineHeight)
  , ("color", if hovered then "" else fromColor color)
  ]


partition : Int -> Int -> Style
partition borderWidth margin =
  [ ("border-bottom-style", "solid")
  , ("border-bottom-width", px 1)
  , ("border-bottom-color", borderColor)
  , ("border-top", "none")
  , ("margin", px margin ++ " 0")
  ]


icon : Int -> Style
icon size =
  [ ("position", "absolute")
  , ("margin-left", px (-size - 4))
  , ("top", "2px")
  ]


----

px : number -> String
px n =
  toString n ++ "px"


fromColor : Color -> String
fromColor color =
  let
    { red, green, blue, alpha } =
      Color.toRgb color
  in
    "rgba(" ++
      toString red ++ "," ++
      toString green ++ "," ++
      toString blue ++ "," ++
      toString alpha ++ ")"

module Configs exposing (..)

import ContextMenu exposing (..)
import Color exposing (Color)


winDesktop : ContextMenu.Config
winDesktop =
  { defaultConfig
  | direction = LeftBottom
  , overflowX = Shift
  , overflowY = Mirror
  , containerColor = lightGray
  , hoverColor = gray
  , invertText = False
  , cursor = Arrow
  , rounded = False
  }


winChrome : ContextMenu.Config
winChrome =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Shift
  , overflowY = Mirror
  , containerColor = white
  , hoverColor = lightGray
  , invertText = False
  , cursor = Arrow
  , rounded = False
  }


winFirefox : ContextMenu.Config
winFirefox =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Shift
  , overflowY = Mirror
  , containerColor = lightGray
  , hoverColor = lightBlue
  , invertText = False
  , cursor = Arrow
  , rounded = False
  }


winEdge : ContextMenu.Config
winEdge =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Mirror
  , overflowY = Mirror
  , containerColor = lightGray
  , hoverColor = gray
  , invertText = False
  , cursor = Arrow
  , rounded = False
  }


mac : ContextMenu.Config
mac =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Mirror
  , overflowY = Shift
  , containerColor = lightGray
  , hoverColor = deepBlue
  , invertText = True
  , cursor = Arrow
  , rounded = True
  }


googleSpreadsheet : ContextMenu.Config
googleSpreadsheet =
  { defaultConfig
  | direction = RightBottom
  , overflowX = Shift
  , overflowY = Shift
  , containerColor = white
  , hoverColor = lightGray
  , invertText = False
  , cursor = Pointer
  , rounded = False
  }


---- COLORS

white : Color
white =
  Color.rgb 255 255 255


lightGray : Color
lightGray =
  Color.rgb 238 238 238


gray : Color
gray =
  Color.rgb 217 217 217


lightBlue : Color
lightBlue =
  Color.rgb 117 199 253


deepBlue : Color
deepBlue =
  Color.rgb 62 126 255

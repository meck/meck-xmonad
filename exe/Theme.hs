module Theme (defaultSpacing, myFont, myPromptTheme, hotPromptTheme, decoBarTheme, copiedBarTheme, shortcutsTheme) where

import Colors.Nord
import XMonad
import XMonad.Actions.GridSelect
import XMonad.Layout.Decoration
import XMonad.Prompt

defaultSpacing :: Integer
defaultSpacing = 8

myFont :: String
myFont = fontW 50 "xft:Inter"

fontW :: Integer -> String -> String
fontW w = (<> ":weight=" <> show w)

--  ╭──────────────────────────────────────────────────────────╮
--  │                          Colors                          │
--  ╰──────────────────────────────────────────────────────────╯

fgCol :: String
fgCol = hexCol nord4

fgColAlt :: String
fgColAlt = hexCol nord0

bgCol :: String
bgCol = hexCol nord1

accentCol :: String
accentCol = hexCol nord9

activeCol :: String
activeCol = hexCol nord10

inactiveCol :: String
inactiveCol = hexCol nord3

urgentCol :: String
urgentCol = hexCol nord12

criticalCol :: String
criticalCol = hexCol nord11

specialCol :: String
specialCol = hexCol nord15

--  ╭──────────────────────────────────────────────────────────╮
--  │                          Themes                          │
--  ╰──────────────────────────────────────────────────────────╯

myPromptTheme :: XPConfig
myPromptTheme =
  def
    { font = myFont,
      bgColor = bgCol,
      fgColor = fgCol,
      fgHLight = activeCol,
      bgHLight = bgCol,
      borderColor = accentCol,
      maxComplRows = Just 10,
      promptBorderWidth = 1,
      height = 55,
      position = CenteredAt 0.33 0.33
    }

hotPromptTheme :: XPConfig
hotPromptTheme = myPromptTheme {borderColor = criticalCol}

decoBarTheme :: Theme
decoBarTheme =
  def
    { activeColor = activeCol,
      inactiveColor = inactiveCol,
      urgentColor = urgentCol,
      activeBorderWidth = 0,
      inactiveBorderWidth = 0,
      urgentBorderWidth = 0,
      fontName = myFont,
      activeTextColor = activeCol,
      inactiveTextColor = inactiveCol,
      urgentTextColor = urgentCol,
      decoHeight = 8,
      decoWidth = 12
    }

-- For indicating a window copied to multiple workspaces
copiedBarTheme :: Theme
copiedBarTheme =
  decoBarTheme
    { activeColor = specialCol,
      inactiveColor = specialCol,
      urgentColor = specialCol,
      activeTextColor = specialCol,
      inactiveTextColor = specialCol,
      urgentTextColor = specialCol
    }

-- Config
shortcutsTheme :: GSConfig (X ())
shortcutsTheme =
  (buildDefaultGSConfig defaultColorizer)
    { gs_cellheight = 50,
      gs_cellwidth = 300,
      gs_cellpadding = 10,
      gs_colorizer = colr,
      gs_font = myFont,
      gs_originFractY = 0.33,
      gs_navigate = navNSearch,
      gs_bordercolor = accentCol
    }
  where
    colr _ True = pure (bgCol, activeCol)
    colr _ False = pure (bgCol, fgCol)

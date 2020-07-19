{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List                      ( foldl' )
import           Numeric                        ( showHex )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           System.Taffybar
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Hooks
import           System.Taffybar.Context        ( TaffyIO )
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Widget
import           Data.GI.Gtk                    ( Widget )
-- import           Utils.Taffybar
import           Colors.Nord

main :: IO ()
main =
    startTaffybar
        $ withLogServer
        $ withToggleServer
        $ toTaffyConfig myConfig
  where
    tray     = sniTrayNew
    myConfig = defaultSimpleTaffyConfig
        { startWidgets  = workspaces
                              : map (>>= buildContentsBox) [layout, windowsW]
        , endWidgets    = map
                              (>>= buildContentsBox)
                              [clock, tray, mpris2New]
        , barPosition   = Top
        , barPadding    = 0
        , barHeight     = 30
        , widgetSpacing = 5
        }

---------------
--  Widgets  --
---------------

workspaces :: TaffyIO Widget
workspaces = workspacesNew $ defaultWorkspacesConfig
    { minIcons            = 0
    , widgetGap           = 5
    , underlineHeight     = 2
    , underlinePadding    = 1
    , labelSetter         = myLabelSetter
    , getWindowIconPixbuf = defaultGetWindowIconPixbuf
    }
  where
    myLabelSetter w = pure $ show (n + 1) <> ": " <> T.unpack name
      where
        (WorkspaceId n) = workspaceIdx w
        isActive        = workspaceState w == Active
        name =
            (if isActive
                    then markText [Foreground nord10, FontWeight 700]
                    else id
                )
                $ T.pack
                $ workspaceName w


layout :: TaffyIO Widget
layout = layoutNew $ LayoutConfig (pure . markText [FontWeight 700])


windowsW :: TaffyIO Widget
windowsW = windowsNew $ WindowsConfig
    defaultGetMenuLabel
    (markText [FontWeight 300] <$> defaultGetActiveLabel)


clock :: TaffyIO Widget
clock = textClockNewWith $ defaultClockConfig
    { clockFormatString   = T.unpack $ markText [FontWeight 700] "%a %-e %b %T"
    , clockUpdateStrategy = RoundedTargetInterval 1 0.1
    }

--------------
--  Colors  --
--------------

-- For scaling according to useage
usageColors :: [(Int, Color)]
usageColors = [(70, nord13), (80, nord12), (90, nord11)]


-- Gets a scaled color
segmentColor :: Ord a => a -> [(a, Color)] -> Maybe Color
segmentColor n = foldl' go Nothing
    where go acc (val, col) = if n >= val then Just col else acc


colorToRGBA :: Color -> RGBA
colorToRGBA (Color n) = (r, g, b, a)
  where
    percent x = fromIntegral x / 256
    r = percent $ n `div` 0x1000000
    g = percent $ n `mod` 0x1000000 `div` 0x10000
    b = percent $ n `mod` 0x10000 `div` 0x100
    a = percent $ n `mod` 0x100


colorToText :: Color -> Text
colorToText (Color n) = T.pack $ "#" <> showHex n ""


-------------
--  Pango  --
-------------
-- https://developer.gnome.org/pango/stable/pango-Markup.html

data PfontSize = Pts Int | Smaller | Larger


instance Show PfontSize where
    show (Pts n) = show n
    show Smaller = "small"
    show Larger  = "large"

data PangoTag = FontDesc Text | Foreground Color | FontSize PfontSize | FontWeight Int


instance Show PangoTag where
    show (FontDesc   fd ) = "font_desc='" <> show fd <> "'"
    show (Foreground col) = "foreground='" <> T.unpack (colorToText col) <> "'"
    show (FontSize   fs ) = "size='" <> show fs <> "'"
    show (FontWeight we ) = "font_weight='" <> show we <> "'"


markText :: [PangoTag] -> Text -> Text
markText attrs t = "<span " <> tags <> ">" <> t <> "</span>"
    where tags = T.pack $ unwords $ show <$> attrs

monoSpace :: Text -> Text
monoSpace t = "<tt>" <> t <> "</tt>"

fontAwe :: PangoTag
fontAwe = FontDesc "FontAwesome"

{-# LANGUAGE TupleSections #-}

module Misc (shortcutsPrompt, myTerminal, myAltTerminal, myBrowser, myLauncher, myCalculator, rofiClip, myProcessViewer, scratchpads) where

import Control.Arrow (first)
import Control.Monad.Trans.State.Lazy (StateT)
import Data.List (intercalate)
import qualified Data.Map as M
import Hooks.Misc
import Text.ParserCombinators.ReadP (readP_to_S)
import Theme
import Util.Scaling
import XMonad
import XMonad.Prompt
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad

--  ╭──────────────────────────────────────────────────────────╮
--  │                       Applications                       │
--  ╰──────────────────────────────────────────────────────────╯

myTerminal :: String
myTerminal = "kitty"

myAltTerminal :: String
myAltTerminal = "alacritty"

myBrowser :: String
myBrowser = "firefox"

myLauncher :: String
myLauncher = "rofi -show combi -show-icons"

myCalculator :: String
myCalculator = "rofi-calc"

myProcessViewer :: String
myProcessViewer = myAltTerminal <> " --title Htop -e htop"

rofiClip :: String
rofiClip =
  "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"

--  ╭──────────────────────────────────────────────────────────╮
--  │                       Scratchpads                        │
--  ╰──────────────────────────────────────────────────────────╯

scratchpads :: [NamedScratchpad]
scratchpads =
  [ NS "Spotify" "spotify" (className =? "Spotify") idHook,
    NS "1Password" "1password" onePassMainWin $ doTopRect (1 / 2, 1 / 2),
    NS "Mpv" "mpv --idle=yes --force-window=yes" (className =? "mpv") doFloatVideo,
    NS "Calculator" "galculator" (className =? "Galculator") doFloatTopCenter,
    NS "Process Viewer" myProcessViewer (title =? "Htop") $ doTopRect (1 / 2, 1 / 2),
    NS "Slack" "slack" (className =? "Slack") $ doTopRect (2 / 3, 2 / 3)
  ]
  where
    onePassMainWin = className =? "1Password" <&&> (not <$> (title =? "Two-factor authentication"))

--  ╭──────────────────────────────────────────────────────────╮
--  │                        Shortcuts                         │
--  ╰──────────────────────────────────────────────────────────╯

shortcutsCmds :: [([Char], [Char], StateT XPState IO ())]
shortcutsCmds =
  [ ("a", "Autorandr", spawn "rofi-autorandr"),
    ("v", "Fetch from clipboard", spawn rofiClip),
    ("e", "Emoji Picker", spawn "rofi-emoji")
  ]


-- Display a prompt with hotkeys
-- as in `shortcutsCmds`
shortcutsPrompt :: X ()
shortcutsPrompt =
  mkXPrompt'
    ShortcutsPrompt
    promptCfg
    (const $ pure [])
    (const $ pure ())
  where
    promptCfg = myPromptTheme {promptKeymap = keymap'}
    keymap' = M.fromList $ first (0,) <$> ((xK_Escape, quit) : keymap)
    keymap =
      ( \(c, _, action) -> (fst $ head $ readP_to_S parseKey c, action >> quit)
      )
        <$> shortcutsCmds

data ShortcutsPrompt = ShortcutsPrompt

instance XPrompt ShortcutsPrompt where
  showXPrompt ShortcutsPrompt =
    intercalate "    " $
      (\(c, desc, _) -> c <> ": " <> desc <> "  ")
        <$> shortcutsCmds

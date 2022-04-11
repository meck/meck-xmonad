module Misc (myTerminal, myAltTerminal, myBrowser, myLauncher, myCalculator, myEmail, myProcessViewer, scratchpads, runShortcuts) where

import Hooks.Misc
import Theme
import Util.Scaling
import XMonad
import XMonad.Actions.SinkAll (sinkAll)
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

myEmail :: String
myEmail = "thunderbird"

myProcessViewer :: String
myProcessViewer = myAltTerminal <> " --title Htop -e htop"

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

shortcutsGScmds :: [(String, X ())]
shortcutsGScmds =
  [ ("Clipboard", spawn rofiClip),
    ("Sink all", sinkAll),
    ("Project: Rename", renameProjectPrompt' myPromptTheme),
    ("Project: cd", changeProjectDirPrompt' myPromptTheme),
    ("Emoji", spawn "rofi-emoji"),
    ("Autorandr", spawn "rofi-autorandr"),
    confirm "Clipboard clear history" $ spawn "greenclip clear"
  ]
  where
    rofiClip = "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"
    confirm label action = (label, confirmPrompt' hotPromptTheme label action)

runShortcuts :: X ()
runShortcuts = runSelectedAction' shortcutsTheme shortcutsGScmds

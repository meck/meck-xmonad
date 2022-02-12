{-# LANGUAGE LambdaCase #-}

module Keymap (myModMask, myKeys, showKeybindings) where

import qualified Data.Map as M
import Layout.Zoom
import Misc
import System.Exit
import System.IO (hClose, hPutStr)
import Theme
import Util.Scaling
import XMonad
import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.MessageFeedback
import XMonad.Actions.Navigation2D
import XMonad.Actions.SwapPromote
import XMonad.Actions.WithAll
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.NamedActions (NamedAction, addName, showKm, subtitle, (^++^))
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (spawnPipe)

--  ╭──────────────────────────────────────────────────────────╮
--  │                         Modmask                          │
--  ╰──────────────────────────────────────────────────────────╯

myModMask :: KeyMask
myModMask = mod4Mask -- Super

--  ╭──────────────────────────────────────────────────────────╮
--  │                       Keybindings                        │
--  ╰──────────────────────────────────────────────────────────╯

{- ORMOLU_DISABLE -}
myKeys :: XConfig Layout -> [((KeyMask, KeySym), NamedAction)]
myKeys conf =
  let subKeys str ks = subtitle str : mkNamedKeymap conf ks
    in
    subKeys "System"
        [ ("M-q"                    , addName "Restart XMonad"              $ spawn "xmonad --restart")
        , ("M-S-q"                  , addName "Quit XMonad"                 $ confirmPrompt' hotPromptTheme "quit XMonad" $ io exitSuccess)
        , ("M-'"                    , addName "Shortcuts Menu"              shortcutsPrompt)
        , ("M-C-<Space>"            , addName "Toggle Keyboard layout"      toggleKeyboard)
        , ("<Pause>"                , addName "Increase Monitor Backlight"  $ spawn "backlight inc")
        , ("<Scroll_lock>"          , addName "Decrease Monitor Backlight"  $ spawn "backlight dec")
        ] ^++^

    subKeys "Launcher"
         [ ("M-<Space>"             , addName "Launcher"                   $ spawn myLauncher)
         , ("M-C-v"                 , addName "Clipboard"                  $ spawn rofiClip)
         , ("M-<Return>"            , addName "Terminal"                   $ spawn myTerminal)
         , ("M-S-<Return>"          , addName "Alt Terminal"               $ spawn myAltTerminal)
         , ("M-\\"                  , addName "Browser"                    $ spawn myBrowser)
         , ("<Print>"               , addName "Screenshot"                 $ spawn "flameshot gui")
         , ("M-n"                   , addName "Calculator"                 $ spawn myCalculator)
         ] ^++^

    subKeys "Scratchpads"
         [ ("M-m"                   , addName "NSP Spotify"                $ namedScratchpadAction scratchpads "Spotify")
         , ("M-v"                   , addName "NSP Mpv"                    $ namedScratchpadAction scratchpads "Mpv")
         , ("M-t"                   , addName "NSP Processes"              $ namedScratchpadAction scratchpads "Process Viewer")
         , ("M-p"                   , addName "NSP Password Manager"       $ namedScratchpadAction scratchpads "1Password")
         , ("M-s"                   , addName "NSP Slack"                  $ namedScratchpadAction scratchpads "Slack")
         ] ^++^

    subKeys "Windows"
         (
         [ ("M-<Backspace>"         , addName "Kill"                       kill1)
         , ("M-S-<Backspace>"       , addName "Kill all or del empty ws"   killAllorRemWS)
         , ("M-b"                   , addName "Promote"                    $ swapPromote' False)
         , ("M-d"                   , addName "Duplicate w to all ws"      toggleCopyToAll)
         ]
         ++ zipM' "M-"              "Navigate window"                      dirKeys dirs windowGo True
         ++ zipM' "M-S-"            "Move window"                          dirKeys dirs windowSwap True
         ++ zipM' "M-"              "Navigate screen"                      arrowKeys dirs screenGo True
         ++ zipM' "M-S-"            "Move window to screen"                arrowKeys dirs windowToScreen True
         ++ zipM' "M-C-"            "Swap workspace to screen"             arrowKeys dirs screenSwap True
         ) ^++^

    subKeys "Resize"
        [ ("M-["                    , addName "Expand L"                   $ tryMsgR (ExpandTowards L) Shrink)
        , ("M-]"                    , addName "Expand R"                   $ tryMsgR (ExpandTowards R) Expand)
        , ("M-S-["                  , addName "Expand U"                   $ tryMsgR (ExpandTowards U) MirrorShrink)
        , ("M-S-]"                  , addName "Expand D"                   $ tryMsgR (ExpandTowards D) MirrorExpand)

        , ("M-C-["                  , addName "Shrink L"                   $ tryMsgR (ShrinkFrom R) Shrink)
        , ("M-C-]"                  , addName "Shrink R"                   $ tryMsgR (ShrinkFrom L) Expand)
        , ("M-C-S-["                , addName "Shrink U"                   $ tryMsgR (ShrinkFrom D) MirrorShrink)
        , ("M-C-S-]"                , addName "Shrink D"                   $ tryMsgR (ShrinkFrom U) MirrorExpand)
        ] ^++^

    subKeys "Workspaces"
         (
         [ ("M-w"                   , addName "Switch to Project"          $ switchProjectPrompt' myPromptTheme)
         , ("M-S-w s"               , addName "Shift to Project"           $ shiftToProjectPrompt' myPromptTheme)
         , ("M-S-w r"               , addName "Rename Project"             $ renameProjectPrompt' myPromptTheme)
         , ("M-S-w d"               , addName "Change project directory"   $ changeProjectDirPrompt' myPromptTheme)
         , ("M-<Escape>"            , addName "Next non-empty workspace"   nextNonEmptyWS)
         , ("M-S-<Escape>"          , addName "Prev non-empty workspace"   prevNonEmptyWS)
         , ("M-`"                   , addName "Next non-empty workspace"   nextNonEmptyWS)
         , ("M-S-`"                 , addName "Prev non-empty workspace"   prevNonEmptyWS)
         , ("M-a"                   , addName "Toggle last workspace"      $ toggleWS' ["NSP"])
         ]
         ++ zipM "M-"               "View      ws"                         wsKeys [0..8] (DO.withNthWorkspace' notNSP W.greedyView)
         ++ zipM "M-S-"             "Move w to ws"                         wsKeys [0..8] (DO.withNthWorkspace' notNSP W.shift)
         ++ zipM "M-S-C-"           "Copy w to ws"                         wsKeys [0..8] (DO.withNthWorkspace' notNSP copy)
         ) ^++^

    subKeys "Layout Management"
         [ ("M-/"                   , addName "Cycle all layouts"          $ sendMessage NextLayout)
         , ("M-S-/"                 , addName "Reset layout"               $ setLayout $ XMonad.layoutHook conf)
         , ("M-="                   , addName "Increace Window Spacing"    $ incScreenWindowSpacing' 3)
         , ("M--"                   , addName "Decreace Window Spacing"    $ decScreenWindowSpacing' 3)
         , ("M-0"                   , addName "Toogle Window Spacing"      $ toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
         , ("M-f"                   , addName "Zoom focused window"        zoomFocus)
         , ("M-r"                   , addName "Rotate/Mirror"              $ tryMsgR Rotate $ Toggle MIRROR)
         , ("M-S-r"                 , addName "Reflect"                    $ sendMessage $ Toggle REFLECTX)
         , ("M-y"                   , addName "Toggle float w"             $ withFocused toggleFloat)
         , ("M-S-y"                 , addName "Tile all floating w"        sinkAll)
         ] ^++^

    subKeys "Media"
        [ ("<XF86AudioLowerVolume>" , addName "Lower Volume"               $ spawn "pactl set-sink-mute  @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioRaiseVolume>" , addName "Raise Volume"               $ spawn "pactl set-sink-mute  @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86AudioMute>"        , addName "Mute"                       $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioPlay>"        , addName "Play/Pause"                 $ spawn "playerctl play-pause")
        , ("<XF86AudioNext>"        , addName "Next"                       $ spawn "playerctl next")
        , ("<XF86AudioPrev>"        , addName "Prev"                       $ spawn "playerctl previous")
        ]

{- ORMOLU_ENABLE -}

--  ╭──────────────────────────────────────────────────────────╮
--  │                        Utilities                         │
--  ╰──────────────────────────────────────────────────────────╯

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $
  io $ do
    h <-
      spawnPipe $
        unwords
          [ "yad",
            "--title='XMonad bindings'",
            "--no-buttons",
            "--text-info",
            "--close-on-unfocus",
            "--fontname='Iosevka 10'"
          ]
    hPutStr h $ unlines ["XMonad bindings", "Search with C-s", ""]
    hPutStr h $ unlines $ showKm x
    hClose h

dirKeys :: [String]
dirKeys = ["j", "k", "h", "l"]

arrowKeys :: [String]
arrowKeys = ["<D>", "<U>", "<L>", "<R>"]

wsKeys :: [String]
wsKeys = show <$> [1 .. 9 :: Integer]

dirs :: [Direction2D]
dirs = [D, U, L, R]

nextNonEmptyWS :: X ()
nextNonEmptyWS =
  findWorkspace getSortByOrderNoSP Next HiddenNonEmptyWS 1
    >>= \t -> windows . W.view $ t

prevNonEmptyWS :: X ()
prevNonEmptyWS =
  findWorkspace getSortByOrderNoSP Prev HiddenNonEmptyWS 1
    >>= \t -> windows . W.view $ t

getSortByOrderNoSP :: X ([WindowSpace] -> [WindowSpace])
getSortByOrderNoSP =
  fmap (. namedScratchpadFilterOutWorkspace) DO.getSortByOrder

notNSP :: [String] -> [String]
notNSP = filter (/= "NSP")

tryMsgR :: (Message a, Message b) => a -> b -> X ()
tryMsgR x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

zipM :: [a] -> String -> [[a]] -> [t] -> (t -> X ()) -> [([a], NamedAction)]
zipM m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as

zipM' :: [a] -> String -> [[a]] -> [t1] -> (t1 -> t2 -> X ()) -> t2 -> [([a], NamedAction)]
zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as

toggleCopyToAll :: X ()
toggleCopyToAll =
  wsContainingCopies >>= \case
    [] -> windows copyToAll
    _ -> killAllOtherCopies

toggleFloat :: Window -> X ()
toggleFloat w = windows $ \s ->
  if M.member w (W.floating s)
    then W.sink w s
    else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s

killAllorRemWS :: X ()
killAllorRemWS =
  gets (W.index . windowset) >>= \case
    [] -> removeEmptyWorkspace
    _ -> confirmPrompt' hotPromptTheme "kill all" killAll

-- Change keyboard layout between us and swe
toggleKeyboard :: X ()
toggleKeyboard = spawn "switch-keyboard-layout"

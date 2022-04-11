{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main (main) where

import Data.List
  ( isPrefixOf,
  )
import Data.Monoid
import Hooks.Misc
import Keymap
import Layout.Custom
import Layout.Zoom
import Misc
import Theme
import Util.PagerHints (pagerHints)
import Util.Scaling
import Util.ShadowTag
import Util.Viewport (viewPortsLogHookCustom, viewPortsStartupHook)
import XMonad
import XMonad.Actions.DynamicProjects
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.Navigation2D
import XMonad.Actions.SpawnOn
import XMonad.Actions.SwapPromote
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.EwmhDesktops
  ( addEwmhWorkspaceSort,
    ewmh,
    ewmhFullscreen,
  )
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
  ( avoidStruts,
    docks,
  )
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.BorderResize
import XMonad.Layout.Decoration
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.NamedActions
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare (filterOutWs)

--  ╭──────────────────────────────────────────────────────────╮
--  │                           Main                           │
--  ╰──────────────────────────────────────────────────────────╯

main :: IO ()
main =
  xmonad
    . docks
    . dynamicProjects projects
    . addEwmhWorkspaceSort orderWs
    . ewmhFullscreen
    . ewmh
    . withNavigation2DConfig myNav2DConf
    . pagerHints
    . addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ myConfig

--  ╭──────────────────────────────────────────────────────────╮
--  │                          Config                          │
--  ╰──────────────────────────────────────────────────────────╯

myConfig =
  def
    { borderWidth = 0,
      workspaces = myWorkspaces,
      startupHook = myStartupHook,
      layoutHook = myLayoutHook,
      manageHook = myManageHook,
      modMask = myModMask,
      handleEventHook = myEventHook,
      logHook = myLogHook,
      terminal = myTerminal
    }

myNav2DConf :: Navigation2DConfig
myNav2DConf =
  def
    { defaultTiledNavigation = centerNavigation,
      floatNavigation = centerNavigation,
      screenNavigation = lineNavigation,
      unmappedWindowRect = [("Full", singleWindowRect)]
    }

--  ╭──────────────────────────────────────────────────────────╮
--  │                        Workspaces                        │
--  ╰──────────────────────────────────────────────────────────╯

wsDefault :: String
wsDefault = "main"

wsMisc :: String
wsMisc = "misc"

wsSys :: String
wsSys = "sys"

wsMail :: String
wsMail = "mail"

myWorkspaces :: [String]
myWorkspaces = [wsDefault, wsMisc]

projects :: [Project]
projects =
  [ Project
      { projectName = wsSys,
        projectDirectory = "/etc/nixos/",
        projectStartHook = Just $ do
          spawnOn wsSys myTerminal
          spawnOn wsSys $ myBrowser <> " --new-window status.nixos.org"
      },
    Project
      { projectDirectory = "~",
        projectName = wsMail,
        projectStartHook = Just $ do
          spawnOn wsSys myEmail
      }
  ]

-- Remove named scratchpads and sort
-- according to DynamicWorkspaces
orderWs = do
  sortWs <- DO.getSortByOrder
  pure (sortWs . filterOutWs [scratchpadWorkspaceTag])

--  ╭──────────────────────────────────────────────────────────╮
--  │                          Events                          │
--  ╰──────────────────────────────────────────────────────────╯

myEventHook :: Event -> X All
myEventHook =
  mffZoomedEventHook
    <> spotifyFloatHook
  where
    -- Spotify changes name after launch so we use a
    -- dynamicTitle event hook to position the window
    spotifyFloatHook =
      dynamicTitle (title =? "Spotify" --> doCenterRect (2 / 3, 2 / 3))

--  ╭──────────────────────────────────────────────────────────╮
--  │                           Logs                           │
--  ╰──────────────────────────────────────────────────────────╯

myLogHook :: X ()
myLogHook = do
  -- Order and remove for ewmh viewport log
  viewPortsLogHookCustom =<< orderWs
  shadowZoomLogHook
  shadowFloatingLogHook
  masterHistoryHook

--  ╭──────────────────────────────────────────────────────────╮
--  │                         Startup                          │
--  ╰──────────────────────────────────────────────────────────╯

myStartupHook :: X ()
myStartupHook = do
  -- For storing and using `GDK_SCALE` see Util.Scaling
  scaleStartupHook

  -- Provide _NET_DESKTOP_VIEWPORT
  viewPortsStartupHook

  -- Some java apps (Quartus II), has issues
  -- with unknown WMs, fake another WM
  setWMName "LG3D"

--  ╭──────────────────────────────────────────────────────────╮
--  │                        ManageHook                        │
--  ╰──────────────────────────────────────────────────────────╯

myManageHook :: ManageHook
myManageHook =
  namedScratchpadManageHook scratchpads
    <> manageSpecific
    <> smartPlaceHook
  where
    manageSpecific =
      composeOne
        [ appName =? "desktop_window" -?> doIgnore,
          isRole =? "GtkFileChooserDialog" -?> doCenterRect (1 / 3, 1 / 2),
          title =? "XMonad bindings" -?> doTopRect (1 / 2, 1 / 2),
          className =? "lxqt-openssh-askpass" -?> doSmartFloat,
          className =? "Nm-connection-editor" -?> doSmartFloat,
          className =? "Pavucontrol" -?> doSmartFloat,
          className =? "Paprefs" -?> doSmartFloat,
          className =? "Pinentry" -?> doSmartFloat,
          isPrefixOf ".blueman-" <$> className -?> doSmartFloat,
          className =? "Slack | mini panel" -?> doSmartFloat,
          className =? "Org.gnome.NautilusPreviewer" -?> doCenterRect (1 / 2, 1 / 2),
          isDialog -?> doCenterFloat,
          isRole =? "pop-up" -?> doCenterFloat,
          isType "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat,
          isState "_NET_WM_STATE_ABOVE" -?> doCenterFloat,
          isFullscreen -?> doFullFloat,
          pure True -?> tileBelow
        ]

    -- Default tiling, XMonad default is `Above Newer`
    tileBelow = insertPosition Below Newer

    isRole = stringProperty "WM_WINDOW_ROLE"
    isType = isInProperty "_NET_WM_WINDOW_TYPE"
    isState = isInProperty "_NET_WM_STATE"

--  ╭──────────────────────────────────────────────────────────╮
--  │                         Layouts                          │
--  ╰──────────────────────────────────────────────────────────╯

myLayoutHook = mkToggle1 ZOOM $ tall ||| threeCol ||| bsp ||| full
  where
    named x = renamed [Replace x]
    defBorder = Border defaultSpacing defaultSpacing defaultSpacing defaultSpacing
    mySpacing = scaledSpacingRaw False defBorder True defBorder True
    addCopiedBar = decoration shrinkText copiedBarTheme CopiedDecoration
    addDecoBar = decoration shrinkText decoBarTheme (SideDecoration U)
    myDecoration = addDecoBar . addCopiedBar

    bsp =
      named "BSP" $
        avoidStruts $
          borderResize $
            myDecoration $
              mkToggle1 MIRROR $
                mkToggle1 REFLECTX $
                  mySpacing emptyBSP

    full =
      named "Full" $
        avoidStruts Full

    tall =
      named "Tall" $
        avoidStruts $
          borderResize $
            myDecoration $
              mySpacing $
                mkToggle1 MIRROR $
                  mkToggle1 REFLECTX $
                    ResizableTall 1 (1 / 200) (9 / 20) []

    threeCol =
      named "Columns" $
        avoidStruts $
          borderResize $
            myDecoration $
              mkToggle1 MIRROR $
                mkToggle1 REFLECTX $
                  mySpacing $
                    ThreeColMid 1 (1 / 200) (7 / 16)

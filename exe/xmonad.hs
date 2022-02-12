-- vim: set foldmethod=marker foldlevel=1 :
-- Imports                                      {{{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import           Control.Arrow                  ( first )
import           Data.List                      ( delete
                                                , intercalate
                                                , isPrefixOf
                                                )
import qualified Data.Map                      as M
import           System.Exit
import           System.IO
import           Text.ParserCombinators.ReadP   ( readP_to_S )

import           XMonad


import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces ( removeEmptyWorkspace )
import           XMonad.Actions.DynamicProjects
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import           XMonad.Actions.MessageFeedback
import           XMonad.Actions.Navigation2D
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.SwapPromote
import           XMonad.Actions.TagWindows
import           XMonad.Actions.WithAll


import           XMonad.Hooks.DynamicProperty
import           XMonad.Hooks.EwmhDesktops      ( ewmh
                                                , ewmhDesktopsLogHookCustom
                                                , ewmhDesktopsEventHookCustom
                                                , fullscreenEventHook
                                                )
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks       ( docks
                                                , avoidStruts
                                                )
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName


import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BorderResize
import           XMonad.Layout.CenteredMaster   ( centerMaster )
import           XMonad.Layout.Decoration
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing
import           XMonad.Layout.ThreeColumns


import           XMonad.Prompt

import qualified XMonad.StackSet               as W


import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

import           Colors.Nord
import           Util.PagerHints ( pagerHints )
import           Util.Scaling
import           Layout.Custom


--------------------------------------------------------------------------}}}
--                                    Main                                {{{
-----------------------------------------------------------------------------

main =
    xmonad
        $ docks
        $ dynamicProjects projects
        $ withNavigation2DConfig myNav2DConf
        $ ewmh
        $ pagerHints
        $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys myConfig



-------------------------------------------------------------------------}}}
--                                 Config                                {{{
----------------------------------------------------------------------------

myConfig = def { borderWidth     = 0
               , workspaces      = myWorkspaces
               , startupHook     = myStartupHook
               , layoutHook      = myLayoutHook
               , manageHook      = myManageHook
               , modMask         = myModMask
               , handleEventHook = myEventHook
               , logHook         = myLogHook
               , terminal        = myTerminal
               }


myNav2DConf = def
    { defaultTiledNavigation    = centerNavigation
    , floatNavigation           = centerNavigation
    , screenNavigation          = lineNavigation
    , unmappedWindowRect        = [("Full", singleWindowRect)]
    }




--------------------------------------------------------------------------}}}
--                                 Theming                                {{{
-----------------------------------------------------------------------------

defaultSpacing = 8

myFont = "xft:Roboto Condensed"

fontW :: Integer -> String -> String
fontW w = (<> ":weight=" <> show w)

fgCol       = hexCol nord4
bgCol       = hexCol nord1
accentCol   = hexCol nord9
activeCol   = hexCol nord10
inactiveCol = hexCol nord3
urgentCol   = hexCol nord12
criticalCol = hexCol nord11
specialCol  = hexCol nord15

myPromptTheme :: XPConfig
myPromptTheme = def { font              = fontW 100 myFont
                    , bgColor           = bgCol
                    , fgColor           = fgCol
                    , fgHLight          = activeCol
                    , bgHLight          = bgCol
                    , borderColor       = accentCol
                    , maxComplRows      = Just 10
                    , promptBorderWidth = 1
                    , height            = 55
                    , position          = CenteredAt 0.33 0.33
                    }

hotPromptTheme = myPromptTheme { borderColor = criticalCol }

decoBarTheme = def { activeColor         = activeCol
                   , inactiveColor       = inactiveCol
                   , urgentColor         = urgentCol
                   , activeBorderWidth   = 0
                   , inactiveBorderWidth = 0
                   , urgentBorderWidth   = 0
                   , fontName            = myFont
                   , activeTextColor     = activeCol
                   , inactiveTextColor   = inactiveCol
                   , urgentTextColor     = urgentCol
                   , decoHeight          = 8
                   , decoWidth           = 12
                   }

-- For indicating a window copied to multiple workspaces
copiedBarTheme = decoBarTheme { activeColor         = specialCol
                              , inactiveColor       = specialCol
                              , urgentColor         = specialCol
                              , activeTextColor     = specialCol
                              , inactiveTextColor   = specialCol
                              , urgentTextColor     = specialCol
                              }



--------------------------------------------------------------------------}}}
--                               Applictions                              {{{
-----------------------------------------------------------------------------

myTerminal = "kitty"
myAltTerminal = "alacritty"
myBrowser = "firefox"
myLauncher = "rofi -show combi -show-icons"
myCalculator = "rofi-calc"
myProcessViewer = myAltTerminal <> " --title Htop -e htop"
rofiClip =
    "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"



--------------------------------------------------------------------------}}}
--                                Shortcut Prompt                         {{{
-----------------------------------------------------------------------------

shortcutsCmds = [ ("a" , "Autorandr" ,            spawn "rofi-autorandr")
                , ("v" , "Fetch from clipboard" , spawn rofiClip)
                , ("e" , "Emoji Picker"         , spawn "rofi-emoji")
                ]

-- Display a prompt with hotkeys
-- as in `shortcutsCmds`
shortcutsPrompt = mkXPrompt' ShortcutsPrompt
                            promptCfg
                            (const $ pure [])
                            (const $ pure ())
  where
    promptCfg = myPromptTheme { promptKeymap = keymap' }
    keymap'   = M.fromList $ first (0, ) <$> ((xK_Escape, quit) : keymap)
    keymap =
        (\(c, _, action) -> (fst $ head $ readP_to_S parseKey c, action >> quit)
            )
            <$> shortcutsCmds

data ShortcutsPrompt = ShortcutsPrompt

instance XPrompt ShortcutsPrompt where
    showXPrompt ShortcutsPrompt =
        intercalate "    "
            $   (\(c, desc, _) -> c <> ": " <> desc <> "  ")
            <$> shortcutsCmds



--------------------------------------------------------------------------}}}
--                                Workspaces                              {{{
-----------------------------------------------------------------------------

wsDefault = "main"
wsMisc    = "misc"
wsSys    = "sys"

myWorkspaces = [wsDefault, wsMisc]

projects =

    [ Project
        { projectName      = wsSys
        , projectDirectory = "/etc/nixos/"
        , projectStartHook = Just $ do
                                 spawnOn wsSys myTerminal
                                 spawnOn wsSys $ myBrowser <> " --new-window status.nixos.org"
        }
    ]




--------------------------------------------------------------------------}}}
--                               Scratchpads                              {{{
-----------------------------------------------------------------------------

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


--------------------------------------------------------------------------}}}
--                                 Events                                 {{{
-----------------------------------------------------------------------------

myEventHook =
  disableMFFHook
    <> fullscreenEventHook
    <> spotifyFloatHook
    <> modifyWSPorderHook
  where

    -- Disable mouse follows focus for these layouts
    noMFFLayouts = []


    disableMFFHook = followOnlyIf $ do

        -- Is current WS a noMFF one
        ws <- gets (W.workspace . W.current . windowset)
        let isNoMFFLayout = description (W.layout ws) `elem` noMFFLayouts

        -- Is current WS Zoomed
        isZoomedLayout <- elem (W.tag ws) . getNFFws <$> XS.get

        pure $ not isNoMFFLayout && not isZoomedLayout


    -- Spotify changes name after launch so we use a
    -- dynamicTitle event hook to position the window
    spotifyFloatHook =
        dynamicTitle (title =? "Spotify" --> doCenterRect (2 / 3, 2 / 3))


    -- Reorder the workspaces using DynamicWorkspaceOrder and
    -- remove NSP workspace from whats sent to polybar.
    -- Used by polybar for clicking workspaces.
    modifyWSPorderHook e = do
        ordS <- DO.getSortByOrder
        ewmhDesktopsEventHookCustom (ordS . namedScratchpadFilterOutWorkspace) e


---------------------------------------------------------------------------}}}
--                                   Logs                                  {{{
------------------------------------------------------------------------------

myLogHook :: X ()
myLogHook =
    modifyWSPorderHook >> shadowZoom >> shadowFloating >> masterHistoryHook
    -- modifyWSPorderHook should be first to mimize flickering
    -- in polybar
  where

    -- Reorder the workspaces using DynamicWorkspaceOrder and
    -- remove NSP workspace from whats sent to polybar.
    modifyWSPorderHook = do
        ordS <- DO.getSortByOrder
        ewmhDesktopsLogHookCustom (ordS . namedScratchpadFilterOutWorkspace)


    -- Add a tag to inform the composer
    -- to put shadows on any floating
    -- windows
    shadowFloating :: X ()
    shadowFloating = do
        stackS <- gets windowset
        let checkTag w = if w `M.member` W.floating stackS
                then addTag sTag w
                else delTag sTag w
        mapM_ checkTag $ W.allWindows stackS
        where sTag = shadowTag <> "_float"


    -- And add a tag informing the composer
    -- to add a shadow to currently zoomed windows.
    shadowZoom :: X ()
    shadowZoom = do
        winSet    <- windowset <$> get
        curZoomed <- getNFFws <$> XS.get
        let focusedWin = W.peek winSet
            allW       = W.allWindows winSet
        if W.currentTag winSet `elem` curZoomed
            then whenJust
                focusedWin
                (\f -> mapM_
                    (\w -> if w == f then addTag zoomTag w else delTag zoomTag w)
                    allW
                )
            else mapM_ (delTag zoomTag) allW
        where zoomTag = shadowTag <> "_zoom"



--------------------------------------------------------------------------}}}
--                                 Startup                                {{{
-----------------------------------------------------------------------------

myStartupHook :: X ()
myStartupHook = do
    -- For storing and using `GDK_SCALE` see Util.Scaling
    scaleStartupHook

    -- Some java apps (Quartus II), has issues
    -- with unknown WMs, fake another WM
    setWMName "LG3D"



--------------------------------------------------------------------------}}}
--                               ManageHook                               {{{
-----------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
  namedScratchpadManageHook scratchpads
    <> manageSpecific
    <> smartPlaceHook
  where
        manageSpecific = composeOne
            [ appName =? "desktop_window"                -?> doIgnore
            , isRole =? "GtkFileChooserDialog"           -?> doCenterRect (1/3, 1/2)
            , title =? "XMonad bindings"                 -?> doTopRect (1/2, 1/2)
            , className =? "lxqt-openssh-askpass"        -?> doSmartFloat
            , className =? "Nm-connection-editor"        -?> doSmartFloat
            , className =? "Pavucontrol"                 -?> doSmartFloat
            , className =? "Paprefs"                     -?> doSmartFloat
            , className =? "Pinentry"                    -?> doSmartFloat
            , isPrefixOf ".blueman-" <$> className       -?> doSmartFloat
            , className =? "Slack | mini panel"          -?> doSmartFloat
            , className =? "Org.gnome.NautilusPreviewer" -?> doCenterRect (1/2, 1/2)
            , isDialog                                   -?> doCenterFloat
            , isRole =? "pop-up"                         -?> doCenterFloat
            , isType "_NET_WM_WINDOW_TYPE_SPLASH"        -?> doCenterFloat
            , isState "_NET_WM_STATE_ABOVE"              -?> doCenterFloat
            , isFullscreen                               -?> doFullFloat
            , pure True                                  -?> tileBelow
            ]

        -- Default tiling, XMonad default is `Above Newer`
        tileBelow = insertPosition Below Newer

        isRole = stringProperty "WM_WINDOW_ROLE"
        isType = isInProperty "_NET_WM_WINDOW_TYPE"
        isState = isInProperty "_NET_WM_STATE"


-- Place any floating windows smartly
smartPlaceHook :: ManageHook
smartPlaceHook = do
  g <- liftX $ scaleDimension 5
  placeHook $ withGaps (g, g, g, g) $ smart (0.5, 0.5)

-- Float and smart place
doSmartFloat :: ManageHook
doSmartFloat = smartPlaceHook <> doFloat

-- Center window with supplied size
doCenterRect :: (Rational, Rational) -> ManageHook
doCenterRect (w, h) = doRectFloat $ W.RationalRect x y w h
  where
    x, y :: Rational
    x = (1 - w) / 2
    y = (1 - h) / 2

-- Center top of screen
doFloatTopCenter :: ManageHook
doFloatTopCenter =
    doFloatDep $ \(W.RationalRect _ _ w h) -> W.RationalRect ((1 - w) / 2) 0 w h

-- Center top of screen with size
doTopRect :: (Rational, Rational) -> ManageHook
doTopRect (w, h) = doRectFloat $ W.RationalRect x 0 w h
  where
    x = (1 - w) / 2

doFloatVideo :: ManageHook
doFloatVideo = doFloatDep $ \(W.RationalRect _ _ w h) ->
    W.RationalRect (1 - 1 / 16 - w / 3) (1 / 10) (w / 3) (h / 3)

--------------------------------------------------------------------------}}}
--                                 Layouts                                {{{
-----------------------------------------------------------------------------

myLayoutHook = mkToggle1 ZOOM $ threeCol ||| tall ||| bsp ||| full

  where

    named x = renamed [Replace x]
    defBorder = Border defaultSpacing defaultSpacing defaultSpacing defaultSpacing
    mySpacing = scaledSpacingRaw False defBorder True defBorder True
    addCopiedBar = decoration shrinkText copiedBarTheme CopiedDecoration
    addDecoBar = decoration shrinkText decoBarTheme (SideDecoration U)
    myDecoration = addDecoBar . addCopiedBar

    bsp =
        named "BSP"
            $ avoidStruts
            $ borderResize
            $ myDecoration
            $ mkToggle1 MIRROR
            $ mkToggle1 REFLECTX
            $ mySpacing emptyBSP

    full =
        named "Full"
            $ avoidStruts Full

    tall =
        named "Tall"
            $ avoidStruts
            $ borderResize
            $ myDecoration
            $ mySpacing
            $ mkToggle1 MIRROR
            $ mkToggle1 REFLECTX
            $ ResizableTall 1 (1 / 200) (11 / 20) []

    threeCol =
        named "Columns"
            $ avoidStruts
            $ borderResize
            $ myDecoration
            $ mkToggle1 MIRROR
            $ mkToggle1 REFLECTX
            $ mySpacing
            $ ThreeColMid 1 (1 / 200) (7 / 16)



-----------------------------------------------------------------------------
--                                 Toggles                                 --
-----------------------------------------------------------------------------

-- Transform the current layout by poping out the master with centerMaster
-- the workspace as not use mouse follow focus
zoomFocus = do
    curWS <- W.tag . W.workspace . W.current . windowset <$> get
    nfWS  <- getNFFws <$> XS.get
    XS.put $ ZoomedWorkspaces $ if curWS `elem` nfWS
        then delete curWS nfWS
        else curWS : nfWS
    sendMessage $ Toggle ZOOM


-- MultiToggle type for zoomFocus
data ZOOM = ZOOM deriving (Read, Show, Eq, Typeable)
instance Transformer ZOOM Window where
    transform ZOOM x k = k (magicFocus $ centerMaster x) (const x)


-- A extendable state for tracking the workspaces currently zoomed
newtype ZoomedWorkspaces = ZoomedWorkspaces { getNFFws :: [WorkspaceId] }
instance ExtensionClass ZoomedWorkspaces where
    initialValue = ZoomedWorkspaces []



-------------------------------------------------------------------------}}}
--                                Misc                                   {{{
----------------------------------------------------------------------------

-- Windows that has a tag begining
-- with this gets a shadow in the composer
shadowTag = "draw_shadow"


-- Change keyboard layout between us and swe
toggleKeyboard = spawn "switch-keyboard-layout"

--------------------------------------------------------------------------}}}
--                              Bindings                                  {{{
-----------------------------------------------------------------------------

myModMask = mod4Mask -- Super

showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
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
  return ()


myKeys conf = let

        subKeys str ks = subtitle str : mkNamedKeymap conf ks

        dirKeys   = ["j", "k", "h", "l"]
        arrowKeys = ["<D>", "<U>", "<L>", "<R>"]
        wsKeys    = show <$> [1 .. 9 :: Integer]
        dirs      = [D, U, L, R]


        nextNonEmptyWS = findWorkspace getSortByOrderNoSP Next HiddenNonEmptyWS 1
            >>= \t -> windows . W.view $ t
        prevNonEmptyWS = findWorkspace getSortByOrderNoSP Prev HiddenNonEmptyWS 1
            >>= \t -> windows . W.view $ t
        getSortByOrderNoSP =
                fmap (.namedScratchpadFilterOutWorkspace) DO.getSortByOrder


        notNSP = filter (/= "NSP")

        tryMsgR x y = sequence_ [tryMessageWithNoRefreshToCurrent x y, refresh]

        zipM m nm ks as f = zipWith (\k d -> (m ++ k, addName nm $ f d)) ks as
        zipM' m nm ks as f b = zipWith (\k d -> (m ++ k, addName nm $ f d b)) ks as


        toggleCopyToAll = wsContainingCopies >>= \case
                                        [] -> windows copyToAll
                                        _ -> killAllOtherCopies

        toggleFloat w = windows $ \s -> if M.member w (W.floating s)
            then W.sink w s
            else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (4 / 5)) s

        killAllorRemWS = gets (W.index . windowset) >>= \case
            [] -> removeEmptyWorkspace
            _  -> confirmPrompt' hotPromptTheme "kill all" killAll


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

-- }}}

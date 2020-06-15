-- Imports                                      {{{
-- vim: set foldmarker={{{,}}} foldmethod=marker foldlevel=1 :
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import           Control.Arrow                  ( first )
import           Data.List                      ( delete
                                                , intercalate
                                                )
import qualified Data.Map                      as M
import           System.Exit
import           System.IO
import           Text.ParserCombinators.ReadP   ( readP_to_S )

import           System.Taffybar.Support.PagerHints
                                                ( pagerHints )

import           XMonad


import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicWorkspaces
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
                                                )
import           XMonad.Hooks.InsertPosition
import           XMonad.Hooks.ManageDocks       ( docks
                                                , avoidStruts
                                                )
import           XMonad.Hooks.ManageHelpers


import           XMonad.Layout.BinarySpacePartition
import           XMonad.Layout.BorderResize
import           XMonad.Layout.CenteredMaster   ( centerMaster )
import           XMonad.Layout.Decoration
import           XMonad.Layout.MagicFocus
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoFrillsDecoration
import           XMonad.Layout.Reflect
import           XMonad.Layout.Renamed
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing


import           XMonad.Prompt
import           XMonad.Prompt.ConfirmPrompt

import qualified XMonad.StackSet               as W


import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.EZConfig
import           XMonad.Util.NamedActions
import           XMonad.Util.NamedScratchpad
import           XMonad.Util.Run

import           Colors.Nord


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
    }




--------------------------------------------------------------------------}}}
--                                 Theming                                {{{
-----------------------------------------------------------------------------

-- Resizing for hidpi,
-- change from nix before building
resScaling :: Float
resScaling = 1.0

scaleRes :: Integer -> Dimension
scaleRes = floor . ( resScaling * ) . realToFrac

defaultSpacing = toInteger $ scaleRes 10

myFont = "xft:Roboto"

fontW :: Integer -> String -> String
fontW w = (<> ":weight=" <> show w)

myMonspaceFont = "xft:Iosevka"

fgCol    = hexCol nord4
lightBg  = hexCol nord2
active   = hexCol nord10
inactive = hexCol nord3
urgent   = hexCol nord12
critical = hexCol nord11

myPromptTheme :: XPConfig
myPromptTheme = def { font              = fontW 300 myFont
                    , bgColor           = lightBg
                    , fgColor           = fgCol
                    , fgHLight          = active
                    , bgHLight          = lightBg
                    , promptBorderWidth = scaleRes 0
                    , height            = scaleRes 30
                    , position          = Top
                    }

hotPromptTheme = myPromptTheme { fgColor = critical
                               , font    = fontW 700 myFont
                               }

topBarTheme = def { activeColor         = active
                  , inactiveColor       = inactive
                  , urgentColor         = urgent
                  , activeBorderWidth   = scaleRes 0
                  , inactiveBorderWidth = scaleRes 0
                  , urgentBorderWidth   = scaleRes 0
                  , fontName            = myFont
                  , activeTextColor     = active
                  , inactiveTextColor   = inactive
                  , urgentTextColor     = urgent
                  , decoHeight          = scaleRes 10
                  }



--------------------------------------------------------------------------}}}
--                               Applictions                              {{{
-----------------------------------------------------------------------------

myTerminal = "alacritty"
myBrowser = "brave"
myLauncher = "rofi"
myCalculator = "galculator"
rofiClip =
    "rofi -modi 'clipboard:greenclip print' -show clipboard -run-command '{cmd}'"



--------------------------------------------------------------------------}}}
--                                Shortcut Prompt                         {{{
-----------------------------------------------------------------------------

shortcutsCmds = [ ("v" , "Fetch from clipboard" , spawn rofiClip)
                , ("n" , "Network config"       , spawn "networkmanager_dmenu")
                , ("c" , "Rofi Calculator"      , spawn "rofi-calc")
                ]

-- Display a prompt with hotkeys
-- as in `shortcutsCmds`
shortcutsPrompt = mkXPrompt ShortcutsPrompt
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
wsConf    = "nix Config"
wsCode    = "code"

myWorkspaces = [wsDefault]

projects =

    [ Project
        { projectName      = wsCode
        , projectDirectory = "~/"
        , projectStartHook = Just $ do
                                 spawnOn wsCode myTerminal
                                 spawnOn wsCode myTerminal
                                 spawnOn wsCode myBrowser
        }

    , Project
        { projectName      = wsConf
        , projectDirectory = "/etc/nixos/"
        , projectStartHook = Just $ do
                                 spawnOn wsConf myTerminal
                                 spawnOn wsConf myBrowser
        }
    ]




--------------------------------------------------------------------------}}}
--                               Scratchpads                              {{{
-----------------------------------------------------------------------------

-- Spotify changes name after launch so we use a
-- dynamicTitle event hook to position the window
scratchpads =
    [ NS "Spotify" ("spotify --force-device-scale-factor=" <> show resScaling) (className =? "Spotify") idHook
    , NS "Mpv" "mpv" (className =? "mpv") doFloatVideo
    , NS "Calculator" myCalculator (className =? "Galculator") doFlotTopCenter
    ]



--------------------------------------------------------------------------}}}
--                                 Events                                 {{{
-----------------------------------------------------------------------------

myEventHook = disableMFFHook <> spotifyFloatHook <> modifyWSPorderHook
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


    spotifyFloatHook =
        dynamicTitle (title =? "Spotify" --> doCenterRect (2 / 3, 2 / 3))


    -- Reorder the workspaces using DynamicWorkspaceOrder and
    -- remove NSP workspace from whats sent to taffybar.
    -- Used by TB for clicking workspaces.
    modifyWSPorderHook e = do
        ordS <- DO.getSortByOrder
        ewmhDesktopsEventHookCustom (ordS . namedScratchpadFilterOutWorkspace) e


---------------------------------------------------------------------------}}}
--                                   Logs                                  {{{
------------------------------------------------------------------------------

myLogHook =
    shadowZoom >> shadowFloating >> masterHistoryHook >> modifyWSPorderHook
  where


    -- Reorder the workspaces using DynamicWorkspaceOrder and
    -- remove NSP workspace from whats sent to taffybar.
    -- Used by TB for selecting workspaces.
    modifyWSPorderHook =
        (ewmhDesktopsLogHookCustom . (namedScratchpadFilterOutWorkspace .))
            =<< DO.getSortByOrder


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

myStartupHook = def




--------------------------------------------------------------------------}}}
--                               ManageHook                               {{{
-----------------------------------------------------------------------------

myManageHook :: ManageHook
myManageHook =
    namedScratchpadManageHook scratchpads <> addShadows <> manageSpecific
  where
        manageSpecific = composeOne
            [ resource =? "desktop_window"        -?> doIgnore
            , resource =? "stalonetray"           -?> doIgnore
            , isRole =? "GtkFileChooserDialog"    -?> doCenterRect (1/3, 1/2)
            , isDialog                            -?> doCenterFloat
            , isRole =? "pop-up"                  -?> doCenterFloat
            , isInProperty "_NET_WM_WINDOW_TYPE"
                           "_NET_WM_WINDOW_TYPE_SPLASH" -?> doCenterFloat
            , isFullscreen                        -?> doFullFloat
            , pure True                           -?> tileBelow
            ]

        tileBelow = insertPosition Below Newer
        isRole = stringProperty "WM_WINDOW_ROLE"

        addShadows :: ManageHook
        addShadows = composeAll [ className =? c --> go | c <- shadowApps ]
          where
            go           = ask >>= (liftX . addTag shadowTagApp) >> idHook
            shadowTagApp = shadowTag <> "_app"
            shadowApps   = ["rofi"]




--------------------------------------------------------------------------}}}
--                                 Layouts                                {{{
-----------------------------------------------------------------------------

myLayoutHook = mkToggle1 ZOOM $ tall ||| bsp ||| full

  where

    named x = renamed [Replace x]

    defBorder = Border defaultSpacing defaultSpacing defaultSpacing defaultSpacing
    mySpacing = spacingRaw False defBorder True defBorder True

    addTopBar = noFrillsDeco shrinkText topBarTheme

    bsp =
        named "BSP"
            $ avoidStruts
            $ borderResize
            $ addTopBar
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
            $ addTopBar
            $ mySpacing
            $ mkToggle1 MIRROR
            $ mkToggle1 REFLECTX
            $ ResizableTall 1 (1 / 200) (6 / 10) []




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


-- Center window with supplied size
doCenterRect :: (Rational, Rational) -> ManageHook
doCenterRect (w, h) = doRectFloat $ W.RationalRect x y w h
  where
    x, y :: Rational
    x = (1 - w) / 2
    y = (1 - h) / 2

-- Center top of screen
doFlotTopCenter :: ManageHook
doFlotTopCenter =
    doFloatDep $ \(W.RationalRect _ _ w h) -> W.RationalRect ((1 - w) / 2) 0 w h

doFloatVideo :: ManageHook
doFloatVideo = doFloatDep $ \(W.RationalRect _ _ w h) ->
    W.RationalRect (1 - 1 / 16 - w / 3) (1 / 10) (w / 3) (h / 3)



--------------------------------------------------------------------------}}}
--                              Bindings                                  {{{
-----------------------------------------------------------------------------

myModMask = mod4Mask -- Super


showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe $ "yad --text-info --fontname=" <> myMonspaceFont
    hPutStr h (unlines $ showKm x)
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
            _  -> confirmPrompt hotPromptTheme "kill all" killAll


    in

    subKeys "System"
        [ ("M-q"                    , addName "Restart XMonad"              $ spawn "xmonad --restart")
        , ("M-S-q"                  , addName "Quit XMonad"                 $ confirmPrompt hotPromptTheme  "Quit XMonad" $ io exitSuccess)
        , ("M-'"                    , addName "Shortcuts Menu"              shortcutsPrompt)
        ] ^++^


    subKeys "Actions"
        [ ] ^++^

    subKeys "Media"
        [ ("<XF86AudioLowerVolume>" , addName "Lower Volume"               $ spawn "pactl set-sink-mute  @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ -5%")
        , ("<XF86AudioRaiseVolume>" , addName "Raise Volume"               $ spawn "pactl set-sink-mute  @DEFAULT_SINK@ false ; pactl set-sink-volume @DEFAULT_SINK@ +5%")
        , ("<XF86AudioMute>"        , addName "Mute"                       $ spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
        , ("<XF86AudioPlay>"        , addName "Play/Pause"                 $ spawn "playerctl play-pause")
        , ("<XF86AudioNext>"        , addName "Next"                       $ spawn "playerctl next")
        , ("<XF86AudioPrev>"        , addName "Prev"                       $ spawn "playerctl previous")
        ] ^++^

    subKeys "Launcher"
         [ ("M-<Space>"             , addName "Launcher"                   $ spawn $ myLauncher <> " -matching fuzzy -show combi")
         , ("M-C-v"                 , addName "Clipboard"                  $ spawn rofiClip)
         , ("M-<Return>"            , addName "Terminal"                   $ spawn myTerminal)
         , ("M-\\"                  , addName "Browser"                    $ spawn myBrowser)
         , ("<Print>"               , addName "Screenshot"                 $ spawn "flameshot gui")
         , ("M-m"                   , addName "NSP Spotify"                $ namedScratchpadAction scratchpads "Spotify")
         , ("M-v"                   , addName "NSP Mpv"                    $ namedScratchpadAction scratchpads "Mpv")
         , ("M-c"                   , addName "NSP Calculator"             $ namedScratchpadAction scratchpads "Calculator")
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
         [ ("M-w"                   , addName "Switch to Project"          $ switchProjectPrompt myPromptTheme)
         , ("M-S-w s"               , addName "Shift to Project"           $ shiftToProjectPrompt myPromptTheme)
         , ("M-S-w r"               , addName "Rename Project"             $ renameProjectPrompt myPromptTheme)
         , ("M-S-w d"               , addName "Change project directory"   $ changeProjectDirPrompt myPromptTheme)
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
         [ ("M-<Tab>"               , addName "Cycle all layouts"          $ sendMessage NextLayout)
         , ("M-S-<Tab>"             , addName "Reset layout"               $ setLayout $ XMonad.layoutHook conf)
         , ("M-="                   , addName "Increace Window Spacing"    $ incScreenWindowSpacing 5)
         , ("M--"                   , addName "Decreace Window Spacing"    $ decScreenWindowSpacing 5)
         , ("M-0"                   , addName "Toogle Window Spacing"      $ toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
         , ("M-f"                   , addName "Zoom focused window"        zoomFocus)
         , ("M-r"                   , addName "Rotate/Mirror"              $ tryMsgR Rotate $ Toggle MIRROR)
         , ("M-S-r"                 , addName "Reflect"                    $ sendMessage $ Toggle REFLECTX)
         , ("M-y"                   , addName "Toggle float w"             $ withFocused toggleFloat)
         , ("M-S-y"                 , addName "Tile all floating w"        sinkAll)
         ]

-- }}}

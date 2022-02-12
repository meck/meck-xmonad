--  ╭──────────────────────────────────────────────────────────╮
--  │       Implement _NET_DESKTOP_VIEWPORT for use with       │
--  │             "polybar" pin-workspace feature              │
--  │     All current workspaces are mapped to the current     │
--  │                     screen viewport                      │
--  │    while all non current are mapped to the currently     │
--  │                      focused screen                      │
--  │                                                          │
--  │             Needs Log hook and startup hook              │
--  ╰──────────────────────────────────────────────────────────╯
-- Inspired by https://github.com/ndwarshuis/xmonad-config

module Util.Viewport (viewPortsLogHook, viewPortsLogHookCustom, viewPortsStartupHook) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Maybe (catMaybes, maybeToList)
import GHC.Base (join)
import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.WorkspaceCompare (getSortByIndex)

newtype DesktopViewports = DesktopViewports [Int]
  deriving (Eq)

instance ExtensionClass DesktopViewports where
  initialValue = DesktopViewports []

viewPortsLogHook :: X ()
viewPortsLogHook = viewPortsLogHookCustom id

-- like `ewmhDesktopsEventHookCustom`
viewPortsLogHookCustom :: ([WindowSpace] -> [WindowSpace]) -> X ()
viewPortsLogHookCustom f = withWindowSet $ \s -> do
  sort' <- getSortByIndex
  let ws = f $ sort' $ W.workspaces s
      focusedScreen = W.current s
      -- workspace tag / screen lookup
      screenLookup = map (\scr -> (W.tag (W.workspace scr), scr)) (W.screens s)
      screenForWS w = currentPos <$> (lookup (W.tag w) screenLookup <|> Just focusedScreen)
      desktopViewports = concat . catMaybes $ screenForWS <$> ws
  whenChanged (DesktopViewports desktopViewports) $ setDesktopViewports desktopViewports
  where
    currentPos = rectXY . screenRect . W.screenDetail
    rectXY (Rectangle x y _ _) = [fromIntegral x, fromIntegral y]

setDesktopViewports :: [Int] -> X ()
setDesktopViewports vps = withDisplay $ \dpy -> do
  r <- asks theRoot
  a <- getAtom "_NET_DESKTOP_VIEWPORT"
  c <- getAtom "CARDINAL"
  io $ changeProperty32 dpy r a c propModeReplace $ map fromIntegral vps

whenChanged :: (Eq a, ExtensionClass a) => a -> X () -> X ()
whenChanged v action = do
  v0 <- XS.get
  unless (v == v0) $ do
    action
    XS.put v

viewPortsStartupHook :: X ()
viewPortsStartupHook = do
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  rWin <- asks theRoot
  wms <- getAtom "_NET_DESKTOP_VIEWPORT"
  a <- getAtom "ATOM"
  withDisplay $ \dpy -> do
    liftIO $ do
      sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED rWin
      when (fromIntegral wms `notElem` sup) $
        changeProperty32 dpy rWin a_NET_SUPPORTED a propModeAppend [fromIntegral wms]

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

--  ╭──────────────────────────────────────────────────────────╮
--  │      A Layout Transformer that pops out the current      │
--  │               master to a center position                │
--  │                   above other windows.                   │
--  │                            --                            │
--  │ `mffZoomedEventHook` must be used to avoid a focus loop  │
--  │        shadows are added with `shadowZoomLogHook`        │
--  ╰──────────────────────────────────────────────────────────╯

module Layout.Zoom (ZOOM (..), zoomFocus, shadowZoomLogHook, mffZoomedEventHook) where

import Data.List (delete)
import Data.Monoid (All)
import Data.Typeable
import Util.ShadowTag
import XMonad
import XMonad.Actions.TagWindows
import XMonad.Layout.CenteredMaster
import XMonad.Layout.MagicFocus
import XMonad.Layout.MultiToggle
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS

zoomFocus :: X ()
zoomFocus = do
  curWS <- W.tag . W.workspace . W.current . windowset <$> get
  nfWS <- getNFFws <$> XS.get
  XS.put $
    ZoomedWorkspaces $
      if curWS `elem` nfWS
        then delete curWS nfWS
        else curWS : nfWS
  sendMessage $ Toggle ZOOM

-- MultiToggle type for zoomFocus
data ZOOM = ZOOM deriving (Read, Show, Eq, Typeable)

instance Transformer ZOOM Window where
  transform ZOOM x k = k (magicFocus $ centerMaster x) (const x)

-- A extendable state for tracking the workspaces currently zoomed
newtype ZoomedWorkspaces = ZoomedWorkspaces {getNFFws :: [WorkspaceId]}

instance ExtensionClass ZoomedWorkspaces where
  initialValue = ZoomedWorkspaces []

-- And add a tag informing the composer
-- to add a shadow to currently zoomed windows.
shadowZoomLogHook :: X ()
shadowZoomLogHook = do
  winSet <- windowset <$> get
  curZoomed <- getNFFws <$> XS.get
  let focusedWin = W.peek winSet
      allW = W.allWindows winSet
  if W.currentTag winSet `elem` curZoomed
    then
      whenJust
        focusedWin
        ( \f ->
            mapM_
              (\w -> if w == f then addTag zoomTag w else delTag zoomTag w)
              allW
        )
    else mapM_ (delTag zoomTag) allW
  where
    zoomTag = shadowTag <> "_zoom"

-- Disable Mouse follow focus on zoomed window to avoid a focus loop
mffZoomedEventHook :: Event -> X All
mffZoomedEventHook = followOnlyIf $ do
  ws <- gets (W.workspace . W.current . windowset)
  isZoomedLayout <- elem (W.tag ws) . getNFFws <$> XS.get
  pure $ not isZoomedLayout

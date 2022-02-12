module Util.ShadowTag (shadowTag, shadowFloatingLogHook) where

--  ╭──────────────────────────────────────────────────────────╮
--  │ Add a tag to any windows that can be used by a compsitor │
--  │      figure out which windows to render shadows to       │
--  ╰──────────────────────────────────────────────────────────╯

import qualified Data.Map as M
import XMonad
import XMonad.Actions.TagWindows
import qualified XMonad.StackSet as W

-- Windows that has a tag begining
-- with this gets a shadow in the composer
shadowTag :: String
shadowTag = "draw_shadow"

shadowFloatingLogHook :: X ()
shadowFloatingLogHook = do
  stackS <- gets windowset
  let checkTag w =
        if w `M.member` W.floating stackS
          then addTag sTag w
          else delTag sTag w
  mapM_ checkTag $ W.allWindows stackS
  where
    sTag = shadowTag <> "_float"

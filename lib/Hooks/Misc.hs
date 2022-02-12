module Hooks.Misc (smartPlaceHook, doSmartFloat, doCenterRect, doFloatTopCenter, doTopRect, doFloatVideo) where

import Util.Scaling
import XMonad
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import qualified XMonad.StackSet as W

--  ╭──────────────────────────────────────────────────────────╮
--  │                  Misc mangage hooks for                  │
--  │                     placing windows                      │
--  ╰──────────────────────────────────────────────────────────╯

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

-- Float video top right
doFloatVideo :: ManageHook
doFloatVideo = doFloatDep $ \(W.RationalRect _ _ w h) ->
  W.RationalRect (1 - 1 / 16 - w / 3) (1 / 10) (w / 3) (h / 3)

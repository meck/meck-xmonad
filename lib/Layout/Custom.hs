{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Layout.Custom (ScaledSpacing (..), scaledSpacingRaw, SideDecoration (..), CopiedDecoration (..)) where

import Util.Scaling
import XMonad
import XMonad.Actions.Navigation2D (Direction2D (D, L, R, U))
import XMonad.Layout.Decoration (DecorationStyle (decorate, shrink), LayoutModifier (modifierDescription, modifyLayout, pureMess), ModifiedLayout (ModifiedLayout))
import XMonad.Layout.LayoutModifier (pureModifier)
import XMonad.Layout.Spacing (Border, Spacing (Spacing))
import qualified XMonad.StackSet as W
import XMonad.Util.XUtils (fi)

--  ╭──────────────────────────────────────────────────────────╮
--  │     Wraper of `ScaledSpacing` that is scaling aware      │
--  ╰──────────────────────────────────────────────────────────╯

newtype ScaledSpacing a = ScaledSpacing (Spacing a) deriving (Show, Read)

instance Eq a => LayoutModifier ScaledSpacing a where
  modifyLayout (ScaledSpacing (Spacing b sb sbe wb wbe)) wsp lr = do
    sb' <- scaleBorder sb
    wb' <- scaleBorder wb
    modifyLayout (Spacing b sb' sbe wb' wbe) wsp lr

  pureModifier (ScaledSpacing s) lr mst wrs = fmap ScaledSpacing <$> pureModifier s lr mst wrs

  -- TODO Messages that modifies the spacing should be fixed
  pureMess (ScaledSpacing s) m = ScaledSpacing <$> pureMess s m

  modifierDescription ScaledSpacing {} = "ScaledSpacing"

scaledSpacingRaw :: Bool -> Border -> Bool -> Border -> Bool -> l a -> ModifiedLayout ScaledSpacing l a
scaledSpacingRaw b sb sbe wb wbe = ModifiedLayout (ScaledSpacing (Spacing b sb sbe wb wbe))

--  ╭──────────────────────────────────────────────────────────╮
--  │    Decorator of any side of the window, scaling aware    │
--  ╰──────────────────────────────────────────────────────────╯

newtype SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  decorate b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = do
      dw' <- scaleDimension dw
      dh' <- scaleDimension dh
      pure $
        Just $ case b of
          SideDecoration U -> Rectangle x y w dh'
          SideDecoration R -> Rectangle (x + fi (w - dw')) y dw' h
          SideDecoration D -> Rectangle x (y + fi (h - dh')) w dh'
          SideDecoration L -> Rectangle x y dw' h
    | otherwise = pure Nothing

--  ╭──────────────────────────────────────────────────────────╮
--  │   Decorator with a bar at the bottom only for windows    │
--  │   that are duplicated to any other workspace then the    │
--  │                       current one                        │
--  ╰──────────────────────────────────────────────────────────╯
data CopiedDecoration a = CopiedDecoration
  deriving (Show, Read)

instance DecorationStyle CopiedDecoration Window where
  shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x y w (h - dh)
  decorate _ _ dh _ _ _ (win, Rectangle x y w h) = do
    dh' <- scaleDimension dh
    ws <- W.hidden <$> gets windowset
    let hiddenWins = ws >>= (W.integrate' . W.stack)
    pure $
      if win `elem` hiddenWins
        then Just $ Rectangle x (y + fi (h - dh')) w dh'
        else Nothing

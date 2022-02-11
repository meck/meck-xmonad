{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Decoration on any side of a Window
-- taken from:
-- https://github.com/xmonad/xmonad/issues/152

module SideDecoration where

import XMonad
import XMonad.Layout.Decoration
import qualified XMonad.StackSet as W
import XMonad.Util.Types

newtype SideDecoration a = SideDecoration Direction2D
  deriving (Show, Read)

instance Eq a => DecorationStyle SideDecoration a where
  shrink b (Rectangle _ _ dw dh) (Rectangle x y w h)
    | SideDecoration U <- b = Rectangle x (y + fi dh) w (h - dh)
    | SideDecoration R <- b = Rectangle x y (w - dw) h
    | SideDecoration D <- b = Rectangle x y w (h - dh)
    | SideDecoration L <- b = Rectangle (x + fi dw) y (w - dw) h

  pureDecoration b dw dh _ st _ (win, Rectangle x y w h)
    | win `elem` W.integrate st && dw < w && dh < h = Just $ case b of
      SideDecoration U -> Rectangle x y w dh
      SideDecoration R -> Rectangle (x + fi (w - dw)) y dw h
      SideDecoration D -> Rectangle x (y + fi (h - dh)) w dh
      SideDecoration L -> Rectangle x y dw h
    | otherwise = Nothing

-- Decorate windows that are duplicated to any
-- other workspace then the current one
data CopiedDecoration a = CopiedDecoration
  deriving (Show, Read)

instance DecorationStyle CopiedDecoration Window where
  shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x y w (h - dh)
  decorate _ _ dh _ _ _ (win, Rectangle x y w h) = do
    ws <- W.hidden <$> gets windowset
    let hiddenWins = ws >>= (W.integrate' . W.stack)
    pure $
      if win `elem` hiddenWins
        then Just $ Rectangle x (y + fi (h - dh)) w dh
        else Nothing

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Colors.Nord where

import Numeric (showHex)

type RGBA = (Double, Double, Double, Double)

newtype Color = Color Int deriving (Show)

nord0 = Color 0x2e3440

nord1 = Color 0x3b4252

nord2 = Color 0x434c5e

nord3 = Color 0x4c566a

nord4 = Color 0xd8dee9

nord5 = Color 0xe5e9f0

nord6 = Color 0xeceff4

nord7 = Color 0x8fbcbb

nord8 = Color 0x88c0d0

nord9 = Color 0x81a1c1

nord10 = Color 0x5e81ac

nord11 = Color 0xbf616a

nord12 = Color 0xd08770

nord13 = Color 0xebcb8b

nord14 = Color 0xa3be8c

nord15 = Color 0xb48ead

hexCol :: Color -> String
hexCol (Color c) = "#" <> showHex c ""

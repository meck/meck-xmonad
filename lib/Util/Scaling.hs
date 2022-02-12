module Util.Scaling (scaleStartupHook, mkXPrompt', confirmPrompt', switchProjectPrompt', renameProjectPrompt', shiftToProjectPrompt', changeProjectDirPrompt', scaleDimension, scaleBorder, incScreenWindowSpacing', decScreenWindowSpacing') where

--  ╭──────────────────────────────────────────────────────────╮
--  │   Track `GDK_SCALE` and provide wrapers of some other    │
--  │                        functions                         │
--  │                  to respect any scaling                  │
--  │                            --                            │
--  │             `scaleStartupHook` must be used              │
--  ╰──────────────────────────────────────────────────────────╯

import Control.Monad ((<=<))
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import XMonad
import XMonad.Actions.DynamicProjects (changeProjectDirPrompt, renameProjectPrompt, shiftToProjectPrompt, switchProjectPrompt)
import XMonad.Layout.Spacing (Border, borderMap, decScreenWindowSpacing, incScreenWindowSpacing)
import XMonad.Prompt (ComplFunction, XPConfig (height), XPrompt, mkXPrompt)
import XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.PureX (XLike)

-- A extendable state for the current display scaling
newtype DisplayScale = DisplayScale Integer deriving (Read, Show)

instance ExtensionClass DisplayScale where
  initialValue = DisplayScale 1
  extensionType = PersistentExtension

scaleStartupHook :: X ()
scaleStartupHook = do
  mScale <- liftIO $ lookupEnv "GDK_SCALE"
  let mScale' = readMaybe =<< mScale
  case mScale' of
    Nothing -> pure ()
    Just c -> XS.put $ DisplayScale c

-- Utility
scaleDimension :: (XLike m, Num b) => b -> m b
scaleDimension d = do
  DisplayScale sf <- XS.get
  pure $ d * fromInteger sf

scaleBorder :: Border -> X Border
scaleBorder b = do
  DisplayScale sf <- XS.get
  pure $ borderMap (* sf) b

-- Scale aware prompts
mkXPrompt' :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X ()) -> X ()
mkXPrompt' p xpc cf a = do
  xpc' <- scaleXPC xpc
  mkXPrompt p xpc' cf a

confirmPrompt' :: XPConfig -> String -> X () -> X ()
confirmPrompt' c p a = do
  c' <- scaleXPC c
  confirmPrompt c' p a

switchProjectPrompt' :: XPConfig -> X ()
switchProjectPrompt' = switchProjectPrompt <=< scaleXPC

shiftToProjectPrompt' :: XPConfig -> X ()
shiftToProjectPrompt' = shiftToProjectPrompt <=< scaleXPC

renameProjectPrompt' :: XPConfig -> X ()
renameProjectPrompt' = renameProjectPrompt <=< scaleXPC

changeProjectDirPrompt' :: XPConfig -> X ()
changeProjectDirPrompt' = changeProjectDirPrompt <=< scaleXPC

scaleXPC :: XPConfig -> X XPConfig
scaleXPC c = do
  DisplayScale sf <- XS.get
  pure $ c {height = height c * fromInteger sf}

-- Scaling aware spacing
incScreenWindowSpacing' :: Integer -> X ()
incScreenWindowSpacing' = incScreenWindowSpacing <=< scaleDimension

decScreenWindowSpacing' :: Integer -> X ()
decScreenWindowSpacing' = decScreenWindowSpacing <=< scaleDimension

{-# LANGUAGE OverloadedStrings #-}
module Utils.Taffybar
    ( pollingIconWidgetWithDelayNew
    )
where

import           Control.Concurrent
import           Control.Exception             as E
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text                     as T
import           Data.GI.Base.Overloading       ( IsDescendantOf )
import           GI.Gtk
import qualified GI.Gtk                        as Gtk
import qualified GI.Gdk                        as Gdk
import           System.Taffybar.Widget.Util
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           StatusNotifier.Tray            ( scalePixbufToSize )



pollingIconWidgetWithDelayNew
    :: MonadIO m
    => IO (Maybe T.Text, Maybe T.Text)  -- ^ Action that get the icon name (Nothing hides the icon), and a tooltip
    -> Maybe (IO ())                    -- ^ Action for clicking the button
    -> Double                           -- ^ Update interval in s
    -> m Widget
pollingIconWidgetWithDelayNew getData mAction interval = do
    lastIconTxt  <- liftIO $ newMVar (Nothing, Nothing)
    icon         <- Gtk.imageNew
    styleCtx     <- Gtk.widgetGetStyleContext =<< Gtk.toWidget icon
    defaultTheme <- Gtk.iconThemeGetDefault

    let extractPixbuf info =
            fst <$> Gtk.iconInfoLoadSymbolicForContext info styleCtx

        setIconForSize name size =
            Gtk.iconThemeLookupIcon defaultTheme name size themeLoadFlags
                >>= traverse extractPixbuf
                >>= traverse (scalePixbufToSize size Gtk.OrientationHorizontal)

    _ <- Gtk.onWidgetRealize icon $ do
        _ <- forkIO $ forever $ do

            newData@( mNewIcon , mNewTooltip) <- getData
            lastData@(mLastIcon, _          ) <- readMVar lastIconTxt

            let update :: IO ()
                update = if newData /= lastData
                    then do
                        case mNewIcon of
                            Nothing         -> Gtk.widgetHide icon
                            Just newIconTxt -> do
                                Gtk.widgetShow icon
                                if mLastIcon /= mNewIcon
                                    then do
                                        _ <- swapMVar lastIconTxt newData
                                        updateImage <- autoSizeImage
                                            icon
                                            (setIconForSize newIconTxt)
                                            Gtk.OrientationHorizontal
                                        postGUIASync updateImage
                                    else pure ()
                        _ <- swapMVar lastIconTxt newData
                        Gtk.widgetSetTooltipMarkup icon mNewTooltip
                    else pure ()

            E.catch update ignoreIOException
            threadDelay $ floor (interval * 1000000)

        pure ()
    putInBox mAction icon

putInBox
    :: (IsDescendantOf Widget b, MonadIO m, GObject b)
    => Maybe (IO a)
    -> b
    -> m Widget
putInBox Nothing       widget = Gtk.toWidget widget
putInBox (Just action) widget = do
    let clickCallback btn = do
            pressType <- Gdk.getEventButtonType btn
            case pressType of
                Gdk.EventTypeButtonPress -> action >> pure True
                _                        -> pure False
    ebox <- Gtk.eventBoxNew
    Gtk.containerAdd ebox widget
    _ <- Gtk.onWidgetButtonPressEvent ebox clickCallback
    Gtk.widgetShowAll ebox
    Gtk.toWidget ebox

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()

{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Control.Monad (void)
import Data.GI.Base
import Data.IORef
import Data.Text qualified as T
import GI.Gio.Interfaces.File
import GI.Gtk qualified as Gtk

main :: IO ()
main = do
  app <-
    new
      Gtk.Application
      [ On #activate (activate ?self)
      ]
  void $ app.run Nothing

data PrintUnixDialogSetPageSetupMethodInfo
  = ImageSelection
  | ImageSelected
  | StarDetectionInProgress
  | StarDetectionFinished

activate :: Gtk.Application -> IO ()
activate app = do
  -- state <- newIORef ImageSelection
  window <-
    new
      Gtk.ApplicationWindow
      [ #application := app
      , #title := "Astro Stacker"
      , #resizable := True
      ]

  headerBar <- Gtk.headerBarNew

  image <- Gtk.imageNewFromFile "./resources/M82/2018-06-20/M82_edited_v2.jpg"

  image.setVexpand True
  image.setHexpand True

  vbox <- Gtk.boxNew Gtk.OrientationVertical 10

  Gtk.boxAppend vbox headerBar
  Gtk.boxAppend vbox image

  window.setChild (Just vbox)

  window.show

  button <- new Gtk.Button [#label := "Select a File"]

  vbox.append button

  void $ on button #clicked $ do
    dialog <-
      Gtk.fileChooserNativeNew
        (Just "Open File")
        (Just window)
        Gtk.FileChooserActionOpen
        (Just "_Open")
        (Just "_Cancel")
    void $ Gtk.onNativeDialogResponse dialog $ \response -> do
      if response == fromIntegral (fromEnum Gtk.ResponseTypeAccept)
        then do
          mfile <- Gtk.fileChooserGetFile dialog
          case mfile of
            Nothing -> putStrLn "No file selected"
            Just file -> do
              path <- fileGetPath file
              putStrLn $ "Selected file: " ++ maybe "" id path
        else
          putStrLn "Dialog cancelled"

      #destroy dialog

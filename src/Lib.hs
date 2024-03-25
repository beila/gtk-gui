module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

someFunc :: IO ()
someFunc = do
  void initGUI
  window <- renderWindow

  widgetShowAll window
  mainGUI

renderWindow :: IO Window
renderWindow = do
  window <- windowNew
  set window [ windowTitle := "Calculator"
             , windowResizable := False
             , windowDefaultWidth := 230
             , windowDefaultHeight := 250 ]
  pure window

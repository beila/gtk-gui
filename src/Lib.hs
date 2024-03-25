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
  window <- windowNew

  widgetShowAll window
  mainGUI

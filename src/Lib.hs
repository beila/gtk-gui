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
  display <- renderDisplay

  grid <- gridNew
  gridSetRowHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
  attach 0 0 5 1 display
  mkBtn "MC"  >>= attach 0 1 1 1
  mkBtn "MR"  >>= attach 1 1 1 1
  mkBtn "MS"  >>= attach 2 1 1 1
  mkBtn "M+"  >>= attach 3 1 1 1
  mkBtn "M–"  >>= attach 4 1 1 1
  mkBtn "←"   >>= attach 0 2 1 1
  mkBtn "CE"  >>= attach 1 2 1 1
  mkBtn "C"   >>= attach 2 2 1 1
  mkBtn "±"   >>= attach 3 2 1 1
  mkBtn "√"   >>= attach 4 2 1 1
  mkBtn "7"   >>= attach 0 3 1 1
  mkBtn "8"   >>= attach 1 3 1 1
  mkBtn "9"   >>= attach 2 3 1 1
  mkBtn "÷"   >>= attach 3 3 1 1
  mkBtn "%"   >>= attach 4 3 1 1
  mkBtn "4"   >>= attach 0 4 1 1
  mkBtn "5"   >>= attach 1 4 1 1
  mkBtn "6"   >>= attach 2 4 1 1
  mkBtn "*"   >>= attach 3 4 1 1
  mkBtn "1/x" >>= attach 4 4 1 1
  mkBtn "1"   >>= attach 0 5 1 1
  mkBtn "2"   >>= attach 1 5 1 1
  mkBtn "3"   >>= attach 2 5 1 1
  mkBtn "–"   >>= attach 3 5 1 1
  mkBtn "="   >>= attach 4 5 1 2
  mkBtn "0"   >>= attach 0 6 2 1
  mkBtn "."   >>= attach 2 6 1 1
  mkBtn "+"   >>= attach 3 6 1 1
  containerAdd window grid

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

renderDisplay :: IO Entry
renderDisplay = do
  display <- entryNew
  set display [ entryEditable := False
              , entryXalign := 1 -- makes contents right-aligned
              , entryText := "0" ]
  pure display

mkBtn :: String -> IO Button
mkBtn label = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  return btn

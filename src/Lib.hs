module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

import Calc

someFunc :: IO ()
someFunc = do
  st <- newIORef (Value "" Nothing)
  void initGUI
  window <- renderWindow
  display <- renderDisplay

  grid <- gridNew
  gridSetRowHomogeneous grid True
  let attach x y w h item = gridAttach grid item x y w h
      mkBtn = mkButton st display
  attach 0 0 5 1 display
  mkBtn "MC"  id >>= attach 0 1 1 1
  mkBtn "MR"  id >>= attach 1 1 1 1
  mkBtn "MS"  id >>= attach 2 1 1 1
  mkBtn "M+"  id >>= attach 3 1 1 1
  mkBtn "M–"  id >>= attach 4 1 1 1
  mkBtn "←"   id >>= attach 0 2 1 1
  mkBtn "CE"  id >>= attach 1 2 1 1
  mkBtn "C"   id >>= attach 2 2 1 1
  mkBtn "±"   id >>= attach 3 2 1 1
  mkBtn "√"   id >>= attach 4 2 1 1
  mkBtn "7"   id >>= attach 0 3 1 1
  mkBtn "8"   id >>= attach 1 3 1 1
  mkBtn "9"   id >>= attach 2 3 1 1
  mkBtn "÷"   id >>= attach 3 3 1 1
  mkBtn "%"   id >>= attach 4 3 1 1
  mkBtn "4"   id >>= attach 0 4 1 1
  mkBtn "5"   id >>= attach 1 4 1 1
  mkBtn "6"   id >>= attach 2 4 1 1
  mkBtn "*"   id >>= attach 3 4 1 1
  mkBtn "1/x" id >>= attach 4 4 1 1
  mkBtn "1"   id >>= attach 0 5 1 1
  mkBtn "2"   id >>= attach 1 5 1 1
  mkBtn "3"   id >>= attach 2 5 1 1
  mkBtn "–"   id >>= attach 3 5 1 1
  mkBtn "="   id >>= attach 4 5 1 2
  mkBtn "0"   id >>= attach 0 6 2 1
  mkBtn "."   id >>= attach 2 6 1 1
  mkBtn "+"   id >>= attach 3 6 1 1
  containerAdd window grid
  window `on` deleteEvent $ do -- handler to run on window destruction
    liftIO mainQuit
    return False

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

mkButton
  :: IORef Value
  -> Entry
  -> String
  -> (Value -> Value)
  -> IO Button
mkButton st display label mutateState = do
  btn <- buttonNew
  set btn [ buttonLabel := label ]
  btn `on` buttonActivated $ do
    value <- atomicModifyIORef st $ \x -> let r = mutateState x in (r, r)
    updateDisplay display value
  return btn

updateDisplay :: Entry -> Value -> IO ()
updateDisplay display value =
  set display [ entryText := renderValue value ]

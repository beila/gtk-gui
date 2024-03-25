module Calc
    ( Value (..)
    , renderValue
    ) where

data Value = Value String (Maybe Action)

data Action
  = Addition String
  | Subtraction String
  | Multiplication String
  | Division String

mapAction :: (String -> String) -> Action -> Action
mapAction f (Addition x) = Addition (f x)
mapAction f (Subtraction x) = Subtraction (f x)
mapAction f (Multiplication x) = Multiplication (f x)
mapAction f (Division x) = Division (f x)

getSndArg :: Action -> String
getSndArg (Addition x) = x
getSndArg (Subtraction x) = x
getSndArg (Multiplication x) = x
getSndArg (Division x) = x

renderValue :: Value -> String
renderValue (Value x action) =
  g x ++ f a ++ (if null y then "" else g y)
  where
    (a, y) =
      case action of
        Nothing -> ("", "")
        Just (Addition arg) -> ("+", arg)
        Just (Subtraction arg) -> ("-", arg)
        Just (Multiplication arg) -> ("*", arg)
        Just (Division arg) -> ("÷", arg)
    f "" = ""
    f l = " " ++ l ++ " "
    g "" = "0"
    g xs = reverse xs

module Calc
    ( Value (..)
    , Action (..)
    , renderValue
    , enterDot
    , enterDigit
    , backspace
    , operator
    , equals
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

enterDot :: Value -> Value
enterDot (Value x action) =
  let f xs = if '.' `elem` xs then xs else '.' : xs
  in case action of
    Nothing -> Value (f x) Nothing
    Just a -> Value x (Just $ mapAction f a)

enterDigit :: Char -> Value -> Value
enterDigit ch (Value x action) =
  case action of
    Nothing -> Value (ch:x) Nothing
    Just a -> Value x (Just $ mapAction (ch:) a)

backspace :: Value -> Value
backspace (Value x action) =
  case action of
    Nothing -> Value (drop 1 x) Nothing
    Just a -> Value x (Just $ mapAction (drop 1) a)

operator :: (String -> Action) -> Value -> Value
operator op value =
  let (Value x action) = equals value
  in Value x $ Just $
    case action of
      Nothing -> op ""
      Just a -> op (getSndArg a)

equals :: Value -> Value
equals (Value x action) =
  case action of
    Nothing -> Value x Nothing
    Just a ->
      if null (getSndArg a)
        then Value x action
        else Value result Nothing
          where
            g :: String -> Double
            g "" = 0
            g ('.':xs) = g ('0':'.':xs)
            g xs = read (reverse xs)
            x' = g x
            y' = g (getSndArg a)
            result = reverse . show $
              case a of
                Addition _ -> x' + y'
                Subtraction _ -> x' - y'
                Multiplication _ -> x' * y'
                Division _ -> x' / y'

module Sensors where

import           Data.Foldable (find)
import           Data.Text     (Text)

import           NameConf

data Sensor = Sensor {
  _temperature :: Double -- celsius
  , _batteryOK :: Bool
  , _channel   :: Int
  , _sid       :: Int
  } deriving Show

evaluate :: Sensor -> [Guess] -> Maybe Text
evaluate Sensor{..} = fmap nameOf . find (eval . exprOf)

  where
    nameOf (Guess _ n) = n
    exprOf (Guess e _) = e
    eval (EComp Channel o x)     = o == compare (fromIntegral _channel) x
    eval (EComp ID o x)          = o == compare (fromIntegral _sid) x
    eval (EComp Temperature o x) = o == compare _temperature x
    eval (EAnd e f)              = eval e && eval f
    eval (EOr e f)               = eval e || eval f


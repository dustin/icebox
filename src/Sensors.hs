module Sensors where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import           Data.Char             (digitToInt)
import           Data.Foldable         (find)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import           Text.Read             (readMaybe)

import           NameConf

type FragMap = Map Int (Map Text BL.ByteString)

data Sensor = Sensor {
  _temperature :: Double -- celsius
  , _batteryOK :: Bool
  , _channel   :: Int
  , _sid       :: Int
  } deriving (Show, Eq)

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

resolve :: FragMap -> Int -> Maybe Sensor
resolve fm i = Sensor
               <$> convl "temperature_F" fs2c
               <*> convl "battery_ok" (pure . (== "1"))
               <*> convl "channel" (pure . digitToInt . head . BC.unpack . BL.toStrict)
               <*> convl "id" readBS

  where
    convl :: Text -> (BL.ByteString -> Maybe a) -> Maybe a
    convl n f = f =<< Map.lookup n =<< Map.lookup i fm

    readBS :: Read a => BL.ByteString -> Maybe a
    readBS = readMaybe . BC.unpack . BL.toStrict

    fs2c = fmap f2c . readBS

f2c :: Double -> Double
f2c f = (f - 32) * 5 / 9

storeFrag :: Int -> Text -> BL.ByteString -> FragMap -> FragMap
storeFrag i k v = Map.unionWith Map.union (Map.singleton i (Map.singleton k v))

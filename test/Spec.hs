{-# LANGUAGE DeriveGeneric #-}

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Control.Applicative   (liftA3)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Map.Strict       as Map
import qualified Data.Set              as Set
import           Data.Text             (Text, pack, unpack)
import           GHC.Generics          (Generic)
import           Text.Megaparsec       (parse)

import           NameConf
import           Sensors

testParsers :: [TestTree]
testParsers = map (\(t, want) -> testCase (unpack t) $ assertEqual "" want (parse parseGuess "" t)) [
  ("channel = 1 -> one", Right (Guess (EComp Channel EQ 1)
                                "one")),
  ("channel = 1 && temperature < 0 -> coldish",
    Right (Guess (EAnd
                   (EComp Channel EQ 1)
                   (EComp Temperature LT 0))
            "coldish")),
  ("channel = 2 || temperature > -3 -> thing",
   Right (Guess (EOr
                  (EComp Channel EQ 2)
                  (EComp Temperature GT (-3)))
           "thing")),
  ("id = 4 || (channel = 1 && temperature > 0) -> parens", -- just don't understand
    Right (Guess (EOr
                  (EComp ID EQ 4)
                  (EAnd
                    (EComp Channel EQ 1)
                    (EComp Temperature GT 0)))
            "parens"))
  ]

testEvaluations :: [Guess] -> [TestTree]
testEvaluations g = map (\(t, want) -> testCase (show t) $ assertEqual "" want (evaluate t g)) [
  (def, Just "one"),
  (def{_sid=4, _channel=3}, Just "overeleven"),
  (def{_sid=6, _channel=3}, Just "six"),
  (def{_channel=2, _temperature=13}, Just "overeleven"),
  (def{_channel=2, _temperature=(-6)}, Just "coldoutside"),
  (def{_channel=2, _temperature=0}, Just "otherwise")
  ]

  where def = Sensor{_temperature=0, _channel=1, _sid=0, _batteryOK=True}

instance Arbitrary Text where
  arbitrary = elements ["temperature_F", "battery_ok", "channel", "id"]

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary
  shrink = fmap BL.pack . shrinkList shrink . BL.unpack

newtype AnID = AnID Int deriving (Eq, Show, Generic)

instance Arbitrary AnID where
  arbitrary = AnID <$> choose (1,7)
  shrink = genericShrink

newtype SomeFragMap = SomeFragMap FragMap deriving (Generic, Eq, Show)

instance Arbitrary SomeFragMap where
  arbitrary = do
    n <- choose (0, 50)
    SomeFragMap . foldr (\(i,k,v) -> storeFrag i k v) mempty <$> vectorOf n vals

      where
        vals = liftA3 (,,) arbitrary arbitrary arbitrary
  shrink = genericShrink

propFragmentsUpdate :: SomeFragMap -> AnID -> Text -> BL.ByteString -> Property
propFragmentsUpdate (SomeFragMap fm) (AnID i) k v = v' === Just v
  where fm' = storeFrag i k v fm
        v' = Map.lookup k =<< Map.lookup i fm'

fmFrom :: Double -> Bool -> Int -> Int -> FragMap
fmFrom t b c i = Map.singleton i (Map.fromList [
                                     ("temperature_F", bs t),
                                     ("battery_ok", if b then "1" else "0"),
                                     ("channel", bs c),
                                     ("id", bs i)])
  where
    bs :: Show a => a -> BL.ByteString
    bs = BL.fromStrict . BC.pack . show

-- With all necessary keys present, a Sensor should resolve
propSensorResolves :: Double -> Bool -> AnID -> Int -> Property
propSensorResolves t b (AnID c) i =  Just (Sensor (f2c t) b c i) === resolve (fmFrom t b c i) i

-- If any of the necessary keys are missing, resolve should return Nothing
propSensorResolveFails :: NonEmptyList Text -> Double -> Bool -> AnID -> Int -> Property
propSensorResolveFails (NonEmpty rm) t b (AnID c) i = Nothing === resolve fm i
  where fm = fmap (flip Map.withoutKeys (Set.fromList rm)) (fmFrom t b c i)

tests :: [Guess] -> [TestTree]
tests g = [
  testGroup "name parsing" testParsers,
  testGroup "evaluation" (testEvaluations g),
  testProperty "fragment storage" propFragmentsUpdate,
  testProperty "sensor resolves" propSensorResolves,
  testProperty "sensor resolve fails" propSensorResolveFails
  ]

main :: IO ()
main = do
  tc <- parseConfFile "test/test.rules"
  defaultMain $ testGroup "All Tests" (tests tc)

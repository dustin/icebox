import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           Data.Text             (Text, pack, unpack)
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

tests :: [Guess] -> [TestTree]
tests g = [
  testGroup "name parsing" testParsers,
  testGroup "evaluation" (testEvaluations g)
  ]

main :: IO ()
main = do
  tc <- parseConfFile "test/test.rules"
  defaultMain $ testGroup "All Tests" (tests tc)

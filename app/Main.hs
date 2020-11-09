{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


module Main where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Logger       (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader       (ReaderT (..), asks, runReaderT)
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Char                  (digitToInt)
import           Data.Foldable              (find)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Network.MQTT.Client        (MQTTClient, MQTTConfig (..), MessageCallback (..), Property (..),
                                             ProtocolLevel (..), QoS (..), SubOptions (..), Topic, connectURI,
                                             mqttConfig, pubAliased, subOptions, subscribe, waitForClient)
import           Network.URI                (parseURI)
import           Options.Generic
import           Text.Read                  (readMaybe)
import           UnliftIO                   (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO,
                                             withRunInIO)

import           Logging
import           NameConf

data Sensor = Sensor {
  _temperature :: Double -- celsius
  , _batteryOK :: Bool
  , _channel   :: Int
  , _sid       :: Int
  } deriving Show

type FragMap = Map Int (Map Text BL.ByteString)

data Options w = Options {
  mqttURI     :: w ::: Text <?> "URI of mqtt server to talk to"
  , inPrefix  :: w ::: Text <?> "Input prefix (e.g., rtl_433/)"
  , outPrefix :: w ::: Text <?> "Output prefix (e.g., sensors/)"
  , nameFile  :: w ::: FilePath <?> "Path for channel naming rules file"
  } deriving (Generic)

instance ParseRecord (Options Wrapped)
deriving instance Show (Options Unwrapped)

data Env = Env {
  options   :: Options Unwrapped
  , guesses :: [Guess]
  , sensors :: TVar (Map Text Sensor)
  , frags   :: TVar FragMap
  }

type Icebox = ReaderT Env (LoggingT IO)

modifyTVarRet :: TVar a -> (a -> a) -> STM a
modifyTVarRet v f = modifyTVar' v f >> readTVar v

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
    f2c f  = (f - 32) * 5 / 9

name :: Sensor -> Icebox (Maybe Text)
name Sensor{..} = asks (fmap nameOf . find (eval . exprOf) . guesses)

  where
    nameOf (Guess _ n) = n
    exprOf (Guess e _) = e
    eval (EComp Channel o x)     = o == compare (fromIntegral _channel) x
    eval (EComp ID o x)          = o == compare (fromIntegral _sid) x
    eval (EComp Temperature o x) = o == compare _temperature x
    eval (EAnd e f)              = eval e && eval f
    eval (EOr e f)               = eval e || eval f


handleSensor :: MQTTClient -> Text -> Maybe Sensor -> Icebox ()
-- To keep things sensible, we only process temperature updates on complete sensors
handleSensor c "temperature_F" (Just s) = do
  Just n <- name s
  prev <- Map.lookup n <$> (readTVarIO =<< asks sensors)
  logDebugL ["prev: ", tshow prev]
  if match prev s
    then proceed n
    else sus n prev

  where
    match o n = maybe True ((_sid n ==) . _sid) o

    sus n prev = logInfoL ["Sensor was sus: ", n, ", prev: ", tshow prev, " new: ", tshow s]

    proceed n = do
      logDebugL ["Plausible ", n, ": ", tshow s]
      m <- asks sensors
      po <- asks (outPrefix . options)
      liftIO $ do
        pubAliased c (po <> n <> "/temperature") (BCL.pack . show . _temperature $ s) True QoS2 [
          PropMessageExpiryInterval 900]
        pubAliased c (po <> n <> "/battery_ok") (if _batteryOK s then "1" else "0") True QoS2 [
          PropMessageExpiryInterval 900]
      atomically . modifyTVar' m $ Map.insert n s

handleSensor _ _ _ = pure ()

message :: MQTTClient -> Topic -> BL.ByteString -> Icebox ()
message c t m = do
  p <- asks (inPrefix . options)
  Just x <- pure $ T.stripPrefix p t
  let [s,v] = T.splitOn "/" x
      i = (read . T.unpack) s :: Int
  fv <- asks frags
  fm <- atomically . modifyTVarRet fv $ storeFrag i v

  handleSensor c v (resolve fm i)

  where
    storeFrag i v fm = Map.unionWith Map.union fm (Map.singleton i (Map.singleton v m))

run :: Icebox ()
run = do
  p <- asks (inPrefix . options)
  Just uri <- asks (parseURI . T.unpack . mqttURI . options)
  mc <- withRunInIO $ \unl -> connectURI mqttConfig{_msgCB=SimpleCallback $ handle unl,
                                                    _protocol=Protocol50, _cleanSession=False,
                                                    _connProps=[PropSessionExpiryInterval 3600,
                                                                PropTopicAliasMaximum 1024]} uri
  (subrv,_) <- liftIO $ subscribe mc [(p <> "+/+", subOptions { _subQoS = QoS2 })] mempty
  logDebugS subrv
  logInfoS =<< (liftIO $ waitForClient mc)

    where
      handle unl c t m _ = unl $ message c t m

main :: IO ()
main = do
  opts@Options{nameFile} <- unwrapRecord "icebox"
  conf <- parseConfFile nameFile

  sv <- newTVarIO mempty
  fv <- newTVarIO mempty
  runStderrLoggingT . runReaderT run $ Env opts conf sv fv

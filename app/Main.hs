{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}


module Main where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Logger       (LogLevel (..), LoggingT, filterLogger, runStderrLoggingT)
import           Control.Monad.Reader       (ReaderT (..), asks, runReaderT)
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BCL
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T
import           Network.MQTT.Client        (MQTTClient, MQTTConfig (..), MessageCallback (..), Property (..),
                                             ProtocolLevel (..), QoS (..), SubOptions (..), Topic, connectURI,
                                             mqttConfig, pubAliased, subOptions, subscribe, waitForClient)
import           Network.URI                (parseURI)
import           Options.Generic
import           UnliftIO                   (STM, TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO,
                                             withRunInIO)

import           Logging
import           NameConf
import           Sensors

data Options w = Options {
  mqttURI     :: w ::: Text <?> "URI of mqtt server to talk to"
  , inPrefix  :: w ::: Text <?> "Input prefix (e.g., rtl_433/)"
  , outPrefix :: w ::: Text <?> "Output prefix (e.g., sensors/)"
  , nameFile  :: w ::: FilePath <?> "Path for channel naming rules file"
  , verbose   :: w ::: Bool <?> "Enable verbose logging"
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

handleSensor :: MQTTClient -> Text -> Maybe Sensor -> Icebox ()
-- To keep things sensible, we only process temperature updates on complete sensors
handleSensor c "temperature_F" (Just s) = do
  Just n <- asks (evaluate s . guesses)
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
  fm <- atomically . modifyTVarRet fv $ storeFrag i v m

  handleSensor c v (resolve fm i)

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
  opts@Options{nameFile, verbose} <- unwrapRecord "icebox"
  conf <- parseConfFile nameFile

  sv <- newTVarIO mempty
  fv <- newTVarIO mempty
  runStderrLoggingT . logfilt verbose . runReaderT run $ Env opts conf sv fv

  where
    logfilt v = filterLogger (\_ -> flip (if v then (>=) else (>)) LevelDebug)

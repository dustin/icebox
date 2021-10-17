{-# LANGUAGE OverloadedStrings  #-}

module Main where

import           Control.Monad.Logger (LogLevel (..), filterLogger, runStderrLoggingT)
import           Control.Monad.Reader (runReaderT)
import           Options.Generic
import           UnliftIO             (newTVarIO)

import           Icebox
import           NameConf

main :: IO ()
main = do
  opts@Options{nameFile, verbose} <- unwrapRecord "icebox"
  conf <- parseConfFile nameFile

  sv <- newTVarIO mempty
  fv <- newTVarIO mempty
  runStderrLoggingT . logfilt verbose . runReaderT run $ Env opts conf sv fv

  where
    logfilt v = filterLogger (\_ -> flip (if v then (>=) else (>)) LevelDebug)

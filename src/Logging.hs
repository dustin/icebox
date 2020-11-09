module Logging where

import           Data.Foldable        (fold)
import           Data.Text            (Text)
import qualified Data.Text            as T

import           Control.Monad.Logger (MonadLogger (..), logDebugN, logInfoN)

logInfoS :: (Show a, MonadLogger m) => a -> m ()
logInfoS = logInfoN . tshow

logDebugS :: (Show a, MonadLogger m) => a -> m ()
logDebugS = logDebugN . tshow

logInfoL :: (Foldable f, MonadLogger m) => f Text -> m ()
logInfoL = logInfoN . fold

logDebugL :: (Foldable f, MonadLogger m) => f Text -> m ()
logDebugL = logInfoN . fold

tshow :: Show a => a -> Text
tshow = T.pack . show

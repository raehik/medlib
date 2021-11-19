module Thread where

import qualified Medlib.Map.Status              as Status

import           Config


import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.IO.Class

-- TODO use cfg
status :: MonadIO m => CConcurrentLogger -> TQueue Status.Msg -> m ()
status _ q = go Status.statusDef
  where
    go s = case Status.indicatesFinished s of
             True  -> return ()
             False -> do
               msg <- liftIO $ atomically $ readTQueue q
               let s' = Status.processMsg s msg
               liftIO $ print s'
               go s'

delegator
    :: MonadIO m
    => CCmdMakePortable -> TMQueue (FilePath, FilePath) -> TQueue Status.Msg
    -> m ()
delegator cfg qFiles qStatus = go
  where
    go =
        (liftIO $ atomically $ readTMQueue qFiles) >>= \case
          Nothing -> sendMsg Status.MsgFullyTraversed
          Just _  -> go
    sendMsg msg = liftIO $ atomically $ writeTQueue qStatus msg

module Medlib.Map.Thread where

import qualified Medlib.Map.Status              as Status

import           Control.Concurrent.STM
import           Control.Concurrent.STM.TMQueue
import           Control.Monad.IO.Class

status :: MonadIO m => TQueue Status.Msg -> m ()
status q = go Status.statusDef
  where
    go s =
        case Status.indicatesFinished s of
          True  -> return ()
          False -> do
            msg <- liftIO $ atomically $ readTQueue q
            let s' = Status.processMsg s msg
            liftIO $ print s'
            go s'

delegator
    :: MonadIO m
    => CCmdMakePortable -> TMQueue (FilePath, FilePath) -> TQueue Status.Msg -> m ()
delegator cfg qFiles qStatus = go
  where
    go =
        (liftIO $ atomically $ readTMQueue qFiles) >>= \case
          Nothing -> sendMsg Status.MsgFullyTraversed
          Just _  -> return ()
    sendMsg msg = liftIO $ atomically $ writeTQueue msg qStatus

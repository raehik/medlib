module Util where

import           Control.Monad.STM
import           Control.Monad.IO.Class

stm :: MonadIO m => STM a -> m a
stm = liftIO . atomically

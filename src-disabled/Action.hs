{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Medlib.Action where

import           GHC.Generics
import           Optics
import           Data.Generics.Product.Any
import           Data.Generics.Product.Typed
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text          ( Text )

-- | The result of executing an action.
data Result
  = Success
  | Skip
  | Fail
    deriving (Eq, Show)

data CFFmpeg = CFFmpeg
  { cFFmpegBinary  :: String
  , cFFmpegFormat  :: String
  , cFFmpegQuality :: String
  } deriving (Eq, Show, Generic)

--type Trans m = (Text -> Char -> IO ()) -> (FP, FP) -> (FP, FP) -> m (Maybe FP, m Result)
type FilePathD = FilePath
type FP = FilePath
type LibPath = FilePath

cffm = CFFmpeg "bin" "fmt" "qual"
cffmbin = cffm ^. the @"cFFmpegBinary"

{-
ffmpegWithHashCheck
    :: (MonadIO m, HasCFFmpeg env, MonadReader env m)
    => Trans m
ffmpegWithHashCheck log (lr, ldr) (lfd, lfn) = do
    x :: _ <- asks $ cFFmpeg
    liftIO $ putStrLn $ view x
    return Fail
        {-
    ffmpegBin <- gets (view cFFmpegBinary)
    let ffmpegArgs = [ "in"
                     , "progress" ]
    readProcessStdout
    -}
-}

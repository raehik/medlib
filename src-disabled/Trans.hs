{-# LANGUAGE TemplateHaskell #-}

module Medlib.Trans where

import           Control.Lens
import           Control.Lens.TH

-- The result of a library entry transformation.
data Result
  = Success
  | Skip
  | Fail
    deriving (Eq, Show)

type Trans = (Text -> Char -> IO ()) -> IO Result

data CFFmpeg = CFFmpeg
  { _cFFmpegBinary  :: String
  , _cFFmpegFormat  :: String
  , _cFFmpegQuality :: String
  }
makeLenses 'CFFmpeg

data CMap = CMap

type FilePathD = FilePath
type FilePathF = FilePath

data LibPath = LibPath
  { lpDir  :: FilePathD
  , lpFile :: FilePathF
  } deriving (Eq, Show)

ffmpegWithHashCheck
    :: (MonadIO m, HasCFFmpeg env, MonadReader env m)
    => (Text -> Char -> IO ()) -> (FilePathD, FilePathD) -> LibPath -> m Result
ffmpegWithHashCheck (lr, ldr) lp = do
    ffmpegBin <- gets (view cFFmpegBinary)
    let ffmpegArgs = [ "in"
                     , "progress" ]
    readProcessStdout

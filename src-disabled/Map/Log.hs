{-# LANGUAGE OverloadedStrings #-}

module Medlib.Map.Log where

type FilePathF = FilePath
type FilePathD = FilePath
type FileAndDir = (FilePathD, FilePathF)

f :: MonadIO m => -> IO a -> m a

data LibPath = LibPath
  { lpDir  :: FilePathD
  , lpFile :: FilePathF
  } deriving (Eq, Show)

exampleAction
    :: MonadIO m
    => (Text -> Char -> IO ()) -> (FilePathD, FilePathD) -> LibPath -> m ()
exampleAction l = do
    liftIO $ l "checking hash" 'H'

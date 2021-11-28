module Medlib.Job.File where

import qualified Medlib.Util.FileProcess    as FileProcess

import           Control.Monad.IO.Class
import qualified Data.Text                  as Text
import           Data.Text                  ( Text )
import qualified System.Directory           as Dir
import qualified Data.List                      as List

cp :: MonadIO m => FilePath -> FilePath -> m (Maybe String)
cp fSrc fDest = do
    liftIO $ Dir.copyFile fSrc fDest
    return Nothing

compareHashes :: MonadIO m => FilePath -> FilePath -> FilePath -> m (Maybe String)
compareHashes hasher fSrc fDest = do
    let fs = [fSrc, fDest]
    FileProcess.compareFileHashes hasher fs >>= \case
      Left  errStr   -> return $ Just errStr
      Right success  -> return $ if   success
                                 then Nothing
                                 else Just $  "hashes didn't match: "
                                           <> List.intercalate ", " fs

-- TODO ugly reporting
compareSizes :: MonadIO m => FilePath -> [FilePath] -> m (Maybe String)
compareSizes f fs = do
    size  <- FileProcess.getFileSize f
    sizes <- mapM sizeWithName fs
    return $ elemsEqTo size sizes
  where
    sizeWithName f' = do
        size <- FileProcess.getFileSize f'
        return (f', size)
    elemsEqTo sCmp = \case
      []   -> Nothing
      s:ss ->
        if   snd s == sCmp
        then elemsEqTo sCmp ss
        else Just $  "sizes didn't match:"
                  <> " main="<>Text.unpack sCmp<>f
                  <> ", fail="<>Text.unpack (snd s)<>fst s

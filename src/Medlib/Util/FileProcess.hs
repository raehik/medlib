module Medlib.Util.FileProcess
  ( hashFiles
  , hashFile
  , compareFileHashes
  ) where

import           Medlib.Util.Process

import           Data.Function
import           Control.Monad.IO.Class
import qualified Data.Text               as Text
import           Data.Text               ( Text )
import qualified Data.Either.Combinators as Either

type FPF = FilePath

-- | Hash a list of files using the given hash command.
--
-- The hasher must be "well-behaved": we give it a non-zero list of arguments,
-- and parse the output as list of @hash   filename@ entries. Badly behaved
-- commands will be scolded, and cause a runtime error.
--
-- If you call us with an empty list, we return a similarly empty success.
--
-- TODO stderr?
hashFiles :: MonadIO m => FPF -> [FPF] -> m (Either Int [(Text, FPF)])
hashFiles hasher = \case
  [] -> return $ Right []
  fs -> Either.mapRight (map parseHashLine . Text.lines) <$> readProcToText hasher fs
  where
    parseHashLine l = let [hash, fname] = Text.words l in (hash, Text.unpack fname)

-- | Hash a file using the given hash command.
hashFile :: MonadIO m => FPF -> FPF -> m (Either Int Text)
hashFile hasher fp = Either.mapRight (fst . head) <$> hashFiles hasher [fp]

compareFileHashes :: MonadIO m => FPF -> [FPF] -> m (Either String Bool)
compareFileHashes hasher fs =
    hashFiles hasher fs >>= \case
      Left  ec     -> return $ Left $  "error code " <> show ec
                                    <> " while hashing filelist"
      Right hashes -> return $ Right $ listEqHead (map snd hashes)

listEqHead :: Eq a => [a] -> Bool
listEqHead []     = True
listEqHead (x:xs) = all (== x) xs

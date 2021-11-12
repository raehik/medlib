module Medlib.Util.FileProcess ( hashFiles ) where

import           Medlib.Util.Process

import           Data.Function
import           Control.Monad.IO.Class
import qualified Data.Text              as Text
import           Data.Text              ( Text )

type FPF = FilePath

-- | Hash a list of files using the given hash command.
--
-- The hasher must be "well-behaved": we give it a non-zero list of arguments,
-- and parse the output as list of @hash   filename@ entries. Badly behaved
-- commands will be scolded, and cause a runtime error.
--
-- TODO stderr?
hashFiles :: MonadIO m => FPF -> [FPF] -> m (Either Int [(Text, FPF)])
hashFiles hasher = \case
  [] -> return $ Right []
  fs -> do
    readProcToText hasher fs >>= \case
      Left ec -> return $ Left ec
      Right unparsedHashes -> do
        let hashes = unparsedHashes & Text.lines & map parseHashLine
         in return $ Right hashes
  where
    parseHashLine l = let [hash, fname] = Text.words l in (hash, Text.unpack fname)

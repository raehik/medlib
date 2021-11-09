module Medlib.Util.File
  ( deepTraverseDir
  , deepTraverseDir'
  ) where

import           Control.Monad.IO.Class
import           Control.Monad          ( forM_ )
import           System.FilePath        ( (</>)
                                        , joinPath
                                        , pathSeparator
                                        , splitPath )
import           System.Directory       ( listDirectory
                                        , doesDirectoryExist )
import qualified Streaming.Prelude      as S
import           Streaming.Prelude      ( Stream, Of )
import qualified Data.List              as List

type FilePathF = FilePath -- | rel/abs path to file
type FilePathD = FilePath -- | rel/abs path to file
type FileNameD = FilePath -- | dir filename (segment)
type FileNameF = FilePath -- | file filename (segment)

type FP = FilePath

-- | Deep traverse a directory.
--
-- The traversal predicate is given a relative directory 'FilePath', and the
-- filename at that directory.
deepTraverseDir
    :: MonadIO m
    => ((FilePathD, FileNameD) -> Bool) -> FilePathD
    -> Stream (Of (FilePathD, FileNameF)) m ()
deepTraverseDir traverseDirPred root = go ""
  where
    go dir = do
        entries <- liftIO $ listDirectory (root </> dir)
        forM_ entries $ \entry -> do
            let entryRel = dir </> entry
            isDirectory <- liftIO $ doesDirectoryExist (root </> entryRel)
            if   isDirectory
            then if   traverseDirPred (dir, entry)
                 then go entryRel
                 else return ()
            else S.yield (dir, entry)

-- Version where we refuse to join the directories. This would actually be
-- useful if paths were hard (unsafe, slow) to re-split.
deepTraverseDir'
    :: MonadIO m
    => (([FP], FP) -> Bool) -> FilePathD
    -> Stream (Of ([FP], FP)) m ()
deepTraverseDir' traverseDirPred root = go []
  where
    go crumbs = do
        entries <- liftIO $ listDirectory (root </> joinPath crumbs)
        forM_ entries $ \entry -> do
            let entryRel = joinPathSegs crumbs </> entry
            isDirectory <- liftIO $ doesDirectoryExist (root </> entryRel)
            if   isDirectory
            then if   traverseDirPred (crumbs, entry)
                 then go $ entry : crumbs
                 else return ()
            else S.yield (crumbs, entry)

-- Extracted from 'System.FilePath.Internal.combineAlways'. We statically
-- rule the extra guards due to only handling single relative segments.
joinPathSegs :: [FilePath] -> FilePath
joinPathSegs = List.intercalate [pathSeparator] . reverse

-- Version that wraps the silly traverser.
deepTraverseDir''
    :: MonadIO m
    => ((FilePathD, FilePathD) -> Bool) -> FilePathD
    -> Stream (Of (FilePathD, FilePathF)) m ()
deepTraverseDir'' traverseDirPred root =
    S.map joiny $ deepTraverseDir' traverseDirPred' root
  where
    joiny :: ([FP], FP) -> (FilePathD, FilePathF)
    joiny (ds, f) = (reverse (joinPathSegs ds), f)
    traverseDirPred' (ds, d) = traverseDirPred (reverse (joinPathSegs ds), d)

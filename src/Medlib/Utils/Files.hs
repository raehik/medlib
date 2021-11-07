module Medlib.Utils.Files where

import           Control.Monad.IO.Class
import           Control.Monad                  ( forM_ )
import           System.FilePath                ( (</>) )
import           System.Directory               ( listDirectory
                                                , doesDirectoryExist )
import qualified Streaming.Prelude              as S
import           Streaming.Prelude              ( Stream, Of )

type FilePathF = FilePath
type FilePathD = FilePath

deepTraverseDir
    :: MonadIO m
    => ((FilePathD, FilePathD) -> Bool) -> FilePath
    -> Stream (Of (FilePathD, FilePathF)) m ()
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

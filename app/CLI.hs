{-# LANGUAGE RecordWildCards #-}

module CLI ( parseOpts ) where

import           Config
import qualified Medlib.Util.String     as Util
import           Options.Applicative
import           Control.Monad.IO.Class
import qualified Data.Map               as Map
import           Data.Map               ( Map )

parseOpts :: MonadIO m => m Command
parseOpts = execParserWithDefaults desc pCommand
  where
    desc = "Media (primarily audio) library tools."

pCommand :: Parser Command
pCommand = hsubparser $
       cmd "make-portable" descPort (CmdMakePortable <$> pCCmdMakePortable)
  where
    descPort = "Process the library into a \"portable\" version (e.g. transcode)."

pCCmdMakePortable :: Parser CCmdMakePortable
pCCmdMakePortable = CCmdMakePortable <$> pCTraverser
                                     <*> pCScheduler
                                     <*> pCTranscoder
                                     <*> pCHasher
                                     <*> pCConcurrentLogger
                                     <*> pCLibrary
                                     <*> pCLibrary' "dest"

pCLibrary :: Parser CLibrary
pCLibrary = CLibrary <$> option str (long ("library") <> help "library root")

pCLibrary' :: String -> Parser CLibrary
pCLibrary' tag = CLibrary <$> option str (long (tag<>"-library") <> help (tag<>" library root"))

pCTraverser :: Parser CTraverser
pCTraverser = CTraverser <$> many (option str (long "skip-dir" <> help "directory to skip while traversing library"))

pCScheduler :: Parser CScheduler
pCScheduler = CScheduler <$> optional pJobs <*> pure 1
  where pJobs = option auto $  long "jobs"
                            <> help "concurrent CPU-bound jobs to run (defaults to number of threads given to Haskell runtime)"

pCTranscoder :: Parser CTranscoder
pCTranscoder = CTranscoder <$> so (long "hash-tag" <> help "" <> value "MedlibOriginalHashBlake3")
                           <*> so (modExe "ffmpeg")
                           <*> so (modExe "ffprobe")
                           <*> pCTranscoderMapping
  where so = option str

pCTranscoderMapping :: Parser (Map String CTranscoderMapping)
pCTranscoderMapping = Map.fromList <$> many pMapStr
  where
    pMapStr :: Parser (String, CTranscoderMapping)
    pMapStr = option (maybeReader readMapStr)
                $  long "transcode"
                <> help "transcode mapping (format: fromExt:toExt:quality)"
    readMapStr :: String -> Maybe (String, CTranscoderMapping)
    readMapStr s =
        let fields = Util.splitBySep ':' s
         in if   length fields /= 3
            then Nothing
            else let extension = fields !! 1
                     quality   = fields !! 2
                  in Just (fields !! 0, CTranscoderMapping{..})

pCHasher :: Parser CHasher
pCHasher = CHasher <$> option str (modExe' "hasher" "b3sum")

pCConcurrentLogger :: Parser CConcurrentLogger
pCConcurrentLogger = CConcurrentLogger <$> pCConcurrentLogMethod

-- TODO write some common config stuff to allow simple isomorphic print/parse
pCConcurrentLogMethod :: Parser CConcurrentLogMethod
pCConcurrentLogMethod =
    option (maybeReader parseLogMethod) $
        long "log-method"
            <> help "how to log (one of dynamic-compact, dynamic, scrolling)"
            <> value CConcurrentLogMethodDynamicPerJobCompact
            <> showDefaultWith prettyPrintLogMethod
  where
    parseLogMethod = \case
      "dynamic-compact" -> Just CConcurrentLogMethodDynamicPerJobCompact
      "dynamic"         -> Just CConcurrentLogMethodDynamicPerJob
      "scrolling"       -> Just CConcurrentLogMethodScrolling
      _                 -> Nothing
    prettyPrintLogMethod = \case
      CConcurrentLogMethodDynamicPerJobCompact -> "dynamic-compact"
      CConcurrentLogMethodDynamicPerJob        -> "dynamic"
      CConcurrentLogMethodScrolling            -> "scrolling"

--------------------------------------------------------------------------------

-- | Execute a 'Parser' with decent defaults.
execParserWithDefaults :: MonadIO m => String -> Parser a -> m a
execParserWithDefaults desc p = liftIO $ customExecParser
    (prefs $ showHelpOnError)
    (info (helper <*> p) (progDesc desc))

-- | Shorthand for the way I always write commands.
cmd :: String -> String -> Parser a -> Mod CommandFields a
cmd name desc p = command name (info p (progDesc desc))

cmd' :: String -> String -> String -> Parser a -> Mod CommandFields a
cmd' name desc h p = command name (info p (progDesc desc <> header h))

modExe :: (HasName f, HasValue f) => String -> Mod f String
modExe exe = long (exe<>"-exe") <> help ("override "<>exe<>" executable") <> value exe

modExe' :: (HasName f, HasValue f) => String -> String -> Mod f String
modExe' exeType exe = long (exeType<>"-exe") <> help ("set "<>exeType<>" executable") <> value exe <> showDefaultWith id

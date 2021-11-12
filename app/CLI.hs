module CLI ( parseOpts ) where

import           Config
import           Options.Applicative
import           Control.Monad.IO.Class

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
                                     <*> pCFFmpeg
                                     <*> pCHasher
                                     <*> pCConcurrentLogger
                                     <*> pCLibrary
                                     <*> pCLibrary' "dest"

pCLibrary :: Parser CLibrary
pCLibrary = CLibrary <$> option str (long ("library") <> help "library root")

pCLibrary' :: String -> Parser CLibrary
pCLibrary' tag = CLibrary <$> option str (long (tag<>"-library") <> help (tag<>" library root"))

pCTraverser :: Parser CTraverser
pCTraverser = CTraverser <$> many (option str (long "skip-dir"))

pCScheduler :: Parser CScheduler
pCScheduler = CScheduler <$> optional pJobs <*> pure 1
  where pJobs = option auto (long "jobs" <> help "concurrent jobs to run")

pCTranscoder :: Parser CTranscoder
pCTranscoder = CTranscoder <$> pure []

pCFFmpeg :: Parser CFFmpeg
pCFFmpeg = CFFmpeg <$> so (long "hash-tag" <> help "" <> value "MedlibOriginalHashBlake3")
                   <*> so (modExe "ffmpeg")
                   <*> so (modExe "ffprobe")
                   <*> so (long "quality" <> help "FFmpeg -q:a audio quality value")
  where so = option str

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

-- ap = async thread for pool, qp = queue for pool
delegator
    :: MonadIO m
    => (FilePathD, FilePathD) -> TVar (Map FilePathD Bool)
    -> (LogMsg -> IO ())
    -> Stream (Of (FilePathD, FilePathD)) m ()
    -> m ()
delegator roots tvDestDirs logAction s = do
    (qpCPU, apCPU) <- createJobQueue ResourceBoundCPU
    (qpIO,  apIO)  <- createJobQueue ResourceBoundIO
    S.mapM_ (delegate qpCPU qpIO) s
    liftIO $ logAction LogMsgFullyTraversed
    liftIO $ atomically $ closeTMQueue qpCPU
    liftIO $ atomically $ closeTMQueue qpIO
    liftIO $ wait apIO
    liftIO $ wait apCPU
  where
    rbs = const 1
    createJobQueue rb = do
        q <- liftIO newTMQueueIO
        a <- liftIO $ async $ poolScheduler (logActionPoolScheduler rb) (rbs rb) worker q
        return (q, a)
    worker = actionRunner roots tvDestDirs
    logActionPoolScheduler rb ca = logAction $ LogMsgUpdateCounter rb (PoolCounterProcessed ca)
    delegate qpCPU qpIO f = do
        let (action, rb) = processor roots f
            q = case rb of
                  ResourceBoundCPU -> qpCPU
                  ResourceBoundIO  -> qpIO
        -- TODO: The order of these two lines is critical -- cool!
        liftIO $ logAction $ LogMsgUpdateCounter rb PoolCounterAssigned
        liftIO $ atomically $ writeTMQueue q action

poolScheduler
    :: MonadIO m
    => (CounterAction -> IO ())
    -> Int -> (itm -> IO ()) -> TMQueue itm -> m ()
poolScheduler logAction poolSizeMax action q = go Set.empty
  where
    go pool = do
        liftIO (atomically (readTMQueue q)) >>= \case
          Just itm -> do
            pool' <- do
                if   Set.size pool >= poolSizeMax
                then waitAnySet pool
                else return pool
            liftIO $ logAction CounterActionIncrement
            a <- liftIO $ async $ action itm
            go $ Set.insert a pool'
          Nothing -> do
            waitAll pool
            liftIO $ logAction CounterActionClose
            return ()
    waitAll pool
     | Set.null pool = return ()
     | otherwise = waitAnySet pool >>= waitAll
    -- | waitAny and remove the winner from the set
    waitAnySet s = do
        (a', _) <- liftIO $ waitAny $ Set.toList s
        return $ Set.delete a' s

-- | Purely decide what to do with a library file using its filename.
--
-- This is a point for improvement later -- we could make this IO instead.
-- Perhaps how I'd implement the FFmpeg metadata checks.
processor
    :: (FilePathD, FilePathD) -> (FilePathD, FilePathF)
    -> ((Action, Maybe FilePathD), ResourceBound)
processor (libRoot, libDestRoot) (fpD, fpF)
 | takeExtension fpF == ".flac" =
    let action =
            ActionShell "ffmpeg"
              [ "-n"
              , "-i", libRoot </> fp
              , "-vn"
              , "-q:a", "5"
              , libDestRoot </> (fpD </> replaceExtension fpF ".ogg")
              ]
     in ((action, Just fpD), ResourceBoundCPU)
 | otherwise =
    let action = ActionIO $ copyFile (libRoot </> fp) (libDestRoot </> fp)
     in ((action, Just fpD), ResourceBoundIO)
  where
    fp = fpD </> fpF

-- | Thread that should run for any action. Will create directories as required,
--   using a map shared across all action workers.
actionRunner
    :: (FilePathD, FilePathD) -> TVar (Map FilePathD Bool)
    -> (Action, Maybe FilePathD) -> IO ()
actionRunner (_, libDestRoot) tvDestDirs (action, mDir) = mMakeDir >> runAction
  where
    runAction =
        case action of
          ActionShell cmd args -> do
            runProcess (nullStdStreams (proc cmd args)) >>= \case
              ExitSuccess   -> return ()
              ExitFailure c -> putStrLn $ "medlib: error: shell command failed with code " <> show c <> ": " <> cmd <> List.intercalate " " args
          ActionIO a -> a
    nullStdStreams = setStdin nullStream . setStdout nullStream . setStderr nullStream
    mMakeDir =
        case mDir of
          Nothing  -> return ()
          Just dir -> do
            makeDir <- atomically $ do
                destDirs <- readTVar tvDestDirs
                case Map.lookup dir destDirs of
                  Nothing    -> do -- not present
                    let destDirs' = Map.insert dir False destDirs
                    writeTVar tvDestDirs destDirs'
                    return True
                  Just False -> retry -- being created
                  Just True  -> return False
            case makeDir of
              False -> return ()
              True  -> do
                createDirectoryIfMissing True (libDestRoot </> dir)
                atomically $ do
                    destDirs <- readTVar tvDestDirs
                    let destDirs' = Map.insert dir True destDirs
                    writeTVar tvDestDirs destDirs'

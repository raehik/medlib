module Main where

import qualified CLI as CLI
import           Config

import qualified Medlib.Action as Action
import qualified Medlib.Util.FileProcess as FileProcess

aFFmpegCfg :: CFFmpeg -> CHasher -> Action.FFmpegCfg
aFFmpegCfg cFFmpeg cHasher = Action.FFmpegCfg
  { Action.ffmpegCfgHashTagName = cFFmpegHashTag cFFmpeg
  , Action.ffmpegCfgHashFile    = hashFile (cHasherExe cHasher)
  , Action.ffmpegCfgFFprobe     = cFFmpegFFprobe cFFmpeg
  , Action.ffmpegCfgFFmpeg      = cFFmpegFFmpeg  cFFmpeg
  , Action.ffmpegCfgQuality     = cFFmpegQuality cFFmpeg
  } where hashFile hasher fp = fmap (fst . head) <$> FileProcess.hashFiles hasher [fp]

main :: IO ()
main = CLI.parseOpts >>= print

module CLI2 where
{-

import           Config
import           Options.Applicative
import           Control.Monad.IO.Class

cffmpegConvert :: CFFmpeg -> CHasher -> FFmpegCfg
cffmpegConvert cf ch = FFmpegCfg
  { ffmpegCfgHashTagName = cFFmpegHashTag cf
  , ffmpegCfgHashFile    = hashFile (cHasherExe ch)
  , ffmpegCfgFFprobe     = cFFmpegFFprobe cf
  , ffmpegCfgFFmpeg      = cFFmpegFFmpeg  cf
  , ffmpegCfgQuality     = cFFmpegQuality cf
  }

-}

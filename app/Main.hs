module Main (main) where

import qualified Medlib.Map as MLM

main :: IO ()
main = MLM.libMap MLM.libDirPredNoEtc ("tmp", "tmp-cp")

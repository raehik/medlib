module Medlib.Util.String where

-- | how is this not in prelude
splitBySep :: Eq a => a -> [a] -> [[a]]
splitBySep sep = go
  where go [] = []
        go l = h : splitBySep sep (drop 1 t) where (h, t) = span (/= sep) l

{-
fitFilepathTo :: Int -> [FilePath] -> FilePath
fitFilepathTo _ []     = ""
fitFilepathTo n (f:ds) =
    let dirPath  = List.intercalate '/' (reverse ds)
        filePath = fitFile 4 (n - length dirPath) f
    in dirPath <> '/' : filePath

-- Keeps the extension as minimum suffix.
fitFile :: Int -> Int -> String -> String
fitFile prefixLen aimLen s = 
  where
    prefix = 
-}

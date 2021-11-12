{-# LANGUAGE OverloadedStrings #-}

module Testing where

import           Data.Reflection
import           Data.Text          ( Text )
import           Data.Proxy

data Data = Data
  { dataOne :: Text
  , dataTwo :: Int
  } deriving (Eq, Show)

dataEx :: Data
dataEx = Data
  { dataOne = "hiya"
  , dataTwo = 0
  }

useData1 :: Given Data => Text
useData1 = dataOne given

useData2 :: Text
useData2 = "nope sry"

useData3 :: Data -> Text
useData3 = dataOne

useData4 :: Reifies s Data => Proxy s -> Text
useData4 p = dataOne $ reflect p

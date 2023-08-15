module Lib
  ( someFunc,
  )
where

import Data.Text.IO qualified as TI
import Pre qualified
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  TI.putStrLn $ Pre.prepare content

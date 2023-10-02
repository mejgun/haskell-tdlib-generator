module Lib
  ( someFunc,
  )
where

import Data.Text.IO qualified as TI
import Parser (parseClass, parseMethod)
import Pre qualified
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  case Pre.parse content of
    Left e -> error $ show e
    Right (class_, methods, funcs) -> do
      mapM_ print class_
      mapM_ print methods
      mapM_ print funcs

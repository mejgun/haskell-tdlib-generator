module Lib
  ( someFunc,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TI
import Parser (allParser)
import Pre qualified
import System.Environment (getArgs)
import Text.Megaparsec (parse, parseTest)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  let (dat, fun) = Pre.prepare content
  TI.putStrLn dat
  TI.putStrLn fun
  parseTest allParser $ T.unpack dat

  parseTest allParser $ T.unpack fun

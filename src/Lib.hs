module Lib
  ( someFunc,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TI
import Parser (allParser)
import Pre qualified
import System.Environment (getArgs)
import Text.Megaparsec (parse)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  let (dat, fun) = Pre.prepare content
  TI.putStrLn dat
  TI.putStrLn fun
  let x = parse allParser fname (T.unpack dat)
  case x of
    Left e -> error $ show e
    Right res -> mapM_ print res

-- parseTest allParser $ T.unpack fun

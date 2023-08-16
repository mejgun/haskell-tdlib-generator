{-# LANGUAGE LambdaCase #-}

module Lib
  ( someFunc,
  )
where

import Data.Text qualified as T
import Data.Text.IO qualified as TI
import Parser (allParser)
import Post (check)
import Pre qualified
import System.Environment (getArgs)
import Text.Megaparsec (parse)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  let (dat, fun) = Pre.prepare content
      pars = parse allParser fname . T.unpack
      func = \case
        Left e -> error $ show e
        Right res -> check res
  -- TI.putStrLn dat
  -- TI.putStrLn fun
  let (x, _) = (pars dat, pars fun)
  func x

-- func y

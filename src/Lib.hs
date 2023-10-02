module Lib
  ( someFunc,
  )
where

import Data.Text.IO qualified as TI
import Parser (parseClass, parseMethod)
import Post ()
import Pre qualified
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
  [fname] <- getArgs
  content <- TI.readFile fname
  let (dat, fun) = Pre.prepare content
      f (cs, ms) x =
        if length x == 1
          then (cs ++ [parseClass x], ms)
          else (cs, ms ++ [parseMethod x])
      (class_, methods) = foldl f ([], []) dat
      funcs = map parseMethod fun
  print $ sequence class_
  print $ sequence methods
  print $ sequence funcs

-- pars = parse allParser fname . T.unpack
--     func = \case
--       Left e -> error $ show e
--       Right res -> mapM_ print res -- >> check res
--       -- TI.putStrLn dat
--       -- TI.putStrLn fun
-- let (x, y) = (pars dat, pars fun)
-- func x
-- func y

-- func y

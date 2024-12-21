{-# LANGUAGE LambdaCase #-}

module Lib
  ( someFunc,
  )
where

import Data.Text.IO qualified as TI
import Parser (Class, Method, parse)
import Pre qualified
import Save (writeData, writeFuncs)
import System.Environment (getArgs)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  case args of
    ["parse", fname] -> load fname >>= justPrint
    ["write", fname, path] -> load fname >>= saveFiles path
    _ -> error "unknown params"
  where
    load fname =
      TI.readFile fname
        >>= ( \case
                Left e -> error $ show e
                Right a -> pure a
            )
          . parse
          . Pre.prepare

justPrint :: ([Class], [Method], [Method]) -> IO ()
justPrint (class_, methods, funcs) = do
  putStrLn "--- Classes:"
  mapM_ print class_
  putStrLn "--- Methods:"
  mapM_ print methods
  putStrLn "--- Functions:"
  mapM_ print funcs
  putStrLn "---"

saveFiles :: FilePath -> ([Class], [Method], [Method]) -> IO ()
saveFiles path (c, m, f) = writeFuncs path f >>= writeData path c m

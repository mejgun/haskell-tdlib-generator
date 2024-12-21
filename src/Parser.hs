module Parser
  ( Method (..),
    Class (..),
    ClassName (..),
    Arg (..),
    ArgVal (..),
    Data,
    parseClass,
    parseMethod,
    parse,
  )
where

import Data.Text qualified as T
import Parser.Internal
  ( Arg (..),
    ArgVal (..),
    Class (..),
    ClassName (..),
    Data,
    Method (..),
    parseClass,
    parseMethod,
  )

parse :: ([[T.Text]], [[T.Text]]) -> Either String ([Class], [Method], [Method])
parse (dat, fun) = do
  c <- sequence class_
  m <- sequence methods
  f <- sequence funcs
  Right (c, m, f)
  where
    fn (cs, ms) x =
      if length x == 1
        then (cs ++ [parseClass x], ms)
        else (cs, ms ++ [parseMethod x])
    (class_, methods) = foldl fn ([], []) dat
    funcs = map parseMethod fun

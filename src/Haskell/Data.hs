module Haskell.Data (generateData) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (Argument (..), DataClass (..), Func (..), quoted)
import Parser (Class, Method)

type Result = Writer [T.Text] ()

moduleName :: DataClass -> Result
moduleName x = tell ["module TD.Query." <> x.name <> " where"]

generateData :: DataClass -> T.Text
generateData c = T.unlines . execWriter $ do
  moduleName c
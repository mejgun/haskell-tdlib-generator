module Haskell.Data (generateData) where

import Data.Text qualified as T
import Parser (Class, Method)

generateData :: Class -> [Method] -> T.Text
generateData _ _ = "undefined"
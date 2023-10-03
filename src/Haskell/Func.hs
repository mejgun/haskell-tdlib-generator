module Haskell.Func (generateFunc) where

import Data.Text qualified as T
import Parser (Method)

generateFunc :: Method -> T.Text
generateFunc _ = "undefined"
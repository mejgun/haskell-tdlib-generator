module Haskell.Internal (upFst) where

import Data.Text qualified as T

upFst :: T.Text -> T.Text
upFst text =
  let h = T.toUpper $ T.take 1 text
      t = T.tail text
   in h <> t
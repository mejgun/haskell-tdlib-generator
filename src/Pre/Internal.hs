module Pre.Internal (commentSplit) where

import Data.Char (isAsciiLower)
import Data.Text qualified as T

commentSplit :: T.Text -> T.Text
commentSplit = T.strip . T.unlines . map go . T.lines
  where
    go x
      | T.isPrefixOf "//@class " x = x
      | T.isPrefixOf "//@" x = T.unwords . map func $ T.words x
      | otherwise = x
    func x
      | T.isPrefixOf "@" x
          && T.length x > 2
          && isAsciiLower (T.index x 1) =
          "\n//" <> x
      | otherwise = x

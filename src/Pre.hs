module Pre (prepare) where

import Data.Char (isAsciiLower)
import Data.Text qualified as T

junk :: [T.Text]
junk =
  [ "double ? = Double;",
    "string ? = String;",
    "int32 = Int32;",
    "int53 = Int53;",
    "int64 = Int64;",
    "bytes = Bytes;",
    "boolFalse = Bool;",
    "boolTrue = Bool;",
    "vector {t:Type} # [ t ] = Vector t;",
    ""
  ]

removeJunk :: T.Text -> T.Text
removeJunk = T.unlines . map T.strip . filter (`notElem` junk) . T.lines

removeMultiComment :: T.Text -> T.Text
removeMultiComment = T.replace "\n//-" " "

commentSplit :: T.Text -> T.Text
commentSplit = T.unlines . map go . T.lines
  where
    go x =
      if T.isPrefixOf "//@class " x
        then x
        else T.unwords . map func $ T.words x
    func x =
      if T.isPrefixOf "@" x
        && T.length x > 2
        && isAsciiLower (T.index x 1)
        then "\n//" <> x
        else x

prepare :: T.Text -> T.Text
prepare =
  commentSplit
    . removeMultiComment
    . removeJunk
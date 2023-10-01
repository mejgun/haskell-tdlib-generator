module Pre (prepare) where

import Data.Text qualified as T
import Pre.Internal

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

splitFuncs :: T.Text -> (T.Text, T.Text)
splitFuncs t = case T.splitOn "---functions---\n" t of
  [dat, fun] -> (dat, fun)
  _ -> error ""

trim :: T.Text -> T.Text
trim = T.unlines . filter (not . T.null) . map T.strip . T.lines

prepare :: T.Text -> (T.Text, T.Text)
prepare =
  splitFuncs
    . trim
    . commentSplit
    . removeMultiComment
    . removeJunk
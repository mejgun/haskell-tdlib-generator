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
    "vector {t:Type} # [ t ] = Vector t;"
  ]

removeJunk :: T.Text -> T.Text
removeJunk = T.unlines . map T.strip . filter (`notElem` junk) . T.lines

removeMultiComment :: T.Text -> T.Text
removeMultiComment = T.replace "\n//-" " "

splitFuncs :: T.Text -> ([[T.Text]], [[T.Text]])
splitFuncs t = case T.splitOn "---functions---\n" t of
  [dat, fun] -> (split dat, split fun)
  _ -> error "no functions separator"

trim :: T.Text -> T.Text
trim = T.unlines . map T.strip . T.lines

addNewLines :: T.Text -> T.Text
addNewLines = T.replace "//@description" "\n\n//@description"

prepare :: T.Text -> ([[T.Text]], [[T.Text]])
prepare =
  splitFuncs
    . trim
    . commentSplit
    . removeMultiComment
    . addNewLines
    . removeJunk

split :: T.Text -> [[T.Text]]
split x = result
  where
    (acc, res) = foldl func ([], []) $ T.lines x
    result = if null acc then res else res ++ [acc]

    func :: ([T.Text], [[T.Text]]) -> T.Text -> ([T.Text], [[T.Text]])
    func a@([], _) "" = a
    func (a, r) "" = ([], r ++ [a])
    func (a, r) s = (a ++ [s], r)

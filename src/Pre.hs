module Pre (parse) where

import Data.Text qualified as T
import Parser (Class, Method, parseClass, parseMethod)
import Pre.Internal (commentSplit)

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
addNewLines = T.replace "//@description " "\n\n//@@description "

fixDescriptionParam :: T.Text -> T.Text
fixDescriptionParam = T.replace "@param_description" "@description"

prepare :: T.Text -> ([[T.Text]], [[T.Text]])
prepare =
  splitFuncs
    . trim
    . commentSplit
    . removeMultiComment
    . fixDescriptionParam
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

parse :: T.Text -> Either String ([Class], [Method], [Method])
parse content = do
  c <- sequence class_
  m <- sequence methods
  f <- sequence funcs
  Right (c, m, f)
  where
    (dat, fun) = prepare content
    fn (cs, ms) x =
      if length x == 1
        then (cs ++ [parseClass x], ms)
        else (cs, ms ++ [parseMethod x])
    (class_, methods) = foldl fn ([], []) dat
    funcs = map parseMethod fun
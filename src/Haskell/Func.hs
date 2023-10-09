module Haskell.Func (generateFunc) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (Argument (..), Func (..), quoted)

type Result = Writer [T.Text] ()

moduleName :: Func -> Result
moduleName x = tell ["module TD.Query." <> x.nameInCode <> " where"]

dataSection :: Func -> Result
dataSection x = do
  tell
    [ "-- | " <> x.comment,
      "data " <> x.nameInCode,
      indent 1 <> "= " <> x.nameInCode
    ]
  printNotEmpty
    (2, "{", ",", "}")
    ( map
        (\a -> (a.nameInCode, ":: " <> a.typeInCode, ("--| " <>) <$> a.comment))
        x.args
    )
  tell [indent 1 <> "deriving (Eq)"]

-- (indentantion, openinig prefix, middle, closing)
-- (fist val, second, comment)
printNotEmpty :: (Int, T.Text, T.Text, T.Text) -> [(T.Text, T.Text, Maybe T.Text)] -> Result
printNotEmpty _ [] = pure ()
printNotEmpty (ind, begin, loop, end) list =
  let (len1, len2) = foldr (\(a, b, _) (m1, m2) -> (max (T.length a) m1, max (T.length b) m2)) (1, 1) list
      h = head list
      t = tail list
      save pre (a, b, c) =
        let p1 = T.justifyLeft len1 ' ' a
            p2 = case c of
              (Just text) -> T.justifyLeft len2 ' ' b <> " " <> text
              Nothing -> b
         in tell
              [indent ind <> pre <> " " <> p1 <> " " <> p2]
   in do
        save begin h
        mapM_ (save loop) t
        tell [indent ind <> end]

indent :: Int -> T.Text
indent i = T.replicate i "  "

showSection :: Func -> Result
showSection x = do
  tell
    [ "instance Show " <> x.nameInCode <> " where",
      indent 1 <> "show",
      indent 2 <> x.nameInCode
    ]
  printRecordInstance x
  tell [indent 4 <> "= " <> quoted x.nameInCode]
  unless (null x.args) $ tell [indent 5 <> "++ U.cc"]
  printNotEmpty
    (5, "[", ",", "]")
    (map (\a -> (quoted a.nameInCode, "`U.p` " <> a.nameTemp, Nothing)) x.args)

printRecordInstance :: Func -> Result
printRecordInstance x =
  printNotEmpty
    (3, "{", ",", "}")
    (map (\a -> (a.nameInCode, "= " <> a.nameTemp, Nothing)) x.args)

toJsonSection :: Func -> Result
toJsonSection x = do
  tell
    [ "instance T.ToJSON " <> x.nameInCode <> " where",
      indent 1 <> "toJSON",
      indent 2 <> x.nameInCode
    ]
  printRecordInstance x
  tell [indent 4 <> "= A.object"]
  printNotEmpty
    (5, "[", ",", "]")
    ( (quoted "@type", ".= T.String " <> quoted x.nameReal, Nothing)
        : map (\a -> (quoted a.nameReal, ".= " <> a.toJsonFunc <> a.nameTemp, Nothing)) x.args
    )

importsSection :: Func -> Result
importsSection x = do
  mapM_ (\v -> tell ["import " <> v]) x.importsRaw
  mapM_ (\(k, v) -> tell ["import qualified " <> k <> " as " <> v]) x.importsQualified

generateFunc :: Func -> T.Text
generateFunc m = T.unlines . execWriter $ do
  moduleName m
  space
  importsSection m
  space
  dataSection m
  space
  showSection m
  space
  toJsonSection m
  where
    space = tell [""]
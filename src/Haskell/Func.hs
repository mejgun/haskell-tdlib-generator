module Haskell.Func (generateFunc) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (Argument (..), Func (..), Result, indent, printNotEmpty, quoted)

moduleName :: Func -> Result
moduleName x = tell ["module TD.Query." <> x.nameInCode <> " where"]

dataSection :: Func -> Result
dataSection x = do
  tell
    [ "data " <> x.nameInCode <> " -- ^ " <> x.comment,
      indent 1 <> "= " <> x.nameInCode
    ]
  printNotEmpty
    (2, "{", ",", "}")
    ( map
        (\a -> (a.nameInCode, ":: " <> a.typeInCode, ("-- ^ " <>) <$> a.comment))
        x.args
    )
  tell [indent 1 <> "deriving (Eq)"]

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
module Haskell.Func (generateFunc) where

import Control.Monad (unless, when)
import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (Argument (..), Func (..), Result, indent, printNotEmpty, quoted)

moduleName :: Func -> Result
moduleName x = do
  tell
    [ "module TD.Query." <> x.nameInCode,
      indent 1 <> "(" <> x.nameInCode <> "(..)"
    ]
  when (length x.args > 1) $
    tell [indent 1 <> ", default" <> x.nameInCode]
  tell [indent 1 <> ") where"]

dataSection :: Func -> Result
dataSection x = do
  tell
    [ "-- | " <> x.comment,
      "data " <> x.nameInCode,
      indent 1 <> "= " <> x.nameInCode
    ]
  printNotEmpty
    (2, "{", ",", Just "}")
    ( map
        (\a -> (a.nameInCode, ":: " <> a.typeInCode, ("-- ^ " <>) <$> a.comment))
        x.args
    )
  tell [indent 1 <> "deriving (Eq, Show)"]

showSection :: Func -> Result
showSection x = do
  tell
    [ "instance I.ShortShow " <> x.nameInCode <> " where",
      indent 1 <> "shortShow",
      indent 2 <> x.nameInCode
    ]
  printRecordInstance x
  tell [indent 4 <> "= " <> quoted x.nameInCode]
  unless (null x.args) $ tell [indent 5 <> "++ I.cc"]
  printNotEmpty
    (5, "[", ",", Just "]")
    (map (\a -> (quoted a.nameInCode, "`I.p` " <> a.nameTemp, Nothing)) x.args)

printRecordInstance :: Func -> Result
printRecordInstance x =
  printNotEmpty
    (3, "{", ",", Just "}")
    (map (\a -> (a.nameInCode, "= " <> a.nameTemp, Nothing)) x.args)

toJsonSection :: Func -> Result
toJsonSection x = do
  tell
    [ "instance AT.ToJSON " <> x.nameInCode <> " where",
      indent 1 <> "toJSON",
      indent 2 <> x.nameInCode
    ]
  printRecordInstance x
  tell [indent 4 <> "= A.object"]
  printNotEmpty
    (5, "[", ",", Just "]")
    ( (quoted "@type", "A..= AT.String " <> quoted x.nameReal, Nothing)
        : map
          ( \a ->
              ( quoted a.nameReal,
                "A..= "
                  <> ( case a.toJsonFunc of
                         Nothing -> ""
                         (Just func) -> func <> " "
                     )
                  <> a.nameTemp,
                Nothing
              )
          )
          x.args
    )

importsSection :: Func -> Result
importsSection x = do
  mapM_ (\(k, v) -> tell ["import qualified " <> k <> " as " <> v]) x.imports

printDefault :: Func -> Result
printDefault m
  | length m.args < 2 = pure ()
  | otherwise = do
      tell
        [ "default" <> m.nameInCode <> " :: " <> m.nameInCode,
          "default" <> m.nameInCode <> " =",
          indent 1 <> m.nameInCode
        ]
      printNotEmpty
        (2, "{", ",", Just "}")
        (map (\a -> (a.nameInCode, "= Nothing", Nothing)) m.args)
      tell [""]

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
  space
  printDefault m
  where
    space = tell [""]
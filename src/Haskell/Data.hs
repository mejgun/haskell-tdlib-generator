module Haskell.Data (generateData) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal
  ( Argument (..),
    DataClass (..),
    DataMethod (..),
    indent,
    printNotEmpty,
    quoted,
  )

type Result = Writer [T.Text] ()

moduleName :: DataClass -> Result
moduleName x = tell ["module TD.Data." <> x.name <> " where"]

importsSection :: DataClass -> Result
importsSection x = do
  mapM_ (\v -> tell ["import " <> v]) x.importsRaw
  mapM_ (\(k, v) -> tell ["import qualified " <> k <> " as " <> v]) x.importsQualified

dataSection :: DataClass -> Result
dataSection x = do
  let cmt = case x.comment of
        (Just t) -> " -- ^ " <> t
        Nothing -> ""
  tell ["data " <> x.name <> cmt]
  let h = head x.methods
      t = tail x.methods
  printMethod "=" h
  mapM_ (printMethod "|") t
  tell [indent 1 <> "deriving (Eq)"]
  where
    printMethod pre m = do
      tell [indent 1 <> pre <> " " <> m.nameInCode <> " -- ^ " <> m.comment]
      printNotEmpty
        (2, "{", ",", "}")
        ( map
            (\a -> (a.nameInCode, ":: " <> a.typeInCode, ("-- ^ " <>) <$> a.comment))
            m.args
        )

showSection :: DataClass -> Result
showSection x = do
  tell ["instance Show " <> x.name <> " where"]
  mapM_ printShow x.methods
  where
    printShow m = do
      tell
        [ indent 1 <> "show",
          indent 2 <> m.nameInCode
        ]
      printRecordInstance m
      tell [indent 4 <> "= " <> quoted m.nameInCode]
      unless (null m.args) $ tell [indent 5 <> "++ U.cc"]
      printNotEmpty
        (5, "[", ",", "]")
        (map (\a -> (quoted a.nameInCode, "`U.p` " <> a.nameTemp, Nothing)) m.args)
      tell [""]

printRecordInstance :: DataMethod -> Result
printRecordInstance x =
  printNotEmpty
    (3, "{", ",", "}")
    (map (\a -> (a.nameInCode, "= " <> a.nameTemp, Nothing)) x.args)

generateData :: DataClass -> T.Text
generateData c = T.unlines . execWriter $ do
  moduleName c
  space
  importsSection c
  space
  dataSection c
  space
  showSection c
  where
    space = tell [""]
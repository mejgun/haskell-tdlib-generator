module Haskell.Data (generateData, generateBoot) where

import Control.Monad.Writer
import Data.Foldable (find)
import Data.HashMap.Strict qualified as HM
import Data.List (nub, (\\))
import Data.Text qualified as T
import Debug.Trace qualified
import Haskell.Internal
  ( Argument (..),
    DataClass (..),
    DataMethod (..),
    indent,
    printNotEmpty,
    quoted,
  )
import Parser (ClassName (ClassName))

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

generateBoot :: [(ClassName, [ClassName])] -> [(ClassName, T.Text)]
generateBoot xs = do
  map (\(x, _) -> (x, "undefined")) $
    filter snd $
      map (\(y, ys) -> (y, go y [] ys)) xs
  where
    get :: ClassName -> [ClassName]
    get y = concatMap snd $ filter ((y ==) . fst) xs

    go :: ClassName -> [ClassName] -> [ClassName] -> Bool
    go _ _ [] = False
    go x acc ys =
      let newacc = nub $ acc ++ ys
          newys = nub $ concatMap get $ ys \\ acc
       in (x `elem` newacc) || go x newacc newys

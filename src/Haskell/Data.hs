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
  mapM_ (\(k, v) -> tell ["import qualified " <> k <> " as " <> v]) x.imports

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
        (2, "{", ",", Just "}")
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
        [ indent 1 <> "show " <> m.nameInCode
        ]
      printRecordInstance 2 m
      tell [indent 3 <> "= " <> quoted m.nameInCode]
      unless (null m.args) $ tell [indent 4 <> "++ I.cc"]
      printNotEmpty
        (4, "[", ",", Just "]")
        (map (\a -> (quoted a.nameInCode, "`I.p` " <> a.nameTemp, Nothing)) m.args)

fromJsonSection :: DataClass -> Result
fromJsonSection x = do
  tell
    [ "instance AT.FromJSON " <> x.name <> " where",
      indent 1 <> "parseJSON v@(AT.Object obj) = do",
      indent 2 <> "t <- obj A..: \"@type\" :: AT.Parser String",
      "",
      indent 2 <> "case t of"
    ]
  printNotEmpty
    (2, " ", " ", Just "")
    (map (\m -> (quoted m.nameReal, " -> parse" <> m.nameInCode <> " v", Nothing)) x.methods)
  tell [indent 2 <> "where"]
  mapM_ printParseFuncs x.methods
  where
    printParseFuncs m = do
      tell
        [ indent 3 <> "parse" <> m.nameInCode <> " :: A.Value -> AT.Parser " <> x.name,
          indent 3 <> "parse" <> m.nameInCode <> " = A.withObject " <> quoted m.nameInCode <> " $ \\o -> do"
        ]
      printNotEmpty
        (3, " ", " ", Nothing)
        ( map
            ( \a ->
                (a.nameTemp, "<- " <> a.fromsonFunc <> "o A..:? ", Just (quoted a.nameReal))
            )
            m.args
        )
      tell [indent 4 <> "pure $ " <> m.nameInCode]
      printRecordInstance 5 m

printRecordInstance :: Int -> DataMethod -> Result
printRecordInstance ind x =
  printNotEmpty
    (ind, "{", ",", Just "}")
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
  space
  fromJsonSection c
  where
    space = tell [""]

generateBoot :: [(ClassName, [ClassName])] -> [(ClassName, T.Text)]
generateBoot xs = do
  map (\(x, _) -> (x, text x)) $
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
    text (ClassName n) =
      T.unlines
        [ "module TD.Data." <> n <> " where",
          "",
          "import Data.Aeson.Types ( FromJSON, ToJSON )",
          "",
          "data " <> n,
          "",
          "instance Eq " <> n,
          "",
          "instance Show " <> n,
          "",
          "instance FromJSON " <> n
        ]

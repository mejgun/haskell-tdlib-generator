module Haskell.Data (generateData, generateBoot, generateGeneralResult) where

import Control.Monad (unless, when)
import Control.Monad.Writer
import Data.List (nub, (\\))
import Data.Text qualified as T
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

moduleName :: DataClass -> Bool -> Result
moduleName x b = do
  tell ["module TD.Data." <> x.name]
  if b
    then
      printNotEmpty
        (1, "(", ",", Just ") where")
        $ (x.name <> "(..)", "", Nothing)
          : ( map
                (\m -> ("default" <> m.nameInCode, "", Nothing))
                . filter (\m -> not (null m.args))
            )
            x.methods
    else tell [indent 1 <> "(" <> x.name <> "(..)) where"]

importsSection :: [[T.Text]] -> DataClass -> Result
importsSection boots x = do
  mapM_
    ( \(k, v) ->
        let b =
              if any (needSource v) boots
                then "{-# SOURCE #-} "
                else ""
         in tell ["import " <> b <> "qualified " <> k <> " as " <> v]
    )
    x.imports
  where
    needSource v bs = x.name `elem` bs && v `elem` bs

dataSection :: DataClass -> Result
dataSection DataClass {methods = []} = error "(dataSection) cannot be. no methods"
dataSection x@DataClass {methods = (h : t)} = do
  case x.comment of
    (Just c) -> tell ["-- | " <> c]
    Nothing -> pure ()
  tell ["data " <> x.name]
  printMethod "=" h
  mapM_ (printMethod "|") t
  tell [indent 1 <> "deriving (Eq, Show)"]
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
  tell ["instance I.ShortShow " <> x.name <> " where"]
  mapM_ printShow x.methods
  where
    printShow m = do
      tell
        [ indent 1 <> "shortShow " <> m.nameInCode
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
      indent 1 <> "parseJSON " <> needV <> "(AT.Object obj) = do",
      indent 2 <> "t <- obj A..: \"@type\" :: AT.Parser String",
      "",
      indent 2 <> "case t of"
    ]
  printNotEmpty
    (2, " ", " ", Just "")
    ( map
        ( \m ->
            ( quoted m.nameReal,
              if null m.args
                then "-> pure " <> m.nameInCode
                else "-> parse" <> m.nameInCode <> " v",
              Nothing
            )
        )
        x.methods
        ++ [("_", "-> mempty", Nothing)]
    )
  when (any (\m -> not (null m.args)) x.methods) $ tell [indent 2 <> "where"]
  mapM_ printParseFuncs x.methods
  tell [indent 1 <> "parseJSON _ = mempty"]
  where
    needV =
      if all (\m -> null m.args) x.methods
        then ""
        else "v@"

    printParseFuncs m
      | null m.args = pure ()
      | otherwise = do
          tell [indent 3 <> "parse" <> m.nameInCode <> " :: A.Value -> AT.Parser " <> x.name]
          tell [indent 3 <> "parse" <> m.nameInCode <> " = A.withObject " <> quoted m.nameInCode <> " $ \\o -> do"]
          printNotEmpty
            (3, " ", " ", Nothing)
            ( map
                ( \a ->
                    ( a.nameTemp,
                      "<- "
                        <> ( case a.fromJsonFunc of
                               Nothing -> ""
                               (Just func) -> func <> " <$> "
                           )
                        <> "o A..:? ",
                      Just (quoted a.nameReal)
                    )
                )
                m.args
            )
          tell [indent 4 <> "pure $ " <> m.nameInCode]
          printRecordInstance 5 m

toJsonSection :: DataClass -> Result
toJsonSection x = do
  tell ["instance AT.ToJSON " <> x.name <> " where"]
  mapM_ printToFuncs x.methods
  where
    printToFuncs m = do
      tell [indent 1 <> "toJSON " <> m.nameInCode]
      printRecordInstance 2 m
      tell [indent 3 <> "= A.object"]
      printNotEmpty
        (4, "[", ",", Just "]")
        ( (quoted "@type", "A..= AT.String " <> quoted m.nameReal, Nothing)
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
              m.args
        )

printRecordInstance :: Int -> DataMethod -> Result
printRecordInstance ind x =
  printNotEmpty
    (ind, "{", ",", Just "}")
    (map (\a -> (a.nameInCode, "= " <> a.nameTemp, Nothing)) x.args)

printDefaults :: DataClass -> Bool -> Result
printDefaults x b =
  when b $ mapM_ go x.methods
  where
    go :: DataMethod -> Result
    go m
      | null m.args = pure ()
      | otherwise = do
          tell
            [ "default" <> m.nameInCode <> " :: " <> x.name,
              "default" <> m.nameInCode <> " =",
              indent 1 <> m.nameInCode
            ]
          printNotEmpty
            (2, "{", ",", Just "}")
            (map (\a -> (a.nameInCode, "= Nothing", Nothing)) m.args)
          tell [""]

generateData :: [[ClassName]] -> [ClassName] -> DataClass -> T.Text
generateData boots requests c = T.unlines . execWriter $ do
  moduleName c (tojson && adddefault)
  space
  importsSection (map (map (\(ClassName n) -> n)) boots) c
  space
  dataSection c
  space
  showSection c
  space
  fromJsonSection c
  space
  when tojson $
    do
      toJsonSection c
      space
      printDefaults c adddefault
  where
    tojson = ClassName c.name `elem` requests

    adddefault = length c.methods == 1

    space = tell [""]

generateBoot :: [(ClassName, [ClassName])] -> [ClassName] -> [[(ClassName, T.Text)]]
generateBoot xs requests = do
  map (map (\y -> (y, text y))) $
    filter (not . null) $
      map (\(y, ys) -> concat (go y [] ys)) xs
  where
    get :: ClassName -> [ClassName]
    get y = concatMap snd $ filter ((y ==) . fst) xs

    go :: ClassName -> [ClassName] -> [ClassName] -> [[ClassName]]
    go _ _ [] = []
    go x acc ys = do
      if x `elem` ys
        then [x : acc]
        else
          concatMap
            ( \y ->
                let newacc = nub $ y : acc
                    newys = nub $ get y \\ newacc
                 in go x newacc newys
            )
            (ys \\ acc)
    text c@(ClassName n) =
      T.unlines
        [ "module TD.Data." <> n <> " (" <> n <> ") where",
          "",
          if c `elem` requests
            then "import Data.Aeson.Types (FromJSON, ToJSON)"
            else "import Data.Aeson.Types (FromJSON)",
          "import TD.Lib.Internal (ShortShow)",
          "",
          "data " <> n,
          "",
          "instance Eq " <> n,
          "",
          "instance Show " <> n,
          "",
          "instance ShortShow " <> n,
          "",
          "instance FromJSON " <> n,
          "",
          if c `elem` requests
            then "instance ToJSON " <> n
            else ""
        ]

generateGeneralResult :: [ClassName] -> T.Text
generateGeneralResult xs = T.unlines . execWriter $ do
  tell
    [ "module TD.GeneralResult (GeneralResult(..)) where",
      "",
      "import Control.Applicative (Alternative ((<|>)))",
      "import Data.Aeson (FromJSON (parseJSON))",
      "import qualified Data.Aeson.Types as T",
      "import qualified TD.Lib.Internal as I"
    ]
  mapM_
    ( \(ClassName x) ->
        tell ["import qualified TD.Data." <> x <> " as " <> x]
    )
    xs
  tell ["", "data GeneralResult"]
  printNotEmpty
    (2, "=", "|", Nothing)
    (map (\(ClassName x) -> (x, x <> "." <> x, Nothing)) xs)
  tell
    [ " deriving (Eq, Show)",
      "",
      "instance I.ShortShow GeneralResult where"
    ]
  mapM_
    ( \(ClassName x) ->
        tell
          [ indent 1 <> "shortShow (" <> x <> " v)",
            indent 2 <> "= " <> quoted x <> " <> \" (\" <> I.shortShow v <> \")\""
          ]
    )
    xs
  tell
    [ "",
      "instance T.FromJSON GeneralResult where",
      " parseJSON v ="
    ]
  printNotEmpty
    (2, "    (", "<|> (", Nothing)
    (map (\(ClassName x) -> (x, " <$> parseJSON v", Just ")")) xs)

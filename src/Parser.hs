module Parser
  ( Method (..),
    Class (..),
    Arg (..),
    ArgVal (..),
    Data,
    parseArgVal,
    parseClass,
  )
where

import Data.Text qualified as T

data Method = Method
  { name :: String,
    comment :: String,
    args :: [Arg],
    result :: Class
  }
  deriving (Show, Eq)

type Data = Method

data Class = Class
  { name :: T.Text,
    comment :: T.Text
  }
  deriving (Show, Eq)

data Arg = Arg
  { name :: String,
    value :: ArgVal,
    comment :: Maybe String,
    null :: Bool
  }
  deriving (Show, Eq)

data ArgVal
  = TInt32
  | TInt53
  | TInt64
  | TBool
  | TString
  | TBytes
  | TModule T.Text
  | TVector ArgVal
  deriving (Show, Eq)

parseArgVal :: T.Text -> ArgVal
parseArgVal "int32" = TInt32
parseArgVal "int53" = TInt53
parseArgVal "int64" = TInt64
parseArgVal "Bool" = TBool
parseArgVal "string" = TString
parseArgVal "bytes" = TBytes
parseArgVal x
  | T.isPrefixOf "vector<" x && T.isSuffixOf ">" x =
      TVector . parseArgVal . T.dropEnd 1 $ T.drop 7 x
parseArgVal x = TModule x

parseClass :: [T.Text] -> Either String Class
parseClass [x] = case T.words x of
  "//@class" : name : "@description" : descr -> Right $ Class name (T.unwords descr)
  _ -> Left $ err x
parseClass x = Left $ err x

err :: (Show a) => a -> String
err t = "not class description " <> show t

{-
classParser :: Parser Entry
classParser = do
  void $ string "//@class "
  name <- var
  void $ string " @description "
  comment <- some printChar
  void newline
  pure $ C $ Class name comment

methodParser :: Parser Entry
methodParser = do
  void $ string "//@description "
  comment <- some printChar
  void newline
  list <- many argcomment
  name <- var
  args <- map (attachcomment list) <$> many argParser
  void $ string " = "
  res <- var
  void $ string ";"
  void newline
  pure $ M $ Method name comment args res
  where
    argcomment = do
      void $ string "//@"
      name <- var
      hspace1
      comment <- some printChar
      void newline
      pure (name, comment)

    argParser :: Parser Arg
    argParser = try $ do
      hspace1
      name <- var
      void $ char ':'
      value <- varVal
      pure $ Arg name value Nothing False

    attachcomment :: [(String, String)] -> Arg -> Arg
    attachcomment xs a = do
      case filter ((== a.name) . fst) xs of
        [(_, x)] ->
          a
            { comment = Just x,
              null = contains "may be null" (T.pack x)
            }
        [] -> a
        _ -> error "should not happen"

    contains :: T.Text -> T.Text -> Bool
    contains needle haystack = length (T.splitOn needle haystack) > 1
-}
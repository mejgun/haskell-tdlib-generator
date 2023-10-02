module Parser
  ( Method (..),
    Class (..),
    Arg (..),
    ArgVal (..),
    Data,
    parseClass,
    parseMethod,
  )
where

import Data.Text qualified as T

data Method = Method
  { name :: T.Text,
    comment :: T.Text,
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
  { name :: T.Text,
    value :: ArgVal,
    comment :: Maybe T.Text,
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

parseClass :: [T.Text] -> Either String Class
parseClass xs = f1 >>= f2
  where
    f1 = case xs of
      [x] -> Right x
      _ -> Left $ "class description must be single line. " <> show xs
    f2 x = case T.words x of
      "//@class" : name : "@description" : descr -> Right $ Class name (T.unwords descr)
      _ -> Left $ "not class description " <> show x

parseMethod :: [T.Text] -> Either String Method
parseMethod xs = do
  (name, arglist, result) <- parseMethodLine $ last xs
  comment <- parseMethodComment $ head xs
  argcomments <- parseArgComments . init $ tail xs
  args <- makeArgList arglist argcomments
  Right $ Method name comment args (Class result "")

parseMethodLine :: T.Text -> Either String (T.Text, [(T.Text, ArgVal)], T.Text)
parseMethodLine = undefined

parseMethodComment :: T.Text -> Either String T.Text
parseMethodComment = undefined

parseArgComments :: [T.Text] -> Either String [(T.Text, T.Text, Bool)]
parseArgComments = undefined

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

makeArgList :: [(T.Text, ArgVal)] -> [(T.Text, T.Text, Bool)] -> Either String [Arg]
makeArgList = undefined

{-

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
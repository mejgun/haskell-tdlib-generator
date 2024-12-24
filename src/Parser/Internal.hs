module Parser.Internal
  ( parseClass,
    parseMethod,
    Class (..),
    Method (..),
    Data,
    ClassName (..),
    Arg (..),
    ArgVal (..),
  )
where

import Data.Text qualified as T

data Method = Method
  { name :: T.Text,
    comment :: T.Text,
    args :: [Arg],
    result :: ClassName
  }
  deriving (Show, Eq)

type Data = Method

data Class = Class
  { name :: ClassName,
    comment :: T.Text
  }
  deriving (Show, Eq)

newtype ClassName = ClassName T.Text deriving (Show, Eq, Ord)

data Arg = Arg
  { name :: T.Text,
    value :: ArgVal,
    comment :: Maybe T.Text
  }
  deriving (Show, Eq)

data ArgVal
  = TInt32
  | TInt53
  | TInt64
  | TBool
  | TDouble
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
      "//@class" : name : "@description" : descr -> Right $ Class (ClassName name) (T.unwords descr)
      _ -> Left $ "not class description " <> show x

parseMethod :: [T.Text] -> Either String Method
parseMethod [] = Left "empty method"
parseMethod [_] = Left "wrong method format"
parseMethod (x : xs) = do
  (name, arglist, result) <- parseMethodLine $ last xs
  comment <- parseMethodComment x
  argcomments <- parseArgComments . init $ xs
  args <- makeArgList arglist argcomments
  Right $ Method name comment args (ClassName result)

-- e.g. userFullInfo personal_photo:chatPhoto bot_info:botInfo = UserFullInfo;
parseMethodLine :: T.Text -> Either String (T.Text, [(T.Text, ArgVal)], T.Text)
parseMethodLine x
  | not $ T.isSuffixOf ";" x = Left $ "method line must end with ; " <> T.unpack x
  | last (init (T.words x)) /= "=" = Left $ "method line must contain '=': " <> T.unpack x
  | otherwise = case T.words x of
      [] -> Left "empty method line"
      [_] -> Left $ "wrong method line: " <> T.unpack x
      (name : xs) ->
        do
          (args, class_) <- go xs []
          Right (name, args, class_)
  where
    go [] _ = Left $ "cannot parse method line: " <> T.unpack x
    go ["=", class_] args = Right (args, T.dropEnd 1 class_)
    go (y : ys) args = do
      a <- parseArg y
      go ys (args <> [a])

parseMethodComment :: T.Text -> Either String T.Text
parseMethodComment x = case T.words x of
  "//@@description" : t -> Right $ T.unwords t
  _ -> Left $ "bad method description " <> T.unpack x

parseArgComments :: [T.Text] -> Either String [(T.Text, T.Text)]
parseArgComments = foldr go (Right [])
  where
    go _ l@(Left _) = l
    go s (Right xs)
      | T.isPrefixOf "//@" s =
          let (name, descr) = T.breakOn " " (T.drop 3 s)
           in Right $ (name, T.strip descr) : xs
      | otherwise = Left $ "bad arg comment " <> show s

parseArg :: T.Text -> Either String (T.Text, ArgVal)
parseArg x = case T.splitOn ":" x of
  [name, val] -> Right (name, parseArgVal val)
  _ -> Left $ "bad argument " <> T.unpack x

parseArgVal :: T.Text -> ArgVal
parseArgVal "int32" = TInt32
parseArgVal "int53" = TInt53
parseArgVal "int64" = TInt64
parseArgVal "double" = TDouble
parseArgVal "Bool" = TBool
parseArgVal "string" = TString
parseArgVal "bytes" = TBytes
parseArgVal x
  | T.isPrefixOf "vector<" x && T.isSuffixOf ">" x =
      TVector . parseArgVal . T.dropEnd 1 $ T.drop 7 x
parseArgVal x = TModule x

makeArgList :: [(T.Text, ArgVal)] -> [(T.Text, T.Text)] -> Either String [Arg]
makeArgList args comments = mapM go args
  where
    go (name, val) = case filter (\(n, _) -> n == name) comments of
      [] -> Right $ Arg name val Nothing
      [(_, descr)] -> Right $ Arg name val (Just descr)
      _ -> Left $ "multiple comment to " <> T.unpack name

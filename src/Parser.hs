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
parseMethodLine x
  | not $ T.isSuffixOf ";" x = Left $ "method line must end with ; " <> T.unpack x
  | last (init (T.words x)) == "=" = Left $ "method line must contain =  " <> T.unpack x
  | otherwise =
      let xs = T.words $ T.dropEnd 1 x
          name = head xs
          class_ = last xs
          args = map parseArg . init . init $ tail xs
       in sequence args >>= \a -> Right (name, a, class_)

parseMethodComment :: T.Text -> Either String T.Text
parseMethodComment x = case T.words x of
  "//@description " : t -> Right $ T.unwords t
  _ -> Left $ "bad method description " <> T.unpack x

parseArgComments :: [T.Text] -> Either String [(T.Text, T.Text, Bool)]
parseArgComments = foldr go (Right [])
  where
    go _ l@(Left _) = l
    go s (Right xs)
      | T.isPrefixOf "//@" s =
          let (name, descr) = T.breakOn " " (T.drop 3 s)
           in Right $ (name, descr, T.isInfixOf "may be null" descr) : xs
      | otherwise = Left $ "bad arg comment " <> show s

parseArg :: T.Text -> Either String (T.Text, ArgVal)
parseArg x = case T.splitOn ":" x of
  [name, val] -> Right (name, parseArgVal val)
  _ -> Left $ "bad argument " <> T.unpack x

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
makeArgList xs ys = mapM go xs
  where
    go (name, val) = case filter (\(n, _, _) -> n == name) ys of
      [] -> Right $ Arg name val Nothing False
      [(_, descr, mbnull)] -> Right $ Arg name val (Just descr) mbnull
      _ -> Left $ "multiple comment to " <> T.unpack name

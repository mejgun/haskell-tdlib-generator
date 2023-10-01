module Parser
  ( Entry (..),
    Method (..),
    Class (..),
    Arg (..),
    ArgVal (..),
    allParser,
  )
where

import Control.Monad (void)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, try),
    Parsec,
    many,
    some,
    (<|>),
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    hspace1,
    letterChar,
    newline,
    printChar,
    string,
  )

data Entry
  = M Method
  | C Class
  deriving (Show, Eq)

data Method = Method
  { name :: String,
    comment :: String,
    args :: [Arg],
    result :: String
  }
  deriving (Show, Eq)

data Class = Class
  { name :: String,
    comment :: String
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
  | TModule String
  | TVector ArgVal
  deriving (Show, Eq)

type Parser = Parsec Void String

allParser :: Parser [Entry]
allParser = some (classParser <|> methodParser) <* eof

var :: Parser String
var = do
  h <- letterChar
  t <- many (alphaNumChar <|> char '_')
  pure (h : t)

varVal :: Parser ArgVal
varVal =
  TInt32 <$ string "int32"
    <|> TInt53 <$ string "int53"
    <|> TInt64 <$ string "int64"
    <|> TBool <$ string "Bool"
    <|> TString <$ string "string"
    <|> TBytes <$ string "bytes"
    <|> string "vector<"
      *> (TVector <$> varVal)
      <* char '>'
    <|> TModule <$> some letterChar

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

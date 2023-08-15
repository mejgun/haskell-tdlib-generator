module Parser
  ( Method (..),
    Arg (..),
    Class (..),
    classParser,
    allParser,
    methodParser,
  )
where

import Control.Monad (guard, void)
import Data.Char (isPrint)
import Data.Void (Void)
import Debug.Trace qualified as Trace
import Text.Megaparsec
import Text.Megaparsec.Char

data Method = Method
  { name :: String,
    comment :: String,
    args :: [Arg],
    result :: String
  }
  deriving (Show, Eq)

data Arg = Arg
  { name :: String,
    value :: String,
    comment :: String,
    null :: Bool
  }
  deriving (Show, Eq)

data Class = Class
  { name :: String,
    comment :: String
  }
  deriving (Show, Eq)

type Parser = Parsec Void String

allParser :: Parser [Class]
allParser = some classParser

s :: Parser ()
s = hspace

sp1 :: Parser ()
sp1 = hspace1

descr :: Parser ()
descr = void $ string "@description"

var :: Parser String
var = do
  h <- letterChar
  t <- some alphaNumChar
  pure (h : t)

getComment :: Parser String
getComment = do
  comment <- takeWhile1P (Just "comment") (\x -> isPrint x && x /= '@')
  list <- many rest <|> pure []
  pure (unwords (comment : list))
  where
    rest :: Parser String
    rest = try $ do
      void newline
      void $ string "//-"
      some printChar

classParser :: Parser Class
classParser = do
  void $ string "//@class "
  name <- var
  sp1
  descr
  sp1
  comment <- getComment
  void $ many newline
  pure $ Class name comment

methodParser :: Parser Method
methodParser = do
  void $ string "//@description "
  comment <- getComment
  Trace.traceM $ " >>> " <> comment <> " <<< "
  list <- many argcomment
  Trace.traceM $ " >>> " <> comment
  void newline
  name <- var
  args <- many argParser
  res <- var
  pure $ Method name comment args res
  where
    argcomment = try $ do
      (newline >> void (string "//")) <|> void sp1
      void $ char '@'
      name <- var
      sp1
      comment <- getComment
      pure (name, comment)

argParser :: Parser Arg
argParser = try $ do
  name <- var
  void $ char ':'
  value <- var
  pure $ Arg name value "" False

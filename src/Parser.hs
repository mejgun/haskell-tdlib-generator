module Parser
  ( Method (..),
    Arg (..),
    Class (..),
    classParser,
    allParser,
  )
where

import Control.Monad (MonadPlus, void)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Method = Method
  { name :: T.Text,
    comment :: T.Text,
    args :: [Arg],
    result :: T.Text
  }

data Arg = Arg
  { name :: T.Text,
    value :: T.Text,
    comment :: T.Text
  }

data Class = Class
  { name :: T.Text,
    comment :: T.Text
  }
  deriving (Show, Eq)

type Parser = Parsec Void T.Text

allParser :: Parser [Class]
allParser = some classParser

s :: Parser ()
s = hspace

sp1 :: Parser ()
sp1 = hspace1

cmt :: Parser ()
cmt = void $ string "//"

descr :: Parser ()
descr = void $ string "@description"

sometext :: (MonadPlus m) => m Char -> m T.Text
sometext x = T.pack <$> some x

classParser :: Parser Class
classParser = do
  cmt
  void $ string "@class"
  sp1
  name <- sometext letterChar
  sp1
  descr
  sp1
  comment <- sometext printChar
  list <- many m <|> pure []
  void $ many newline
  pure $ Class name (T.intercalate " " (comment : list))
  where
    m = try $ do
      void newline
      void $ string "//-"
      T.pack <$> some printChar

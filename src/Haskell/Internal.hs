module Haskell.Internal (upFst, argValToHaskellVal) where

import Data.Text qualified as T
import Parser (Arg (..), ArgVal (..))

upFst :: T.Text -> T.Text
upFst text =
  let h = T.toUpper $ T.take 1 text
      t = T.tail text
   in h <> t

argValToHaskellVal :: Arg -> T.Text
argValToHaskellVal Arg {value = v} =
  mb <> go v
  where
    mb = "Maybe "
    go y =
      case y of
        TInt32 -> "Int"
        TInt53 -> "Int"
        TInt64 -> "Int"
        TBool -> "Bool"
        TString -> "Text"
        TBytes -> "ByteString"
        (TModule t) -> t
        (TVector x) -> "[" <> go x <> "]"

{-
data Func = Func
  { name :: T.Text,
    comment :: T.Text,
    args :: [Argument],
    returns :: T.Text
  }

data Argument = Argument
  { codeName :: T.Text,
    realName :: T.Text,
    type_ :: T.Text,
    comment :: Maybe T.Text
  }

methodToFunc :: Method -> Func
methodToFunc m =
  Func
    { name = m.name,
      comment = m.comment,
      returns = cname m.result,
      args = []
    }
  where
    cname (ClassName n) = n

    changeName n|n`elem` ["id","length"]
    -}
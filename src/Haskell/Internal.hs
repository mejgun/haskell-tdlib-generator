module Haskell.Internal (upFst, argValToHaskellVal, Func (..), Argument (..), methodToFunc, quoted, justify) where

import Data.Text qualified as T
import Parser (Arg (..), ArgVal (..), ClassName (ClassName), Method (..))

upFst :: T.Text -> T.Text
upFst text =
  let h = T.toUpper $ T.take 1 text
      t = T.tail text
   in h <> t

data Func = Func
  { nameInCode :: T.Text,
    nameReal :: T.Text,
    comment :: T.Text,
    args :: [Argument],
    returns :: T.Text
  }

data Argument = Argument
  { nameInCode :: T.Text,
    nameReal :: T.Text,
    nameTemp :: T.Text,
    typeInCode :: T.Text,
    toJsonFunc :: T.Text,
    comment :: Maybe T.Text
  }

methodToFunc :: Method -> Func
methodToFunc m =
  Func
    { nameInCode = upFst m.name,
      nameReal = m.name,
      comment = m.comment,
      returns = cname m.result,
      args = map convertArg m.args
    }
  where
    cname (ClassName n) = n

    convertArg :: Arg -> Argument
    convertArg a =
      Argument
        { nameReal = a.name,
          nameTemp = changeName a.name <> "_",
          nameInCode = changeName a.name,
          typeInCode = argValToHaskellVal a.value,
          toJsonFunc = argValToToJsonFunc a.value,
          comment = a.comment
        }

    changeName n
      | n `elem` ["id", "length"] = "_" <> n
      | otherwise = n

argValToToJsonFunc :: ArgVal -> T.Text
argValToToJsonFunc TInt64 = "U.toS "
argValToToJsonFunc _ = "x "

argValToHaskellVal :: ArgVal -> T.Text
argValToHaskellVal v =
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

quoted :: T.Text -> T.Text
quoted x = "\"" <> x <> "\""

justify :: Int -> T.Text -> T.Text
justify i = T.justifyLeft i ' '
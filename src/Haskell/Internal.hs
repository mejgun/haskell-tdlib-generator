module Haskell.Internal
  ( upFst,
    argValToHaskellVal,
    Func
      ( nameInCode,
        importsQualified,
        importsRaw,
        args,
        nameReal,
        comment
      ),
    Argument (..),
    methodToFunc,
    quoted,
    justify,
    DataClass (..),
    classToDataClass,
  )
where

import Data.HashMap.Strict qualified as HM
import Data.List qualified as L
import Data.Text qualified as T
import Parser
  ( Arg (..),
    ArgVal (..),
    Class (..),
    ClassName (ClassName),
    Method (..),
  )

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
    returns :: T.Text,
    importsQualified :: [(T.Text, T.Text)],
    importsRaw :: [T.Text]
  }

data DataClass = DataClass
  { name :: T.Text,
    comment :: T.Text,
    methods :: [DataMethod],
    importsQualified :: [(T.Text, T.Text)],
    importsRaw :: [T.Text]
  }

data DataMethod = DataMethod
  { nameInCode :: T.Text,
    nameReal :: T.Text,
    comment :: T.Text,
    args :: [Argument]
  }

data Argument = Argument
  { nameInCode :: T.Text,
    nameReal :: T.Text,
    nameTemp :: T.Text,
    typeInCode :: T.Text,
    toJsonFunc :: T.Text,
    comment :: Maybe T.Text
  }

classToDataClass :: Class -> [Method] -> DataClass
classToDataClass cl ms = do
  DataClass
    { name = cname cl.name,
      comment = cl.comment,
      methods = snd $ L.mapAccumL dataMethodToFunc initMap ms,
      importsQualified = [],
      importsRaw = []
    }

dataMethodToFunc :: ArgsMap -> Method -> (ArgsMap, DataMethod)
dataMethodToFunc acc m =
  let as = L.mapAccumL argToArgument acc m.args
   in ( fst as,
        DataMethod
          { nameInCode = upFst m.name,
            nameReal = m.name,
            comment = m.comment,
            args = snd as
          }
      )

cname :: ClassName -> T.Text
cname (ClassName n) = n

type ArgsMap = HM.HashMap T.Text ArgVal

argToArgument :: ArgsMap -> Arg -> (ArgsMap, Argument)
argToArgument acc a = do
  let (mp, nm) = findName acc a.name
   in ( mp,
        Argument
          { nameReal = a.name,
            nameTemp = nm <> "_",
            nameInCode = nm,
            typeInCode = argValToHaskellVal a.value,
            toJsonFunc = argValToToJsonFunc a.value,
            comment = a.comment
          }
      )
  where
    findName mp nm = case HM.lookup nm mp of
      (Just v)
        | v == a.value -> (acc, nm)
        | otherwise -> findName mp ("_" <> nm)
      Nothing -> (HM.insert nm a.value mp, nm)

initMap :: ArgsMap
initMap =
  HM.fromList
    [ ("id", TModule "non-existent"),
      ("length", TModule "non-existent")
    ]

methodToFunc :: Method -> Func
methodToFunc m =
  Func
    { nameInCode = upFst m.name,
      nameReal = m.name,
      comment = m.comment,
      returns = cname m.result,
      args = snd $ L.mapAccumL argToArgument initMap m.args,
      importsQualified =
        [ ("Data.Aeson", "A"),
          ("Data.Aeson.Types", "T"),
          ("Utils", "U")
        ],
      importsRaw = ["Data.Aeson ((.=))"]
    }

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
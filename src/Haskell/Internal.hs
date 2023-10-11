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
    DataMethod (..),
    classToDataClass,
    printNotEmpty,
    indent,
    Result,
  )
where

import Control.Monad.Writer (MonadWriter (tell), Writer)
import Data.HashMap.Strict qualified as HM
import Data.List (nub)
import Data.List qualified as L
import Data.Text qualified as T
import Parser
  ( Arg (..),
    ArgVal (..),
    Class (..),
    ClassName (ClassName),
    Method (..),
  )

type Result = Writer [T.Text] ()

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
    comment :: Maybe T.Text,
    methods :: [DataMethod],
    importsQualified :: [(T.Text, T.Text)],
    importsRaw :: [T.Text]
  }

data DataMethod = DataMethod
  { nameInCode :: T.Text,
    nameReal :: T.Text,
    nameTemp :: T.Text,
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

classToDataClass :: Maybe Class -> [Method] -> DataClass
classToDataClass cl ms =
  let clname = (head ms).result
   in DataClass
        { name = cname clname,
          comment = (.comment) <$> cl,
          methods = snd $ L.mapAccumL (dataMethodToFunc clname) initMap ms,
          importsQualified =
            [ ("Data.Aeson", "A"),
              ("Data.Aeson.Types", "AT"),
              ("Data.Text", "T"),
              ("Data.ByteString", "BS"),
              ("Utils", "U")
            ]
              ++ nub
                ( foldr
                    (getImport clname . (.value))
                    []
                    (foldr (\m acc -> m.args ++ acc) [] ms)
                ),
          importsRaw =
            [ "Data.Aeson ((.=))"
            ]
        }

dataMethodToFunc :: ClassName -> ArgsMap -> Method -> (ArgsMap, DataMethod)
dataMethodToFunc cln acc m =
  let as = L.mapAccumL (argToArgument cln) acc m.args
   in ( fst as,
        DataMethod
          { nameInCode = upFst m.name,
            nameReal = m.name,
            nameTemp = m.name <> "_",
            comment = m.comment,
            args = snd as
          }
      )

cname :: ClassName -> T.Text
cname (ClassName n) = n

type ArgsMap = HM.HashMap T.Text ArgVal

argToArgument :: ClassName -> ArgsMap -> Arg -> (ArgsMap, Argument)
argToArgument cln acc a = do
  let (mp, nm) = findName acc a.name
   in ( mp,
        Argument
          { nameReal = a.name,
            nameTemp = nm <> "_",
            nameInCode = nm,
            typeInCode = argValToHaskellVal cln a.value,
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
  HM.fromList $
    map
      (,TModule "non-existent")
      ["id", "length", "type", "data", "error"]

methodToFunc :: Method -> Func
methodToFunc m =
  Func
    { nameInCode = upFst m.name,
      nameReal = m.name,
      comment = m.comment,
      returns = cname m.result,
      args = snd $ L.mapAccumL (argToArgument m.result) initMap m.args,
      importsQualified =
        [ ("Data.Aeson", "A"),
          ("Data.Aeson.Types", "AT"),
          ("Data.Text", "T"),
          ("Data.ByteString", "BS"),
          ("Utils", "U")
        ]
          ++ foldr (getImport m.result . (.value)) [] m.args,
      importsRaw =
        [ "Data.Aeson ((.=))"
        ]
    }

getImport :: ClassName -> ArgVal -> [(T.Text, T.Text)] -> [(T.Text, T.Text)]
getImport (ClassName nm) (TModule modname) acc =
  let md = upFst modname
   in if md == nm
        then acc
        else ("TD.Data." <> md, md) : acc
getImport n (TVector v) acc = getImport n v acc
getImport _ _ acc = acc

argValToToJsonFunc :: ArgVal -> T.Text
argValToToJsonFunc TInt64 = "U.toS "
argValToToJsonFunc _ = ""

argValToHaskellVal :: ClassName -> ArgVal -> T.Text
argValToHaskellVal (ClassName nm) v =
  mb <> go v
  where
    mb = "Maybe "
    go y =
      case y of
        TInt32 -> "Int"
        TInt53 -> "Int"
        TInt64 -> "Int"
        TBool -> "Bool"
        TString -> "T.Text"
        TBytes -> "BS.ByteString"
        TDouble -> "Double"
        (TModule t) ->
          let upt = upFst t
           in if upt == nm then upt else upt <> "." <> upFst t
        (TVector x) -> "[" <> go x <> "]"

quoted :: T.Text -> T.Text
quoted x = "\"" <> x <> "\""

justify :: Int -> T.Text -> T.Text
justify i = T.justifyLeft i ' '

-- (indentantion, openinig prefix, middle, closing)
-- (fist val, second, comment)
printNotEmpty :: (Int, T.Text, T.Text, T.Text) -> [(T.Text, T.Text, Maybe T.Text)] -> Result
printNotEmpty _ [] = pure ()
printNotEmpty (ind, begin, loop, end) list =
  let (len1, len2) = foldr (\(a, b, _) (m1, m2) -> (max (T.length a) m1, max (T.length b) m2)) (1, 1) list
      h = head list
      t = tail list
      save pre (a, b, c) =
        let p1 = T.justifyLeft len1 ' ' a
            p2 = case c of
              (Just text) -> T.justifyLeft len2 ' ' b <> " " <> text
              Nothing -> b
         in tell
              [indent ind <> pre <> " " <> p1 <> " " <> p2]
   in do
        save begin h
        mapM_ (save loop) t
        tell [indent ind <> end]

indent :: Int -> T.Text
indent i = T.replicate i "  "

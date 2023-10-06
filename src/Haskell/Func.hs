module Haskell.Func (generateFunc) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (Argument (..), Func (..))

type Result = Writer [T.Text] ()

moduleName :: Func -> Result
moduleName x = tell ["module TD.Query." <> x.nameInCode <> " where"]

dataSection :: Func -> Result
dataSection x = do
  tell
    [ "-- | " <> x.comment,
      "data " <> x.nameInCode <> " = " <> x.nameInCode
    ]
  doIfArgs
    x.args
    (addField "{")
    (addField ",")
    ( tell
        [ indent 1 <> "}",
          indent 1 <> "deriving (Eq)"
        ]
    )
  where
    addField :: T.Text -> Argument -> Result
    addField pre a = do
      tell [indent 1 <> pre]
      case a.comment of
        (Just c) -> tell [indent 2 <> "-- | " <> c]
        _ -> pure ()
      tell [indent 2 <> a.nameInCode <> " :: " <> a.typeInCode]

doIfArgs ::
  [Argument] -> (Argument -> Result) -> (Argument -> Result) -> Result -> Result
doIfArgs as ffirst fbody ffinal = case as of
  [] -> pure ()
  (x : xs) -> do
    ffirst x
    mapM_ fbody xs
    ffinal

indent :: Int -> T.Text
indent i = T.replicate i "  "

toJsonSection :: Func -> Result
toJsonSection x = do
  tell
    [ "instance T.ToJSON " <> x.nameInCode <> " where",
      indent 1 <> "toJSON",
      indent 2 <> x.nameInCode
    ]
  doIfArgs
    x.args
    (addField "{")
    (addField ",")
    (tell [indent 3 <> "}"])
  tell [indent 4 <> "= A.object"]
  tell [indent 5 <> "[ \"@type\" A..= T.String \"" <> x.nameReal <> "\""]
  doIfArgs
    x.args
    addJsonField
    addJsonField
    (pure ())
  tell [indent 5 <> "]"]
  where
    addField :: T.Text -> Argument -> Result
    addField pre a =
      tell [indent 3 <> pre <> " " <> a.nameInCode <> " = " <> a.nameTemp]

    addJsonField :: Argument -> Result
    addJsonField a =
      tell [indent 5 <> ", \"" <> a.nameReal <> "\" A..= " <> a.nameTemp]

generateFunc :: Func -> T.Text
generateFunc m = T.unlines . execWriter $ do
  moduleName m
  tell [""]
  dataSection m
  tell [""]
  toJsonSection m
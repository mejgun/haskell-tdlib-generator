module Haskell.Func (generateFunc) where

import Control.Monad.Writer
import Data.Text qualified as T
import Haskell.Internal (upFst)
import Parser (Arg (..), Method (..))

moduleName :: T.Text -> Writer [T.Text] ()
moduleName t = tell ["module TD.Query." <> upFst t <> " where", ""]

dataSection :: Method -> Writer [T.Text] ()
dataSection m = do
  tell
    [ "-- | " <> m.comment,
      "data " <> upFst m.name <> " = " <> upFst m.name
    ]
  case m.args of
    [] -> pure ()
    (x : xs) -> do
      addField "{ " x
      mapM_ (addField ", ") xs
  tell
    [ "",
      indent <> "} deriving (Eq)"
    ]
  where
    indent = "  "

    addField pre a = do
      case a.comment of
        (Just c) -> tell ["", indent <> pre <> "-- | " <> c]
        _ -> tell [pre]
      tell [indent <> "  " <> a.name <> " = " <> a.name]

generateFunc :: Method -> T.Text
generateFunc m = T.unlines . execWriter $ do
  moduleName m.name
  dataSection m
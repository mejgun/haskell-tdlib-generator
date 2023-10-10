module Save (writeData, writeFuncs) where

import Data.List (find, groupBy)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Haskell.Data (generateData)
import Haskell.Func (generateFunc)
import Haskell.Internal (classToDataClass, methodToFunc, upFst)
import Parser (Class (..), ClassName (ClassName), Method (..), result)

dataDir :: String
dataDir = "/TD/Data/"

funcDir :: String
funcDir = "/TD/Query/"

writeData :: FilePath -> [Class] -> [Method] -> IO ()
writeData path classes methods = mapM_ (save . f) listwClass
  where
    list :: [[Method]]
    list = groupBy (\a b -> a.result == b.result) methods

    listwClass :: [(ClassName, Maybe Class, [Method])]
    listwClass =
      map
        ( \ms ->
            let m = head ms
             in ( m.result,
                  find (\c -> c.name == m.result) classes,
                  ms
                )
        )
        list

    f :: (ClassName, Maybe Class, [Method]) -> (ClassName, T.Text)
    f (name, mbc, ms) =
      (name, generateData (classToDataClass mbc ms))

    save :: (ClassName, T.Text) -> IO ()
    save (c, text) = TIO.writeFile (fileName c) text

    fileName :: ClassName -> FilePath
    fileName (ClassName name) = path <> dataDir <> T.unpack name <> ".hs"

writeFuncs :: FilePath -> [Method] -> IO ()
writeFuncs path = mapM_ (save . f)
  where
    f :: Method -> (T.Text, T.Text)
    f m@Method {name = name} = (name, generateFunc (methodToFunc m))

    save :: (T.Text, T.Text) -> IO ()
    save (name, text) = TIO.writeFile (fileName name) text

    fileName :: T.Text -> FilePath
    fileName n = path <> funcDir <> T.unpack (upFst n) <> ".hs"

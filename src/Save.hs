module Save (writeData, writeFuncs) where

import Data.List (find, groupBy)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Haskell.Data (generateBoot, generateData)
import Haskell.Func (generateFunc)
import Haskell.Internal (classToDataClass, methodToFunc, upFst)
import Parser (Arg (..), ArgVal (..), Class (..), ClassName (ClassName), Method (..), result)

dataDir :: String
dataDir = "/TD/Data/"

funcDir :: String
funcDir = "/TD/Query/"

writeData :: FilePath -> [Class] -> [Method] -> IO ()
writeData path classes methods = do
  mapM_ (save ".hs" . f) listwClass
  mapM_ (save ".hs-boot") $ generateBoot importsList
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

    importsList :: [(ClassName, [ClassName])]
    importsList =
      map
        (\(n, _, ms) -> (n, concatMap getModules ms))
        listwClass
      where
        go (TModule mn) = [ClassName (upFst mn)]
        go (TVector v) = go v
        go _ = []

        getModules :: Method -> [ClassName]
        getModules m =
          concatMap (\a -> go a.value) m.args

    f :: (ClassName, Maybe Class, [Method]) -> (ClassName, T.Text)
    f (name, mbc, ms) =
      (name, generateData (classToDataClass mbc ms))

    save :: String -> (ClassName, T.Text) -> IO ()
    save suffix (c, text) = TIO.writeFile (fileName c suffix) text

    fileName :: ClassName -> String -> FilePath
    fileName (ClassName name) suffix =
      path <> dataDir <> T.unpack name <> suffix

writeFuncs :: FilePath -> [Method] -> IO ()
writeFuncs path = mapM_ (save . f)
  where
    f :: Method -> (T.Text, T.Text)
    f m@Method {name = name} = (name, generateFunc (methodToFunc m))

    save :: (T.Text, T.Text) -> IO ()
    save (name, text) = TIO.writeFile (fileName name) text

    fileName :: T.Text -> FilePath
    fileName n = path <> funcDir <> T.unpack (upFst n) <> ".hs"
